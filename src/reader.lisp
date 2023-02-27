(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defun read-coalton-toplevel-open-paren (stream char)
  (declare (optimize (debug 2)))

  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (flet ((maybe-read-form (stream)
           (loop :do
             ;; On empty lists report nothing
             (when (eq #\) (peek-char t stream))
               (read-char stream)
               (return (values nil nil)))

             ;; Otherwise, try to read in the next form
             (multiple-value-bind (form type)
                 (eclector.reader:call-as-top-level-read
                  eclector.base:*client*
                  (lambda ()
                    (eclector.reader:read-maybe-nothing
                     eclector.base:*client*
                     stream
                     nil 'eof))
                  stream
                  nil 'eof
                  nil)

               ;; Return the read form when valid
               (when (eq :object type)
                 (return (values form t)))))))

    (let ((first-form
            (multiple-value-bind (form presentp)
                (maybe-read-form stream)
              (unless presentp
                (return-from read-coalton-toplevel-open-paren
                  nil))
              form)))
      (case first-form
        (coalton:coalton-toplevel
          (let* ((pathname (or *compile-file-truename* *load-truename*))
                 (filename (if pathname (namestring pathname) "<unknown>"))

                 (file-input-stream
                   (cond
                     ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                          nil)
                      (open (pathname stream)))
                     (t
                      stream)))
                 (file (error:make-coalton-file :stream file-input-stream :name filename)))

            (handler-bind
                ((parser:parse-error
                   (lambda (c)
                     (set-highlight-position-for-error stream (funcall (parser:parse-error-err c)))))
                 (tc:tc-error
                   (lambda (c)
                     (set-highlight-position-for-error stream (funcall (tc:tc-error-err c))))))
              (multiple-value-bind (program env)
                  (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
                (setf entry:*global-environment* env)
                program))))

        (coalton:coalton
         (let* ((pathname (or *compile-file-truename* *load-truename*))
                (filename (if pathname (namestring pathname) "<unknown>"))

                (file-input-stream
                  (cond
                    ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                         nil)
                     (open (pathname stream)))
                    (t
                     stream)))
                (file (error:make-coalton-file :stream file-input-stream :name filename)))

           (handler-bind
               ((parser:parse-error
                  (lambda (c)
                    (set-highlight-position-for-error stream (funcall (parser:parse-error-err c)))))
                (tc:tc-error
                  (lambda (c)
                    (set-highlight-position-for-error stream (funcall (tc:tc-error-err c))))))
             (entry:expression-entry-point (parser:read-expression stream file) file))))

        ;; Fall back to reading the list manually
        (t
         (let ((collected-forms (list first-form))
               (dotted-context nil))
           (loop :do
             (handler-case
                 (multiple-value-bind (form presentp)
                     (maybe-read-form stream)

                   (cond
                     ((and (not presentp)
                           dotted-context)
                      (error "Invalid dotted list"))

                     ((not presentp)
                      (return-from read-coalton-toplevel-open-paren
                        (nreverse collected-forms)))

                     (dotted-context
                      (when (nth-value 1 (maybe-read-form stream))
                        (error "Invalid dotted list"))

                      (return-from read-coalton-toplevel-open-paren
                        (nreconc collected-forms form)))

                     (t
                      (push form collected-forms))))
               (eclector.reader:invalid-context-for-consing-dot (c)
                 (when dotted-context
                   (error "Invalid dotted list"))
                 (setf dotted-context t)
                 (eclector.reader:recover c))))))))))

(defun set-highlight-position-for-error (stream error)
  "Set the highlight position within the editor using implementation specific magic."
  #+sbcl
  ;; We need some way of setting FILE-POSITION so that
  ;; when Slime grabs the location of the error it
  ;; highlights the correct form.
  ;;
  ;; In SBCL, we can't unread more than ~512
  ;; characters due to limitations with ANSI streams,
  ;; which breaks our old method of unreading
  ;; characters until the file position is
  ;; correct. Instead, we now patch in our own
  ;; version of FILE-POSITION.
  ;;
  ;; This is a massive hack and might start breaking
  ;; with future changes in SBCL.
  (when (typep stream 'sb-impl::form-tracking-stream)
    (let* ((file-offset
             (- (sb-impl::fd-stream-get-file-position stream)
                (file-position stream)))
           (loc (error:coalton-error-location error)))
      (setf (sb-impl::fd-stream-misc stream)
            (lambda (stream operation arg1)
              (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                  (+ file-offset loc 1)
                  (sb-impl::tracking-stream-misc stream operation arg1)))))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-open-paren)
  (:macro-char #\` #'(lambda (s c)
                       (let ((*coalton-reader-allowed* nil))
                         (funcall (get-macro-character #\` (named-readtables:ensure-readtable :standard)) s c))))
  (:macro-char #\, #'(lambda (s c)
                       (let ((*coalton-reader-allowed* t))
                         (funcall (get-macro-character #\, (named-readtables:ensure-readtable :standard)) s c)))))

(defmacro coalton:coalton-toplevel (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*)))
        (stream (make-string-input-stream (cl:format cl:nil "(~S ~{~S~%~})" 'coalton:coalton-toplevel forms))))
    (cl:read stream)))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON (~A)" *compile-file-truename*)))
        (stream (make-string-input-stream (cl:format cl:nil "(~S ~{~S~%~})" 'coalton:coalton forms))))
    (cl:read stream)))
