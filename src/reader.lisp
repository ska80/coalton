(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defvar *coalton-inside-reader* nil)

(defconstant +coalton-dot-value+ 'dot)

;; TODO: Read the first form then condition on that. Don't do stream-level stuff.

(defun read-coalton-toplevel-open-paren (stream char)
  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall #'#.(get-macro-character #\() stream char)))
  
  (let ((first-form (read stream)))
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
               (file (parser:make-coalton-file :stream file-input-stream :name filename)))

          (handler-case
              (let ((program (parser:read-program stream file :mode :toplevel-macro)))
                (multiple-value-bind (program env)
                    (entry:entry-point program)
                  (setf entry:*global-environment* env)
                  `(progn
                     #+ignore
                     (setf entry:*global-environment* ,env)
                     ,program)))
            (parser:parse-error (c)
              (set-highlight-position-for-error stream (parser:parse-error-err c))
              (error c))
            (tc:tc-error (c)
              (set-highlight-position-for-error stream (tc:tc-error-err c))
              (error c)))))
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
              (file (parser:make-coalton-file :stream file-input-stream :name filename))

              (expression
                (handler-case
                    (parser:read-expression stream file)

                  (parser:parse-error (c)
                    (set-highlight-position-for-error stream (parser:parse-error-err c))
                    (error c))
                  (tc:tc-error (c)
                    (set-highlight-position-for-error stream (tc:tc-error-err c))
                    (error c)))))
         `(format t "~A" ,(format nil "~A" expression))))

      ;; Fall back to the default open paren reader
      (t
       (let* ((*coalton-inside-reader* t)
              (*readtable* (named-readtables:ensure-readtable 'coalton:coalton))

              (rest-forms (read-delimited-list #\) stream)))

         (cond
           ((eq +coalton-dot-value+ (car rest-forms))
            (unless (null (cddr rest-forms))
              (error "Invalid dotted list"))

            (cons first-form (cadr rest-forms)))
           (t
            (cons first-form rest-forms))))))))

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
           (loc (parser:coalton-error-location error)))
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
                         (funcall #'#.(get-macro-character #\`) s c))))
  (:macro-char #\, #'(lambda (s c)
                       (let ((*coalton-reader-allowed* t))
                         (funcall #'#.(get-macro-character #\,) s c))))
  (:macro-char #\. #'(lambda (s c)
                       (cond
                         ((and *coalton-inside-reader*
                               (not (eql (peek-char nil s) (peek-char t s))))
                          +coalton-dot-value+)
                         (t
                          (unread-char c s)
                          (let ((*readtable* (named-readtables:ensure-readtable :standard)))
                            (read s)))))))

(defmacro coalton:coalton-toplevel (&rest forms)
  ;; lol.
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*))))
    (cl:read-from-string (cl:format cl:nil "(COALTON-TOPLEVEL ~{~S~%~})" forms))))
