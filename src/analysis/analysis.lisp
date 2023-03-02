(defpackage #:coalton-impl/analysis/analysis
  (:use
   #:cl
   #:coalton-impl/analysis/pattern-exhaustiveness)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:export
   #:analyze-translation-unit))

(in-package #:coalton-impl/analysis/analysis)

(define-condition non-exhaustive-match-warning (error:coalton-base-warning)
  ())

(define-condition useless-pattern-warning (error:coalton-base-warning)
  ())

(defun analyze-translation-unit (translation-unit env file)
  "Perform analysis passes on TRANSLATION-UNIT, potentially producing errors or warnings."
  (declare (type tc:translation-unit translation-unit)
           (type tc:environment env)
           (type error:coalton-file file))

  (let ((analysis-traverse-block
          (tc:make-traverse-block
           :match (lambda (node)
                    (let ((patterns (mapcar #'tc:node-match-branch-pattern (tc:node-match-branches node))))
                      (let ((exhaustive-or-missing
                              (find-non-matching-value (mapcar #'list patterns) 1 env)))
                        (unless (eq t exhaustive-or-missing)
                          (warn
                           'non-exhaustive-match-warning
                           :err (error:coalton-error
                                 :type :warn
                                 :file file
                                 :span (tc:node-source node)
                                 :message "Non-exhaustive match"
                                 :primary-note "non-exaustive match"
                                 :notes (when (first exhaustive-or-missing)
                                          (list
                                           (error:make-coalton-error-note
                                            :type :secondary
                                            :span (tc:node-source node)
                                            :message (format nil "Missing case ~W" (first exhaustive-or-missing))))))))
                        (loop :for pattern :in patterns
                              :unless (useful-pattern-p patterns pattern env) :do
                                (warn
                                 'useless-pattern-warning
                                 :err (error:coalton-error
                                       :type :warn
                                       :file file
                                       :span (tc:pattern-source pattern)
                                       :message "Useless match case"
                                       :primary-note "useless match case"
                                       :notes (list (error:make-coalton-error-note
                                                     :type :secondary
                                                     :span (tc:node-source node)
                                                     :message "in this match")))))))
                    node))))

    ;; Run analysis on definitions
    (loop :for define :in (tc:translation-unit-definitions translation-unit) :do
      (tc:traverse (tc:toplevel-define-body define) analysis-traverse-block))
    ;; Run analysis on instance definitions
    (loop :for instance :in (tc:translation-unit-instances translation-unit) :do
      (loop :for method :being :the :hash-value :of (tc:toplevel-define-instance-methods instance) :do
        (tc:traverse (tc:instance-method-definition-body method) analysis-traverse-block)))))
