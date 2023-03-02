(defpackage #:coalton-impl/typechecker/type-errors
  (:use
   #:cl
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error))
  (:import-from
   #:coalton-impl/error
   #:coalton-internal-type-error)
  (:export
   #:coalton-internal-type-error)
  (:export
   #:unknown-binding-error                     ; CONDITION
   #:unification-error                         ; CONDITION
   #:infinite-type-unification-error           ; CONDITION
   #:type-declaration-too-general-error        ; CONDITION
   #:type-construction-error                   ; CONDITION
   #:kind-mismatch-error                       ; CONDITION
   #:type-kind-mismatch-error                  ; CONDITION
   #:invalid-operator-type-error               ; CONDITION
   #:predicate-unification-error               ; CONDITION
   #:overlapping-instance-error                ; CONDITION
   #:instance-missing-context-error            ; CONDITION
   #:cyclic-class-definitions-error            ; CONDITION
   #:invalid-typed-node-type                   ; CONDITION
   #:unknown-type-variable                     ; CONDITION
   #:unknown-constructor                       ; CONDITION
   #:invalid-constructor-arguments             ; CONDITION
   #:context-reduction-failure                 ; CONDITION
   #:weak-context-error                        ; CONDITION
   #:recursive-binding-error                   ; CONDITION
   #:self-recursive-toplevel-form              ; CONDITION
   #:self-recursive-non-constructor-call       ; CONDITION
   #:self-recursive-partial-application        ; CONDITION
   #:self-recursive-non-default-repr           ; CONDITION
   #:mutually-recursive-function-and-data      ; CONDITION
   #:declared-type-missing-predicates          ; CONDITION
   #:declared-type-additional-predicates       ; CONDITION
   #:toplevel-monomorphism-restriction         ; CONDITION
   #:ambigious-constraint                      ; CONDITION
   #:ambigious-constraint-pred                 ; ACCESSOR
   #:unresolvable-constraint                   ; CONDITION
   #:duplicate-definition                      ; CONDITION
   #:unexpected-return                         ; CONDITION
   #:duplicate-ctor                            ; CONDITION
   #:coalton-type-parse-error                  ; CONDITION
   #:error-parsing-type                        ; FUNCTION
   #:fundep-conflict                           ; CONDITION
   #:fundep-variable-error                     ; CONDITION
   #:ambigious-constraint-variables            ; CONDITION
   #:invalid-fundep-change                     ; CONDITION
   #:overlapping-specialization-error          ; CONDITION
   #:overlapping-specialization-error-new      ; ACCESSOR
   #:overlapping-specialization-error-existing ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/type-errors)

(define-condition unification-error (error:coalton-internal-type-error)
  ((type1 :initarg :type1
          :reader unification-error-type1)
   (type2 :initarg :type2
          :reader unification-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Failed to unify types ~A and ~A"
               (unification-error-type1 c)
               (unification-error-type2 c))))))

(define-condition infinite-type-unification-error (error:coalton-internal-type-error)
  ((type :initarg :type
         :reader infinite-type-unification-error-type))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Cannot construct infinite type by unifying ~A with internal variable."
               (infinite-type-unification-error-type c))))))


(define-condition kind-mismatch-error (error:coalton-internal-type-error)
  ((type :initarg :type
         :reader kind-mismatch-error-type)
   (kind :initarg :kind
         :reader kind-mismatch-error-kind))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Kind mismatch between type ~A of kind ~A and kind ~A"
               (kind-mismatch-error-type c)
               (kind-of (kind-mismatch-error-type c))
               (kind-mismatch-error-kind c))))))

(define-condition type-kind-mismatch-error (error:coalton-internal-type-error)
  ((type1 :initarg :type1
          :reader type-kind-mismatch-error-type1)
   (type2 :initarg :type2
          :reader type-kind-mismatch-error-type2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Kind mismatch between type ~A of kind ~A and type ~A kind ~A"
               (type-kind-mismatch-error-type1 c)
               (kind-of (type-kind-mismatch-error-type1 c))
               (type-kind-mismatch-error-type2 c)
               (kind-of (type-kind-mismatch-error-type2 c)))))))


(define-condition predicate-unification-error (error:coalton-internal-type-error)
  ((pred1 :initarg :pred1
          :reader unification-error-pred1)
   (pred2 :initarg :pred2
          :reader unification-error-pred2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Failed to unify types ~A and ~A"
               (unification-error-pred1 c)
               (unification-error-pred2 c))))))

(define-condition overlapping-instance-error (error:coalton-internal-type-error)
  ((inst1 :initarg :inst1
          :reader overlapping-instance-error-inst1)
   (inst2 :initarg :inst2
          :reader overlapping-instance-error-inst2))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Instance ~A overlaps with instance ~A"
               (overlapping-instance-error-inst1 c)
               (overlapping-instance-error-inst2 c))))))

(define-condition ambigious-constraint (error:coalton-internal-type-error)
  ((pred :initarg :pred
         :reader ambigious-constraint-pred))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Ambigious constraint ~A~%"
               (ambigious-constraint-pred c))))))

(define-condition fundep-conflict (error:coalton-internal-type-error)
  ((new-pred :initarg :new-pred
             :reader fundep-conflict-new-pred
             :type ty-predicate)
   (old-pred :initarg :old-pred
             :reader fundep-conflict-old-pred
             :type ty-predicate)
   (fundep   :initarg :fundep
             :reader fundep-conflict-fundep
             :type t)
   (class    :initarg :class
             :reader fundep-conflict-class
             :type symbol)
   (class-vars :initarg :class-vars
               :reader fundep-conflict-class-vars
               :type util:symbol-list)
   (class-fundeps :initarg :class-fundeps
                  :reader fundep-conflict-class-fundeps
                  :type t)
   (old-from-tys :initarg :old-from-tys
                 :reader fundep-conflict-old-from-tys
                 :type ty-list)
   (new-from-tys :initarg :new-from-tys
                 :reader fundep-conflict-new-from-tys
                 :type ty-list)
   (old-to-tys :initarg :old-to-tys
               :reader fundep-conflict-old-to-tys
               :type ty-list)
   (new-to-tys :initarg :new-to-tys
               :reader fundep-conflict-new-to-tys
               :type ty-list))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (with-pprint-variable-context ()
         (format s "Instance ~A conflicts previous instance ~A~%the fundep ~S on class ~S~{ ~S~}~{ ~S~}~%binds~{ ~A~} to~{ ~A~}~%which conflicts with the previous binding of~{ ~A~} to~{ ~A~}"
                 (fundep-conflict-new-pred c)
                 (fundep-conflict-old-pred c)
                 (fundep-conflict-fundep c)
                 (fundep-conflict-class c)
                 (fundep-conflict-class-vars c)
                 (fundep-conflict-class-fundeps c)
                 (fundep-conflict-new-from-tys c)
                 (fundep-conflict-new-to-tys c)
                 (fundep-conflict-old-from-tys c)
                 (fundep-conflict-old-to-tys c)))))))

(define-condition overlapping-specialization-error (error:coalton-internal-type-error)
  ((new :initarg :new
        :reader overlapping-specialization-error-new)
   (existing :initarg :existing
             :reader overlapping-specialization-error-existing)))
