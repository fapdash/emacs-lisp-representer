;; -*- lexical-binding: t; -*-

(require 'mode-local)
(require 'cl-lib)
(require 'eieio)
(require 'package)
(require 'generator)

(defvar exercism-var1 nil
  "Hello.")

(defconst exercism-var2 nil
  "Hello.")

(defvar-1 'exercism-var3 nil "Hello.")

(defvar-local exercism-var4 nil
  "Hello.")

(defvar-mode-local exercism-var5 nil "Hello.")

(defconst-1 'exercism-var6 nil "Hello.")

(defconst-mode-local exercism-var7 nil "Hello.")

(defalias 'exercism-alias 'defun "Hello.")

(defvaralias 'exercism-foo 'exercism-bar "Hello.")

(define-obsolete-variable-alias 'exercism-foo 'exercism-bar "Hello.")

(oclosure-define exercism-oclosure
  "Hello.")

(autoload 'defun "foobar" "Hello.")

(define-generic-mode
 'foo-mode ;; name of the mode to create
 '("#") ;; comments start with '!!'
 '("for" "if" "else") ;; some keywords
 '(("=" . 'font-lock-operator) ;; '=' is an operator
   ("+" . 'font-lock-operator) ;; '=' is an operator
   ("-" . 'font-lock-operator) ;; '=' is an operator
   ("'" . 'font-lock-builtin) ;; '=' is an operator
   ("<-" . 'font-lock-operator) ;; '=' is an operator
   ("*" . 'font-lock-operator) ;; '=' is an operator
   ("/" . 'font-lock-operator) ;; '=' is an operator
   ("," . 'font-lock-builtin) ;; ';' is a built-in
   (";" . 'font-lock-builtin)) ;; ';' is a built-in
 '("\\.myext$") ;; files for which to activate this mode
 nil ;; other functions to call
 "A generic mode for myext files") ;; doc string for this mode


(define-abbrev-table 'global-abbrev-table
  '(("afaict" "as far as I can tell" nil 1)
    ("omuse" "http://www.emacswiki.org/cgi-bin/oddmuse.pl" nil 0)
    ("btw" "by the way" nil 3)
    ("wether" "whether" nil 5)
    ("ewiki" "http://www.emacswiki.org/cgi-bin/wiki.pl" nil 3)
    ("pov" "point of view" nil 1))
  "Hello.")

(define-package "foobar" "0.1" "Hello.")

(lambda () "Returning a string literal")

(lambda ()
  "Hello."
  (+ 4 5))

(lambda ()
  "Hello."
  (interactive)
  (read-string "Question?"))

(lambda ()
  (interactive)
  (read-string "Still interactive?"))


(lambda (foo bar baz)
  "Hello."
  (+ foo bar baz 42))

(lambda (foo bar baz)
  "Hello."
  (interactive)
  (+ foo bar baz 42)
  (read-string "Input with interactive"))


(defsubst exercism-subst1 ()
  "Hello.")

(defsubst exercism-subst2 ()
  "Hello."
  "Returning a string")

(defsubst exercism-subst3 ()
  "Hello."
  (declare)
  "Returning a string")

(defsubst exercism-subst4 ()
  "Hello."
  (declare)
  (+ 1 2))

(defsubst exercism-subst5 (foo bar)
  "Hello."
  (declare)
  (+ foo bar 42))

(define-inline exercism-inline1 () "Hello.")

(iter-defun exercism-iter1 ()
  "Hello.")

(define-derived-mode
 exercism-derived-mode1 text-mode "Exercism Derived Mode 1" "Hello.")

(define-category -1 "Hello.")

(defun exercism-function1 ()
  "Hello.")

(cl-defun exercism-cl-function1 ()
  "Hello.")

(exercism-cl-function1)

(cl-defun exercism-cl-function2 ()
  "Hello."
  (+ 1 2))


(cl-defgeneric exercism-generic1 ()
  "Hello.")

(cl-defmethod exercism-method1 ()
  "Hello.")

(cl-defmethod exercism-method2 :extra
  "foo"
  ()
  "Hello.")

(cl-defmethod exercism-method3 :after
  ()
  "Hello.")

(cl-defstruct exercism-struct1
  "Hello.")

(cl-defstruct exercism-struct2
  "Hello."
  name
  age)

(cl-defstruct (person
               (:constructor create-person) (:type list)
               :named)
  "Hello."
  (name nil :documentation "The name of the person")
  (age 1 :documentation "The age of the person"))

(defsubst exercism-defsubst1 ()
  "Hello.")

(cl-defsubst exercism-cl-defsubst1 ()
  "Hello.")

(defmacro exercism-macro1 ()
  "Hello.")

(pcase-defmacro exercism-pcase-macro1 ()
  "Hello.")

(defclass exercism-class1 () () "Hello.")

(defclass
 person ()
 ((name
   :initarg
   :name
   :initform ""
   :type string
   :custom string
   :documentation "The name of a person.")
  (birthday
   :initarg
   :birthday
   :initform "Jan 1, 1970"
   :custom string
   :type string
   :documentation "The person's birthday.")
  (phone :initarg :phone :initform "" :documentation "Phone number."))
 "A class for tracking people I know.")

(defclass
 employee (person)
 ((company
   :initarg
   :company
   :initform ""
   :type string
   :custom string
   :documentation "The company the person works for.")
  (manager
   :initarg
   :manager
   :initform ""
   :type string
   :custom string
   :documentation "The person who can fire me."))
 "A class for tracking my wage slave details.")
