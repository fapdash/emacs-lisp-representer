;;; representer.el --- Exercism Emacs Lisp Representer  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.4") (treepy "20230715.2154"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'treepy)
(require 'symbols-from-obarray)

(defun exercism/represent (exercise-slug input-dir output-dir)
  (let* ((expressions-symbols-replaced
          (exercism//represent input-dir exercise-slug)))
    (with-temp-file (file-name-concat output-dir "mapping.json")
      (insert (json-encode (exercism//placeholders->alist)))
      (json-pretty-print-buffer-ordered)
      (goto-char (point-max))
      (newline))
    (with-temp-file (file-name-concat output-dir "representation.txt")
      (pp-emacs-lisp-code expressions-symbols-replaced))
    (with-temp-file (file-name-concat output-dir
                                      "representation.json")
      (insert (json-encode '(("version" . 1))))
      (json-pretty-print-buffer))))

(defun exercism//represent (input-dir exercise-slug)
  (let ((symbols-not-to-replace
         (exercism//find-all-defined-symbols
          (file-name-concat input-dir
                            (concat exercise-slug "-test.el")))))
    (thread-first
      (exercism//file-to-string
       (file-name-concat input-dir (concat exercise-slug ".el")))
      (exercism//read-all-from-string)
      (exercism//macroexpand-all)
      (exercism//replace-symbols-with-placeholders
       symbols-not-to-replace))))

(defun exercism//file-to-string (file)
  "Convert FILE to string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun exercism//read-all-from-string (string)
  (let* ((result '())
         current-final-index)
    (condition-case _error
        (while t
          (cl-destructuring-bind (object-read . final-string-index)
              (read-from-string string current-final-index)
            (setq result (cons object-read result))
            (setq current-final-index final-string-index))
          result)
      (end-of-file))
    (nreverse result)))

(defun exercism//macroexpand-all (expressions)
  (mapcar
   (lambda (expression) (macroexpand-all expression)) expressions))

(defun exercism//find-all-defined-symbols (test-file)
  "Find all symbols defined in the current Emacs environment
and symbols from the test file."
  (let ((test-file-expressions
         (exercism//read-all-from-string
          (exercism//file-to-string test-file)))
        (symbols (exercism//symbols-from-obarray)))
    (treepy-prewalk
     (lambda (ele)
       (cond
        ((and (listp ele) (eq 'declare-function (car ele)))
         nil)
        ((symbolp ele)
         (puthash ele t symbols))
        (t
         ele)))
     test-file-expressions)
    symbols))

(defun exercism//replace-symbols-with-placeholders
    (expressions symbols-not-to-replace)
  (treepy-prewalk
   (lambda (ele)
     (cond
      ((and (symbolp ele)
            (or (exercism//symbol-is-keyword-p ele)
                (gethash ele symbols-not-to-replace)))
       ele)
      ((and (symbolp ele))
       ele
       (exercism//add-placeholder ele))
      (t
       ele)))
   expressions))

(defun exercism//symbol-is-keyword-p (symbol)
  (eq (aref (symbol-name symbol) 0) ?&))


(defvar exercism//placeholders '())
(defvar exercism//counter 0)

(defun exercism//find-placeholder (symbol)
  (car (rassoc symbol exercism//placeholders)))

(defun exercism//find-original-symbol (placeholder)
  (assoc placeholder exercism//placeholders))

(defun exercism//new-placeholder ()
  (prog1 (intern (format "PLACEHOLDER-%d" exercism//counter))
    (cl-incf exercism//counter)))

(defun exercism//add-placeholder (symbol)
  (if (and symbol (symbolp symbol))
      (let ((existing (exercism//find-placeholder symbol)))
        (or existing
            (let ((new-symbol (exercism//new-placeholder)))
              (setf exercism//placeholders
                    (cl-acons
                     new-symbol symbol exercism//placeholders))
              new-symbol)))
    symbol))

(defun exercism//placeholders->alist ()
  (mapcar
   #'(lambda (acons)
       (cons
        (prin1-to-string (car acons) t)
        (prin1-to-string (cdr acons) t)))
   exercism//placeholders))


(provide 'representer)
;;; representer.el ends here
