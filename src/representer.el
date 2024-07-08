;;; representer.el --- Exercism Emacs Lisp Representer  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.4") (treepy "20230715.2154"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)
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
      (json-pretty-print-buffer)
      (goto-char (point-max))
      (newline))))

(defun exercism//represent (input-dir exercise-slug)
  (let ((symbols-not-to-replace
         (exercism//find-all-defined-symbols
          (file-name-concat input-dir
                            (concat exercise-slug "-test.el")))))
    (thread-first
      (exercism//file-to-string
       (file-name-concat input-dir (concat exercise-slug ".el")))
      (exercism//read-all-from-string)
      (exercism//remove-docstrings)
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

(defun exercism//remove-docstrings (expressions)
  (treepy-prewalk
   (lambda (ele)
     (cond
      ((and (listp ele)
            (length> ele 3)
            (seq-some
             (lambda (symbol) (eq symbol (car ele)))
             '(defvar defconst defvar-1
                defvar-local
                defvar-mode-local
                defconst-1
                defconst-mode-local))
            (stringp (nth 3 ele)))
       (seq-remove-at-position ele 3))
      (t
       ele)))
   expressions))

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
      ;; check for ~#'(lambda ...)~
      ((and (listp ele)
            (length> ele 1)
            (eq (car ele) 'function)
            (listp (nth 1 ele))
            (length> (nth 1 ele) 2))
       (let* ((lambda-expr (nth 1 ele))
              (doc-string (nth 2 lambda-expr)))
         (if (stringp doc-string)
             (exercism//remove-nth-element 2 (nth 1 ele))))
       ele)
      ((and (symbolp ele))
       (exercism//add-placeholder ele))
      (t
       ele)))
   expressions))

(defun exercism//remove-nth-element (nth list)
  (if (zerop nth)
      (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

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
