;;; robot-name.el --- Robot Name (exercism)

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

(require 'cl-lib)

(defvar exercisms--robohash (make-hash-table)
  "Hash table that holds all created robots.")
(defvar exercisms--sequence 0
  "Counter used to create unique robot names.")

(defun build-robot ()
  (let ((random (generate-name)))
    (while (gethash random exercisms--robohash)
      (setq random (generate-name)))
    (cl-incf exercisms--sequence)
    (puthash exercisms--sequence random exercisms--robohash)
    exercisms--sequence))

(defun robot-name (name)
  (gethash name exercisms--robohash))

(defun reset-robot (robot)
  (puthash robot (generate-name) exercisms--robohash))

(defun generate-name ()
  (format "%c%c%03d"
          (+ ?A (random 26))
          (+ ?A (random 26))
          (random 1000)))

(provide 'robot-name)
;;; robot-name.el ends here
