;;;; advent_of_code_2020.lisp

(in-package #:advent_of_code_2020)


(defparameter *project-dir* (asdf:system-source-directory :advent_of_code_2020))

(defun get-file-pathname (problem-name)
  (merge-pathnames *project-dir* (make-pathname :name
                                             (format nil "data/~A" problem-name)
                                             :type "txt")))
