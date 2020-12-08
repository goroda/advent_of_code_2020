(in-package :q7)

(defun parse-rule (line)
  (let* ((words (cl-ppcre:split " " line))
         (name (concatenate 'string (first words) (second words)))
         (rest (cadr (cl-ppcre:split " contain " line)))
         (split (cl-ppcre:split ", " rest))
         (out (list name)))
    (unless (string= "no other bags." (car split))
      (loop for words in split
            for contains = (cl-ppcre:split " " words)
            do (let ((first-num (parse-integer (car contains)))
                     (first-name (concatenate 'string
                                              (second contains)
                                              (third contains))))
                 (setq out (push (cons first-name first-num) out)))))
    (reverse out)))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-rule line))))

(defun form-tree (res)
  (let ((parents (make-hash-table :test 'equal))
        (children (make-hash-table :test 'equal)))
    (loop for stuff in res
          do (setf (gethash (car stuff) parents) (list))
             (setf (gethash (car stuff) children)
                   (cdr stuff)))
    (loop for stuff in res
          do (loop for child in (cdr stuff)
                   do (setf (gethash (car child) parents)
                            (push (cons (car stuff) (cdr child))
                                  (gethash (car child) parents)))))
    (values parents children)))

(defun walk-tree-backward (tree name num visited)
  (let ((parents (gethash name tree)))
    (if (null parents)
        (list num visited)
        (loop for (parent . num) in parents
              unless (find parent visited :test #'string=)
                do (setq visited (push parent visited))
                   (let ((res (walk-tree-backward tree parent
                                                  (1+ num) visited)))
                     (setq num (car res))
                     (setq visited (cadr res)))))
    (list num visited)))


(defun walk-tree-forward (tree name num mult)
  (let ((children (gethash name tree)))
    (if (null children)
        (list num mult)
        (loop for (child . num-bag) in children
              do (let ((res (walk-tree-forward tree child
                                               (+ num (* mult num-bag))
                                               (* mult num-bag))))
                   (setq num (car res)))))
    (list num mult)))

(defun get-answer (&key (filename "p7"))
  (multiple-value-bind (parents children)
      (form-tree (read-file-as-lines-p1 filename))
    (format t "Answer 1 = ~A ~%"
            (length (second (walk-tree-backward parents "shinygold" 0
                                                (list)))))
    (format t "Answer 2 = ~A ~%"
            (first (walk-tree-forward children "shinygold" 0 1)))))



;; Original
;; Q7> (time (get-answer))
;; Answer 1 = 121 
;; Answer 2 = 3805 
;; Evaluation took:
;;   0.008 seconds of real time
;;   0.008380 seconds of total run time (0.007482 user, 0.000898 system)
;;   100.00% CPU
;;   18,453,650 processor cycles
;;   2,283,328 bytes consed
