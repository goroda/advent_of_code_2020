(in-package :q3)

(defun read-file-as-lines-p1 (filename &key (num-right 3) (num-down 1))
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (let ((val 0))
      (loop for line = (read-line in nil nil)
            for i from 0
            while line
            when (and (>= i 1) (= (mod i num-down) 0))
              do (progn
                   ;; (format t "ii = ~A~%" i)
                   (when (and (= i 1) (string= "#" (aref line num-right)))
                     (setq val (1+ val)))
                   (when (and (> i 1) (string= "#" (aref line (mod (/ (* i num-right) num-down ) 31))))
                     (setq val (1+ val)))))
      (format t "val = ~A ~%" val)
      val)))


(defun get-answer (&key (filename "p3"))  
  (let ((ans1 (read-file-as-lines-p1 filename))
        (ans2 (read-file-as-lines-p1 filename :num-right 1))
        (ans3 (read-file-as-lines-p1 filename :num-right 5))
        (ans4 (read-file-as-lines-p1 filename :num-right 7))
        (ans5 (read-file-as-lines-p1 filename :num-right 1 :num-down 2)))
    (format t "lines = ~A~%" ans1)
    (format t "lines = ~A~%" ans2)
    (format t "lines = ~A~%" ans3)
    (format t "lines = ~A~%" ans4)
    (format t "lines = ~A~%" ans5)
    (format t "prod = ~A~%" (* ans1 ans2 ans3 ans4 ans5))))
    ;; (format t "line = ~A~%" (length (car lines)))))
