(in-package :q5)

(defun get-row-id (line)
  (let ((row
          (loop with lb = 0
                with ub = 128
                for i from 0 to 6
                if (string= "F" (aref line i))
                  do (setq ub (+ lb (/ (- ub lb) 2)))
                else
                  do (setq lb (+ lb (/ (- ub lb) 2)))
                finally (return lb)))
        (col
          (loop with lb = 0
                with ub = 8
                for i from 7 to 9
                if (string= "L" (aref line i))
                  do (setq ub (+ lb (/ (- ub lb) 2)))
                else
                  do (setq lb (+ lb (/ (- ub lb) 2)))
                finally (return lb))))
    (+ col (* row 8))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          maximize (get-row-id line))))

(defun read-file-as-lines-p2 (filename &key)
  "Read file into a list of lines."
  (let ((arr (make-array (* 128 8) :initial-element 0)))
    (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
      (loop for line = (read-line in nil nil)
            while line
            do (setf (aref arr (get-row-id line)) 1)))
    (loop for i from 1 to (1- (* 128 8))
          if (and (= 0 (aref arr i))
                  (= 1 (aref arr (1- i)))
                  (= 1 (aref arr (1+ i))))
            do (return i))))

(defun get-answer (&key (filename "p5"))  
  (let ((ans1 (read-file-as-lines-p1 filename))
        (ans2 (read-file-as-lines-p2 filename)))
    (format t "answer1 = ~A~%" ans1)
    (format t "answer2 = ~A~%" ans2)))
