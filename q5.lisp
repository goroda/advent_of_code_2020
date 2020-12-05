(in-package :q5)

(defun get-row-id (line)
  (let ((row (loop for i from 0 to 6
                   if (char= #\B (aref line i))
                     sum (expt 2 (- 6 i)) into ret
                   finally (return ret)))
        (col (loop for i from 7 to 9
                   if (char= #\R (aref line i))
                     sum (expt 2 (- 9 i)) into ret                
                   finally (return ret))))
    (+ col (* row 8))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          for id = (get-row-id line)
          maximize id into max
          minimize id into min
          sum      id into add
          finally (return (list max min add)))))

(defun get-answer (&key (filename "p5"))  
  (let ((stats (read-file-as-lines-p1 filename)))
    (format t "answer1 = ~A~%" (car stats))
    (format t "answer2 = ~A~%" (-
                                (* (- (first stats) (second stats) -1)
                                   (/ (+ (first stats)
                                         (second stats))
                                      2))
                                (third stats)))))

;; Original
;; Q5> (time (get-answer))
;; answer1 = 878
;; answer2 = 504
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.002381 seconds of total run time (0.002027 user, 0.000354 system)
;;   100.00% CPU
;;   5,235,572 processor cycles
;;   654,768 bytes consed

;; Second after removing second read
;; Q5> (time (get-answer))
;; answer1 = 878
;; answer2 = 504
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.000940 seconds of total run time (0.000751 user, 0.000189 system)
;;   100.00% CPU
;;   2,066,902 processor cycles
;;   327,552 bytes consed

;; Cleaned up final version with improved binary computation
;; Q5> (time (get-answer))
;; answer1 = 878
;; answer2 = 504
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000528 seconds of total run time (0.000435 user, 0.000093 system)
;;   100.00% CPU
;;   1,159,277 processor cycles
;;   65,472 bytes consed
