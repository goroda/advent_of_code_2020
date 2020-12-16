(in-package :q15)

(defun part1 (nums num-turns)
  (let ((dict (make-hash-table)))
    (loop for num in nums
          for ii from 1
          do (setf (gethash num dict) ii))
    (loop with spoken = (car (last nums))
          for ii from (length nums) to (1- num-turns)
          for val = (gethash spoken dict)
          do (setf (gethash spoken dict) ii)          
          if val
            do (setq spoken (- ii val))
          else
            do (setq spoken 0)
          finally (return spoken))))

(defun get-answer ()
  (let* ((check1 (part1 (list 0 3 6) 10))
         (check2 (part1 (list 1 3 2) 2020))
         (check3 (part1 (list 2 1 3) 2020))
         (check4 (part1 (list 1 2 3) 2020))
         (check5 (part1 (list 2 3 1) 2020))
         (ans1 (part1 (list 1 0 15 2 10 13) 2020))
         (check6 (part1 (list 0 3 6) 30000000))
         (ans2 (part1 (list 1 0 15 2 10 13) 30000000)))
    (format t "Check 1 = ~A~%" check1)
    (format t "Check 2 = ~A~%" check2)
    (format t "Check 3 = ~A~%" check3)
    (format t "Check 4 = ~A~%" check4)
    (format t "Check 5 = ~A~%" check5)
    (format t "Check 6 = ~A~%" check6)
    (format t "ans1 = ~A~%" ans1)
    (format t "ans2 = ~A~%" ans2)))

;; Original version
;; Q15> (time (get-answer))
;; Check 1 = 0
;; Check 2 = 1
;; Check 3 = 10
;; Check 4 = 27
;; Check 5 = 78
;; Check 6 = 175594
;; ans1 = 211
;; ans2 = 2159626
;; Evaluation took:
;;   8.103 seconds of real time
;;   7.759518 seconds of total run time (7.307830 user, 0.451688 system)
;;   [ Run times consist of 0.469 seconds GC time, and 7.291 seconds non-GC time. ]
;;   95.77% CPU
;;   17,826,591,004 processor cycles
;;   914,376,416 bytes consed
