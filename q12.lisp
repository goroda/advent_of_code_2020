(in-package :q12)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          with left-right-move = 0
          with up-down-move = 0
          with current-dir = 0
          while line
          for num = (parse-integer line :start 1)
          do ;; (format t "direction = ~A left-right = ~A up-down = ~A ~%" (aref line 0) left-right-move up-down-move)
             ;; (format t "num = ~A current-direction = ~A ~%" num current-dir)
             (cond ((string= (aref line 0) "F")
                    (cond ((= current-dir 0)
                           (setq left-right-move (+ left-right-move num)))
                          ((= current-dir 180)
                           (setq left-right-move (- left-right-move num)))
                          ((= current-dir 90)
                           (setq up-down-move (+ up-down-move num)))
                          ((= current-dir 270)
                           (setq up-down-move (- up-down-move num)))
                          (t (error "Not accounted for the direction"))))
                   ((string= (aref line 0) "L")
                    (setq current-dir (+ current-dir num)))
                   ((string= (aref line 0) "R")
                    (setq current-dir (- current-dir num)))
                   ((string= (aref line 0) "N")
                    (setq up-down-move (+ up-down-move num)))
                   ((string= (aref line 0) "S")
                    (setq up-down-move (- up-down-move num)))
                   ((string= (aref line 0) "E")
                    (setq left-right-move (+ left-right-move num)))
                   ((string= (aref line 0) "W")
                    (setq left-right-move (- left-right-move num))))
             (when (< current-dir 0)
               (setq current-dir (+ 360 current-dir)))
             (when (= current-dir 360)
               (setq current-dir 0))
             (when (> current-dir 360)
               (setq current-dir (- current-dir 360)))
          finally (return (+ (abs left-right-move) (abs up-down-move)))))) 

(defun read-file-as-lines-p2 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          with left-right-move = 0
          with up-down-move = 0
          with way-point-x = 10
          with way-point-y = 1
          while line
          for num = (parse-integer line :start 1)
          do ;; (format t "~%direction = ~A left-right = ~A up-down = ~A ~%" (aref line 0) left-right-move up-down-move)
             ;; (format t "way-point-x = ~A way-point-y = ~A ~%" way-point-x way-point-y)
             ;; (format t "num = ~A cos = ~A ~%" num (cos (/ num 180.0)))
             (cond ((string= (aref line 0) "F")
                    (progn
                      (setq left-right-move (+ left-right-move (* num way-point-x)))
                      (setq up-down-move (+ up-down-move (* num way-point-y)))))
                   ((string= (aref line 0) "L")
                    (let ((next-x (- (* way-point-x (cos (* pi (/ num 180.0))))
                                          (* way-point-y (sin (* pi (/ num 180.0))))))
                          (next-y (+ (* way-point-x (sin (* pi (/ num 180.0))))
                                          (* way-point-y (cos (* pi (/ num 180.0)))))))
                      (setq way-point-x next-x)
                      (setq way-point-y next-y)))
                   ((string= (aref line 0) "R")
                    (let ((next-x (+ (* way-point-x (cos (* pi (/ num 180.0))))
                                     (* way-point-y (sin (* pi (/ num 180.0))))))
                          (next-y (+ (* -1 way-point-x (sin (* pi (/ num 180.0))))
                                     (* way-point-y (cos (* pi (/ num 180.0)))))))
                      (setq way-point-x next-x)
                      (setq way-point-y next-y)))
                   ((string= (aref line 0) "N")
                    (setq way-point-y (+ way-point-y num)))
                   ((string= (aref line 0) "S")
                    (setq way-point-y (- way-point-y num)))
                   ((string= (aref line 0) "E")
                    (setq way-point-x (+ way-point-x num)))
                   ((string= (aref line 0) "W")
                    (setq way-point-x (- way-point-x num))))
          finally (return (round (+ (abs left-right-move) (abs up-down-move))))))) 

(defun get-answer (&key (filename "p12"))
  (let* ((code (read-file-as-lines-p1 filename))
         (code2 (read-file-as-lines-p2 filename)))
    (format t "Answer 1 = ~A~%" code)
    (format t "Answer 2 = ~A~%" code2)))

;; Original
;; Q12> (time (get-answer))
;; Answer 1 = 1319
;; Answer 2 = 62434
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.000745 seconds of total run time (0.000620 user, 0.000125 system)
;;   100.00% CPU
;;   1,629,770 processor cycles
;;   294,848 bytes consed
