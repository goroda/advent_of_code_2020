(in-package :q11)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun lines-to-array (lines)
  (loop with arr = (make-array (list (+ 2 (length lines)) (+ 2 (length (first lines)))) :initial-element 0)
        for row in lines
        for ii from 1
        do (loop for jj from 0 to (1- (length row))
                 do (setf (aref arr ii (1+ jj))
                          (cond ((string= (aref row jj) "L") -1)
                                ((string= (aref row jj) "#") 1)
                                ((string= (aref row jj) ".") 0))))
        finally (return arr)))

(defun analyze-array (arr)
  (loop with next-array = (make-array (array-dimensions arr) :initial-element 0)
        for ii from 1 to (- (array-dimension arr 0) 2)
        do (loop for jj from 1 to (- (array-dimension arr 1) 2)
                 do (cond ((and (= (aref arr ii jj) -1)
                                (/= 1 (aref arr (1- ii) (1- jj)))
                                (/= 1 (aref arr ii (1- jj)))
                                (/= 1 (aref arr (1+ ii) (1- jj)))
                                (/= 1 (aref arr (1- ii) jj))
                                (/= 1 (aref arr (1+ ii) jj))
                                (/= 1 (aref arr (1- ii) (1+ jj)))
                                (/= 1 (aref arr ii (1+ jj)))
                                (/= 1 (aref arr (1+ ii) (1+ jj))))
                           (setf (aref next-array ii jj) 1))
                          ((and (= (aref arr ii jj) 1)
                                (< 3 (+ (if (= 1 (aref arr (1- ii) (1- jj))) 1 0)
                                        (if (= 1 (aref arr ii (1- jj))) 1 0)
                                        (if (= 1 (aref arr (1+ ii) (1- jj))) 1 0)
                                        (if (= 1 (aref arr (1- ii) jj)) 1 0)
                                        (if (= 1 (aref arr (1+ ii) jj)) 1 0)
                                        (if (= 1 (aref arr (1- ii) (1+ jj))) 1 0)
                                        (if (= 1 (aref arr ii (1+ jj))) 1 0)
                                        (if (= 1 (aref arr (1+ ii) (1+ jj))) 1 0))))
                           (setf (aref next-array ii jj) -1))
                          (t (setf (aref next-array ii jj) (aref arr ii jj)))))
        finally (return next-array)))

(defun check-if-occupied (arr current-row current-col row-change col-change)
  (let ((num-rows (array-dimension arr 0))
        (num-cols (array-dimension arr 1)))    
    (loop for ii from 1
          for row = (+ current-row (* ii row-change)) 
          for col = (+ current-col (* ii col-change))
          while (and (> row 0) (< row num-rows)
                     (> col 0) (< col num-cols)
                     (/= (aref arr row col) -1))
          if (= (aref arr row col) 1)
            do (return 1)
          finally (return 0))))

(defun analyze-array-2 (arr)
  (loop with next-array = (make-array (array-dimensions arr) :initial-element 0)
        for ii from 1 to (- (array-dimension arr 0) 2)
        do (loop for jj from 1 to (- (array-dimension arr 1) 2)
                 do (cond ((and (= (aref arr ii jj) -1)
                                (= 0 (check-if-occupied arr ii jj 1 1))
                                (= 0 (check-if-occupied arr ii jj 0 1))
                                (= 0 (check-if-occupied arr ii jj -1 1))
                                (= 0 (check-if-occupied arr ii jj 1 0))
                                (= 0 (check-if-occupied arr ii jj -1 0))
                                (= 0 (check-if-occupied arr ii jj 1 -1))
                                (= 0 (check-if-occupied arr ii jj 0 -1))
                                (= 0 (check-if-occupied arr ii jj -1 -1)))
                           (setf (aref next-array ii jj) 1))
                          ((and (= (aref arr ii jj) 1)
                                (< 4 (+ (check-if-occupied arr ii jj 1 1)
                                        (check-if-occupied arr ii jj 0 1)
                                        (check-if-occupied arr ii jj -1 1)
                                        (check-if-occupied arr ii jj 1 0)
                                        (check-if-occupied arr ii jj -1 0)                                       
                                        (check-if-occupied arr ii jj 1 -1)
                                        (check-if-occupied arr ii jj 0 -1)
                                        (check-if-occupied arr ii jj -1 -1))))
                           (setf (aref next-array ii jj) -1))
                          (t (setf (aref next-array ii jj) (aref arr ii jj)))))
        finally (return next-array)))

(defun diff-array (arr1 arr2)
  (loop for ii from 0 to (1-  (* (array-dimension arr1 0) (array-dimension arr1 1)))
        sum (- (row-major-aref arr1 ii) (row-major-aref arr2 ii))))

(defun sum-occupied (arr1)
  (loop for ii from 0 to (1- (* (array-dimension arr1 0) (array-dimension arr1 1)))
        if (= (row-major-aref arr1 ii) 1)
          sum 1))

(defun run-loop (func start)
  (loop with current-array = (funcall func start)
        for next-array = (funcall func current-array)
        for diff = (diff-array current-array next-array)
        do (setq current-array next-array)        
        while (/= diff 0)        
        finally (return next-array)))

(defun get-answer (&key (filename "p11"))
  (let* ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (sum-occupied (run-loop
                                               #'analyze-array
                                               (lines-to-array code))))
    (format t "Answer 2 = ~A~%" (sum-occupied (run-loop
                                               #'analyze-array-2
                                               (lines-to-array code))))))


;; Original Version
;; Q11> (time (get-answer))
;; Answer 1 = 2483
;; Answer 2 = 2285
;; Evaluation took:
;;   0.313 seconds of real time
;;   0.308323 seconds of total run time (0.306538 user, 0.001785 system)
;;   98.40% CPU
;;   688,836,906 processor cycles
;;   15,295,008 bytes consed
