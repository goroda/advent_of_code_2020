(in-package :q16)

(defun parse-fields (fields)
  (loop for field in fields
        for split = (cl-ppcre:split ":" field)
        for name = (first split)
        for ranges = (second split)
        collect (cons name (loop for term in (cl-ppcre:split "or" ranges)
                                 for bounds = (cl-ppcre:split "-" term)
                                 collect (cons (parse-integer (first bounds))
                                               (parse-integer (second bounds)))))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with on-part = 0
          for line = (read-line in nil nil)
          while line
          if (string= line "")
            do (setq on-part (incf on-part))
               (setq line (read-line in nil nil))
          if (= on-part 0)
            collect line into fields
          else
            if (and (= 0 (length (cl-ppcre:all-matches "your ticket:"line))) (= on-part 1))
              collect line into mine
          else
            if (and (= 0 (length (cl-ppcre:all-matches "nearby ticket"line))) (= on-part 2))
              collect line into nearby
            
          finally (return (list fields
                                (map 'vector #'parse-integer (cl-ppcre:split "," (first mine)))
                                nearby)))))


(defun check-ticket (ticket fields)
  (loop for numtxt in (cl-ppcre:split "," ticket)
        for num = (parse-integer numtxt)
        if (= 0 (loop for (field . bnds) in fields
                       sum (loop for (lb . ub) in bnds
                                 if (and (>= num lb) (<= num ub))
                                   do (return 1)
                                 finally (return 0))))
          do (return num)
        finally (return 0)))

(defun do-something (lines)  
  (loop with fields = (parse-fields (first lines))
        for ticket in (third lines)
        sum (check-ticket ticket fields)))


(defun check-ticket-two (ticket fields)
  (loop for numtxt in (cl-ppcre:split "," ticket)
        for num = (parse-integer numtxt)
        collect (loop for (field . bnds) in fields
                      if (= 1 (loop for (lb . ub) in bnds
                                    if (and (>= num lb) (<= num ub))
                                      do (return 1)
                                    finally (return 0)))
                        collect field)))

(defun do-something-two (lines)  
  (let* ((fields (parse-fields (first lines)))
         (num-fields (length fields))
         (myticket (second lines))
         (valids (map 'list #'(lambda (ticket) (check-ticket-two ticket fields))
                      (remove-if #'(lambda (ticket) (/= 0 (check-ticket ticket fields))) (third lines))))         
         (possibilities (loop for num from 0 to (1- num-fields)
                              for valid = (reduce #'intersection (loop for val in valids collect (nth num val)))
                              collect (cons num valid))))
    (loop with done = nil
          with maps = (make-array num-fields :element-type 'string :initial-element "none")
          while possibilities
          for next-possibilities = (loop for (num . next) in possibilities
                                         if (= (length next) 1)
                                           do (setf (aref maps num) (cons num (car next)))
                                              (setq done (cons (car next) done))
                                         else
                                           collect (cons num (loop for check in next if (not (find check done))
                                                                   collect check)))
          do (setf possibilities next-possibilities)
          finally (return (reduce  #'(lambda (v x)
                                       (if (not (null (cl-ppcre:all-matches "departure" (cdr x))))
                                           (* v (aref myticket (car x)))
                                           v))
                                   maps :initial-value 1)))))

(defun get-answer (&key (filename "p16"))
  (let* ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (do-something code))
    (format t "Answer 2 = ~A~%" (do-something-two code))))

;; Original version
;; Q16> (time (get-answer))
;; Answer 1 = 18227
;; Answer 2 = 2355350878831
;; Evaluation took:
;;   0.014 seconds of real time
;;   0.013581 seconds of total run time (0.012092 user, 0.001489 system)
;;   100.00% CPU
;;   30,212,876 processor cycles
;;   3,505,584 bytes consed
