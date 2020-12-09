(in-package :q9)


(defun do-something-with-line (line)
  line)

(defun read-file-as-lines-p1 (filename &key (preamble-length 5))
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          for i from 0
          while line
          collect (parse-integer line) into all
          if (< i preamble-length)
            collect (parse-integer line) into preamble
          else
            collect (parse-integer line) into rest
          finally (return (list
                           (make-array (length preamble) :initial-contents
                                       preamble)
                           (make-array (length rest) :initial-contents
                                       rest)
                           (make-array (length all) :initial-contents all))))))

(defun compute-all-sums (code)
  (let ((num (length code)))
    (loop for i from 0 to (1- num)
          append (loop for j from 0 to (1- num)
                       if (/= i j)
                         collect (+ (aref code i)
                                    (aref code j))))))

(defun check-code (preamble code)
  (loop with on-num = 0
        for i from 0 to (1- (length code))
        for preamble-sums = (compute-all-sums preamble)
        if (find (aref code i) preamble-sums)
          do (setf (aref preamble on-num) (aref code i))
             (setq on-num (mod (incf on-num) (length preamble)))
        else
          do (return (aref code i))))

(defun find-sum (nums-list check-sum)
  (loop named outer for ii from 0 to (1- (length nums-list))
        with sums = (make-array (length nums-list) :initial-element 0)
        do (loop for jj from 0 to ii
                 do (setf (aref sums jj) (+ (aref sums jj)
                                            (aref nums-list ii)))
                 if (= (aref sums jj) check-sum)
                   do (loop for kk from jj to ii
                            minimize (aref nums-list kk) into smallest
                            maximize (aref nums-list kk) into largest
                            finally (return-from outer (+ smallest largest))))))

(defun get-answer (&key (filename "p9"))
  (let* ((code (read-file-as-lines-p1 filename :preamble-length 25))
         (ans1  (check-code (first code) (second code))))
    (format t "Answer 1 = ~A~%" ans1)
    (format t "Answer 2 = ~A~%" (find-sum (third code) ans1))))

;; Original version
;; Q9> (time (get-answer))
;; Answer 1 = 21806024
;; Answer 2 = 2986195
;; Evaluation took:
;;   0.007 seconds of real time
;;   0.007350 seconds of total run time (0.007267 user, 0.000083 system)
;;   100.00% CPU
;;   16,163,348 processor cycles
;;   9,467,632 bytes consed
