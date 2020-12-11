(in-package :q10)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (sort (loop for line = (read-line in nil nil)
                   for i from 0
                   while line
                   collect (parse-integer line))
             #'<)))

(defun analyze1 (list)
  (loop for ii from 0 to (- (length list) 2)
        with diffs = (make-array (length list)
                                 :initial-element 0)
        do (setf (aref diffs ii)
                 (-  (nth (1+ ii) list)
                     (nth ii list)))
        if (= (aref diffs ii) 1)
          sum 1 into ones
        if (= (aref diffs ii) 3)
          sum 1 into threes
        finally (progn
                  (setq threes (incf threes)) ;; account for last
                  (if (= 1 (first list))
                      (setq ones (incf ones))
                      (when (= 3 (first list))
                        (setq threes (incf threes))))
                  (return (* ones threes)))))

(defun analyze3 (lst)
  ;;lst is increasingq
  (loop with vals = (make-array (1+ (length lst))
                                :initial-element 0)        
        for elem in lst
        for ii from 0        
        while elem
        if (= ii 0)
          do (setf (aref vals ii) 1)
        else
          do (setf (aref vals ii)
                   (loop for jj from (1- ii) downto 0
                         while (<= (- (nth jj lst) elem) 3)
                         sum (aref vals jj) into out
                         finally (return out)))
        finally (progn (setf (aref vals (length lst))
                             (loop for jj from (1- (length lst))
                                   downto 0
                                   while (<= (nth jj lst) 3)
                                   sum (aref vals jj) into out
                                   finally (return out)))
                       (return vals))))

(defun get-answer (&key (filename "p10"))
  (let* ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (analyze1 code))
    (format t "Answer 2 = ~A~%" (aref (analyze3 (reverse code))
                                      (length code)))
    ;;; I know I know dont reverse and rewrite the code..
    ))


;; Original version
;; Q10> (time (get-answer))
;; Answer 1 = 2475
;; Answer 2 = 442136281481216
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000213 seconds of total run time (0.000158 user, 0.000055 system)
;;   100.00% CPU
;;   465,584 processor cycles
;;   0 bytes consed
