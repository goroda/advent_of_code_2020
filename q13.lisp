(in-package :q13)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun do-something (lines)
  (let ((timestamp (parse-integer (first lines)))
        (bus-list (loop for elem in (cl-ppcre:split "," (second lines))
                        if (string/= elem "x")
                          collect (parse-integer elem))))    
    ;; (format t "timestamp = ~A~%" timestamp)
    ;; (format t "bus-list = ~A~%" bus-list)
    (loop for time from timestamp
          for done = (loop for bus in bus-list
                            if (= 0 (mod time bus))
                              do (return bus)
                           finally (return 0))
          if (/= done 0)
            do (return (* done (- time timestamp))))))

(defun extended-euclidean-algorithm (a b)
  "Returns greatest common divisor of and b along wiht coefficients of bezouts identity (x, y)
such that ax + by = gcd (a, b)"  
  (loop with old-r = a
        and  r     = b
        and  old-s = 1
        and      s = 0
        and  old-t = 0
        and     te = 1
        while (/= r 0)
        for quotient = (floor (/ old-r r))
        for new-r = (- old-r (* quotient r))
        for new-s = (- old-s (* quotient s))
        for new-t = (- old-t (* quotient te))
        do (setq old-r r)
           (setq old-s s)
           (setq old-t te)
           (setq r new-r)
           (setq s new-s)
           (setq te new-t)
        finally (return (list old-r old-s old-t))))

(defun do-something-two (lines)
  "Chinese remainder theorem --- awesome!"
  (let* ((bus-list (reverse (loop for elem in (cl-ppcre:split "," (second lines))
                                  for ii from 0
                                  if (string/= elem "x")
                                    collect (cons ii (parse-integer elem)))))
         (N (loop with out = 1
                  for (ii . id) in bus-list
                  do (setq out (* out id))
                  finally (return out))))
    ;; (format t "bus-list = ~A N = ~A ~%" bus-list N)
    (loop with base-t = (car (first bus-list))
          for (ii . id) in bus-list
          for (gcd x y) = (extended-euclidean-algorithm (/ N id) id)
          for a = (- base-t ii)
          ;; do (format t "gcd = ~A x=~A y=~A ai=~A check=~A ~%" gcd x y a (+ (* x (/ N id))
          ;;                                                                  (* y id)))
          sum (* a x (/ N id)) into an-answer
          finally (return (- (mod an-answer N) base-t)))))

(defun get-answer (&key (filename "p13"))
  (let* ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (do-something code))
    (format t "Answer 2 = ~A~%" (do-something-two code))))

;; Original
;; Q13> (time (get-answer))
;; Answer 1 = 4722
;; Answer 2 = 825305207525452
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000261 seconds of total run time (0.000187 user, 0.000074 system)
;;   100.00% CPU
;;   570,492 processor cycles
;;   32,768 bytes consed
