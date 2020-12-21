(in-package :q18)

(defun evaluate-expression (line start)
  (loop with op = #'+
        with val = 0
        for ii from start to (1- (length line))
        ;; do (format t "ii = ~A val = ~A start = ~A thing = ~A ~%" ii val start (aref line ii))
        do (cond ((string= "(" (aref line ii)) (multiple-value-bind (v next) (evaluate-expression line (1+ ii))
                                                 (setq val (funcall op val v))
                                                 (setq ii next)))
                 ((string= ")" (aref line ii)) (progn ;; (format t "return val = ~A~%" val)
                                                 (return (values val ii))))
                 ((string= "*" (aref line ii)) (setq op #'*))
                 ((string= "+" (aref line ii)) (setq op #'+))                 
                 ((string/= " " (aref line ii)) (setq val (funcall op val (parse-integer (string (aref line ii))))) )
;                 (t (format t "ok~%" ))
                 )
        finally (return (values val ii))))

;; (evaluate-expression "1 + 2 * 3 + 4 * 5 + 6" 0)
;; => 71

;; (evaluate-expression "(1 + (2 * 3) + (4 * (5 + 6))" 0)
;; => 51

;; (evaluate-expression "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 0)
;; => 13632

(defun eval-clean (line)
  "Evaluate with no parenthesis"
  (loop while line
        for (fe . (se . (te . rest))) = line
        while (or fe se te)
        if (string= "+" se)
          collect (+ (parse-integer (string fe))
                     (parse-integer (string te))) into out
          and
            do (setq line rest)
        else
          if (string= "*" se)
            collect (parse-integer fe) into out
            and
              do (setq line (cons te rest))
          else
            if (string= "+" fe)
              do (setf (car (last out)) (+ (parse-integer se) (car (last out))))
                 (setq line (cons te rest))
            else
              if (string= "*" fe)
                do (setq line (cons se (cons te rest)))
              else 
                do (setq line rest)
                and
                  collect (parse-integer fe) into out
          end
        end
        finally (return (reduce #'* out))))

(defun pre-proc (line)
  (cl-ppcre:split " " (cl-ppcre:regex-replace-all "[)]"
                                                  (cl-ppcre:regex-replace-all "[(]" line "( ")
                                                  " )")))


(defun parse-two (line)
  (loop with temp = nil
        while line
        for (fe . rest) = line
        if (string= "(" fe)
          do (multiple-value-bind (v r) (parse-two rest)
               (setq rest r)
               (setq temp v))
          and 
            collect temp into out
        else if (string= ")" fe)
               do (return (values (format nil "~A" (eval-clean out)) rest))
             else
               collect fe into out
             end
        end 
        do (setq line rest)
        finally (return (values out nil))))

;; (eval-clean (parse-two (pre-proc "1 + (2 * 3) + (4 * (5 + 6))")))
;; => 51

;; (eval-clean (parse-two (pre-proc "2 * 3 + (4 * 5)")))
;; => 46

;; (eval-clean (parse-two (pre-proc "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
;; => 1445

;; (eval-clean (parse-two (pre-proc "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
;; => 669060

;; (eval-clean (parse-two (pre-proc "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))
;; => 34440


(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          for ii from 0
          while line
          sum (evaluate-expression line 0) into out1
          sum (eval-clean (parse-two (pre-proc line))) into out2
          finally (return (values out1 out2)))))


(defun get-answer (&key (filename "p18"))
  (multiple-value-bind (ans1 ans2) (read-file-as-lines-p1 filename)
    (format t "Answer 1 = ~A~%" ans1)
    (format t "Answer 2 = ~A~%" ans2)))


;; Original Version
;; Q18> (time (get-answer))
;; Answer 1 = 9535936849815
;; Answer 2 = 472171581333710
;; Evaluation took:
;;   0.010 seconds of real time
;;   0.009414 seconds of total run time (0.009316 user, 0.000098 system)
;;   90.00% CPU
;;   20,686,478 processor cycles
;;   5,043,456 bytes consed


