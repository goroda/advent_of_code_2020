(in-package :q19)

(defun parse-rules (rules)  
  (cond ((cl-ppcre:all-matches "[a-z]" rules) (list (aref rules 1)))
        ((find "|" rules :test #'string=)
         (let* ((options (cl-ppcre:split "[|]" rules)))
           (mapcar #'(lambda (opts)
                       (map 'list #'parse-integer
                            (cl-ppcre:split " " (string-trim " " opts))))
                   options)))
        (t (map 'list #'parse-integer (cl-ppcre:split " " (string-trim " " rules))))))

(defun parse-line-into-rules (line rules)
  (let ((key-vals (cl-ppcre:split ": " line)))
    (setf (gethash (parse-integer (car key-vals)) rules)
          (parse-rules (cadr key-vals)))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with rules = (make-hash-table)
          with part1 = t
          for line = (read-line in nil nil)
          while line
          if (string= "" line)
            do (setq part1 nil)
               (setq line (read-line in nil nil))
          ;; do (format t "line = ~A~%" line)
          if part1
            do (parse-line-into-rules line rules)
          else
            collect line into checks
          finally (return (values rules checks)))))

(defun satisfies0 (all-rules str)
  (let ((len (length str))
        (initial-rule (gethash 0 all-rules)))
    (labels ((check-rule (rule next-rule start)
               ;; (format t "~t Checking Rule ~A; Next Rule is ~A; str = ~A start = ~A~%"
               ;;         rule next-rule str start)
               (if (null rule)
                   (if (= start len) t nil)
                   (unless (>= start len)
                     (etypecase rule
                       (character (if (char= (aref str start) rule)
                                      (check-rule (car next-rule) (cdr next-rule) (1+ start))
                                      nil))
                       (integer  (let ((to-check (gethash rule all-rules)))
                                   (if (listp (car to-check))
                                       (check-rule to-check next-rule start)
                                       (check-rule (car to-check) (append (cdr to-check) next-rule) start))))
                       (cons (if (check-rule (caar rule) (append (cdar rule) next-rule) start)
                                   t
                                   (check-rule (cdr rule) next-rule start))))))))
      (check-rule (car initial-rule) (cdr initial-rule) 0))))

(defun get-answer1 (&key (filename "p19"))
  (multiple-value-bind (rules strings) (read-file-as-lines-p1 filename)    
    (loop for str in strings
          :when (satisfies0 rules str)
            count 1)))

(defun get-answer2 (&key (filename "p19b"))
  (multiple-value-bind (rules strings) (read-file-as-lines-p1 filename)    
    (loop for str in strings
          :when (satisfies0 rules str)
            count 1)))

(defun get-answer ()
  (format t "Answer 1: ~A~%" (get-answer1))
  (format t "Answer 2: ~A" (get-answer2)))


;; Original version
;; Q19> (time (get-answer))
;; Answer 1: 147
;; Answer 2: 263
;; Evaluation took:
;;   0.022 seconds of real time
;;   0.022165 seconds of total run time (0.021866 user, 0.000299 system)
;;   100.00% CPU
;;   48,689,070 processor cycles
;;   3,669,744 bytes consed


;; Cleaner version -- added labels for closure and typecase for matching
;; Q19> (time (get-answer))
;; Answer 1: 147
;; Answer 2: 263
;; Evaluation took:
;;   0.017 seconds of real time
;;   0.017460 seconds of total run time (0.017266 user, 0.000194 system)
;;   100.00% CPU
;;   38,445,390 processor cycles
;;   3,669,504 bytes consed
