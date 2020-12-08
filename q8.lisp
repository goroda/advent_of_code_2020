(in-package :q8)

(defun parse-instruction (line)
  (let* ((split (cl-ppcre:split " " line))
         (ins (car split))
         (val (parse-integer (cadr split))))
    (list ins val)))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-instruction line))))

(defun run-code (instructions)
  (loop with pointer = 0
        with accum = 0
        with run-codes = (list)
        for (ins val) = (nth pointer instructions)
        do (setq run-codes (push pointer run-codes))
           (setq pointer (incf pointer))
           (cond ((string= "acc" ins) (setq accum (+ accum val)))
                 ((string= "jmp" ins) (setq pointer (+ -1 pointer val))))
        if (find pointer run-codes)
          do (return (list accum nil))           
        if (= pointer (length instructions))
          do (return (list accum t))))

(defun find-bug-and-fix (code)
  (loop for num from 0 to (1- (length code))
        if (string= "jmp" (car (nth num code)))
          do (setf (car (nth num code)) "nop")
             (let ((ans (run-code code)))
               (setf (car (nth num code)) "jmp")
               (when (cadr ans)
                 (return (car ans))))
        if (string= "nop" (car (nth num code)))
          do (setf (car (nth num code)) "jmp")
             (let ((ans (run-code code)))
               (setf (car (nth num code)) "nop")
               (when (cadr ans)
                 (return (car ans))))))

(defun get-answer (&key (filename "p8"))
  (let ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (car (run-code code)))
    (format t "Answer 2 = ~A~%" (find-bug-and-fix code))))



;; Original Version
;; Q8> (time (get-answer))
;; Answer 1 = 1337
;; Answer 2 = 1358
;; Evaluation took:
;;   0.092 seconds of real time
;;   0.092030 seconds of total run time (0.091691 user, 0.000339 system)
;;   100.00% CPU
;;   203,230,010 processor cycles
;;   557,040 bytes consed
