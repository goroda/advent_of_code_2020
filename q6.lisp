(in-package :q6)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with num-valid = 0
          and group = (list)
          for line = (read-line in nil nil)
          while line
          if (string= "" line)
            do (setq num-valid (+ num-valid (length group)))
               (setq group (list))
          else
            do (setq group (nunion group (cl-ppcre:split "" line)
                                   :test #'string=))
          finally (return num-valid))))

(defun read-file-as-lines-p2 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with num-valid = 0
          and group = nil
          and on-num = 0
          for line = (read-line in nil nil)
          while line
          if (string= "" line)            
            do (setq num-valid (+ num-valid (length group)))
               (setq group nil)
               (setq on-num 0)
          else
            if (= on-num 0)
              do (setq group (cl-ppcre:split "" line))
                 (setq on-num (incf on-num))
              else
                unless (null group)
                  do (setq group (nintersection group
                                                (cl-ppcre:split "" line)
                                                :test #'string=))
          end
          end
          finally (return num-valid))))


(defun get-answer (&key (filename "p6"))  
  (let ((ans1 (read-file-as-lines-p1 filename))
        (ans2 (read-file-as-lines-p2 filename)))
    (format t "Answer 1 = ~A~%" ans1)
    (format t "Answer 2 = ~A~%" ans2)))


;; Original
;; Q6> (time (get-answer))
;; Answer 1 = 6170
;; Answer 2 = 2947
;; Evaluation took:
;;   0.009 seconds of real time
;;   0.008701 seconds of total run time (0.008550 user, 0.000151 system)
;;   100.00% CPU
;;   19,137,006 processor cycles
;;   8,449,632 bytes consed

;; Cleaner version -- interesting much smaller consing but longer
;; Q6> (time (get-answer))
;; Answer 1 = 6170
;; Answer 2 = 2947
;; Evaluation took:
;;   0.012 seconds of real time
;;   0.012557 seconds of total run time (0.012426 user, 0.000131 system)
;;   108.33% CPU
;;   27,634,588 processor cycles
;;   2,424,592 bytes consed
