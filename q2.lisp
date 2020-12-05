(in-package :q2)

(defun read-file-as-lines-p1 (filename)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line          
          for split = (cl-ppcre:split ":" line)
          for nums-letter = (cl-ppcre:split " " (car split))
          for letter = (cadr nums-letter)
          for nums = (cl-ppcre:split "-" (car nums-letter))
          for lb = (parse-number:parse-number (car nums))
          for ub = (parse-number:parse-number (cadr nums))
          for num-matches = (/ (length (cl-ppcre:all-matches letter (cadr split))) 2)
          if (and (>= num-matches lb) (<= num-matches ub))
            sum 1)))

(defun read-file-as-lines-p2 (filename)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line          
          for split = (cl-ppcre:split ":" line)
          for pass = (cadr split)
          for nums-letter = (cl-ppcre:split " " (car split))
          for letter = (cadr nums-letter)
          for nums = (cl-ppcre:split "-" (car nums-letter))
          for first = (parse-number:parse-number (car nums))
          for second = (parse-number:parse-number (cadr nums))
          for first-letter = (aref pass first)
          for second-letter = (aref pass second)
          if (or (and (string= first-letter letter)
                      (not (string= second-letter letter)))
                 (and (string= second-letter letter) 
                      (not (string= first-letter letter))))
            sum 1)))

(defun get-answer (&key (filename "p2"))  
  (let ((num-valid-p1 (read-file-as-lines-p1 filename))
        (num-valid-p2 (read-file-as-lines-p2 filename)))
    (format t "part 1 num-valid = ~A~%" num-valid-p1)
    (format t "part 2 num-valid = ~A~%" num-valid-p2)))
