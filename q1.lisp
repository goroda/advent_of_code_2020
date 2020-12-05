(in-package :q1)

(defun read-file-as-lines (filename)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-number:parse-number line))))

(defun get-answer (&key (filename "p1"))  
  (let ((array (read-file-as-lines filename)))
    (format t "Answer for part 2 is: ~A~%"
            (loop named outer for val in array
                  do (loop for val2 in array
                           do (loop for val3 in array
                                    when (= 2020 (+ val val2 val3))
                                      do (return-from outer (* val val2 val3))))))))
