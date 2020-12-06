(in-package :q6)

(defun in-array (array elem)
  (if (or (string= "" elem) (string= " " elem))
      1
      (loop for ii in array
            if (string= elem ii)
              do (return 1)
            finally (return 0))))

(defun parse-group (group)
  (loop
    with elems = (list)
    for ii from 0 to (1- (length group))
    for char = (aref group ii)
    if (= (in-array elems char) 0)
      do (setq elems (concatenate 'list elems (list (string char))))
    finally (return (length elems))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with num-valid = 0
           and group = ""
          for line = (read-line in nil nil)
          while line
          ;; do (format t "line = ~A~%" line)
          if (string= "" line)
            do (setq num-valid (+ num-valid (parse-group group)))
               (setq group "")
          else
            do (setq group (concatenate 'string group " " line))
          finally (return num-valid))))

(defun in-array-2 (array elem)
  ;; (format t "array = ~A elem = ~A ~A~%" array elem (length array))
  (if (= (length array) 1)
      (if (string= elem array)
          1
          0)
      (loop for ind from 0 to (1- (length array))
           for ii = (aref array ind)
           ;; do (format t "elemin = ~A~%" elem)
           if (string= (string elem) ii)
             do (return 1)
            finally (return 0))))

(defun parse-group-2 (group)
  (if (= (length group) 1)
      (length (car group))  
      (loop
        with num-unique = 0
        for ii from 0 to (1- (length (car group)))
        for char = (aref (car group) ii)
        if (= 1 (loop for gr in (cdr group)
                      minimize (in-array-2 (string gr) (string char))))
          do (setq num-unique (1+ num-unique))
        finally (return num-unique))))

(defun read-file-as-lines-p2 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with num-valid = 0
           and group = (list)
          for line = (read-line in nil nil)
          while line
          ;; do (format t "line = ~A~%" line)
          if (string= "" line)
            do (setq num-valid (+ num-valid (parse-group-2 group)))
               (setq group (list))
          else
            do (setq group (append group (list line)))
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
