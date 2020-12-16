(in-package :q14)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun binary-list (n)
  "from stackoverflow https://stackoverflow.com/a/22668372"
  (cond ((= n 0) (list 0))
        ((= n 1) (list 1))
        (t (nconc (binary-list (truncate n 2)) (list (mod n 2))))))

(defun integer-to-bit-vector (n)
  (loop with arr = (make-array 36 :element-type 'bit :initial-element 0)
        with lst = (binary-list n)
        for ii from (- 36 (length lst))
        for jj in lst
        do (setf (aref arr ii) jj)
        finally (return arr)))

(defun bit-vector-to-integer (bin)
  (loop for jj from 35 downto 0
        sum (* (aref bin jj) (expt 2 (- 35 jj)))))

(defun mask-array (mask arr)
  (let ((new-array (copy-seq arr)))
    (loop for ii from 0 to 35
          do (cond ((char= (aref mask ii) #\0) (setf (aref new-array ii) 0))
                   ((char= (aref mask ii) #\1) (setf (aref new-array ii) 1)))
          finally (return new-array))))

(defun do-something (lines)  
  (loop with stuff = nil
        with value = nil
        with addr = nil
        with mask = nil
        for line in lines        
        if (string= "mask" (subseq line 0 4))
          do (setq mask (subseq line 7))
        else
          do (setq stuff (cl-ppcre:split "=" line))
             (setq value (integer-to-bit-vector (parse-integer (second stuff))))
             (setq addr (parse-integer (first stuff) :start 4 :junk-allowed t))
             and 
               collect (cons addr (bit-vector-to-integer (mask-array mask value)))))

(defun mask-array-two (mask arr)
  (let ((new-array (copy-seq arr)))
    ;; (format t "mask = ~A~%" mask)
    (loop for ii from 0 to 35
          do (cond ((char= (aref mask ii) #\1) (setf (aref new-array ii) 1))
                   ((char= (aref mask ii) #\2) (setf (aref new-array ii) 0))
                   ((char= (aref mask ii) #\3) (setf (aref new-array ii) 1)))
          finally (return new-array))))

(defun all-masks (mask arr &optional (current nil))
  (let* ((xlocs (loop for ii from 0 to 35 if (char= #\X (aref mask ii)) collect ii))
         (num-x (length xlocs)))
    (if (= num-x 0)
        (nconc current (list (mask-array-two mask arr)))
        (let ((mask-copy (copy-seq mask)))
          (setf (aref mask-copy (first xlocs)) #\3)
          (setq current (all-masks mask-copy arr current))
          (setf (aref mask-copy (first xlocs)) #\2)
          (setq current (all-masks mask-copy arr current))
          current))))

(defun do-something2 (lines)  
  (loop with stuff = nil
        with value = nil
        with addr = nil
        with mask = nil
        for line in lines        
        if (string= "mask" (subseq line 0 4))
          do (setq mask (subseq line 7))
        else
          do (setq stuff (cl-ppcre:split "=" line))
             (setq value (parse-integer (second stuff)))
             (setq addr (integer-to-bit-vector (parse-integer (first stuff) :start 4 :junk-allowed t)))
          and
            append (loop for add in (all-masks mask addr)
                         collect (cons (bit-vector-to-integer add) value))))

(defun parse-answer (vals)
  (loop with seen-locs = (list)
        with sum = 0
        for (loc . value) in (reverse vals)
        if (not (find loc seen-locs))
          do (setq seen-locs (push loc seen-locs))
             (setq sum (+ sum value))
        finally (return sum)))

(defun get-answer (&key (filename "p14"))
  (let* ((code (read-file-as-lines-p1 filename)))
    (format t "Answer 1 = ~A~%" (parse-answer (do-something code)))
    (format t "Answer 2 = ~A~%" (parse-answer (do-something2 code)))))


;; Original version -- not very efficient because store all memories instead of overwriting
;; Q14> (time (get-answer))
;; Answer 1 = 13476250121721
;; Answer 2 = 4463708436768
;; Evaluation took:
;;   31.411 seconds of real time
;;   31.195969 seconds of total run time (31.125560 user, 0.070409 system)
;;   99.32% CPU
;;   69,100,525,983 processor cycles
;;   28,770,432 bytes consed
