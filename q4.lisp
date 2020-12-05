(in-package :q4)


(defstruct password
  byr iyr eyr hgt hcl ecl pid cid)

(defun verify-password (pass)
  (if (or (null (password-byr pass))
          (null (password-iyr pass))
          (null (password-eyr pass))
          (null (password-hgt pass))
          (null (password-hcl pass))
          (null (password-ecl pass))
          (null (password-pid pass)))
      nil
      pass))

(defun parse-password (line)
  "Parse a password, return nil if not valid"
  ;; (format t "password = ~A~%" line)
  (loop with pass = (make-password)
        for text in (cl-ppcre:split " " line)
        for split = (cl-ppcre:split ":" text)
        for name = (car split)
        for val = (cadr split)
        do (cond ((string= name "byr")
                  (let ((year (parse-number:parse-number val)))
                    (if (or (< year 1920) (> year 2002))
                        (return nil)
                        (setf (password-byr pass) year))))
                 ((string= name "iyr")
                  (let ((year (parse-number:parse-number val)))
                    (if (or (< year 2010) (> year 2020))
                        (return nil)
                        (setf (password-iyr pass) year))))
                 ((string= name "eyr")
                  (let ((year (parse-number:parse-number val)))
                    (if (or (< year 2020) (> year 2030))
                        (return nil)
                        (setf (password-eyr pass) year))))
                 ((string= name "hgt")
                  (let ((hgt (parse-integer val :junk-allowed t))
                        (in (cl-ppcre:all-matches "in" val)))
                    (if  (> (length in) 0)
                         (if (or (< hgt 59) (> hgt 76))
                             (return nil)
                             (setf (password-hgt pass) (cons hgt "in")))
                         (if (or (< hgt 150) (> hgt 193))
                             (return nil)
                             (setf (password-hgt pass) (cons hgt "cm"))))))
                 ((string= name "pid") ;; doesnt check if its a number
                  (if (not (= (length val) 9))
                      (return nil)
                      (setf (password-pid pass) val)))
                 ((string= name "ecl") 
                  (if (or (string= "amb" val)
                          (string= "blu" val)
                          (string= "brn" val)
                          (string= "gry" val)
                          (string= "grn" val)
                          (string= "hzl" val)
                          (string= "oth" val))
                      (setf (password-ecl pass) val)
                      (return nil)))
                 ((string= name "hcl") 
                  (if (not (string= "#" (aref val 0)))
                      (return nil)
                      (if (not (= (/ (length (cl-ppcre:all-matches "[0-9a-f]" val)) 2) 6))
                          (return nil)
                          (setf (password-hcl pass) val)))))
        finally (return (verify-password pass))))

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop with num-valid = 0
           and bump = ""
          for line = (read-line in nil nil)
          while line
          if (string= "" line)
            do (unless (null (parse-password bump))
                 (setq num-valid (+ num-valid 1)))
               (setq bump "")               
          else
            do (setq bump (concatenate 'string bump " " line))
          finally (return num-valid))))

(defun get-answer (&key (filename "p4"))  
  (let ((ans1 (read-file-as-lines-p1 filename)))
    (format t "answer = ~A~%" ans1)))


;; Original version
;; Q4> (time (get-answer))
;; answer = 186
;; Evaluation took:
;;   0.013 seconds of real time
;;   0.012878 seconds of total run time (0.011595 user, 0.001283 system)
;;   100.00% CPU
;;   28,318,942 processor cycles
;;   3,110,112 bytes consed

;; Second version with cleaner outer loop
;; Q4> (time (get-answer))
;; answer = 186
;; Evaluation took:
;;   0.011 seconds of real time
;;   0.010726 seconds of total run time (0.010606 user, 0.000120 system)
;;   100.00% CPU
;;   23,579,546 processor cycles
;;   3,110,640 bytes consed


;; Version with clear password parsing
;; Q4> (time (get-answer))
;; answer = 186
;; Evaluation took:
;;   0.004 seconds of real time
;;   0.003631 seconds of total run time (0.003507 user, 0.000124 system)
;;   100.00% CPU
;;   8,004,824 processor cycles
;;   916,832 bytes consed
