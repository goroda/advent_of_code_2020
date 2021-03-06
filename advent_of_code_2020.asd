;;;; advent_of_code_2020.asd

(asdf:defsystem #:advent_of_code_2020
  :description "My advent of code 2020"
  :author "Alex Gorodetsky alex@alexgorodetsky.com"
  :license  "GPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:parse-number
               #:uiop)  
  :components ((:file "package")
               (:file "advent_of_code_2020")
               (:file "q1")
               (:file "q2")
               (:file "q3")
               (:file "q4")
               (:file "q5")
               (:file "q6")
               (:file "q7")
               (:file "q8")
               (:file "q9")
               (:file "q10")
               (:file "q11")
               (:file "q12")
               (:file "q13")
               (:file "q14")
               (:file "q15")
               (:file "q16")
               (:file "q17")
               (:file "q18")
               (:file "q19")))
