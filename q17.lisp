(in-package :q17)

(defun read-file-as-lines-p1 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          for ii from 0
          while line
          append (loop for jj from 0 to (1- (length line))
                       if (string= "#" (aref line jj))
                         collect (list ii jj 0)))))

(defun is-active (check map)
  (position check map :test #'(lambda (x y) (and (= (first x) (first y))
                                                 (= (second x) (second y))
                                                 (= (third x) (third y))))))

(defun create-options (current)
  (let ((x (first current))
        (y (second current))
        (z (third current)))
    ;; (format t "~A~A~A~%" x y z)
    (loop for ii in (list -1 0 1)
          append (loop for jj in (list -1 0 1)
                       append (loop for kk in (list -1 0 1)
                                      if (not (= 0 ii jj kk))
                                        collect (list (+ x ii)
                                                      (+ y jj)
                                                      (+ z kk)))))))

(defun num-active-neighbors (current map)
  (let ((x (first current))
        (y (second current))
        (z (third current)))
    ;; (format t "~A~A~A~%" x y z)
    (loop for ii in (list -1 0 1)
          sum (loop for jj in (list -1 0 1)
                    sum (loop for kk in (list -1 0 1)
                              if (and (not (= 0 ii jj kk))
                                      (is-active (list (+ x ii)
                                                       (+ y jj)
                                                       (+ z kk))
                                                 map))
                                sum 1)))))

(defun all-active-sites (current map)
  (remove-duplicates (loop for option in (create-options current)
                           for num-active-neighbors = (num-active-neighbors option map)
                           for active? = (is-active option map)
                           if (and active?
                                   (or (= num-active-neighbors 2)
                                       (= num-active-neighbors 3)))
                             collect option
                           if (and (not active?) (= num-active-neighbors 3))
                             collect option)
                     :test #'(lambda (x y)
                               (and (= (first x) (first y))
                                    (= (second x) (second y))
                                    (= (third x) (third y))))))

(defun copy-map (map)
  (loop for term in map
        collect (copy-seq term)))

(defun do-something (map num-trials)
  ;; (format t "map = ~A~% " map)
  (loop for ii from 1 to num-trials
        for new-map = (remove-duplicates
                       (loop for active-site in map
                             ;; do (format t "active-site = ~A; is active=~A ~%"
                             ;;            active-site (is-active active-site map))
                             append (all-active-sites active-site map))
                       :test #'(lambda (x y)
                                 (and (= (first x) (first y))
                                    (= (second x) (second y))
                                    (= (third x) (third y)))))
        do (setq map (copy-map new-map))
        ;; do (format t "new-map = ~A~%" map )
        finally (return new-map)))



(defun is-active-two (check map)
  (position check map :test #'(lambda (x y) (and (= (first x) (first y))
                                                 (= (second x) (second y))
                                                 (= (third x) (third y))
                                                 (= (fourth x) (fourth y))))))

(defun create-options-two (current)
  (let ((x (first current))
        (y (second current))
        (z (third current))
        (w (fourth current)))
    ;; (format t "~A~A~A~%" x y z)
    (loop for ii in (list -1 0 1)
          append (loop for jj in (list -1 0 1)
                       append (loop for kk in (list -1 0 1)
                                    append (loop for zz in (list -1 0 1)
                                                 if (not (= 0 ii jj kk zz))
                                                   collect (list (+ x ii)
                                                                 (+ y jj)
                                                                 (+ z kk)
                                                                 (+ w zz))))))))

(defun num-active-neighbors-two (current map)
  (let ((x (first current))
        (y (second current))
        (z (third current))
        (w (fourth current)))
    ;; (format t "~A~A~A~%" x y z)
    (loop for ii in (list -1 0 1)
          sum (loop for jj in (list -1 0 1)
                    sum (loop for kk in (list -1 0 1)
                              sum (loop for zz in (list -1 0 1)
                                        if (and (not (= 0 ii jj kk zz))
                                                (is-active-two (list (+ x ii)
                                                                     (+ y jj)
                                                                     (+ z kk)
                                                                     (+ w zz))
                                                               map))
                                          sum 1))))))

(defun all-active-sites-two (current map)
  (remove-duplicates (loop for option in (create-options-two current)
                           for num-active-neighbors = (num-active-neighbors-two option map)
                           for active? = (is-active-two option map)
                           if (and active?
                                   (or (= num-active-neighbors 2)
                                       (= num-active-neighbors 3)))
                             collect option
                           if (and (not active?) (= num-active-neighbors 3))
                             collect option)
                     :test #'(lambda (x y)
                               (and (= (first x) (first y))
                                    (= (second x) (second y))
                                    (= (third x) (third y))
                                    (= (fourth x) (fourth y))))))

(defun do-something-two (map num-trials)
  ;; (format t "map = ~A~% " map)
  (loop for ii from 1 to num-trials
        for new-map = (remove-duplicates
                       (loop for active-site in map
                             append (all-active-sites-two active-site map))
                       :test #'(lambda (x y)
                                 (and (= (first x) (first y))
                                      (= (second x) (second y))
                                      (= (third x) (third y))
                                      (= (fourth x) (fourth y)))))
        do (setq map (copy-map new-map))
        ;; do (format t "new-map = ~A~%" map )
        finally (return new-map)))

(defun read-file-as-lines-p2 (filename &key)
  "Read file into a list of lines."
  (with-open-file (in (advent_of_code_2020:get-file-pathname filename))
    (loop for line = (read-line in nil nil)
          for ii from 0
          while line
          append (loop for jj from 0 to (1- (length line))
                       if (string= "#" (aref line jj))
                         collect (list ii jj 0 0)))))

(defun get-answer (&key (filename "p17"))
  (let* ((code (read-file-as-lines-p1 filename))
         (code2 (read-file-as-lines-p2 filename)))
    (format t "Answer 1 = ~A~%" (length (do-something code 6)))
    (format t "Answer 2 = ~A~%" (length (do-something-two code2 6)))))

;; Long run time -- beware
;; Long run time stems form the fact that I check whether a site is active (and count its neighbors)
;; more than once -- makes implementation wasteful


