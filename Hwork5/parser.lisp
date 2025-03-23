(defun match (token input)
  (if (and input (> (length input) 0) (char= (char input 0) token))
      (subseq input 1)
      nil))

(defun parse-Lprime (input)
  (if (and input (> (length input) 0) (char= (char input 0) #\s))
      (let ((rest (match #\s input)))
        (when rest
          (parse-Lprime rest)))
      input)) ; ε-case

(defun parse-L (input)
  (let ((rest (match #\s input)))
    (when rest
      (parse-Lprime rest))))

(defun parse-S (input)
  (cond
    ((and input (> (length input) 0) (char= (char input 0) #\s))
     (match #\s input))
    ((and input (> (length input) 0) (char= (char input 0) #\d))
     (let ((rest (match #\d input)))
       (when rest
         (let ((l (parse-L rest)))
           (when l
             (match #\b l))))))
    (t nil)))

(defun parse-G (input)
  (if (and input (> (length input) 0)
           (member (char input 0) '(#\x #\y #\z #\w)))
      (subseq input 1)
      nil))

(defun parse-Eprime (input)
  (if (and input (> (length input) 0) (char= (char input 0) #\o))
      (let ((rest (match #\o input)))
        (when rest
          (let ((g (parse-G rest)))
            (when g
              (parse-Eprime g)))))
      input)) ; ε case

(defun parse-E (input)
  (let ((g (parse-G input)))
    (when g
      (parse-Eprime g))))

(defun parse-Iprime (input)
  (if (and input (> (length input) 0) (char= (char input 0) #\e))
      (let ((rest (match #\e input)))
        (when rest
          (parse-S rest)))
      input)) ; ε case

(defun parse-I (input)
  (let ((rest (match #\i input)))
    (when rest
      (let ((e (parse-E rest)))
        (when e
          (let ((s (parse-S e)))
            (when s
              (parse-Iprime s))))))))

(defun accept (input)
  (let ((result (parse-I input)))
    (and result (string= result "")))) ; input fully consumed

(defun test-14-strings ()
  (let ((valid '("ixoyowdssbes" "ixoys" "ixozs" "ixozdssb"
                 "ixoyowdssb" "ixozdssbes" "ixydssb"))
        (invalid '("ixoyowdssbess" "ixoy" "ixoysx" "ixozdss"
                   "ixoyowdssbe" "ixozddssb" "ixydssbx")))
    (format t "~%Testing Valid Strings:~%")
    (dolist (str valid)
      (format t "~a => ~a~%" str (if (accept str) "Accepted" "Rejected")))
    
    (format t "~%Testing Invalid Strings:~%")
    (dolist (str invalid)
      (format t "~a => ~a~%" str (if (accept str) "Accepted" "Rejected")))))
