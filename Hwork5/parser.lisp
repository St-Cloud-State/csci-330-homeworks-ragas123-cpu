(defun parse-G (tokens pos)
  "Parses G -> x | y | z | w"
  (let ((token (first tokens)))
    (if (member token '(x y z w))
        (values (cdr tokens) (1+ pos) (list 'G- token))
        (error "Unexpected symbol: ~A at position ~D. Expected 'x', 'y', 'z', or 'w'" token pos))))

(defun parse-E-prime (tokens pos)
  "Parses E' -> o G E' | ε"
  (if (and tokens (eq (first tokens) 'o))
      (multiple-value-bind (remaining pos g-node) (parse-G (rest tokens) (1+ pos))
        (multiple-value-bind (remaining pos e-node) (parse-E-prime remaining pos)
          (values remaining pos (list 'E'- 'o g-node e-node))))
      (values tokens pos 'ε)))  ; epsilon case

(defun parse-E (tokens pos)
  "Parses E -> G E'"
  (multiple-value-bind (remaining pos g-node) (parse-G tokens pos)
    (multiple-value-bind (remaining pos e-node) (parse-E-prime remaining pos)
      (values remaining pos (list 'E- g-node e-node)))))

(defun parse-L-prime (tokens pos)
  "Parses L' -> s L' | ε"
  (if (and tokens (eq (first tokens) 's))
      (multiple-value-bind (remaining pos l-node) (parse-L-prime (rest tokens) (1+ pos))
        (values remaining pos (list 'L'- 's l-node)))
      (values tokens pos 'ε)))  ; epsilon case

(defun parse-L (tokens pos)
  "Parses L -> s L'"
  (if (eq (first tokens) 's)
      (multiple-value-bind (remaining pos l-node) (parse-L-prime (rest tokens) (1+ pos))
        (values remaining pos (list 'L- 's l-node)))
      (error "Unexpected symbol: ~A at position ~D. Expected 's'" (first tokens) pos)))

(defun parse-S (tokens pos)
  "Parses S -> s | d L b"
  (let ((token (first tokens)))
    (cond
      ((eq token 's)
       (values (rest tokens) (1+ pos) (list 'S- 's)))  ; Match 's'
      ((eq token 'd)
       (multiple-value-bind (remaining pos l-node) (parse-L (rest tokens) (1+ pos))
         (if (eq (first remaining) 'b)
             (values (rest remaining) (1+ pos) (list 'S- 'd l-node 'b))
             (error "Unexpected symbol: ~A at position ~D. Expected 'b'" (first remaining) pos))))
      (t (error "Unexpected symbol: ~A at position ~D. Expected 's' or 'd'" token pos)))))

(defun parse-I-prime (tokens pos)
  "Parses I' -> e S | ε"
  (if (and tokens (eq (first tokens) 'e))
      (multiple-value-bind (remaining pos s-node) (parse-S (rest tokens) (1+ pos))
        (values remaining pos (list 'I'- 'e s-node)))
      (values tokens pos 'ε)))  ; epsilon case

(defun parse-I (tokens pos)
  "Parses I -> i E S I'"
  (if (not (eq (first tokens) 'i))
      (error "Unexpected symbol: ~A at position ~D. Expected 'i'" (first tokens) pos))
  (multiple-value-bind (remaining pos e-node) (parse-E (rest tokens) (1+ pos))
    (multiple-value-bind (remaining pos s-node) (parse-S remaining pos)
      (multiple-value-bind (remaining pos i-node) (parse-I-prime remaining pos)
        (if remaining
            (error "Unexpected symbol: ~A at position ~D. Expected end of input" (first remaining) pos))
        (format t "~&Parsing successful! String length: ~D~%Derivation: ~A~%" pos (list 'I- 'i e-node s-node i-node))
        (values remaining pos (list 'I- 'i e-node s-node i-node))))))

(defun grammar_parser (string)
  "Starts parsing from I with an initial position."
  (parse-I string 1))
