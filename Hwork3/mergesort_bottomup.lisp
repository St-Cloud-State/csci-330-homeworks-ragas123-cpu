(defun merge-lists (list1 list2)
  "Merge two sorted lists into one sorted list."
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((<= (car list1) (car list2))
     (cons (car list1) (merge-lists (cdr list1) list2)))
    (t
     (cons (car list2) (merge-lists list1 (cdr list2))))))

(defun pairwise-sort (lst)
  "Partition the list into sorted pairs."
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list lst))
    (t
     (let ((first (car lst))
           (second (cadr lst))
           (rest (cddr lst)))
       (if (<= first second)
           (cons (list first second) (pairwise-sort rest))
           (cons (list second first) (pairwise-sort rest)))))))

(defun merge-pass (lst)
  "Merge adjacent lists in a single pass."
  (format t "Merging pass: ~a~%" lst) ;; Prints the current state before merging
  (cond
    ((null lst) nil)
    ((null (cdr lst)) lst)
    (t (let ((merged (merge-lists (car lst) (cadr lst))))
         (format t "Merging ~a and ~a -> ~a~%" (car lst) (cadr lst) merged)
         (cons merged (merge-pass (cddr lst)))))))

(defun bottom-up-mergesort (lst)
  "Bottom-up merge sort implementation."
  (let ((sorted-pairs (pairwise-sort lst)))
    (format t "First pass (sorted pairs): ~a~%" sorted-pairs) ;; Capturing first pass
    (loop while (> (length sorted-pairs) 1) do
      (setq sorted-pairs (merge-pass sorted-pairs)))
    (car sorted-pairs)))

;; Example usage:
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))
