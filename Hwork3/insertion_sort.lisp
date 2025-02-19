(defun insert-into-sorted (x sorted)
  "Inserts an element X into a sorted list in ascending order."
  (cond
    ((null sorted) (list x))  ; If sorted list is empty, return single element list
    ((<= x (car sorted)) (cons x sorted))  ; Insert at the beginning if smaller
    (t (cons (car sorted) (insert-into-sorted x (cdr sorted))))))  ; Recursively insert

(defun insertion-sort-helper (sorted unsorted)
  "Recursive helper function for insertion sort."
  (if (null unsorted)
      sorted  ; If no unsorted elements left, return sorted list
      (insertion-sort-helper
        (insert-into-sorted (car unsorted) sorted)  ; Insert first element into sorted list
        (cdr unsorted))))  ; Continue with remaining elements

(defun insertion-sort (lst)
  "Insertion Sort implementation in Lisp."
  (insertion-sort-helper nil lst))  ; Start with an empty sorted list
(print (insertion-sort '(5 3 8 1 2 9 4 7 6)))
