(defun split-list (lst)
  "Splits a list into two nearly equal halves recursively."
  (if (or (null lst) (null (cdr lst)))  ; Base case: List has 0 or 1 element
      (values lst nil)
      (let ((first nil)
            (second nil))
        (loop for (a b) on lst by #'cddr do
              (push a first)
              (when b (push b second)))
        (values (nreverse first) (nreverse second)))))
(defun merge-lists (lst1 lst2)
  "Merges two sorted lists recursively."
  (cond ((null lst1) lst2)  ; If first list is empty, return second
        ((null lst2) lst1)  ; If second list is empty, return first
        ((<= (car lst1) (car lst2))  ; Compare first elements
         (cons (car lst1) (merge-lists (cdr lst1) lst2)))
        (t (cons (car lst2) (merge-lists lst1 (cdr lst2))))))
(defun merge-lists (lst1 lst2)
  "Merges two sorted lists recursively."
  (cond ((null lst1) lst2)  ; If first list is empty, return second
        ((null lst2) lst1)  ; If second list is empty, return first
        ((<= (car lst1) (car lst2))  ; Compare first elements
         (cons (car lst1) (merge-lists (cdr lst1) lst2)))
        (t (cons (car lst2) (merge-lists lst1 (cdr lst2))))))
(defun mergesort (lst)
  "Mergesort algorithm using the partition and merge functions."
  (if (or (null lst) (null (cdr lst)))  ; Base case: list with 0 or 1 element
      lst
      (multiple-value-bind (first-half second-half) (split-list lst)
        (merge-lists (mergesort first-half) (mergesort second-half)))))
(print (mergesort '(4 1 7 3 9 2 6 5 8))) 