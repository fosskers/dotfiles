;;; fp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/scanl (f zero items)
  "`scanl' from Haskell."
  (pcase items
    ('() (list zero))
    ((seq a &rest rest)
     (cons zero (colin/scanl f (funcall f zero a) rest)))))

;;;###autoload
(defun colin/pairs (items)
  "Form adjacent ITEMS into pairs.
If there is an odd number of elements, the last item is dropped."
  (pcase items
    ((pred (lambda (xs) (not (cdr xs)))) nil)
    ((seq a b &rest xs) (cons (cons a b) (colin/pairs xs)))))

