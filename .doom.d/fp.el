;;; fp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/scanl (f zero items)
  "`scanl' from Haskell."
  (cond ((not items) (list zero))
        (t (cons zero
                 (colin/scanl f
                              (funcall f zero (car items))
                              (cdr items))))))
