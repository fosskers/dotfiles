;;; autoload/math.el -*- lexical-binding: t; -*-

(defun colin/weights (length)
  "Given a target LENGTH, produce a list of weight values."
  (let ((interval (/ 1.0 length))
        (indices (number-sequence 0 (1- length))))
    (mapcar (lambda (ix) (- 1.0 (* ix interval))) indices)))

;;;###autoload
(defun colin/apply-weights (items)
  "Given some numerical ITEMS, reduce them by their calculated weights."
  (thread-last (length items)
               (colin/weights)
               (cl-mapcar #'* items)))

;;;###autoload
(defun colin/mean (items)
  "Find the average value of some numerical ITEMS."
  (/ (apply #'+ items)
     (float (length items))))

(defun colin/mean-lenient (items)
  "Find the average value of some numerical ITEMS.
Unlike `colin/mean', will not crash if some elements are nil."
  (colin/mean (colin/filter-non-nil items)))

;;;###autoload
(defun colin/median (items)
  "Find the median value of some ITEMS."
  (let* ((len (length items))
         (med (/ len 2))
         (sorted (seq-sort #'< items)))
    (cond ((cl-evenp len) (/ (+ (nth med sorted)
                                (nth (1- med) sorted))
                             2.0))
          (t (nth med sorted)))))

;;;###autoload
(defun colin/correlation (table x-pos y-pos)
  "Find the correlation of two variables (columns) in a TABLE.
Accounts for missing data by ignoring pairs where one or both is nil,
instead of crashing."
  (let* ((x-vals (mapcar (lambda (row) (colin/lisp-object-to-number (nth x-pos row))) table))
         (y-vals (mapcar (lambda (row) (colin/lisp-object-to-number (nth y-pos row))) table))
         (x-mean (colin/mean-lenient x-vals))
         (y-mean (colin/mean-lenient y-vals))
         (x-diff (mapcar (lambda (x) (when x (- x x-mean))) x-vals))
         (y-diff (mapcar (lambda (y) (when y (- y y-mean))) y-vals))
         (numer  (apply #'+ (cl-mapcar (lambda (x y) (* (or x 0) (or y 0))) x-diff y-diff)))
         (denom  (sqrt (* (apply #'+ (mapcar (lambda (x) (math-pow (or x 0) 2)) x-diff))
                          (apply #'+ (mapcar (lambda (y) (math-pow (or y 0) 2)) y-diff))))))
    (/ numer denom)))

;;;###autoload
(defun colin/correlation-matrix (table-name)
  "Given a TABLE-NAME, produce a correlation matrix of its data."
  (save-excursion
    (when-let* ((table (colin/org-table-to-lisp table-name))
                (col-pairs (colin/org-table-columns table))
                (col-ixs (mapcar #'car col-pairs))
                (col-names (mapcar #'cdr col-pairs))
                (indices (number-sequence 0 (1- (length col-names))))
                (rows (thread-last (-drop 2 table)
                                   (mapcar (lambda (row)
                                             (colin/filter-non-nil (cl-mapcar (lambda (i a) (when (seq-contains-p col-ixs i) a))
                                                                              (number-sequence 0 (1- (length row)))
                                                                              row)))))))
      (cons (cons "" col-names)
            (cons 'hline
                  (mapcar (lambda (y) (cons (nth y col-names)
                                            (mapcar (lambda (x) (format "%.2f" (colin/correlation rows x y)))
                                                    indices)))
                          indices))))))
