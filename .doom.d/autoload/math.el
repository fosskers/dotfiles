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
  "Find the correlation of two variables (columns) in a TABLE."
  (let* ((x-vals (seq-filter #'identity (mapcar (lambda (row) (colin/lisp-object-to-number (nth x-pos row))) table)))
         (y-vals (seq-filter #'identity (mapcar (lambda (row) (colin/lisp-object-to-number (nth y-pos row))) table)))
         (x-mean (colin/mean x-vals))
         (y-mean (colin/mean y-vals))
         (x-diff (mapcar (lambda (x) (- x x-mean)) x-vals))
         (y-diff (mapcar (lambda (y) (- y y-mean)) y-vals))
         (numer  (apply #'+ (cl-mapcar #'* x-diff y-diff)))
         (denom  (sqrt (* (apply #'+ (mapcar (lambda (x) (math-pow x 2)) x-diff))
                          (apply #'+ (mapcar (lambda (y) (math-pow y 2)) y-diff))))))
    (/ numer denom)))

;;;###autoload
(defun colin/correlation-matrix (table-name)
  "Given a TABLE-NAME, produce a correlation matrix of its data."
  (save-excursion
    (colin/org-table-goto-named table-name)
    (org-table-analyze)
    (let* ((col-names (mapcar #'car org-table-column-names))
           (col-ixs (mapcar (lambda (pair) (1- (string-to-number (cdr pair)))) org-table-column-names))
           (indices (number-sequence 0 (1- (length col-names))))
           (rows (thread-last (-drop 2 (org-table-to-lisp))
                              (mapcar (lambda (row) (seq-filter #'identity
                                                           (cl-mapcar (lambda (i a) (when (seq-contains-p col-ixs i) a))
                                                               (number-sequence 0 (1- (length row)))
                                                               row)))))))
      (cons (cons "" col-names)
            (cons 'hline
                  (mapcar (lambda (y) (cons (nth y col-names)
                                       (mapcar (lambda (x) (format "%.2f" (colin/correlation rows x y)))
                                               indices)))
                          indices))))))
