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

;; ;;;###autoload
;; (defun colin/mean-lenient (items)
;;   "Find the average value of some numerical ITEMS.
;; Unlike `colin/mean', will not crash if some elements are nil."
;;   (colin/mean (colin/filter-non-nil items)))

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
(defun colin/correlation-matrix (table)
  "Given a TABLE, produce a correlation matrix of its data."
  (when-let* ((col-pairs (colin/org-table-columns table))
              (col-ixs (mapcar #'car col-pairs))
              (col-names (mapcar #'cdr col-pairs))
              (clean-table (seq-filter #'listp (-drop 2 table)))
              (col-diffs (mapcar (fn! (colin/correlation-diffs clean-table %)) col-ixs))
              (indices (number-sequence 0 (1- (length col-diffs)))))
    (cons (cons "" col-names)
          (cons 'hline
                (mapcar (lambda (y) (cons (nth y col-names)
                                         (mapcar (lambda (x) (format "%.2f" (colin/correlation-fast (nth x col-diffs)
                                                                                               (nth y col-diffs))))
                                                 indices)))
                        indices)))))

(defun colin/correlation-diffs (table ix)
  "Given a TABLE and a column IX, find all the correlation diff values."
  (let* ((vals (mapcar (lambda (row) (colin/lisp-object-to-number (nth ix row))) table))
         (mean (colin/mean-lenient vals)))
    (mapcar (lambda (n) (when n (- n mean))) vals)))

(defun colin/correlation-fast (x-diff y-diff)
  "The final step of the correlation calculation between two measurement categories."
  (let* ((numer (apply #'+ (cl-mapcar (lambda (x y) (* (or x 0) (or y 0))) x-diff y-diff)))
         (denom (sqrt (* (apply #'+ (mapcar (lambda (x) (math-pow (or x 0) 2)) x-diff))
                         (apply #'+ (mapcar (lambda (y) (math-pow (or y 0) 2)) y-diff))))))
    (/ numer denom)))
