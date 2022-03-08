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
  (let* ((x-vals (mapcar (lambda (row) (nth x-pos row)) table))
         (y-vals (mapcar (lambda (row) (nth y-pos row)) table))
         (x-mean (colin/mean x-vals))
         (y-mean (colin/mean y-vals))
         (x-diff (mapcar (lambda (x) (- x x-mean)) x-vals))
         (y-diff (mapcar (lambda (y) (- y y-mean)) y-vals))
         (numer  (apply #'+ (cl-mapcar #'* x-diff y-diff)))
         (denom  (sqrt (* (apply #'+ (mapcar (lambda (x) (math-pow x 2)) x-diff))
                          (apply #'+ (mapcar (lambda (y) (math-pow y 2)) y-diff))))))
    (/ numer denom)))

;;;###autoload
(defun colin/correlation-matrix (table labels start end)
  "Given START and END indices into a TABLE, produce a matrix of correlation values and note it with LABELS."
  (cons (cons "" labels)
        (mapcar (lambda (x) (cons (nth (- x start) labels)
                                  (mapcar (lambda (y) (let* ((cor (colin/correlation table x y))
                                                             (formatted (format "%.2f" cor)))
                                                        (if (> cor 0.5)
                                                            ;; (propertize formatted 'face '(:background "green"))
                                                            (propertize formatted 'face 'italic)
                                                          formatted)))
                                          (number-sequence start end))))
                (number-sequence start end))))
