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
