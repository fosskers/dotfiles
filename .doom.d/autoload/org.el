;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/org-today ()
  "Open today's agenda view."
  (interactive)
  (org-agenda-list)
  (org-agenda-day-view))

;;;###autoload
(defun colin/org-sort ()
  "Automate the sorting of org headings by their TODO status."
  (interactive)
  (outline-up-heading 1 'invisible-ok)
  (org-sort-entries nil ?o)
  (+org/toggle-fold)
  (+org/toggle-fold))

;;;###autoload
(defun colin/org-table-colour ()
  "Colour the org-mode table at 'point'."
  (interactive)
  (let* ((lisp (org-table-to-lisp))
         (rows (length lisp))
         (cols (length (car lisp))))
    (save-excursion
      (mapc (lambda (x) (mapc (lambda (y) (progn (org-table-goto-line y))
                                (org-table-goto-column x)
                                (colin/org--colour-cell x y))
                              (number-sequence 2 rows)))
            (number-sequence 2 cols)))))

(defun colin/org--colour-cell (x y)
  "Colour the cell at point."
  (when-let* ((cell (org-table-get y x))
              (nmbr (string-to-number cell))
              (face (cond ((> nmbr 0.5) '(:background "green"))
                          ((< nmbr -0.5) '(:foreground "black" :background "red"))
                          (t nil))))
    (when face
      (let ((overlay (make-overlay
                      (point)
                      (progn
                        (org-table-end-of-field 1)
                        (point)))))
        (overlay-put overlay 'face face)))))
