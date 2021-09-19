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
