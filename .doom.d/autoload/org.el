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
(defun colin/org-table-goto-named (name)
  "Move `point' to the start of an org table named NAME."
  (interactive "sTable name: ")
  (goto-char (point-min))
  (re-search-forward (format "#\\+name: %s" name))
  (forward-line))

;;;###autoload
(defun colin/org-table-select (table columns)
  "Given the names of COLUMNS, filter a TABLE to contain only those.
Preserves unnamed columns, assuming they're providing row labels, etc.")

(defun colin/org-table-columns (table)
  "Retrieve the names and 0-based indices of the columns of a TABLE.

Table -> [(Int, String)]")

;;;###autoload
(defun colin/org-can-i-go-home-yet ()
  "Can I go home yet?"
  (interactive)
  (org-babel-with-temp-filebuffer "/home/colin/contracting/upwork.org"
    (let* ((minutes (org-clock-sum-today))
           (hours (/ minutes 60.0)))
      (cond ((>= hours 5.0) (message "%.2f hours: You can go home!" hours))
            (t (message "%.2f hours: Keep at it." hours))))))
