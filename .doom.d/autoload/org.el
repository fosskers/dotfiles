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
(defun colin/org-table-select (columns table)
  "Given the names of COLUMNS, filter a TABLE to contain only those.
Preserves unnamed columns, assuming they're providing row labels, etc.

colin/org-table-select :: [String] -> Table -> Table"
  (when-let* ((col-names (colin/org-table-columns table))
              (filtered (seq-filter (lambda (pair) (-contains-p columns (cdr pair))) col-names))
              (first-col (car (car filtered))))
    (thread-last (-drop 2 table)
                 (mapcar (lambda (row)
                           (append (-take first-col row)
                                   (mapcar (lambda (pair) (nth (car pair) row))
                                           filtered))))
                 (cons 'hline)
                 (cons (append (-repeat first-col "")
                               (mapcar #'cdr filtered))))))

(defun colin/org-table-columns (table)
  "Retrieve the names and 0-based indices of the columns of a TABLE.

colin/org-table-columns :: Table -> [(Int, String)]"
  (let* ((top-row (car table))
         (cols (length top-row)))
    (seq-filter #'identity
                (cl-mapcar (lambda (i item)
                             (pcase item
                               ((or "" "!") nil)
                               (thing (cons i thing))))
                           (number-sequence 0 (1- cols))
                           top-row))))

;;;###autoload
(defun colin/org-table-get-column (column table)
  "Fetch a COLUMN from a TABLE.

colin/org-table-get-column :: String -> Table -> [String]"
  (when-let* ((pairs (colin/org-table-columns table))
              (index (car (-find (lambda (pair) (string-equal column (cdr pair))) pairs))))
    (mapcar (lambda (row) (nth index row))
            (-drop 2 table))))

;;;###autoload
(defun colin/org-table-to-lisp (table-name)
  "Find a table named by TABLE-NAME and yields its contents as a Lisp object.
In this case, the Table is a list-of-lists, except for the second row, which is
assumed to be the `'hline' symbol.

colin/org-table-get-table :: String -> Table"
  (colin/org-table-goto-named table-name)
  (org-table-to-lisp))

;;;###autoload
(defun colin/org-can-i-go-home-yet ()
  "Can I go home yet?"
  (interactive)
  (org-babel-with-temp-filebuffer "/home/colin/contracting/upwork.org"
    (let* ((minutes (org-clock-sum-today))
           (hours (/ minutes 60.0)))
      (cond ((>= hours 5.0) (message "%.2f hours: You can go home!" hours))
            (t (message "%.2f hours: Keep at it." hours))))))
