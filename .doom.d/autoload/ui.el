;;; autoload/ui.el -*- lexical-binding: t; -*-

(defconst colin/dashboard-messages
  '(("A good name is rather to be chosen than great riches,"
     "and loving favour rather than silver and gold."
     "Proverbs 22:1")
    ("And when Abram was ninety years old and nine,"
     "the Lord appeared to Abram, and said unto him,"
     "I am the Almighty God; walk before me, and be thou perfect."
     "Genesis 17:1")
    ("You envy those who are clean and organised.")
    ("You love to sing.")
    ("You like being surrounded by trees."))
  "Messages to appear on the splash screen.")

(defun colin/dashboard-center (str)
  "Yield a STR centered on the dashboard."
  (+doom-dashboard--center +doom-dashboard--width str))

;;;###autoload
(defun colin/display-saying ()
  "Pick a random saying to display on the Doom splash screen."
  (let* ((saying (seq-random-elt colin/dashboard-messages))
         (centered (mapcar #'colin/dashboard-center saying)))
    (insert "\n" (string-join centered "\n") "\n")))
