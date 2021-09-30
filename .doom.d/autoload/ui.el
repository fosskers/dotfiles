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
    ("You like being surrounded by trees.")
    ("You love mountains.")
    ("What would JP say?")
    ("What would Dad say?")
    ("Keep thy heart with all diligence;"
     "for out of it are the issues of life."
     "Proverbs 4:23")
    ("Strange, is it not? that of the myriads who"
     "Before us pass'd the door of Darkness through"
     "Not one returns to tell us of the Road,"
     "Which to discover we must travel too."
     "Omar Khayyam")
    ("節操"))
  "Messages to appear on the splash screen.")

(defun colin/dashboard-center (str)
  "Yield a STR centered on the dashboard."
  (+doom-dashboard--center +doom-dashboard--width str))

;;;###autoload
(defun colin/display-saying ()
  "Pick a random saying to display on the Doom splash screen."
  (thread-last (seq-random-elt colin/dashboard-messages)
    (mapcar #'colin/dashboard-center)
    (funcall (lambda (centered) (insert "\n" (string-join centered "\n") "\n")))))
