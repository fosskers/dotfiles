;;; autoload/ui.el -*- lexical-binding: t; -*-

(defun colin/dashboard-center (str)
  "Yield a STR centered on the dashboard."
  (+doom-dashboard--center +doom-dashboard--width str))

;;;###autoload
(defun colin/display-saying ()
  "Pick a random saying to display on the Doom splash screen."
  (insert
   "\n\n"
   (colin/dashboard-center "A good name is rather to be chosen than great riches,")
   "\n"
   (colin/dashboard-center "and loving favour rather than silver and gold.")))
