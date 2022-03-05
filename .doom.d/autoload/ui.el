;;; autoload/ui.el -*- lexical-binding: t; -*-

(defconst colin/dashboard-messages
  '(("A good name is rather to be chosen than great riches,"
     "and loving favour rather than silver and gold."
     "-- Proverbs 22:1")
    ("And when Abram was ninety years old and nine,"
     "the Lord appeared to Abram, and said unto him,"
     "I am the Almighty God; walk before me, and be thou perfect."
     "-- Genesis 17:1")
    ("Vater unser im Himmel,"
     "geheiligt werde dein Name."
     "Dein Reich komme."
     "Dein Wille geschehe,"
     "wie in Himmel so auf Erden."
     "Unser tägliches Brot gib uns heute."
     "Und vergib uns unsere Schuld,"
     "wie auch wir vergeben unsern Schuldigern."
     "Und führe uns nicht in Versuchung,"
     "sondern erlöse uns von dem Bösen."
     "Denn dein ist das Reich und die Kraft"
     "und die Herrlichkeit in Ewigkeit."
     "Amen.")
    ("You envy those who are clean and organised.")
    ("You love to sing.")
    ("You like being surrounded by trees.")
    ("You love mountains.")
    ("You are a language learner.")
    ("You love plane trips.")
    ("What would JP say?")
    ("What would Dad say?")
    ("Keep thy heart with all diligence;"
     "for out of it are the issues of life."
     "-- Proverbs 4:23")
    ("Strange, is it not? that of the myriads who"
     "Before us pass'd the door of Darkness through"
     "Not one returns to tell us of the Road,"
     "Which to discover we must travel too."
     "-- Omar Khayyam")
    ("節操")
    ("Set up your environment for success.")
    ("But thou seest all are not of thy Train;"
     "there be who Faith prefer, and Pietie to God,"
     "though then to thee not visible,"
     "when I alone seemed in thy World erroneous to dissent from all:"
     "my Sect thou seest, now learn too late"
     "how few sometimes may know, when thousands err."
     "-- Abdiel, to Satan before the First Battle")
    ("Live to 120.")
    ("Devotees of Zen remind themselves that the spot"
     "upon which they sit for meditation is the throne"
     "of all the buddhas of the past and future.")
    ("When you're lost, get a map.")
    ("Stop doing things for kids.")
    ("The dose makes the poison.")
    ("One who cannot cast away a treasure at need is in fetters."
     "-- Aragorn")
    ("You should be comin' and goin'."
     "-- Chris Rock")
    ("Get up.")
    ("Pupil: Where do we go when we die?"
     "Master: How badly do you want to know?"))
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
