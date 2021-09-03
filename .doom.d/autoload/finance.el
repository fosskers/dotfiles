;;; autoload/finance.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/upwork-earnings (rate hours usd-to-cad)
  "Estimate earnings for an hourly job on Upwork.

Given an hourly RATE and the HOURS to be worked, projects a final
pay amount with the Upwork cuts taken off. Applies a USD-TO-CAD
conversion rate at the end."
  (let* ((gross (* rate hours))
         (usd (cond ((<= gross 500.0) (* gross 0.8))
                    ((<= gross 10000.0) (+ (* 0.8 500)
                                           (* 0.9 (- gross 500.0))))
                    (t (+ (* 0.8 500)
                          (* 0.9 9500)
                          (* 0.95 (- gross 10000.0)))))))
    (round (* usd usd-to-cad))))

;;;###autoload
(defun colin/hledger-transfers (income liabilities extra)
  "Automate monthly transfers, given some INCOME and LIABILITIES.
Adds EXTRA to the budget for this month."
  (interactive "nIncome: \nnMastercard: ")
  (when-let* ((buffer (find-file-noselect hledger-jfile))
              (raw (colin/hledger-transfers-raw income liabilities extra)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (insert raw)
      (save-buffer))))

;;;###autoload
(defun colin/hledger-transfers-raw (income liabilities extra)
  "Given a month's INCOME, its LIABILITIES, and some EXTRA cash to pad the budget, produce a valid Hledger transaction string."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (tax (* income 0.30))
         (take-home (- income tax))
         (donation (* take-home 0.10))
         (tfsa (* take-home 0.10))
         (expense-budget 2000))
    (concat (format "%s Monthly Transfers\n" today)
            (format "    assets:koho:vault:tax       %.2f C\n" tax)
            (format "    assets:koho:vault:donation  %.2f C\n" donation)
            (format "    assets:qtrade:tfsa          %.2f C\n" tfsa)
            (format "    liabilities:bs:mastercard   %.2f C\n" liabilities)
            (format "    assets:koho:spend         = %.2f C\n" (+ expense-budget extra))
            ;; Germany gets all the leftovers, or if there was a deficit, money is pulled back out.
            (format "    assets:koho:germany"))))
