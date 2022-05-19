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
(cl-defun colin/hledger-transfers (income &key (expenses 1500))
  "Automate monthly transfers, given some INCOME.
An EXPENSES value can also be added manually."
  (interactive "nIncome: ")
  (when-let* ((buffer (find-file-noselect hledger-jfile))
              (raw (colin/hledger-transfers-raw income expenses)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (insert raw)
      (save-buffer))))

;;;###autoload
(defun colin/hledger-transfers-raw (income expenses)
  "Given a month's INCOME and expected EXPENSES, produce a valid Hledger transaction string."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (tax (* income 0.30))
         (take-home (- income tax))
         (donation (* take-home 0.10)))
         ;; (japan (* take-home 0.10)))
         ;; (house (* take-home 0.10)))  ;; Or it should get the rest?
         ;; (piggy (* take-home 0.05)))  ;; For big purchases
         ;; (travel (* take-home 0.05)))
         ;; (tfsa (* take-home 0.10)))
    (concat (format "%s Monthly Transfers\n" today)
            (format "    assets:koho:vault:tax:2022  %.2f C\n" tax)
            (format "    assets:koho:vault:donation  %.2f C\n" donation)
            ;; (format "    assets:tan:chequing        %.2f C  ; tfsa\n" tfsa)
            (format "    assets:koho:spend         = %.2f C\n" expenses)
            (format "    assets:koho:vault:japan\n"))))
