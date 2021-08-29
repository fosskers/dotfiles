;;; autoload/kanji.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/kin-graph (kanji)
  "Produce a `kanji-net' graph based on KANJI and open it in a new buffer.

You can pass as many Kanji as you want as a single string. Spaces
aren't necessary, but will be accounted for on kin's end."
  (interactive "sKanji: ")
  (message! "You gave: %s" kanji)
  (let* ((outpath "/tmp/graph.png")
         (res (doom-call-process "kin" "graph" kanji "--output" outpath)))
    (when (= 0 (car res))
      (find-file-read-only outpath))))

;;;###autoload
(defun colin/kin-graph-by-parents (parents)
  "Produce a `kanji-net' graph based on some Kanji's PARENTS and open it in a new buffer.

You can pass as many Kanji as you want as a single string. Spaces
aren't necessary, but will be accounted for on kin's end."
  (interactive "sKanji Parents: ")
  (let* ((outpath "/tmp/graph.png")
         (res (doom-call-process "kin" "graph" parents "--parents" "--output" outpath)))
    (when (= 0 (car res))
      (find-file-read-only outpath))))
