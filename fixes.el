(defun evil-adjust-cursor (&optional _)
  "Move point one character back if at the end of a non-empty line.
This behavior is controlled by `evil-move-beyond-eol'."
  (and (not evil-move-beyond-eol)
       (not (bolp)) ;; fix scrolling past images in org mode
       (eolp)
       (= (point) (save-excursion (evil-move-end-of-line) (point)))
       (evil-move-cursor-back t)))
