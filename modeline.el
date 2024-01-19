;; Modeline Customizations

;; Active Window Tracking

(defun st/is-active ()
  (window-parameter (selected-window) 'st/active))

(defun st/walk-track-active ()
  (let* ((track (lambda (w)
                  (set-window-parameter w
                   'st/active
                   ;; If the minibuffer is active, the "active window'
                   ;; is actually the one the minbuffer will fall back to.
                   (eq w (if (minibufferp)
                             (minibuffer-selected-window)
                           (selected-window)))))))
    (walk-windows track nil t)))

(add-hook 'window-state-change-hook
          #'st/walk-track-active)

;; Custom faces

(defface st/modeline-active
  '((t :inherit mode-line-active))
  "Default face for the modeline.")

(defface st/modeline-inactive
  '((t :inherit mode-line-inactive))
  "Default face for the modeline in inactive windows.")

(defun st/declare-modeline-element-faces (sym)
  "Define a pair of modeline-* and modeline-*-inactive faces."
  (let* ((state-inactive (intern (format "modeline-%s-inactive" (symbol-name sym))))
         (state          (intern (format "modeline-%s" (symbol-name sym)))))
    (custom-declare-face
     state '((t :inherit st/modeline-active))
     (format "Face for the %s modeline element." (symbol-name sym)))
    (custom-declare-face
     state-inactive '((t :inherit st/modeline-inactive))
     (format "Face for the %s modeline element in inactive windows." (symbol-name sym)))))

(defmacro st/fallback-to-inactive (face)
  "An expression for use with :eval to add a 'face property that changes with the active window."
  `(if (st/is-active)
       (intern (format "modeline-%s" (symbol-name ,face)))
     (intern (format "modeline-%s-inactive" (symbol-name ,face)))))


;; Library of custom modeline elements

(st/declare-modeline-element-faces 'vi-cursor)

(defconst st/vi-cursor
  `(:eval
    (propertize " %l:%c "
                'face ,(st/fallback-to-inactive 'vi-cursor)))
  "Mode line construct for displaying the position of the point.")

(dolist (st '(normal insert visual replace motion emacs))
  (let* ((vi-state (intern (format "vi-state-%s" st))))
    (st/declare-modeline-element-faces vi-state)))

(defun st/format-evil-state ()
  "Print the current EVIL-state in a manner suitable for the mode-line."
  (if evil-mode
      (let* ((name (symbol-name evil-state))
             (abbr (substring name 0 1)))
        (format " %s " (capitalize abbr)))))

(defconst st/vi-state
  `(:eval (let* ((name (format "vi-state-%s" evil-state))
                 (face (st/fallback-to-inactive (intern-soft name))))
            (propertize (st/format-evil-state)
                        'face face)))
  "Mode line construct for displaying the EVIL Vi state")

;; Mode for managing the custom modeline

(defvar st/original-modeline-format nil
  "Variable to store the original mode-line-format.")

(defvar st/modeline-format
  '((evil-mode st/vi-state)
    "%e"
    " "
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    (vc-mode vc-mode)
    st/vi-cursor
    mode-line-misc-info
    mode-line-end-spaces)
  "Customized modeline format.")

(define-minor-mode st-modeline-mode
  "Toggle My Custom Minor Mode."
  :init-value nil
  :global t
  (if st-modeline-mode
      (progn
        (unless st/original-modeline-format
          (setq st/original-modeline-format mode-line-format))
        (setq mode-line-format st/modeline-format))
    ;; Restore the original mode-line-format
    (setq mode-line-format st/original-modeline-format)))

(st-modeline-mode 1)
(st-modeline-mode 0)


mode-line-format
