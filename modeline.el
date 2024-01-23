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
  '(:eval
    (propertize " %l:%c "
                'face (st/fallback-to-inactive 'vi-cursor)))
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
  '(:eval (let* ((name (format "vi-state-%s" evil-state))
                 (face (st/fallback-to-inactive (intern-soft name))))
            (propertize (st/format-evil-state)
                        'face face)))
  "Mode line construct for displaying the EVIL Vi state")

;; https://emacs.stackexchange.com/a/7542
(defun st/modeline-render (left right)
  (let* ((available-width (- (window-width) (length left))))
    (format (format "%%s %%%ds" available-width) left right)))

(defvar st/modeline-left
  '((evil-mode st/vi-state)
    "%e "
    mode-line-front-space
    mode-line-mule-info

    mode-line-client
    mode-line-modified
    mode-line-remote

    mode-line-frame-identification
    mode-line-buffer-identification


    (vc-mode vc-mode)
    "  "
    mode-line-misc-info
    mode-line-end-spaces)
  "Left-hand side of custom modeline.")

(defvar st/modeline-right
  '((t st/vi-cursor)
    (t mode-name))
  "Right-hand side of custom modeline.")

(setq-default mode-line-format 
              '((:eval (st/modeline-render
                        (format-mode-line st/modeline-left)
                        (format-mode-line st/modeline-right)))))
