(require 'corfu)
(require 'evil)
(require 'vertico)
(require 'consult)
(require 'vterm)
(require 'multi-vterm)

;; Unbinds
(keymap-global-unset "C-x C-b")
(keymap-unset vterm-mode-map "C-w")
(keymap-unset evil-normal-state-map "C-t")
(keymap-global-unset "C-t")

;; Make completions not hijack the enter key.
(keymap-unset corfu-map "RET")

;; Stop completions when leaving insert mode.
(keymap-set evil-insert-state-map "<escape>"
            (lambda () (interactive) (corfu-quit) (evil-normal-state)))

;; Evil window motions globally.
(keymap-global-set "C-w" 'evil-window-map)

;; Buffer navigation
(keymap-global-set "C-c C-," 'mode-line-other-buffer)

;; :x save and kill buffer instead of closing window.
(defun st/save-kill ()
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(evil-ex-define-cmd "x" 'st/save-kill)

;; Consult
(keymap-global-set "C-x b" 'consult-buffer)
(keymap-global-set "C-x p b" 'consult-project-buffer)
(keymap-global-set "C-s" 'consult-line)

;; Navigating named terminals

(defun st/named-vterm (name)
  "Create a named vterm"
  (let* ((buf (get-buffer name)))
    (if buf (pop-to-buffer buf)
      (setq buf (multi-vterm-get-buffer))
      (set-buffer buf)
      (rename-buffer name)
      (switch-to-buffer buf))))


(defun st/vt-sh ()
  (interactive)
  (st/named-vterm "*sh*"))

(defun st/vt-htop ()
  (interactive)
  (let ((vterm-shell "/bin/htop"))
    (st/named-vterm "*htop*")))

(unless (fboundp 'htop)
  (defalias 'htop 'st/vt-htop))

(keymap-global-set "C-t" 'st/vt-sh)
