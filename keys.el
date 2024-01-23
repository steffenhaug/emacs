(require 'corfu)
(require 'evil)
(require 'ivy)

;; Stop completions when leaving insert mode.
(keymap-set evil-insert-state-map "<escape>"
            (lambda () (interactive) (corfu-quit) (evil-normal-state)))

;; Make completions not hijack the enter key.
(keymap-unset corfu-map "RET")

;; Evil window motions globally.
(keymap-global-set "C-w" 'evil-window-map)
(keymap-unset vterm-mode-map "C-w")

;; Buffer navigation
(keymap-global-set "C-c C-," 'mode-line-other-buffer)
(keymap-global-set "C-c C-;" 'ivy-switch-buffer)
