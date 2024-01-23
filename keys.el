;; Stop completion when leaving insert mode.
(keymap-set evil-insert-state-map "ESC"
            (lambda () (interactive) (corfu-quit) (evil-normal-state)))

(keymap-unset corfu-map "RET")

;; Evil window motions globally.
(keymap-unset vterm-mode-map "C-w")
(keymap-global-set "C-w" 'evil-window-map)

;; Buffer navigation
(keymap-global-set "C-c C-," 'mode-line-other-buffer)
(keymap-global-set "C-c C-;" 'ivy-switch-buffer)
