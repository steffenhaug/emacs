;; LSP and language-specific modes
(require 'eglot)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package corfu
  :ensure t
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.02)
  (setq corfu-preview-current nil)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package eldoc
  :ensure t
  :config
  (setq eldoc-idle-delay 0.0)
  (setq eldoc-echo-area-use-multiline-p 2)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;; OCAML
   
(use-package tuareg
  :ensure t
  :config
  (keymap-unset tuareg-mode-map "C-c"))

(use-package dune :ensure t)


;; JULIA

(use-package julia-mode
  :ensure t)
