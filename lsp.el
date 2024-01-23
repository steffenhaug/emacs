;; LSP and language-specific modes


;; OCAML
   
(use-package tuareg
  :ensure t
  :config
  (keymap-unset tuareg-mode-map "C-c"))

(use-package dune :ensure t)


;; JULIA

(use-package julia-mode
  :ensure t)

