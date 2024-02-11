(require 'org)

(use-package ob-julia-vterm
  :ensure t
  :config
  (defalias 'org-babel-execute:julia
    'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia
    'org-babel-variable-assignments:julia-vterm)
  (setenv "JULIA_PROJECT" "@."))

(use-package polymode
  :ensure t)

(use-package poly-org
  :ensure t
  :after polymode)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))

(setq org-confirm-babel-evaluate nil)

(setq org-image-actual-width 500)

(setq org-src-window-setup 'split-window-below)
(setq org-src-tab-acts-natively t)


(keymap-set poly-org-mode-map "C-c C-c" 'org-ctrl-c-ctrl-c)
