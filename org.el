(require 'org)

(use-package ob-julia-vterm
  :ensure t
  :config
  (defalias 'org-babel-execute:julia
    'org-babel-execute:julia-vterm)
  (defalias 'org-babel-variable-assignments:julia
    'org-babel-variable-assignments:julia-vterm)
  (setenv "JULIA_PROJECT" "@."))

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))

(setq org-confirm-babel-evaluate nil)

(setq org-image-actual-width 500)
