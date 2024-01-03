;; Use MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Keep the directory clean!
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(use-package no-littering :ensure t)
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file)

(progn
  (load-theme 'modus-operandi t)
  (custom-set-faces
   '(mode-line          ((t (:box nil))))
   '(mode-line-inactive ((t (:box nil))))))

(use-package ivy
  :ensure t
  :config
  (setq ivy-count-format "")
  (setq ivy-height 8)
  (ivy-mode))

(setq-default indent-tabs-mode nil)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.01)

(setq echo-keystrokes 0.01)

(setq frame-resize-pixelwise t)
(setq pixel-scroll-precision-interpolate-mice t)
(pixel-scroll-precision-mode)

(setq inhibit-startup-screen t)

(setq-default truncate-lines t)
(scroll-bar-mode -1)        ; Scrollbar
(tool-bar-mode -1)          ; Toolbar
(tooltip-mode -1)           ; Tooltips (hovering with mouse)
(set-fringe-mode 15)        ; Padding on the sides of the text
(menu-bar-mode -1)          ; Disable the menu bar (File, Edit, ...)
(line-number-mode -1)       ; Line number in modeline
(save-place-mode 1)
(winner-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :after vterm
  :ensure t)

(use-package magit
  :ensure t)
   
(use-package tuareg
  :ensure t
  :config
  (keymap-unset tuareg-mode-map "C-c"))

(use-package dune :ensure t)

(defvar oaty/compile-called nil
  "Flag to track if I have compiled before.")

(make-variable-buffer-local 'oaty/compile-called)

(defun oaty/compile ()
  (interactive)
  (if oaty/compile-called
      (recompile)
    (setq oaty/compile-called t)
    (if (project-current)
        (project-compile)
      (compile))))

(keymap-global-set "C-c C-c" 'oaty/compile)
