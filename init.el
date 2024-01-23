;; Use MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Anti-littering measures
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(use-package no-littering :ensure t)
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file)

;; GUI Stuff
(load-theme 'phoebe t)
(set-face-attribute
 'default nil
 :height 140)

(use-package ivy
  :ensure t
  :config
  (setq ivy-count-format "")
  (setq ivy-height 8)
  (ivy-mode))


(setq-default indent-tabs-mode nil)

;; Auto-refresh buffers after changes on disk.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.01)

(setq echo-keystrokes 0.01)
(setq scroll-conservatively 5)

;; Resize and scroll pixelwise
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

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-v$-excludes-newline t)
  (setq evil-split-window-below t)
  (setq evil-echo-state nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (setq evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(load (locate-user-emacs-file "modeline.el"))
(load (locate-user-emacs-file "lsp.el"))
(load (locate-user-emacs-file "keys.el"))
