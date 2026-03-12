;;; dmg-dired.el --- dired and dirvish setup & related functions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025  Daniel Griswold

;; Author: Daniel Griswold <kc5gmr@gmail.com>
;; URL: 
;; Version: 0.1.0


;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; functions that modify/tweak/extend org-mode operation for my Emacs

;;; Code:

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :hook
  (dired-mode . dired-omit-mode))

(load-library "dired-open")

(require 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)

;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("w" "~/Wp/"                       "Wp")
     ("s" "~/Wp/Sermons/2026/"          "Sermons")
     ("w" "~/Wp/Scholarship/"           "Writing/Scholarship")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-x d" . dirvish-dwim)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   ("^"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; register a plain text dirvish dispatcher for docx files
(dirvish-define-preview docx (file ext)
  "Preview docx files in plain text
 Require: `pandoc' (executable)"
  :require ("pandoc" )
  (cond ((equal ext "docx") `(shell . ("pandoc" "-t" "plain",file)))))

(add-to-list 'dirvish-preview-dispatchers 'docx)

;; register a plain text dirvish dispatcher for odt files
(dirvish-define-preview odt (file ext)
  "Preview docx files in plain text
 Require: `pandoc' (executable)"
  :require ("pandoc" )
  (cond ((equal ext "odt") `(shell . ("pandoc" "-t" "plain",file)))))

(add-to-list 'dirvish-preview-dispatchers 'odt)

(dirvish-define-preview doc (file ext)
  "Preview doc files in plain text
 Require: `catdoc' (executable)"
  :require ("catdoc" )
  (cond ((equal ext "doc") `(shell . ("catdoc" ,file)))))

(add-to-list 'dirvish-preview-dispatchers 'doc)


(provide 'dmg-dired)
