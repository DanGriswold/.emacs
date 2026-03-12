;;; dmg-display.el --- Display and formatting settings & functions for my dotemacs -*- lexical-binding: t -*-

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
;; Functions that affect how things are displayed

;;; Code:

(require 'all-the-icons-completion)
(use-package diminish)
(global-set-key (kbd "C-x C-r") 'recentf)
(recentf-mode 1)

(global-set-key "\C-x\C-b" 'ibuffer)

(add-hook 'ibuffer-mode-hook 
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-do-sort-by-alphabetic)))
(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package uniquify)
   
(global-set-key [S-mouse-1] 'mouse-appearance-menu)

(bind-key [f5] 'describe-face-at-point)


(add-hook 'server-after-make-frame-hook 'dmg-reset-frame-params)

(setq split-width-threshold 100)
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 115)
(set-face-attribute 'variable-pitch nil :family "Source Sans Variable" :height 115)

(tooltip-mode -1)
(modify-all-frames-parameters
'((right-divider-width . 40)
  (internal-border-width . 40)))

(use-package circadian
  :config
  (setq calendar-latitude 42.8)
  (setq calendar-longitude -86.0)
  (circadian-setup))

(add-hook 'server-after-make-frame-hook #'dmg-circadian-setup)

(defun dmg-circadian-setup ()
  (circadian-setup)
   (remove-hook 'server-after-make-frame-hook #'dmg-circadian-setup))

(global-set-key (kbd "<WakeUp>") 'circadian-setup)

;; Opacity
(bind-key "C-7" 'dmg-dim)
(bind-key "C-8" 'dmg-opacity-modify)
(bind-key "C-9" 'dmg-opacity-less)
(bind-key "C-0" 'dmg-full)

;; Fontsets: Set my fonts so they look right for symbols and Greek  
(set-fontset-font "fontset-default" nil
                  (font-spec :name "Symbola"))

(set-fontset-font "fontset-default"
                  'greek-iso8859-7
                  "Gentium Plus")

(set-fontset-font "fontset-default"
                  'greek
                  (font-spec :family "Gentium Plus"))

(set-fontset-font "fontset-default"
                  'hebrew
                  (font-spec :family "Ezra SIL" :size 25))

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #x1f00)
                        (decode-char 'ucs #x1fef))
                  "Gentium Plus")

(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #x0590)
                        (decode-char 'ucs #x05f4))
                  "Ezra SIL")
(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #xf000)
                        (decode-char 'ucs #xf2b4))
                  "FontAwesome")

(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; my functions

(defun dmg-reset-frame-params (&optional theme)
  (interactive)
  (if multiple-frames nil
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))))

;;;###autoload
(defun dmg-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 10%-steps"
  (interactive)
  (let* ((alpha-or-nil (frame-parameter nil 'alpha-background)) ; nil before setting
	    (oldalpha (if alpha-or-nil alpha-or-nil 100))
	    (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
	 (modify-frame-parameters nil (list (cons 'alpha-background newalpha))))))

(defun dmg-opacity-less ()
  (interactive)
  (let* ((alpha-or-nil (frame-parameter nil 'alpha-background)) ; nil before setting
	    (oldalpha (if alpha-or-nil alpha-or-nil 100))
	    (newalpha (- oldalpha 10)))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
	 (modify-frame-parameters nil (list (cons 'alpha-background newalpha))))))
  


;;;###autoload
(defun dmg-dim ()
  (interactive)
  "Turn opacity way down / transparency way up"
  (modify-frame-parameters nil `((alpha-background . 20))))

;;;###autoload
(defun dmg-full ()
  (interactive)
  "Turn opacity way up / transparency off"
  (modify-frame-parameters nil `((alpha-background . 100))))

;;;###autoload
(defun dmg-variable-pitch-mode (&optional arg)
  "My own Variable-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `dmg-variable-pitch' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke (font-face-attributes "Alex Brush-30") t nil))

;;;###autoload
(defun dmg-unfill-paragraph ()
  (let ((fill-column (point-max)))
    (if (use-region-p)
	   (fill-region (region-beginning) (region-end))
	 (fill-paragraph nil t))))

;;;###autoload
(defun dmg-show_keys ()
  "display window of interesting emacs keystrokes"
  (interactive)
  (find-file-other-frame "~/Dropbox/Org_other/keys.org")
  (org-mode)
  (use-local-map (copy-keymap org-mode-map))
  (local-set-key "q" 'delete-frame)
  (set-frame-parameter nil 'menu-bar-lines 0)
  (setq mode-line-format nil)
  (when (display-graphic-p)
    (set-frame-size (selected-frame) 49 11)
    (set-frame-position (selected-frame) 0 28)))

(bind-key [M-f1] 'dmg-show_keys)

(defun dmg-downcase-word ()
  "Make the whole word lowercase"
  (interactive)
  (backward-to-word 1)
  (downcase-word 1))

(defun dmg-capitalize-word ()
  "Make this word capitalized, from its first letter"
  (interactive)
  (backward-to-word 1)
  (capitalize-word 1))

(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (let ((face (get-char-property (point) 'face)))
    (if (listp face) 
	(progn
	  (message (format "Full value: %s" (prin1-to-string face)))
	  (setq face (car face))))
    (describe-face face)))


(provide 'dmg-display)
