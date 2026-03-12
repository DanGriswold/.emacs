;;; dmg-mode-line.el --- fancy mode line -*- lexical-binding: t -*-

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

;; From https://blog.tygr.info/emacs/mode-line.html#org350b2e1 but that site no longer exists; if the original has done more work this I'd love to see it

;;; Code:


(use-package minions
  :init
  (setq minions-mode-line-lighter "...")
  (setq minions-direct '(flyspell-mode))
  (add-hook 'after-init-hook #'minions-mode))

(defun my-mode-line/padding ()
   (let ((r-length (length (format-mode-line global-mode-string))))
     (propertize " "
		 'display `(space :align-to (- right ,r-length)))))

(setq-default mode-line-format
  ;; (if (display-graphic-p)
  (if t ; made this change because my emacs starts as a systemd service
      '("%e"
        my-mode-line/modal-editing
        my-mode-line/modified
        my-mode-line/major-mode
        " %l %o "
        ;; my-mode-line/dir
        mode-line-buffer-identification
        "  "
        minions-mode-line-modes
        (:eval (my-mode-line/padding))
	global-mode-string)
      '("%e"
        mode-line-front-space
        mode-line-modified
        " %l:%C %o "
        ;; my-mode-line/dir
        mode-line-buffer-identification
        "  "
        mode-line-modes
        (:eval (my-mode-line/padding)))))

(defvar-local my-mode-line/modified nil)
;;;###autoload
(put 'my-mode-line/modified 'risky-local-variable t)

(defun my-mode-line/update-modified (&optional arg)
  (let ((icon (cond (buffer-read-only
                       (propertize (all-the-icons-octicon "lock")
                         'help-echo 'mode-line-read-only-help-echo
                         'local-map (purecopy (make-mode-line-mouse-map
                                                'mouse-1
                                                #'mode-line-toggle-read-only))
                         'mouse-face 'mode-line-highlight))
                    ((or (buffer-modified-p) (string= arg "modified"))
                       (propertize (all-the-icons-faicon "chain-broken")
                         'help-echo 'mode-line-modified-help-echo
                         'local-map (purecopy (make-mode-line-mouse-map
                                               'mouse-1
                                               #'save-buffer))
                         'mouse-face 'mode-line-highlight))
                    (t
                       (propertize (all-the-icons-faicon "link")
                         'help-echo 'mode-line-read-only-help-echo
                         'local-map (purecopy (make-mode-line-mouse-map
                                                'mouse-1
                                                #'mode-line-toggle-read-only))
                         'mouse-face 'mode-line-highlight)))))
    (setq my-mode-line/modified
          (format " %s " (propertize icon 'display '(raise 0.01))))
    (force-mode-line-update)))

;; First change hook runs before buffer is modified. Passing "modified" to my function
;; will override the result of (buffer-modified-p) and the chain-broken icon will be displayed
(add-hook 'first-change-hook (lambda () (my-mode-line/update-modified "modified")))
(add-hook 'buffer-list-update-hook #'my-mode-line/update-modified)
(add-hook 'after-save-hook #'my-mode-line/update-modified)
(add-hook 'read-only-mode-hook #'my-mode-line/update-modified)
(advice-add 'undo :after #'my-mode-line/update-modified)

(defvar-local my-mode-line/major-mode nil)
;;;###autoload
(put 'my-mode-line/major-mode 'risky-local-variable t)

(defun my-mode-line/update-major-mode (&rest _)
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon)
      (setq my-mode-line/major-mode
            (format " %s "
              (propertize icon
                'display '(raise 0.0)
                'help-echo (format "Major mode: `%s`" major-mode)))))))

(add-hook 'buffer-list-update-hook #'my-mode-line/update-major-mode)
(add-hook 'after-change-major-mode-hook #'my-mode-line/update-major-mode t)

(defvar-local my-mode-line/dir nil)
;;;###autoload
(put 'my-mode-line/dir 'risky-local-variable t)

(setq my-mode-line/dir-length 7)

(defun my-mode-line/update-dir ()
  (setq my-mode-line/dir
    (if buffer-file-name
      (propertize
        (format
          " %s:%s" (or (system-name) (getenv "SYSTEM_NAME"))
          (reverse
            (truncate-string-to-width
              (reverse
                (replace-regexp-in-string
                  "\\(^\\\./\\)"
                  ""
                  (file-relative-name default-directory "~")))
              (+ 1 my-mode-line/dir-length) nil nil "-")))
        'help-echo "Open dired buffer" 'local-map
        (purecopy
          (make-mode-line-mouse-map
            'mouse-1
            (lambda () (interactive) (dired default-directory))))
        'mouse-face 'mode-line-highlight)
      nil)))

(add-hook 'find-file-hook #'my-mode-line/update-dir)

(setq display-time-string-forms
      (if (display-graphic-p)
          '((propertize
              (format "%s, %s%s %s:%s%s "
                dayname monthname day 12-hours minutes (upcase am-pm))
              'help-echo "Open calendar"
              'local-map (purecopy (make-mode-line-mouse-map
                                     'mouse-1
                                     (lambda () (interactive) (calendar))))
              'mouse-face 'mode-line-highlight))
          '((format "%s:%s%s "
              12-hours minutes (upcase am-pm)))))

(display-time-mode -1)


(provide 'dmg-mode-line)
