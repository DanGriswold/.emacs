;;; dmg-org.el --- org-mode setup & related functions for my dotemacs -*- lexical-binding: t -*-

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

(use-package org
  ;; :diminish outline-minor-mode
  :hook ((org-mode . visual-line-mode)
         (org-mode . global-org-modern-mode)
         (org-mode .
                   (lambda ()
                     (setq-local eldoc-documentation-function
                                 #'ignore))))

  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         :map org-mode-map
         ;; ([(control tab)] . other-window)
         ("C-a" . org-beginning-of-line)
         ("C-e" . org-end-of-line)
         ([f12]  . dmg-agenda-export))
  :config
  (require 'org-crypt)
  (require 'org-modern)
  (org-crypt-use-before-save-magic)
  (setq org-agenda-overriding-columns-format
        "%50ITEM(Task) %5Effort(e){:} %TAGS"))

(setq org-tag-alist
      '((:startgroup)
        ("Location")
        (:grouptags)
        ("HOME" . ?h)
        ("CAR" . ?C)
        (:endgrouptag)
        (:startgroup)
        ("Tool")
        (:grouptags)
        ("EMAIL" . ?e)
        ("PHONE" . ?p)
        ("TEXT" . ?t)
        ("NET" . ?n)
        (:endgrouptag)
        (:startgroup)
        ("Attention")
        (:grouptags)
        ("shlw" . ?s)
        ("deep" . ?d)
        (:endgrouptag)
        (:startgroup)
        ("Creativity")
        (:grouptags)
        ("open" . ?o)
        ("closed" . ?c)
        (:endgrouptag)))



(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :config
  (org-pretty-tags-global-mode))

(set-face-attribute 'org-modern-symbol nil :family "Hack Nerd Font")

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-autolist
  :diminish org-autolist-mode
  :hook (org-mode . org-autolist-mode))
  
(use-package org-agenda
  :defer t
  :config
  ;; Override the key definition for org-exit
  (define-key org-agenda-mode-map "d" 'sacha/org-agenda-done))

(use-package ox-odt :defer t)

(require 'ox-pandoc)

(require 'elegant-agenda-mode)
(add-hook 'org-agenda-mode-hook 'elegant-agenda-mode)

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n s" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n b" . org-roam-dailies-goto-previous-note)
         ("C-c n f" . org-roam-dailies-goto-next-note))
  :hook (org-roam-mode . org-roam-db-autosync-enable))
(org-roam-db-autosync-mode)

(use-package org-roam-ui :defer t)

(use-package org-roam-dailies :defer t)

(setq org-agenda-cmp-user-defined 'dmg-org-agenda-cmp-user-defined-created-date)

(use-package org-capture :defer t)

(use-package org-protocol :defer t)

(autoload 'org-in-clocktable-p "org-clock")

(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

(define-key org-mode-map (kbd "C-c SPC") #'org-table-blank-field)

;; have bold/italic work over several lines
(setcar (nthcdr 4 org-emphasis-regexp-components) 30)
(org-set-emph-re 'org-emphasis-regexp-components
		 org-emphasis-regexp-components)




;; Functions

(defun dmg-new-journal-daily-dentry ()
  "set up a new daily entry in today's org-roam-dailies daily-note"
  (org-roam-dailies-capture-today)
  (org-agenda nil "d")
  (set-buffer "*Org Agenda(d:)*")
  (org-agenda-goto-today)
  (org-agenda-earlier 1)
  (if (eq (calendar-day-of-week (calendar-current-date)) 1)
      (find-dired "~/Dropbox/" "-mtime -3")
    (find-dired "~/Dropbox/" "-mtime -1"))
  (find-file (concat "~/Dropbox/Org_Roam/daily/" (string-trim (shell-command-to-string "ls ~/Dropbox/Org_Roam/daily | tail -n 1")))))

(defun dmg-new-journal-weekly-dentry ()
  "Set up a new weekly entry in today's org-roam-dailies daily-note"
  (org-roam-dailies-goto-today)
  (goto-char (point-max))
  (insert-file-contents "~/Dropbox/Org_other/jwtemplate_dailies.org"))

(defun dmg-new-journal-monthly-dentry ()
  "Set up a new weekly entry in today's org-roam-dailies daily-note"
  (org-roam-dailies-goto-today)
  (goto-char (point-max))
  (insert-file-contents "~/Dropbox/Org_other/jmtemplate_dailies.org"))

;;;###autoload
(defun dmg-new-theology-journal-entry ()
  "Set up a new entry in theojournal.org"
  (find-file "~/Dropbox/Brain/theojournal.org")
  (setq-local eldoc-documentation-function #'ignore)
  (org-datetree-find-date-create (org-date-to-gregorian
                                  (format-time-string "%Y-%m-%d %H:%M")))
  (goto-char (point-max)))

;;;###autoload
(defun dmg-agenda-export ()
  (message "Export starting ...")
  (save-excursion
    (org-up-heading-safe)
    (org-up-heading-safe)
    (org-open-file (org-latex-export-to-pdf nil t)))
  (message "Export completed"))

;;;###autoload
(defun dmg-compile-book ()
  (save-window-excursion
    (if (get-buffer "bookmaster.org")
        (switch-to-buffer "bookmaster.org")
      (find-file "~/Wp/Scholarship/CallingCounterpoint/bookmaster.org"))
    (org-latex-export-to-pdf)))

(defun dmg-org-agenda-cmp-user-defined-created-date (a b)
  "Org Agenda user function to sort tasks based on CREATED property."
  ;; from https://emacs.stackexchange.com/a/60616
  (let* (
         (marker-a (get-text-property 0 'org-marker a))
         (marker-b (get-text-property 0 'org-marker b))
         (time-a (if marker-a (org-entry-get marker-a "CREATED") nil))
         (time-b (if marker-b (org-entry-get marker-b "CREATED") nil)))

    (if (and time-a time-b)
        (if (org-time< time-a time-b)
            -1
          (if (org-time> time-a time-b) 1 nil))
      (if time-a -1 1)
      )))


;;;###autoload
(defun dmg-org-insert-offset-time-stamp (offset)
    "Inserts org formatted dates based on previously entered and saved value"
  ;; (unless (boundp 'dmg-ssdate) (setq dmg-ssdate org-last-inserted-timestamp))
  (org-insert-time-stamp (time-add dmg-eventdate (days-to-time offset))))


;;;###autoload
(defun dmg-org-new-deadline ()
    "Prompts for new event date, sets deadline, inserts date for conclusion of the project (3 days after the event)"
  (setq dmg-eventdate (org-read-date nil t nil "Event Date:"))
  (dmg-org-insert-offset-time-stamp 3))

;;;###autoload
(defun my/org-todo-age (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (let* ((created
              (org-time-string-to-time
               (org-entry-get (or pos (point)) "CREATED" t)))
             (age (time-subtract (current-time) created)))
        (format "%.0fd" (time-to-number-of-days age))))))

;;;###autoload
(defun my/org-todo-daysleft (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "DEADLINE" t)))
    (when stamp
      (let* ((goaldate
              (org-time-string-to-time
               (org-entry-get (or pos (point)) "DEADLINE" t)))
             (age (time-subtract goaldate (current-time))))
        (format "%.0fd" (time-to-number-of-days age))))))

;;;###autoload
(defun my/org-todo-monthyr (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "DEADLINE" t)))
    (when stamp
      (let* ((deadlndate
              (org-time-string-to-time
               (org-entry-get (or pos (point)) "DEADLINE" t))))
        (format-time-string "%b '%y" deadlndate)))))

(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
    This changes the line at point, all other lines in the agenda referring to
    the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
    
(defun dmg-buffer-beginning ()
  (goto-char (point-min)))
(add-hook 'org-agenda-finalize-hook #'dmg-buffer-beginning t)

(provide 'dmg-org)
