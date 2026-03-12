;;; dmg-etc.el --- miscellaneous functions for my dotemacs -*- lexical-binding: t -*-

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
;; some functions for my Emacs that don't fit elsewhere

;;; Code:



;;;###autoload
(defun dmg-quorum (active requested)
  "function for calculating the adjusted quorum"
  (let* ((adjust (- active requested))
         (c (ceiling adjust 2)))
    (next-line 1)
    (beginning-of-line)
    (kill-line)
    (if (cl-evenp adjust)
        (insert (format " *Adjusted quorum: %s*" (+ c 1)))
      (insert (format " *Adjusted quorum: %s*" c)))))


;;;###autoload
(defun dmg-atime (&optional arg)
  "Calculate time in the agenda"
  (if (> arg 55) (setq agendaminutes arg))
  (setq amins agendaminutes)
  (if (and (< arg 56) (> arg 0))
      (setq agendaminutes (+ arg agendaminutes)))
  (if (= arg 0) (setq agendaminutes 0))
  (let* ((hours (/ amins 60))
         (minutes (- amins (* hours 60))))
    (if t; (and org-export-current-backend "latex")
        (concat "\\marginpar{" (number-to-string hours) ":" (format "%02d" minutes) "}"))))

;;;###autoload
(defun dmg-search-sermons (text)
  "Look in the sermons for the text"
  (interactive "sEnter search text: ")
  (grep-compute-defaults)
  (rgrep text "*.tex" "/home/dan/Wp/Sermons/" nil))

(defun wc-non-interactive (start end)
  "Count the number of words in the current region."
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (count-matches "\\sw+"))))

;;;###autoload
(defun wc-buffer ()
  "Display the number of words in the current buffer."
  (interactive)
  (message (concat "The current buffer contains "
		       (number-to-string
			(wc-non-interactive (point-min) (point-max)))
		       " words.")))

;;;###autoload
(defun wc-region (start end)
  "Display number of words in the region."
  (interactive "r")
  (message (concat "The current region contains "
		       (number-to-string
			(wc-non-interactive start end))
		       " words.")))

;;;###autoload
(defun wc-dwim ()
  "Display a word count.
If there is a region defined, display the count for the region.
If not, display a word count for the whole buffer."
  (interactive)
  (if mark-active
	  (wc-region (point) (mark))
	(wc-buffer)))

(defalias 'wc 'wc-dwim)


(provide 'dmg-etc)
