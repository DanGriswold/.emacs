;;; dmg-latex.el --- latex functions for my dotemacs -*- lexical-binding: t -*-

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
;; LaTeX related functions for my Emacs

;;; Code:


;; Attribution for ldots:
;; Copyright (C) 2005 Jesse Rosenthal
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Created: 30 Oct 2005
;; Description: On-the-fly rewriting of three dots as `\ldots'.

;;;###autoload
(defun test-for-periods()
  (save-excursion
    (let ((first (char-to-string (char-before (- (point) 1))))
          (second (char-to-string (preceding-char))))
      (concat first second))))

;;;###autoload
(defun period-to-ldots()
  (interactive)
  (cond ((and (/= (point) 1) (/= (point) 2) (string= (test-for-periods) ".."))
         (backward-delete-char 2)
         (insert "\\ldots"))
        (t (insert "."))))

;;;###autoload
(defun latex-auto-ldots ()
  (bind-key "." 'period-to-ldots LaTeX-mode-map))

(add-hook 'LaTeX-mode-hook 'latex-auto-ldots)
(add-hook 'LaTeX-mode-hook 'olivetti-mode)

;;;###autoload
(defun dmg-mkverse (begin end)
  (interactive "r")
  (if (eq major-mode 'LaTeX-mode)
	 (save-restriction
	   (narrow-to-region begin end)
	   (LaTeX-insert-environment "verse")
	   (narrow-to-region begin (- end 1))
	   (goto-char (point-min))
	   (while (search-forward-regexp
		   "\\,”\\|\\.”\\|\\?”\\|\\,\\|\\.\\|\\?" nil t)
	     (replace-match "\\&\\\\\\\\ \n"))
	   (goto-char (point-min))
	   (insert "\n")
	   (widen)
	   (forward-line 1)
	   (fill-paragraph))))

;;;###autoload
(defun dmg-sermonsizes ()
  "Display a list of the tex files in the current
directory with word count, reverse sorted by size"
  (interactive)
  (shell-command "find . -name \"*.tex\" -print0 | xargs -n1 -0 -Ixxx sh -c 'printf \"%20s: %5s\n\" `basename xxx .tex`  `detex xxx |grep -v document | wc -w`' |sort -k 2 -n"
		    "*Sermon Sizes*")
  (other-window 1)
  (switch-to-buffer "*Sermon Sizes*")
  (view-mode t)
  (setq view-exit-action
	   (lambda (buffer)
	     (kill-buffer "*Sermon Sizes*")
	     (other-window 1)
	     (delete-other-windows))))

;;;###autoload
(defun dmg-narrow-latex ()
  (interactive)
  (narrow-to-region (+ 18 (string-match "begin{document}"(buffer-string)))
		       (- (string-match "end{document}"(buffer-string)) 1)))

;;;###autoload
(defun dmg-greek-switch ()
  "Switch the font for variable face between Century Schoolbook L and
Galatia SIL. The former is more readable for latin script, while the
latter is superior for polytonic greek"
  (interactive)
  (let ((fa (prin1-to-string
		  (custom-face-attributes-get 'variable-pitch (selected-frame))))
	   )
    (message "%s" "switching font")
    (if (string-match "SIL" fa)
	   (set-face-font 'variable-pitch "Century Schoolbook L-16")
	 (set-face-font 'variable-pitch "Galatia SIL-16"))))

(setq font-latex-quote-list
	 '(("“" "”")
	   ("``" "''")))

;;;###autoload
(defun dmg-emph (&optional arg)
  "As in cdlatex-mode, puts \\emph{} around the word on \"'e\""
  (interactive "p")
  (or arg (setq arg 1))
  (let ((char (read-key-sequence "?")))
    (if (string= char "e")
	   (progn
	     (insert "}")
	     (save-excursion
	       (left-word arg)
	       (insert "\\emph{" )))
	 (insert "'" char))))

;;;###autoload
(defun dmg-latex-tfb ()
  (if (eq major-mode 'LaTeX-mode)
	 (progn
	   (TeX-fold-buffer))))
(add-hook 'after-save-hook 'dmg-latex-tfb)

;;;###autoload
(defun convert-quotes ()
  "Convert regular quotes to LaTeX quotes."
  (interactive)
  (when (eq major-mode 'LaTeX-mode)
    (push-mark)
    (while (re-search-forward " \"" nil t)
	 (replace-match "``" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([^ ]\\)\"" nil t)
	 (replace-match "\\1''" nil nil))
    (pop-mark)
    (exchange-point-and-mark)))

;;;###autoload
(defun dmg-convert-quotes ()
  "Convert LaTeX ascii quotes with unicode quotes"
  (interactive)
  (when (eq major-mode 'LaTeX-mode)
    (push-mark)
    (while (re-search-forward "``" nil t)
	 (replace-match "“" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "''" nil t)
	 (replace-match "”" nil nil))
    (pop-mark)
    (exchange-point-and-mark)))

;;;###autoload
(defun dmg-add-tex-style ()
    (TeX-add-style-hook
    "sermon"
    (lambda ()
      (TeX-add-symbols
       '("title"
         (TeX-arg-string "Title")
         (TeX-arg-string "Date")
         (TeX-arg-string "Bible 1")
         (TeX-arg-string "Bible 2"))))))

(with-eval-after-load "tex-fold"
  (defun TeX-fold-overfull-p (ov-start ov-end display-string)
    "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
    (and
     (save-excursion
       (goto-char ov-end)
       (search-backward "\n" ov-start t))
     (not (string-match "\n" display-string))
     (> (+ (- ov-start
              (save-excursion
		(goto-char ov-start)
		(line-beginning-position)))
           (length display-string)
           (- (save-excursion
		(goto-char ov-end)
		(line-end-position))
              ov-end))
	(current-fill-column)))))

;;;###autoload
(defun dmg-agenda-export ()
  (interactive)
  (message "Export starting ...")
  (save-excursion
    (org-up-heading-safe)
    (org-up-heading-safe)
    (org-open-file (org-latex-export-to-pdf nil t)))
  (message "Export completed"))

(provide 'dmg-latex)
