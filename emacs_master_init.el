;; * Header
;; .emacs file for Dan Griswold
;; hostname: cantor
;; Time-stamp: "2017-08-05 11:13:07 prediger dan"

;; * Initial settings
(when (display-graphic-p)
  (modify-frame-parameters nil `((alpha . 40))))

;(setq debug-on-error nil)
(load-library "url-handlers")

(package-initialize)

(when (display-graphic-p)
  (mode-icons-mode))

(require 'airline-themes)

;; (add-hook 'after-init-hook #'global-ify-mode)

(use-package hydra
  :bind
  ([f9] . dmg/hydra-emoji-template/body)
  ([f8] . hydra-clockin/body)

  :config
  (defhydra dmg/hydra-emoji-template (:color blue :hint nil)
    "
_t_hink     _b_uy      _a_sk     _p_ick/decide
_c_all      _l_ook     _w_rite
"
    ("t" (insert "🤔"))
    ("b" (insert "🛒"))
    ("a" (insert "❓"))
    ("p" (insert "⛏"))
    ("c" (insert "📞"))
    ("l" (insert "🔎"))
    ("w" (insert "🖋")))
  
  (defun dmg/clockin-thing (link)
    (interactive)
    (org-open-link-from-string link t)
    (with-current-buffer "worklog.org" (org-clock-in))
    (other-window 1)
    (delete-other-windows))

  
  (defhydra hydra-clockin (:color blue
			   :hint nil)
    "
_d_: start Day      _e_: Email          _t_: start Timer   _o_: clock Out
_j_: Journaling     _f_: phone calls    _s_: Stop          _i_: c-i last
_p_: Prayer         _r_: Reading        _u_: paUse         _w_: sermon Writing
                  _D_: Daily review
"
    ("e" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Email]]"))
    ("f" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Phone%20Calls]]"))
    ("r" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Reading/Study]]"))
    ("d" (progn
	   (dmg/clockin-thing
	    "[[file:~/Dropbox/GTD/worklog.org::*Start%20up%20the%20day]]")
	   (org-clock-goto)
	   (next-line)
	   (org-flag-drawer nil)
	   (next-line)
	   (org-end-of-line)
	   (backward-char 2)))
    ("j" (dmg/journal-do) )
    ("p" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Prayer]]"))
    ("D" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Daily%20Review]]"))
    ("t" org-timer-start)
    ("s" org-timer-stop)
    ("u" org-timer-pause-or-continue)
    ("o" org-clock-out)
    ("i" (org-clock-in '(4)))
    ("w" (dmg/clockin-thing "[[file:~/Dropbox/GTD/worklog.org::*Writing]]"))
    )
  
  (defun dmg/journal-do ()
    "Open a journal entry"
    (interactive)
    (save-excursion
      (org-open-link-from-string "[[file:~/Dropbox/GTD/worklog.org::*Journaling][Journaling]]" t)
      (with-current-buffer "worklog.org"
	(org-clock-in))
      (dmg/new-journal-entry)
      (org-agenda nil "y" nil))))



(eval-when-compile
  (require 'use-package))

(use-package diminish
  :config
  (setq diminished-mode-alist
	'((outline-minor-mode " Outl")
	  (auto-fill-function " Fill")
	  (abbrev-mode)
	  (buffer-face-mode "BF"))))

(use-package bind-key)

(use-package cl)

(setq my/work-agenda-files
      (list "~/Dropbox/GTD/life.org"
	    "~/Dropbox/GTD/trinity.org"
	    "~/Dropbox/GTD/rsa.org"
	    "~/Dropbox/GTD/ce.org"
	    "~/Dropbox/GTD/ci.org"
	    "~/Dropbox/GTD/classis.org")
      my/personal-agenda-files
      (list "~/Dropbox/GTD/life.org"
	    "~/Dropbox/GTD/pso.org"
	    "~/Dropbox/GTD/ce.org"
	    "~/Dropbox/GTD/personal.org"
	    "~/Dropbox/GTD/ci.org")
      my/total-agenda-files
      (list "~/Dropbox/GTD/rsa.org"
	    "~/Dropbox/GTD/ci.org"
	    "~/Dropbox/GTD/classis.org"
	    "~/Dropbox/GTD/pso.org"
	    "~/Dropbox/GTD/ce.org"
	    "~/Dropbox/GTD/personal.org"
	    "~/Dropbox/GTD/life.org"
	    "~/Dropbox/GTD/trinity.org"))


(bind-key "C-h V" 'customize-variable)

;; * Customized variables and faces
(setq custom-file "~/Dropbox/Emacs_git/emacs-custom.el")
(load custom-file)



;; * Display settings

(cond
 ((string-equal system-name  "alto3880")
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "Cousine-14")
    (setq initial-frame-alist
	  '((top . 19) (left . 270) (width . 81) (height . 45)))
    (set-frame-size (selected-frame) 81 31)
    (set-frame-position (selected-frame) 250 23))
  (load-theme 'airline-cool)
  (load-theme 'solarized-dark))
 ((string-equal system-name "cantor")
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-14" :height 136)
    (setq initial-frame-alist
     	  '((top . 21) (left . 250) (width . 81) (height . 40)))
    (set-frame-size (selected-frame) 81 40)
    (set-frame-position (selected-frame) 220 20))
    ;; (load-theme 'airline-cool)
    ;; (load-theme 'solarized-dark)
    (setq sml/theme 'dark)
    (sml/setup)
  )
 ((string-equal system-name "prediger")
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-14" :height 136))
    (set-frame-size (selected-frame) 81 44)
    (set-frame-position (selected-frame) 215 28)
    (load-theme 'airline-cool)
    (load-theme 'solarized-dark)))


(global-set-key (kbd "C-a") 'beginning-of-visual-line)
(global-set-key (kbd "C-e") 'end-of-visual-line)

;; sets cursor color blue for regular, red for overwrite, black for read-only
(setq hcz-set-cursor-color-color ""
      hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "pink"
	   (if overwrite-mode "red"
	     "blue"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(diminish 'auto-fill-function)

(use-package ido
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))


(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(global-set-key "\C-x\C-b" 'ibuffer)

(defvar my/filtergroup
  (if (string-equal system-name "prediger")
      (quote "work")
    (quote "home")))

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups my/filtergroup)
	     (ibuffer-do-sort-by-alphabetic)))

(defun ac (code)
  "Lookup area code"
  (interactive "sEnter area code: ")
  (find-file "/usr/share/doc/miscfiles/na.phone.gz")
  (occur code)
  (kill-buffer "na.phone.gz"))


;(auto-image-file-mode 1)

(use-package uniquify)

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(global-set-key [S-mouse-1] 'mouse-appearance-menu)

(add-hook 'js-mode-hook (lambda ()
			  (when (string= ".conkerorrc" (buffer-name))
			    (conkeror-minor-mode 1))))

;; Opacity
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil 100))
	 (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal

(when (display-graphic-p)
  (progn
    (global-set-key (kbd "C-7") '(lambda()(interactive)
				   (modify-frame-parameters nil
							    `((alpha . 20)))))
    (global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
    (global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
    (global-set-key (kbd "C-0") '(lambda()(interactive)
				   (modify-frame-parameters nil
							    `((alpha . 100)))))))

(use-package tramp)

(use-package persistent-scratch)
(persistent-scratch-setup-default)

(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'scroll-left      'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column  'disabled nil)

(use-package calendar)

(setq completion-ignored-extensions
      (append '(".log" ".xdv" ".dvi") completion-ignored-extensions))

(use-package dired)
(use-package dired-details
  :init
  (require 'dired+)
  (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
  :config
  (require 'dired-sort)
  (dired-details-install))

(use-package dired-x
  :init
  (add-hook 'dired-mode-hook (lambda ()
;			       (dired-icon-mode)
			       (dired-omit-mode 1))))


(use-package dired-sort-menu)

(eval-after-load 'tramp
 '(progn
    ;; Allow to use: /sudo:user@host:/path/to/file
    (add-to-list 'tramp-default-proxies-alist
		  '(".*" "\\`.+\\'" "/ssh:%h:"))))


(setq leuven-scale-outline-headlines nil)

(global-pretty-mode 1)

(defun dmg/switch-light-dark ()
  (interactive)
  (if (find 'solarized-light custom-enabled-themes)
      (progn
	(disable-theme 'solarized-light)
	(load-theme 'solarized-dark))
    (progn
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light))))


;; * Editing
;; ** General editing prefs

(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/.myabbrevs")
(setq abbrev-file-name "~/Dropbox/.myabbrevs")
(setq save-abbrevs t)

(global-set-key (kbd "M-T") 'transpose-sentences)
		
(setq ispell-really-hunspell t)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(pending-delete-mode 1)

(global-font-lock-mode t)

(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x10000)
			(decode-char 'ucs #x1ffff))
		  "Segoe UI Symbol")

(set-fontset-font "fontset-default"
                  'greek-iso8859-7
                  "Galatia SIL-13")

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f00)
			(decode-char 'ucs #x1fef))
		  "Galatia SIL-13")

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x0590)
			(decode-char 'ucs #x05f4))
                  "Ezra SIL")

 (setq face-font-rescale-alist
         '(
           (".*SIL.*" . 1.5)))

;(auto-fill-mode)

;(autoload 'smiley-region "smiley" nil t)

;; font lock settings

;(global-font-lock-mode 1 t)
(setq lazy-lock-continuity-time nil
      lazy-lock-stealth-time nil
      x-super-keysym 'alt)

;; function to find out which face is which
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (let ((face (get-char-property (point) 'face)))
    (if (listp face) 
        (progn
          (message (format "Full value: %s" (prin1-to-string face)))
          (setq face (car face))))
;    (hyper-describe-face face)))
    (describe-face face)))

(bind-key [f5] 'describe-face-at-point)


(setq auto-mode-alist (cons '("mozex" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("conkerorrc" . js-mode) auto-mode-alist))
(push '("\\[tT]e[xX]\\'" . latex-mode) auto-mode-alist)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-to-list 'auto-mode-alist '("/var/log/.*" . syslog-mode))

;; de-Word docs
;; (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
;; (add-to-list 'auto-mode-alist '("\\.DOC\\'" . no-word))


(defun no-word ()
  "Run antiword on the entire buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "antiword - " t t))

(setq auto-mode-alist (cons '("\\.pl$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . cperl-mode) auto-mode-alist))

(defalias 'perl-mode 'cperl-mode)


;(use-package midi-input)

(defun dmg/insert-footnote ()
  "insert \footnote macro at cursor point."
  (interactive)
  (insert  "\\footnote{}")
  (backward-char 1))


(bind-key [C-tab] 'other-window)
(bind-key [M-BS] 'backward-kill-word)

(autoload 'remember "remember" nil t)
(autoload 'remember-region "remember" nil t)


;(load-library "graphviz-dot-mode")

(use-package mm-url)

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

(provide 'wc)


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(defalias 'yes-or-no-p 'y-or-n-p)

;; (add-hook 'weblogger-entry-mode
;; 	  '(lambda ()
;; 	     (longlines-mode t)
;; 	     (longlines-show-hard-newlines t)
;; 	     ))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil) 	t))))

(use-package dictionary
  :bind ([mouse-3] . dictionary-mouse-popup-matching-words)
  :config
  (defun my-dictionary-search ()
    (interactive)
    (let ((word (current-word))
	  (enable-recursive-minibuffers t)
	  (val))
      (setq val (read-from-minibuffer
		 (concat "Word"
			 (when word
			   (concat " (" word ")"))
			 ": ")))
      (dictionary-new-search
       (cons (cond
              ((and (equal val "") word)
               word)
              ((> (length val) 0)
               val)
              (t
               (error "No word to lookup")))
	     dictionary-default-dictionary)))))

; long lines for blog edits
(setq auto-mode-alist (append '(("www\\.chicagoinvitation\\.org/.*\\.txt"
				. longlines-mode))
			      auto-mode-alist))


(defun dmg/term-send-backward-kill-word ()
  "Kill just the previous sub-word, not the whole blasted thing"
  (interactive)
  (term-send-raw-string "\e\C-H"))


(use-package wc-mode)

(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package outlined-elisp-mode)
 
(setq ad-redefinition-action 'accept)

(bind-key "M-SPC" 'cycle-spacing)

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)



(defun dmg/downcase-word ()
  "Make the whole word lowercase"
  (interactive)
  (backward-to-word 1)
  (downcase-word 1))

(bind-key "M-l" 'dmg/downcase-word)

(defun dmg/capitalize-word ()
  "Make this word capitalized, from its first letter"
  (interactive)
  (backward-to-word 1)
  (capitalize-word 1))

(bind-key "M-c" 'dmg/capitalize-word)

(unbind-key "M-.")



;; ** Org-mode

(use-package org
  :diminish outline-minor-mode
  :init
  (setq org-ellipsis "⤵")

  :bind
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda)
  ("\C-cc" . org-capture)

  :config
  (bind-keys :map org-mode-map
	     ([(control tab)] . other-window)
	     ([f12]  . dmg/agenda-export)
	     ("'" . dmg/org-emph))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)


  (defun dmg/org-emph (&optional arg)
    "Makes previous word italicized, in org-mode"
    (interactive "p")
    (if (org-at-heading-p)
	(insert "'")
      (progn
	(or arg (setq arg 1))
	(let ((char (read-key-sequence "?")))
	  (if (string= char "e")
	      (progn
		(mark-word -1)
		(org-emphasize ?/))
	    (insert "'" char))))))

  (defun dmg/clockin ()
    (interactive)
    (let ((entry (org-entry-get nil "worklog" t)))
      (if entry
	  (progn
	    (org-open-link-from-string entry t)
	    (with-current-buffer "worklog.org"
	      (org-clock-in))
	    (other-window 1)
	    (delete-other-windows))
	(message "No worklog entry here"))))

  (defun dmg/agenda-clockin ()
    (interactive)
    (org-agenda-switch-to)
    (dmg/clockin)
    (switch-to-buffer "*Org Agenda(a)*"))


  (use-package org-bullets)

  (font-lock-add-keywords 'org-mode
  			  '(("^ +\\([-*]\\) "
  			     (0 (prog1 () (compose-region
					   (match-beginning 1)
					   (match-end 1) "•"))))))

  (add-hook 'org-mode-hook
	    (lambda()
	      (when (string= "agendas.org" (buffer-name))
		(message-box "Press <F12> for exporting agenda at location"))
	      (org-password-manager-key-bindings)
	      (org-bullets-mode 1)
	      (emojify-turn-on-emojify-mode)
	      (column-number-mode)))


  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))
  
  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (looking-back "^")
	  (hydra-org-template/body)
	(self-insert-command 1))))

  )

(use-package org-agenda
  :defer t
  :config
  (add-hook 'org-agenda-mode-hook 'emojify-turn-on-emojify-mode)

  (defun my/org-todo-age (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
	(let* ((created
		(org-time-string-to-time
		 (org-entry-get (or pos (point)) "CREATED" t)))
	       (age (time-subtract (current-time) created)))
	  (format "%.0fd" (time-to-number-of-days age))))))

  (defun my/org-todo-daysleft (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "GOALDATE" t)))
      (when stamp
	(let* ((goaldate
		(org-time-string-to-time
		 (org-entry-get (or pos (point)) "GOALDATE" t)))
	       (age (time-subtract goaldate (current-time))))
	  (format "%.0fd" (time-to-number-of-days age))))))

  (defun dmg/org-agenda-done (&optional arg)
    "Mark current item as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
    (interactive "P")
    (org-agenda-todo "DONE"))
  
  (bind-keys :map org-agenda-mode-map
	     ("d" . dmg/org-agenda-done)
	     ("I" . dmg/agenda-clockin)
	     ("i" . dmg/agenda-clockin))
  )

(use-package org-capture
  :defer t)

(use-package lilypond-mode
  :mode ("\\.ly$" . LilyPond-mode)
  :load-path "/usr/share/emacs/site-lisp/"
  :config
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock))))


(use-package hl-line
  :config
  (set-face-background 'hl-line "moccasin"))

;;iimage
(use-package iimage
  :config
  (setq iimage-mode-image-search-path (expand-file-name "~/")))

;;Match org file: links
(add-to-list 'iimage-mode-image-regex-alist
	     (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
			   "\\)\\]")  1))


;; ;; the 'w' corresponds with the 'w' used before as in:
;;   emacsclient \"org-protocol:/capture:/w/  [...]
(use-package org-protocol)

(autoload 'org-in-clocktable-p "org-clock")



(defun dmg/export-worship-sermon-planning ()
  "Export an html version of my sermon planning sheet for Jana"
  (interactive)
  (if (string= (buffer-name) "wsprep.org")
      (progn
	(org-html-export-to-html)
	(org-odt-export-to-odt)
	(kill-buffer "wsprep.odt")
	(copy-file "~/Dropbox/Org_other/wsprep.odt"
		   "/ftp:autometrics.us:wsprep.odt" t)
	(copy-file "~/Dropbox/Org_other/wsprep.html"
		   "/ftp:autometrics.us:wsprep.html" t)
	(rename-file "~/Dropbox/Org_other/wsprep.html" "~/Dropbox/Dan/wsprep.html" t)
	(rename-file "~/Dropbox/Org_other/wsprep.odt" "~/Dropbox/Dan/wsprep.odt" t)
	(delete-file "~/Dropbox/Org_other/wsprep.docx"))))

(add-hook 'after-save-hook 'dmg/export-worship-sermon-planning )

(defun my-odt-filter-pagerefs (text backend info)
       "Make page references, not textual references in ODT export."
       (when (org-export-derived-backend-p backend 'odt)
             (replace-regexp-in-string "text:reference-format=\"text\"" "text:reference-format=\"page\"" text)))

(defun dmg/update-tasks ()
  "Update the orgwidget on the Awesome wm bottom box"
  (interactive)
  (save-excursion
    (set-buffer "*scratch*")
    (org-agenda nil "U" nil)
    (delete-other-windows)
    (set-buffer "*Org Agenda(U:)*")
    (end-of-buffer)
    (let ((items (concat "echo '" "orgwidget:set_text(\" " (number-to-string (- (string-to-number (format-mode-line "%l")) 3)) " \")" "'" "| awesome-client" )))
      (shell-command items))
    (kill-buffer "*Org Agenda(U:)*")))

(defun dmg/task-updater ()
  "Handle call from after-save-hook"
  (if (assoc-string (abbreviate-file-name (buffer-file-name)) org-agenda-files)
      (dmg/update-tasks)))

(add-hook 'after-save-hook 'dmg/task-updater)

;(remove-hook 'after-save-hook 'dmg/task-updater)

;(use-package org-capture-pop-frame)

(use-package org-mobile)

;; this seems to mess up the locate function on prediger
;; (defvar monitor-attributes nil "Cached file attributes to be monitored.")
;; (defun install-monitor (file secs)
;;   (run-with-timer
;;    0 secs
;;    (lambda (f p)
;;      (let ((att (elt (file-attributes f) 5)))
;;        (unless (or (null monitor-attributes) (equalp monitor-attributes att))
;;          (org-mobile-pull)
;;                  (org-mobile-push)
;;                  )
;;        (setq monitor-attributes att)))
;;    file secs))
;; (defvar monitor-timer (install-monitor (concat org-mobile-directory "/mobileorg.org") 5)   "Check if MobileOrg/mobileorg.org is changed every 5s.")


(defun dmg/new-journal-entry ()
  "Set up a new entry in journal.org"
  (interactive)
  (find-file "~/Dropbox/GTD/journal.org")
  (org-datetree-find-date-create (org-date-to-gregorian
				  (format-time-string "%Y-%m-%d")))
  (next-line)
  (let ((lev (org-current-level)))
    (unless (= lev 4)
      (insert-file-contents "~/Dropbox/Org_other/jtemplate.org")))
  )

(defun dmg/agenda-export ()
  (interactive)
  (message "Export starting ...")
  (save-excursion
    (org-up-heading-safe)
    (org-up-heading-safe)
    (org-open-file (org-latex-export-to-pdf nil t)))
  (message "Export completed"))

(defun dmg/show_keys ()
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

(bind-key [M-f1] 'dmg/show_keys)

(use-package ox-odt
  :config
  (progn
    (setq org-odt-data-dir "/usr/local/share/emacs/etc/org/")))

(add-hook 'kill-emacs-hook
	  '(lambda ()
	     (if (marker-buffer org-clock-marker)
		 (progn
		   (org-clock-out)
		   (save-some-buffers t)))
	     (org-mobile-push)))

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

;; ** Edit Server
;; http://www.emacswiki.org/emacs/Edit_with_Emacs
;; `edit-server' package to edit the text boxes in Chrome browser using
;; emacsclient
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  (add-hook 'edit-server-start-hook #'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook #'edit-server-maybe-htmlize-buffer)
  :config
  (setq edit-server-default-major-mode 'org-mode))

;; Integration with Gmail
(use-package edit-server-htmlize
  :ensure t
  :defer t)




;; ** LaTeX

(use-package tex  :ensure auctex
  :init
  (defun dmg/emph (&optional arg)
    "Works like cdlatex-mode, but only for \"'e\": puts \emph{} around the word"
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
  :config
  (use-package latex
    :defer t
    :config
    (require 'preview)
    (bind-keys :map LaTeX-mode-map
	       ("'" . dmg/emph)
	       ([f6] . dmg/sermonsizes)
	       ([S-return] . "\\\\\n"))
    (add-hook 'LaTeX-mode-hook (lambda () (mixed-pitch-mode t)))
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 
	      '(lambda () (flyspell-mode t)))

    (setq reftex-plug-into-AUCTeX t
	  LaTeX-section-hook
	  '(LaTeX-section-heading
	    LaTeX-section-title
	    LaTeX-section-toc
	    LaTeX-section-section
	    )
	  LaTeX-letter-sender-address
	  "69 Stratton Road\\\\Rochester, NY 14610")
    
    ;;; the folllowing is from:
;;; ldots.el -- changes three dots to `\ldots' on the fly

    ;; Filename: ldots.el
    ;; Copyright (C) 2005 Jesse Rosenthal
    ;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
    ;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
    ;; Created: 30 Oct 2005
    ;; Description: On-the-fly rewriting of three dots as `\ldots'.

    (defun test-for-periods()
      (save-excursion
	(let ((first (char-to-string (char-before (- (point) 1))))
	      (second (char-to-string (preceding-char))))
	  (concat first second))))

    (defun period-to-ldots()
      (interactive)
      (cond ((and (/= (point) 1) (/= (point) 2) (string= (test-for-periods) ".."))
	     (backward-delete-char 2)
	     (insert "\\ldots"))
	    (t (insert "."))))

    (defun latex-auto-ldots ()
      (bind-key "." 'period-to-ldots LaTeX-mode-map))

    (add-hook 'LaTeX-mode-hook 'latex-auto-ldots)

    (defun dmg/mkverse (begin end)
      (interactive "r")
      (if (eq major-mode 'latex-mode)
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
    (bind-key [f7] 'dmg/mkverse LaTeX-mode-map))
  
  (defun dmg/latex-tfb ()
    (if (eq major-mode 'latex-mode)
	(progn
	  (TeX-fold-buffer))))
  (add-hook 'after-save-hook 'dmg/latex-tfb)

  )



(defun dmg/reftex-buf ()
  "Conditionally turn on reftex"
  (if (buffer-file-name)
      (if (or (string= (file-name-directory buffer-file-name)
		       "/home/dan/Wp/Dissertation/")
	      (string= (file-name-directory buffer-file-name)
		       "/home/dan/Dissertation/"))
	  (reftex-mode))))

(global-set-key "\377" (quote backward-kill-word))

(defun convert-quotes ()
  "Convert regular quotes to LaTeX quotes."
  (interactive)
  (when (eq major-mode 'latex-mode)
    (push-mark)
    (while (re-search-forward " \"" nil t)
      (replace-match "``" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([^ ]\\)\"" nil t)
      (replace-match "\\1''" nil nil))
    (pop-mark)
    (exchange-point-and-mark)))

(defun dmg/convert-quotes ()
  "Convert LaTeX ascii quotes with unicode quotes"
  (interactive)
  (when (eq major-mode 'latex-mode)
    (push-mark)
    (while (re-search-forward "``" nil t)
      (replace-match "“" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "''" nil t)
      (replace-match "”" nil nil))
    (pop-mark)
    (exchange-point-and-mark)))

(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(add-hook 'outline-minor-mode-hook
  (lambda ()
    (bind-key [(shift tab)] 'org--cycle outline-minor-mode-map)))

;; set some vars for auctex

(eval-after-load "tex"
  '(TeX-add-style-hook
    "sermon"
    (lambda ()
      (TeX-add-symbols
       '("title"
	 (TeX-arg-string "Title")
	 (TeX-arg-string "Date")
	 (TeX-arg-string "Bible 1")
	 (TeX-arg-string "Bible 2"))))))


(defun dmg/sermonsizes ()
  "Display a list of the tex files in the current
directory with word count, reverse sorted by size"
  (interactive)
  (shell-command "find . -name \"*.tex\" -print0 | xargs -n1 -0 -Ixxx sh -c 'printf \"%20s: %5s\n\" `basename xxx .tex`  `untex -aoe xxx |grep -v document | wc -w`' |sort -k 2 -n"
		 "*Sermon Sizes*")
  (other-window 1)
  (switch-to-buffer "*Sermon Sizes*")
  (view-mode t)
  (setq view-exit-action
	(lambda (buffer)
	  (kill-buffer "*Sermon Sizes*")
	  (other-window 1)
	  (delete-other-windows))))

(defun dmg/narrow-latex ()
  (interactive)
  (narrow-to-region (+ 18 (string-match "begin{document}"(buffer-string)))
		    (- (string-match "end{document}"(buffer-string)) 1)))

(defadvice TeX-insert-quote (around wrap-region activate)
  (cond
   (mark-active
    (let ((skeleton-end-newline nil))
      (skeleton-insert `(nil ,TeX-open-quote _ ,TeX-close-quote) -1)))
   ((looking-at (regexp-opt (list TeX-open-quote TeX-close-quote)))
    (forward-char (length TeX-open-quote)))
   (t
    ad-do-it)))

(put 'TeX-insert-quote 'delete-selection nil)

(defun dmg/greek-switch ()
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


(defun dmg/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (if (use-region-p)
	(fill-region (region-beginning) (region-end))
      (fill-paragraph nil t))))

(bind-key [f12] 'dmg/unfill-paragraph)


;; ** web/php setup


(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.htm$" . web-mode)
  ("\\.asp$" . web-mode)
  ("\\.php$" . web-mode)
  ("\\.css$" . web-mode)
  :config
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
	    '(lambda ()
	       (make-local-variable 'web-mode-code-indent-offset)
	       (make-local-variable 'web-mode-markup-indent-offset)
	       (make-local-variable 'web-mode-css-indent-offset)
	       (emmet-mode))))




;; * Tools
;; ** magit
(use-package magit
  :bind
  ("\C-xg" . magit-status))

  
;; ** BBDB

(use-package bbdb)


;; ** Encryption
(setq user-mail-address "kc5gmr@gmail.com")
(setq epa-file-encrypt-to '("kc5gmr@gmail.com"))


;; ** etc
(use-package which-key
  :disabled t
  :diminish which-key-mode
  :commands which-key-mode
;  :defer 10
  :config
  (defalias 'display-buffer-in-major-side-window
    'window--make-major-side-window))

(which-key-mode 1)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))

(load "~/.emacs.d/lisp/o2b.el") ; org2blog and other credentials

(use-package engine-mode
  :commands (engine/search-github engine/search-google)
  :config
  (engine-mode t)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=\"%s\""
    :keybinding "g"))

(require 'grep)

(defun dmg/search-sermons (text)
  "Look in the sermons for the text"
  (interactive "sEnter search text: ")
  (rgrep text "*.tex" "~/Wp/Sermons/" nil))

;; * fun stuff
(use-package meme
  :load-path "~/Software/Git/meme"
  :commands (meme meme-file))

;; * final setup

;; get Google calendar set up in diary
(let ((my-basic-last-mod (nth 5 (file-attributes "~/Temp/basic.ics")))
      (my-basic-size (nth 7 (file-attributes "~/Temp/basic.ics")))
      (my-diary-google-last-mod (nth 5
				     (file-attributes
				      "~/.emacs.d/diary-google"))))
  (when (and
	 (> my-basic-size 0)
	 (time-less-p my-diary-google-last-mod my-basic-last-mod))
    (delete-file "~/.emacs.d/diary-google")
    (icalendar-import-file "~/Temp/basic.ics" "~/.emacs.d/diary-google")
    (kill-buffer "diary-google")
    (kill-buffer "basic.ics")))

(modify-frame-parameters nil `((alpha . 100)))
(recentf-mode 1)

;; set up the agenda files for work or non-work
(if (y-or-n-p "Are you at work?")
    (progn
      (setq org-agenda-files my/work-agenda-files)
      (setq my/agenda-files-for-update-tasks
	    (list "~/Dropbox/GTD/life.org"
		  "~/Dropbox/GTD/trinity.org"
		  "~/Dropbox/GTD/rsa.org"
		  "~/Dropbox/GTD/classis.org"))
      (find-file "~/Dropbox/GTD/worklog.org"))
  (progn
    (setq org-agenda-files my/personal-agenda-files)
    (setq my/agenda-files-for-update-tasks
	  (list "~/Dropbox/GTD/life.org"
		"~/Dropbox/GTD/pso.org"
		"~/Dropbox/GTD/ce.org"
		"~/Dropbox/GTD/personal.org"
		"~/Dropbox/GTD/ci.org"))))

(mapc 'find-file org-agenda-files)


; have bold/italic work over several lines
(setcar (nthcdr 4 org-emphasis-regexp-components) 40)
(org-reload)

(put 'timer-list 'disabled nil)
(dmg/update-tasks)
(org-mobile-pull)



;; Local Variables:
;; eval: (outlined-elisp-mode)
;; End:

