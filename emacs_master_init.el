(setq custom-file "~/.emacs.d/lisp/emacs-custom.el")
(load custom-file)

(defun dont-delay-compile-warnings (fun type &rest args)
  (if (eq type 'bytecomp)
      (let ((after-init-time t))
        (apply fun type args))
    (apply fun type args)))
(advice-add 'display-warning :around #'dont-delay-compile-warnings)

(require 'package)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package bind-key)

;; (use-package cl)

(push "/home/dan/.emacs.d/lisp" load-path)

(require 'dmg-display)
(require 'dmg-etc)
(require 'dmg-completion)
(require 'dmg-info)
(require 'dmg-org)

;; (bind-key "C-a" 'beginning-of-visual-line)
;; (bind-key "C-e" 'end-of-visual-line)
(bind-key "C-h V" 'customize-variable)
(bind-key "M-T" 'transpose-sentences)
(bind-key [C-tab] 'other-window)
(bind-key [M-BS] 'backward-kill-word)
(bind-key "M-SPC" 'cycle-spacing)
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'scroll-left      'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column  'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq completion-ignored-extensions
      (append '(".log" ".xdv" ".dvi") completion-ignored-extensions)
      ispell-really-hunspell t
      lazy-lock-continuity-time nil
      lazy-lock-stealth-time nil
      x-super-keysym 'alt)


(pending-delete-mode 1)

(global-font-lock-mode t)

(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/.myabbrevs")
(diminish 'abbrev-mode)

(push '("\\[tT]e[xX]\\'" . latex-mode) auto-mode-alist)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-to-list 'auto-mode-alist '("/var/log/.*" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))


(defalias 'perl-mode 'cperl-mode)

(bind-key [f12] 'dmg-unfill-paragraph)

(use-package tex
  :defer t
  :ensure auctex
  :hook ((LaTeX-mode . outline-minor-mode)
	 (LaTeX-mode . variable-pitch-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . TeX-fold-mode)
	 (LaTeX-mode . dmg-add-tex-style)))

(use-package latex
  :defer t
  :config
  (require 'dmg-latex)
  (bind-keys :map LaTeX-mode-map
             ("'" . dmg-emph)
             ([f6] . dmg-sermonsizes)
             ([f7] . dmg-mkverse)
             ([S-return] . "\\\\\n")))

(use-package hydra
  :defer t
  :bind
  ([f9] . dmg-hydra-emoji-template/body)
  ([f8] . hydra-journal/body)

  :config
  (defhydra dmg-hydra-emoji-template (:color blue :hint nil)
    "
_t_hink     _b_uy      _a_sk     _p_ick/decide
_c_all      _l_ook     _w_rite   inbox _z_ero!
"
    ("t" (insert "🤔"))
    ("b" (insert "🛒"))
    ("a" (insert "❓"))
    ("p" (insert "⛏"))
    ("c" (insert "📞"))
    ("z" (insert "📥0⃣"))
    ("l" (insert "🔎"))
    ("w" (insert "🖋")))

  (defhydra hydra-journal (:color blue
                           :hint nil)
    "
_d_: Daily journal     _w_: Weekly journal     _m_: Monthly journal    _t_: Theo journal
"
    ;; ("d" (dmg-new-journal-entry))
    ;; ("d" (org-roam-dailies-capture-today))
    ("d" (dmg-new-journal-daily-dentry))
    ("w" (dmg-new-journal-weekly-dentry))
    ("m" (dmg-new-journal-monthly-dentry))
    ("t" (dmg-new-theology-journal-entry))))

(use-package needs_hydras
  :after (hydra)
  :bind ([f4] . dmg-hydra-needs-main/body))

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

(require 'org-tempo) ; Required from org 9 onwards for old template expansion
  ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
(setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
(defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-tempo-complete-tag)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (or (region-active-p) (looking-back "^"))
        (hydra-org-template/body)
      (self-insert-command 1))))

(defhydra dmg-hydra-writingprojects
  (:color blue :hint nil)
  "
_c_ Calling in Counterpoint
_r_ Reformed theololgy
_e_ Evangelicalism
"
  ("c" (dired "~/Wp/Scholarship/CallingCounterpoint/"))
  ("r" (find-file "~/Wp/Scholarship/RCA_theology/intro_draft.org"))
  ("e" (find-file "~/Wp/Scholarship/Understanding Evangelicalism/theological markers.org")))

(bind-key "C-c <f12>" 'dmg-hydra-writingprojects/body)

(use-package persistent-scratch)
(persistent-scratch-setup-default)

(bind-key "M-l" 'dmg-downcase-word)

(bind-key "M-c" 'dmg-capitalize-word)

(unbind-key "M-.")
(pdf-tools-install)

(use-package lilypond-mode
  :defer t
  :mode ("\\.ly$" . LilyPond-mode)
  :load-path "/usr/share/emacs/site-lisp/"
  :hook (LilyPond-mode . turn-on-font-lock))

(use-package hl-line
  :defer t
  :config
  (set-face-background 'hl-line "moccasin"))

(require 'smartparens-config)
(require 'smartparens-org)
(smartparens-global-mode t)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)


(require 'titlecase)

(global-set-key (kbd "C-c C-o") 'find-file-at-point)

(require 'dmg-dired)

(use-package dictionary
  :defer t
  :bind ([mouse-3] . dictionary-mouse-popup-matching-words))

 (setq epa-file-encrypt-to '("kc5gmr@gmail.com"))
 (setq auto-save-file-name-transforms
       `((".*" "~/.emacs.d/autosaves/" t)))

 ;; (load "~/.emacs.d/lisp/o2b.el") ; org2blog and other credentials

  

(use-package calendar
  :hook (calendar-mode . set-buffer-to-small)
  :config
  (defun set-buffer-to-small ()
    (face-remap-add-relative 'default '(:height 75))))

(add-to-list 'load-path "/home/dan/.emacs.d/lisp/meme-master/")

(use-package meme
  :commands (meme meme-file))

(which-key-mode -1)

(require 'dmg-mode-line)

(require 'show-font)

(pdf-loader-install)

(require 'dmg-casual)

(with-current-buffer "*scratch*"
  (org-superstar-mode 1)
  
  (visual-line-mode))

(unless (file-exists-p "~/Dropbox/Management/cal.org")
  (dmg-get-gcal))
(unless (file-updated-today-p "~/Dropbox/Management/cal.org")
  (dmg-get-gcal))

(diminish 'org-pretty-tags-mode)
(diminish 'visual-line-mode)
(diminish 'buffer-face-mode)

(setq auto-window-vscroll nil)

(start-process "encouragement" nil "encouragement.sh")
