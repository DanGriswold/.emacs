;; * Header
;; .emacs file for Dan Griswold
;; hostname: cantor
;; Time-stamp: "2016-02-07 13:57:57 alto3880 daniel"

;; * Initial settings
(setq debug-on-error nil)
(load-library "url-handlers")
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package cl)

(setq my/work-agenda-files
      (list "~/Dropbox/GTD/life.org"
	    "~/Dropbox/GTD/trinity.org"
	    "~/Dropbox/GTD/rsa.org"
	    "~/Dropbox/GTD/coaching.org"
	    "~/Dropbox/GTD/classis.org"
	    "~/Dropbox/GTD/ce.org"
	    "~/Dropbox/GTD/calendar.org"))


(setq my/personal-agenda-files
      (list "~/Dropbox/GTD/life.org"
	    "~/Dropbox/GTD/pso.org"
	    "~/Dropbox/GTD/ce.org"
	    "~/Dropbox/GTD/cot.org"
	    "~/Dropbox/GTD/personal.org"
	    "~/Dropbox/GTD/ci.org"
	    "~/Dropbox/GTD/coaching.org"
	    "~/Dropbox/GTD/minbook.org"
	    "~/Dropbox/GTD/calendar.org"))

(setq my/agenda-files-for-update-tasks
      (if (string-equal system-name "prediger")
	  (list "~/Dropbox/GTD/life.org"
		"~/Dropbox/GTD/trinity.org"
		"~/Dropbox/GTD/rsa.org"
		"~/Dropbox/GTD/coaching.org"
		"~/Dropbox/GTD/classis.org")
	(list "~/Dropbox/GTD/life.org"
	      "~/Dropbox/GTD/pso.org"
	      "~/Dropbox/GTD/ce.org"
	      "~/Dropbox/GTD/cot.org"
	      "~/Dropbox/GTD/personal.org"
	      "~/Dropbox/GTD/ci.org"
	      "~/Dropbox/GTD/minbook.org")))


(global-set-key (kbd "C-h V") 'customize-variable)


;; * Customized variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   (quote
    (("\\`fontspec\\'" "xelatex %S%(PDFout)")
     ("" "%(PDF)%(latex) %S%(PDFout)"))))
 '(LaTeX-default-options (quote ("12pt")))
 '(LaTeX-item-indent 0)
 '(LaTeX-mode-hook
   (quote
    (LaTeX-preview-setup
     (lambda nil
       (define-key LaTeX-mode-map
	 [f6]
	 (quote dmg-sermonsizes)))
     outline-minor-mode TeX-fold-mode latex-auto-ldots
     (lambda nil
       (variable-pitch-mode t))
     dmg-reftex-buf
     (lambda nil
       (setq fit-frame-max-width 79)))) t)
 '(LaTeX-style-list
   (quote
    (("article")
     ("beamer")
     ("book")
     ("letter")
     ("minimal")
     ("prosper")
     ("report")
     ("sermon"))))
 '(LilyPond-command-alist
   (quote
    (("LilyPond" "lilypond %s" "%s" "%l" "View")
     ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
     ("2png" "lilypond -f png %s" "%s" "%n" "ViewPNG")
     ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
     ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
     ("View" "evince %f")
     ("ViewPDF" "evince %f")
     ("ViewPS" "gv --watch %p")
     ("Midi" "")
     ("MidiAll" ""))))
 '(LilyPond-expand-alist
   (quote
    (("%s" . ".ly")
     ("%t" . ".tex")
     ("%d" . ".dvi")
     ("%f" . ".pdf")
     ("%p" . ".ps")
     ("%l" . ".tex")
     ("%x" . ".tely")
     ("%m" . ".midi")
     ("%n" . ".png"))))
 '(LilyPond-gv-command "gv --watch")
 '(LilyPond-pdf-command "evince")
 '(TeX-auto-save t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Webify" "webify %s" TeX-run-command t t :help "Upload this sermon's files to the web site"))))
 '(TeX-fold-env-spec-list (quote (("[comment]" ("comment")))))
 '(TeX-fold-macro-spec-list
   (quote
    (("[f]"
      ("footnote"))
     ("[c]"
      ("cite"))
     ("[l]"
      ("label"))
     ("[r]"
      ("ref" "pageref"))
     ("[i]"
      ("index"))
     ("*"
      ("item"))
     ("..."
      ("ldots"))
     (1
      ("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup" "textsuperscript"))
     ("                                     ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
      ("jump"))
     ("¿"
      ("textquestiondown"))
     ("¡"
      ("textexclamdown"))
     (1
      ("title"))
     ("%"
      ("%")))))
 '(TeX-master t)
 '(TeX-newline-function (quote newline-and-indent))
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d +")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "^booklet$" "%(o?)xdvi %dS -paper usr %d")
     ("^dvi$" "^landscape$" "%(o?)xdvi %dS -paper usr %d")
     ("^dvi$" "." "%(o?)xdvi -watchfile 5 %dS %d +")
     ("^pdf$" "." "xpdf -remote \"%s\" -raise %o %(outpage)")
     ("^html?$" "." "netscape %o"))))
 '(TeX-parse-self t)
 '(TeX-print-command "%(o?)dvips -r -P%p %r %s")
 '(TeX-printer-list
   (quote
    (("Xerox" "dvips -f %s | lp -d Xerox_5135 -o Collate=true -t %s" "lpq -P Xerox_5135")
     ("XeroxDoubleSided" "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexNoTumble -t %s" "lpq -P Xerox_5135")
     ("Xerox landscape" "dvips -t landscape -f %s | lp -d Xerox_5135 -o Collate=true -t %s")
     ("XeroxDoubleL" "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexTumble -t %s"))))
 '(TeX-style-private (quote ("\\home\\dan\\tex\\")))
 '(TeX-view-program-selection
   (quote
    (((output-pdf style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(TeX-view-style
   (quote
    (("^a4\\(?:dutch\\|paper\\|wide\\)?\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^a5\\(?:comb\\|paper\\)?$" "%(o?)xdvi %dS -paper a5 %d")
     ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^letterpaper$" "%(o?)xdvi + %dS -paper us %d")
     ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("." "%(o?)xdvi + %dS %d"))))
 '(ange-ftp-try-passive-mode t)
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(appt-display-duration 6)
 '(appt-display-format (quote echo))
 '(bbdb-accept-name-mismatch 0)
 '(bbdb-default-area-code 585)
 '(bbdb-default-country "")
 '(bbdb-default-label-list (quote ("Home" "Office" "Mobile" "Other")))
 '(bbdb-mail-user-agent (quote gnus-user-agent))
 '(bbdb-message-all-addresses t)
 '(bbdb-message-pop-up t)
 '(bbdb-mua-pop-up t)
 '(bbdb-phone-label-list
   (quote
    ("Home" "Office" "Mobile" "Other" "Work" "Cell" "Pager" "FAX")))
 '(bbdb-pop-up-layout (quote one-line))
 '(bbdb-pop-up-window-size 1)
 '(before-save-hook (quote (time-stamp)))
 '(bibtex-maintain-sorted-entries t)
 '(blink-matching-paren-distance 55600)
 '(blink-matching-paren-on-screen t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-firefox-arguments (quote ("-new-window")))
 '(browse-url-galeon-arguments (quote ("-n")))
 '(browse-url-galeon-startup-arguments (quote ("-x")))
 '(browse-url-generic-args nil)
 '(browse-url-generic-program "/usr/bin/conkeror")
 '(browse-url-netscape-program "netscoop")
 '(browse-url-new-window-flag t)
 '(browse-url-temp-dir "/home/dan/.tmp/")
 '(cal-tex-daily-string nil)
 '(cal-tex-diary t)
 '(cal-tex-holidays t)
 '(calendar-christian-all-holidays-flag t)
 '(calendar-initial-window-hook nil)
 '(calendar-latitude 42.3)
 '(calendar-location-name "Rochester, NY")
 '(calendar-longitude -71.0)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(calendar-today-visible-hook (quote (calendar-mark-today)))
 '(canlock-password "4964a429d11273d9eb5886f7ab2e92a669c9da5e")
 '(case-fold-search t)
 '(color-theme-is-cumulative nil)
 '(compilation-message-face (quote default))
 '(confirm-nonexistent-file-or-buffer nil)
 '(cperl-hairy t)
 '(cperl-lazy-help-time 1)
 '(csv-separators (quote ("," "	")))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(current-language-environment "ASCII")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "21e37baa0460d29970b6e2eabd562a509c2a72cb1f328edba4d51419ed66e0e8" "6f5dc752ca593ab14b2e0ad33a1cfea69c11397dfd641b08fdf61b779d37e858" "f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "d0e97afdf241e6931af47ebe03bace80524f56bd6a2668204d33db47f728f484" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "25f330cb050c7e7ec402af1b60243e8185a7837b455af0fa026593d4f48a78b2" "d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad" "a2c537c981b4419aa3decac8e565868217fc2995b74e1685c5ff8c6d77b198d6" "31bfef452bee11d19df790b82dea35a3b275142032e06c6ecdc98007bf12466c" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "3ad55e40af9a652de541140ff50d043b7a8c8a3e73e2a649eb808ba077e75792" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "27470eddcaeb3507eca2760710cc7c43f1b53854372592a3afa008268bcf7a75" "e85dd0d1b43cc1d725db627298c2753b0c3e90dc0b195e80f09f97a4e1e5660c" "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "0f8f704ffc80ef1f511e7a7b54977d11cbb32d772511a3f351aeb239c7d96b33" "98ad28b9f7df3e53b85b4f8fcc300c353aeeac097016c5ac897e870501d87be9" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "1a7b620db388c2e4ae288794bbe7ed3b1e279cf67e8a51b6a678e4853467c748" "9ed7382aeb47f94ad0712ad57959354d03aa5df9" "f4c4f3eb70bd3dc14bbcca8a24d96719089fdd89" "207bb5b99ebc26b45e2d575342724c10236acd74" "ce4d82359c6d47de03485db52f9e1b44141666f7" "2bee775c3a3640f7c6f2c123d4ccaeab55f89962" "2eb734da07dcd6095f66709b0e85319e2624ef16" "549e06b318bd90ab10065db84e240f733b5af7fa" "64b170bd7204fe8e9c45b8f4f445dfb5f52d12ac" "2bfbc800988b899e101edd59f601ab530ea97686" "517aecb1202bfa31fd3c44473d72483c5166124d" default)))
 '(debug-on-error nil)
 '(diary-display-function (quote (diary-fancy-display)))
 '(diary-list-entries-hook
   (quote
    (diary-include-other-diary-files diary-sort-entries)))
 '(diary-mark-entries-hook (quote (diary-mark-included-diary-files)))
 '(dictionary-default-dictionary "moby-thesaurus")
 '(dictionary-server "localhost")
 '(dictionary-tooltip-dictionary "gcide")
 '(diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
 '(diredp-hide-details-initially-flag nil)
 '(diredp-image-preview-in-tooltip (quote full))
 '(display-time-interval 30)
 '(display-time-mail-face (quote mode-line-emphasis))
 '(display-time-mode nil)
 '(display-time-string-forms
   (quote
    ((if
	 (and
	  (not display-time-format)
	  display-time-day-and-date)
	 (format-time-string "%a %b %e " now)
       "")
     (format-time-string
      (or display-time-format
	  (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
      now)
     (if mail
	 (concat " "
		 (propertize "Mail"
			     (quote display)
			     (\`
			      (when
				  (and display-time-use-mail-icon
				       (display-graphic-p))
				(\,@ display-time-mail-icon)
				(\,@
				 (list :background
				       (face-attribute display-time-mail-face :background)))))
			     (quote help-echo)
			     "mouse-2: Read mail"
			     (quote local-map)
			     (make-mode-line-mouse-map
			      (quote mouse-2)
			      read-mail-command)))
       ""))))
 '(display-time-use-mail-icon t)
 '(ebib-preload-bib-files (quote ("/home/dan/Wp/Dissertation/big.bib")))
 '(emacs-lisp-mode-hook (quote (imenu-add-menubar-index)))
 '(emms-player-ogg123-parameters (quote ("-d alsa")))
 '(emms-source-file-default-directory "/home/dan/Multimedia/Music/")
 '(emms-source-file-directory-tree-function (quote emms-source-file-directory-tree-find))
 '(emms-source-playlist-default-format (quote native))
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#org-mode" "#emacs")
     ("oftc.net" "#awesome"))))
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring smiley sound stamp track)))
 '(erc-nick "kc5gmr")
 '(erc-prompt
   (lambda nil
     (if
	 (and
	  (boundp
	   (quote erc-default-recipients))
	  (erc-default-target))
	 (erc-propertize
	  (concat
	   (erc-default-target)
	   ">")
	  (quote read-only)
	  t
	  (quote rear-nonsticky)
	  t
	  (quote front-nonsticky)
	  t)
       (erc-propertize
	(concat "ERC>")
	(quote read-only)
	t
	(quote rear-nonsticky)
	t
	(quote front-nonsticky)
	t))))
 '(erc-track-exclude-types
   (quote
    ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-track-position-in-mode-line t)
 '(erc-track-shorten-cutoff 7)
 '(etwit-username "viola_pastor")
 '(fci-rule-color "#383838")
 '(font-lock-maximum-size 1048576)
 '(g-user-email "dgriswol@rochester.rr.com")
 '(gblogger-user-email "dgriswol@rochester.rr.com")
 '(global-font-lock-mode t nil (font-lock))
 '(global-magit-file-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(holiday-bahai-holidays nil)
 '(holiday-christian-holidays
   (quote
    ((if calendar-christian-all-holidays-flag
	 (holiday-fixed 1 6 "Epiphany"))
     (holiday-easter-etc 0 "Easter Sunday")
     (holiday-easter-etc -2 "Good Friday")
     (holiday-easter-etc -46 "Ash Wednesday")
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc -14 "Passion Sunday"))
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc -7 "Palm Sunday"))
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc -3 "Maundy Thursday"))
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc 39 "Ascension Day"))
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc 49 "Pentecost"))
     (if calendar-christian-all-holidays-flag
	 (holiday-easter-etc 56 "Trinity Sunday"))
     (if calendar-christian-all-holidays-flag
	 (holiday-advent 0 "Advent"))
     (holiday-fixed 12 25 "Christmas")
     (if calendar-christian-all-holidays-flag
	 (holiday-julian 12 25 "Eastern Orthodox Christmas")))))
 '(holiday-general-holidays
   (quote
    ((holiday-fixed 1 1 "New Year's Day")
     (holiday-float 1 1 3 "Martin Luther King Day")
     (holiday-fixed 2 2 "Groundhog Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-float 2 1 3 "President's Day")
     (holiday-fixed 4 1 "April Fools' Day")
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 5 1 -1 "Memorial Day")
     (holiday-fixed 6 14 "Flag Day")
     (holiday-float 6 0 3 "Father's Day")
     (holiday-fixed 7 4 "Independence Day")
     (holiday-float 9 1 1 "Labor Day")
     (holiday-float 10 1 2 "Columbus Day")
     (holiday-fixed 10 31 "Halloween")
     (holiday-fixed 11 11 "Veteran's Day")
     (holiday-float 11 4 4 "Thanksgiving"))))
 '(holiday-islamic-holidays nil)
 '(holiday-oriental-holidays nil)
 '(holiday-solar-holidays nil)
 '(ibuffer-never-show-predicates (quote ("\\.org_archive$")) nil (ibuf-ext))
 '(ibuffer-saved-filter-groups
   (quote
    (("work"
      ("LaTeX"
       (mode . latex-mode))
      ("Org"
       (or
	(name . "^\\*Org Agenda\\*$")
	(mode . org-mode)))
      ("Org Archives"
       (name . ".*org_archive$"))
      ("Lilypond"
       (mode . LilyPond-mode))
      ("Programming"
       (or
	(mode . html-mode)
	(mode . web-mode)
	(mode . php-html-helper-mode)
	(mode . css-mode)
	(mode . emacs-lisp-mode)
	(mode . lisp-interaction-mode)
	(mode . lua-mode)))
      ("Dired"
       (mode . dired-mode))
      ("Customize"
       (mode . Custom-mode))
      ("Finding Stuff"
       (or
	(mode . grep-mode)
	(mode . locate-mode)))
      ("Help"
       (or
	(name . "*Help*")
	(name . "*Apropos*")
	(name . "*info*"))))
     ("home"
      ("Org"
       (or
	(name . "^\\*Org Agenda\\*$")
	(mode . org-mode)))
      ("Lilypond"
       (mode . LilyPond-mode))
      ("LaTeX"
       (mode . latex-mode))
      ("Gnus & News"
       (or
	(mode . message-mode)
	(mode . bbdb-mode)
	(mode . mail-mode)
	(mode . gnus-group-mode)
	(mode . gnus-summary-mode)
	(mode . gnus-article-mode)))
      ("Calendar"
       (or
	(name . "^\\*Calendar\\*$")
	(name . "^newdiary$")
	(mode . diary-fancy-display-mode)
	(mode . cfw:calendar-mode)))
      ("Twit"
       (mode . twittering-mode))
      ("ERC"
       (mode . erc-mode))
      ("Web Dev"
       (or
	(mode . html-mode)
	(mode . web-mode)
	(mode . php-html-helper-mode)
	(mode . css-mode)))
      ("elisp"
       (or
	(mode . emacs-lisp-mode)
	(mode . lisp-interaction-mode)))
      ("lua"
       (mode . lua-mode))
      ("Customize"
       (mode . Custom-mode))
      ("Dired"
       (mode . dired-mode))
      ("Finding Stuff"
       (or
	(mode . grep-mode)
	(mode . locate-mode)))
      ("Help"
       (or
	(name . "*Help*")
	(name . "*Apropos*")
	(name . "*info*")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(identica-username "dangriswold")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".org" ".tex")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")))
 '(ido-use-filename-at-point (quote guess))
 '(imenu-auto-rescan t)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(inhibit-startup-screen t)
 '(ispell-check-comments nil)
 '(ispell-program-name "aspell")
 '(latex-run-command "latex -src-specials")
 '(ledger-reports
   (quote
    (("register" "ledger register")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(lookout-bbdb-mapping-table
   (quote
    (("lastname" . "LastName")
     ("firstname" . "FirstName")
     ("net1" . "Email1Address")
     ("gnus-private" . "gnus-private"))))
 '(lpr-command "xpp")
 '(lpr-printer-switch "-d")
 '(magit-diff-use-overlays nil)
 '(mail-user-agent (quote gnus-user-agent))
 '(max-lisp-eval-depth 500)
 '(menu-bar-mode t)
 '(message-cite-function (quote message-cite-original-without-signature))
 '(message-log-max 200)
 '(message-mode-hook (quote (turn-on-orgstruct turn-on-auto-fill)))
 '(message-send-mail-partially-limit nil)
 '(message-signature-setup-hook (quote (dmg-gnus-select-sig)))
 '(midi-input-device "/dev/midi2")
 '(midi-input-shift-key 0)
 '(midi-input-shift-velocity 100)
 '(mm-enable-external t)
 '(mm-inline-text-html-with-images t)
 '(mm-keep-viewer-alive-types
   (quote
    ("application/postscript" "application/msword" "application/vnd.ms-excel" "application/pdf" "application/x-dvi" "text/html")))
 '(mm-text-html-renderer (quote shr))
 '(mm-w3m-safe-url-regexp nil)
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-url-list
   (quote
    (("TPM" "http://feeds.feedburner.com/talking-points-memo" "" nil nil)
     ("A M-N" "http://revangiem-n.com/feed/" nil nil nil)
     ("Rachel Held-Evans" "http://feeds2.feedburner.com/RachelHeldEvans" nil nil nil)
     ("Tim TenClay" "http://tenclay.org/blog/feed/" nil nil nil)
     ("Michael Hyatt" "http://michaelhyatt.com/feed" nil nil nil)
     ("Practically Efficient" "http://www.practicallyefficient.com/home?format=rss" nil nil nil)
     ("The Scotch Noob" "http://scotchnoob.com/feed/" nil nil nil))))
 '(newsticker-url-list-defaults
   (quote
    (("NY Times" "http://partners.userland.com/nytRss/nytHomepage.xml"))))
 '(nnir-excite-remove-prefix "/home/dan/GMail/")
 '(nnir-glimpse-remove-prefix "/home/dan/GMail/")
 '(nnir-search-engine (quote swish-e))
 '(nnir-swish++-configuration-file "/home/dan/GMail/swish++.conf")
 '(nnir-swish++-index-file "/home/dan/GMail/swish++.index")
 '(nnir-swish++-remove-prefix "/home/dan/GMail/")
 '(nnir-swish-e-index-file "/home/dan/GMail/index.swish-e")
 '(nnir-swish-e-remove-prefix "/home/dan/GMail/")
 '(nnir-wais-remove-prefix "/home/dan/GMail/")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-category-icon-alist
   (quote
    (("trinity" "~/SpiderOak Hive/OrgIcons/church-icon.png" nil nil :ascent center)
     ("personal" "~/SpiderOak Hive/OrgIcons/icon6.png" nil nil :ascent center)
     ("schedule" "~/SpiderOak Hive/OrgIcons/calendar_icon.png" nil nil :ascent center)
     ("scholars" "~/SpiderOak Hive/OrgIcons/library-icon.png" nil nil :ascent center)
     ("minbook" "~/SpiderOak Hive/OrgIcons/book.png" nil nil :ascent center)
     ("pso" "~/SpiderOak Hive/OrgIcons/music.png" nil nil :ascent center)
     ("google" "~/SpiderOak Hive/OrgIcons/appt.png" nil nil :ascent center)
     ("cot" "~/SpiderOak Hive/OrgIcons/rca.png" nil nil :ascent center)
     (".*" "~/SpiderOak Hive/OrgIcons/check.png" nil nil :ascent center))))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 2 :formula %)))
 '(org-agenda-custom-commands
   (quote
    (("p" "Current Projects" tags "+LEVEL=2+PROJECT/CURRENT"
      ((org-agenda-sorting-strategy
	(quote
	 (category-keep todo-state-down)))
       (org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote nottodo)
	  (quote todo))))))
     ("j" "Things for Jana" tags "+Jana/-DONE" nil)
     ("w" "Items I'm waiting on" todo "WAIT" nil)
     ("f" "Phone calls" tags "+@PHONE/-DONE" nil)
     ("E" "E-mail" tags "+@EMAIL/-DONE"
      ((org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote nottodo)
	  (quote todo))))
       (org-agenda-sorting-strategy
	(quote
	 (category-keep todo-state-up)))))
     ("W" . "Weekly Review searches")
     ("Ww" "Weekly Review for work"
      ((agenda ""
	       ((org-agenda-overriding-header "Summary of Last Week")
		(org-agenda-files my/work-agenda-files)
		(org-agenda-start-with-log-mode t)
		(org-agenda-archives-mode t)
		(org-agenda-start-day "-2Sun")
		(org-agenda-span
		 (quote week))
		(org-agenda-skip-function
		 (quote
		  (org-agenda-skip-entry-if
		   (quote todo)
		   (quote
		    ("TODO")))))
		(org-agenda-prefix-format
		 (quote
		  ((agenda . " %?-12t% s"))))))
       (agenda ""
	       ((org-agenda-overriding-header "This Week's stuff")
		(org-agenda-files my/work-agenda-files)
		(org-agenda-span
		 (quote week))))
       (tags "PROJECT+TODO=\"CURRENT\""
	     ((org-agenda-overriding-header "Current Projects")
	      (org-agenda-files my/work-agenda-files)
	      (org-agenda-sorting-strategy
	       (quote
		(category-keep todo-state-up alpha-up)))))
       (tags "PROJECT+TODO=\"PERHAPS\"|PROJECT+TODO=\"BREWING\""
	     ((org-agenda-overriding-header "Uncertain Projects")
	      (org-agenda-files my/work-agenda-files)
	      (org-agenda-sorting-strategy
	       (quote
		(category-keep todo-state-up alpha-up)))))
       (search "+TODO -SCHEDULED -DEADLINE"
	       ((org-agenda-overriding-header "Unscheduled TODOs")
		(org-agenda-files my/work-agenda-files)
		(org-agenda-skip-function
		 (quote
		  (org-agenda-skip-entry-if
		   (quote nottodo)
		   (quote todo)))))))
      ((org-agenda-start-with-log-mode
	(quote
	 (closed clock state)))))
     ("Wp" "Weekly Review for non-work items"
      ((agenda ""
	       ((org-agenda-overriding-header "Summary of Last Week")
		(org-agenda-files my/personal-agenda-files)
		(org-agenda-start-with-log-mode t)
		(org-agenda-archives-mode t)
		(org-agenda-start-day "-2Sun")
		(org-agenda-span
		 (quote week))
		(org-agenda-skip-function
		 (quote
		  (org-agenda-skip-entry-if
		   (quote todo)
		   (quote
		    ("TODO")))))
		(org-agenda-prefix-format
		 (quote
		  ((agenda . " %?-12t% s"))))))
       (agenda ""
	       ((org-agenda-overriding-header "This Week's stuff")
		(org-agenda-files my/personal-agenda-files)
		(org-agenda-span
		 (quote week))))
       (tags "+LEVEL=2+PROJECT/+CURRENT"
	     ((org-agenda-overriding-header "Current Projects")
	      (org-agenda-files my/personal-agenda-files)
	      (org-agenda-sorting-strategy
	       (quote
		(category-keep todo-state-up alpha-up)))))
       (tags "+LEVEL=2+PROJECT/-CURRENT-WRAPPED-DROPPED"
	     ((org-agenda-overriding-header "Uncertain Projects")
	      (org-agenda-files my/personal-agenda-files)
	      (org-agenda-sorting-strategy
	       (quote
		(category-keep todo-state-up alpha-up)))))
       (search "+TODO -SCHEDULED -DEADLINE"
	       ((org-agenda-overriding-header "Unscheduled TODOs")
		(org-agenda-files my/personal-agenda-files)
		(org-agenda-skip-function
		 (quote
		  (org-agenda-skip-entry-if
		   (quote nottodo)
		   (quote todo)))))))
      ((org-agenda-start-with-log-mode
	(quote
	 (closed clock state)))))
     ("b" "MobileOrg agendas"
      ((agenda ""
	       ((org-agenda-span
		 (quote day))
		(org-agenda-files
		 (quote
		  ("~/Dropbox/GTD/trinity.org" "~/Dropbox/GTD/personal.org" "~/Dropbox/GTD/life.org")))))
       (tags "+PROJECT/+CURRENT"
	     ((org-agenda-files
	       (quote
		("~/Dropbox/GTD/personal.org" "~/Dropbox/GTD/trinity.org"))))))
      nil nil)
     ("r" "Clock Report" agenda ""
      ((org-agenda-overriding-header "Clock Report")
       (org-agenda-span
	(quote week))
       (org-agenda-start-on-weekday 1)
       (org-agenda-start-with-clockreport-mode t)
       (org-agenda-files
	(quote
	 ("~/Dropbox/GTD/worklog.org")))))
     ("u" "Unscheduled TODOs not assigned to a project" search "+TODO - SCHEDULED -DEADLINE -CURRENT"
      ((org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote nottodo)
	  (quote todo))))
       (org-agenda-tag-filter-preset
	(quote
	 ("-PROJECT"))))))))
 '(org-agenda-diary-file "~/Dropbox/GTD/events.org")
 '(org-agenda-exporter-settings
   (quote
    ((ps-number-of-columns 1)
     (ps-landscape-mode nil)
     (htmlize-output-type
      (quote css)))))
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-insert-diary-strategy (quote top-level))
 '(org-agenda-log-mode-items (quote (closed clock)))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-inherited-tags nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-sticky nil)
 '(org-agenda-use-time-grid nil)
 '(org-archive-mark-done nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (lilypond . t) (gnuplot . t))))
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total (quote today))
 '(org-columns-default-format "%50ITEM %5CATEGORY(Cat) %Effort(e){:} %TAGS")
 '(org-completion-use-ido t)
 '(org-deadline-warning-days 7)
 '(org-directory "~/Dropbox/GTD/")
 '(org-email-link-description-format "%s (%c)")
 '(org-enforce-todo-dependencies nil)
 '(org-export-backends (quote (ascii html icalendar latex md odt org rss)))
 '(org-export-invisible-backends (quote (ascii org)))
 '(org-export-latex-date-format "%B %d, %Y")
 '(org-export-with-section-numbers nil)
 '(org-export-with-smart-quotes t)
 '(org-export-with-toc nil)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(org-footnote-auto-adjust t)
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0:05 0:10 0:15 0:20 0:25 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("Ink_ALL" . "Blue \"Blue Black\" Black Antietem Burgundy \"Purple Martin\" Beaver \"Apricot Orange\" \"Forest Green\" \"Quick Lime\" \"Nantucket Blue\" EMPTY"))))
 '(org-hide-leading-stars t)
 '(org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")
 '(org-icalendar-include-body nil)
 '(org-icalendar-include-sexps t)
 '(org-icalendar-timezone "America/New_York")
 '(org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{fontspec}
\\usepackage[doublespacing]{setspace}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont{Cardo}
\\usepackage[it,small]{titlesec}
\\renewcommand{\\thesection}{\\arabic{section}.}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("book" "\\documentclass[12pt, twoside]{book}
[NO-DEFAULT-PACKAGES]
\\usepackage[onehalfspacing]{setspace}
\\usepackage{relsize,etoolbox}
\\AtBeginEnvironment{quote}{\\smaller}
\\expandafter\\def\\expandafter\\quote\\expandafter{\\quote\\singlespacing}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{\\chaptername #1}}
\\fancyhf{}
\\renewcommand{\\headrulewidth}{0pt}
\\fancyfoot[CE,CO]{\\thepage}
\\fancyhead[LO]{\\texthdrtitle{CALLING IN COUNTERPOINT}} % odd pages: book title
\\fancyhead[RE]{\\texthdrtitle{\\leftmark}}   % even pages: chapter title
\\pagenumbering{roman}
\\usepackage{titlesec}
\\titleformat{\\chapter}[display]{\\large\\centering}{\\LARGE\\texthdrtitle{\\thechapter}}{1ex}{\\rule{2em}{.5pt}\\\\\\vspace{2ex}\\LARGE}{}
\\titleformat{\\section}[block]{\\hdrtitle\\centering}{}{0pt}{\\ }{}
\\renewcommand{\\thesection}{\\Alph{section}.}
\\usepackage[margin=1.25in, headheight=16pt]{geometry}
\\usepackage{fontspec}
\\usepackage{xcolor}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont{Cardo}
\\newfontfamily\\hdrtitle{Lato}
\\DeclareTextFontCommand{\\texthdrtitle}{\\hdrtitle\\addfontfeature{Color=gray}}
\\renewcommand{\\thesection}{\\arabic{section}.}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage[normalem]{ulem}
\\usepackage{textcomp}
\\usepackage{capt-of}
\\usepackage[breaklinks=true]{hyperref}"
      ("\\chapter{%s}" . "\\chapter{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("agenda" "\\documentclass[11pt]{extarticle}
\\usepackage[margin=1in]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{multicol}
\\usepackage{sectsty}
\\usepackage{fontspec}
\\setromanfont[Mapping=tex-text]{Dustismo Roman}
\\setsansfont[Mapping=tex-text]{Inconsolata}
\\newfontfamily{\\artsyfont}{Comfortaa}
\\DeclareTextFontCommand{\\zaph}{\\artsyfont}
\\allsectionsfont{\\zaph}
\\sectionfont{\\centering\\zaph}
\\usepackage{calc}
\\usepackage{ifthen}
\\newcounter{hours}\\newcounter{minutes}\\newcounter{agmin}
\\setcounter{agmin}{420}
\\setlength{\\marginparsep}{12pt}\\setlength{\\marginparwidth}{25pt}
\\newcommand{\\at}[1]{%
  \\ifthenelse{\\value{agmin}>779}{\\setcounter{agmin}{\\value{agmin}-720}}{}%
  \\setcounter{hours}{\\value{agmin}/60}%
  \\setcounter{minutes}{\\value{agmin}-\\value{hours}*60}%
  \\marginpar{\\rmfamily\\thehours:%
    \\ifthenelse{\\value{minutes}<10}{0}{}\\theminutes}%
    \\setcounter{agmin}{\\value{agmin}+#1}%
  }
\\reversemarginpar{}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\pretitle{\\begin{center}\\huge\\zaph}
\\posttitle{\\end{center}\\vspace{-18pt}}
\\preauthor{\\begin{center}\\Large\\zaph}
\\postauthor{\\end{center}\\vspace{-18pt}}
\\predate{\\begin{center}\\Large\\zaph}\\postdate{\\normalfont\\par\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s} ")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("notes" "\\documentclass[12pt]{article}
\\usepackage{palatino}
\\usepackage{scalefnt}
[NO-DEFAULT-PACKAGES]
\\setlength{\\textheight}{10.25in}
\\setlength{\\topmargin}{-1in}
\\setlength{\\headsep}{0in}
\\setlength{\\headheight}{0in}
\\setlength{\\topskip}{0in}
\\setlength{\\textwidth}{6.25in}
\\setlength{\\oddsidemargin}{0.1in}
\\setlength\\evensidemargin{0.1in}
\\setlength{\\leftmargini}{2em}
\\special{landscape}
\\usepackage[1to1]{booklet}% initial run
%\\usepackage[print,1to1]{booklet}\\nofiles\\pagespersignature{48} %final run
\\target{\\magstepminus2}{\\paperheight}{\\paperwidth}
\\usepackage{sectsty}
\\usepackage{hyperref}
\\sectionfont{\\centering}
\\paragraphfont{\\hspace*{3em}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("lecture" "\\documentclass[12pt]{article}
\\usepackage[left=1.5in,vmargin=1in]{geometry}
\\usepackage{palatino}
\\usepackage[onehalfspacing]{setspace}
[NO-DEFAULT-PACKAGES]
\\usepackage{sectsty}
\\usepackage{hyperref}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\usepackage[rm]{titlesec}
\\renewcommand{\\thesubsection}{\\hspace{28pt}\\alph{subsection}.}
\\renewcommand{\\thesubsubsection}{\\hspace{48pt}\\alph{subsubsection}.}
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("syllabus" "\\documentclass[12pt]{article}
\\usepackage{palatino}
\\usepackage[margin=1in]{geometry}
\\usepackage{hyperref}
\\setcounter{secnumdepth}{5}
\\usepackage[T1]{fontenc}
\\newcommand{\\zaph}{\\usefont{OT1}{pzc}{m}{n}}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\let\\temp\\author
\\let\\author\\date
\\let\\date\\temp
\\pretitle{\\begin{center}\\em\\Large}
\\posttitle{\\end{center}\\vspace{-14pt}}
\\preauthor{\\begin{center}\\large}
\\postauthor{\\end{center}}%\\vspace{-18pt}}
\\predate{\\noindent\\normalfont}
\\postdate{\\par
  \\vspace{3pt}\\hrule}
\\usepackage{titlesec}
\\titleformat{\\section}[block]{\\mdseries\\upshape\\large\\hspace{-4pt}}{}{0pt}{\\ }{}
\\titleformat{\\subsection}[block]{\\normalfont\\bfseries\\hspace{-4pt}}{}{0pt}{\\ }{}
\\titlespacing{\\subsection}{12pt}{6pt}{3pt}
\\titlespacing{\\paragraph}{24pt}{0pt}{3pt}
\\renewcommand{\\theparagraph}{\\arabic{paragraph}.}
\\newcommand{\\Unit}[1]{\\begin{center}
\\bfseries \\em Unit #1
\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("proposal" "\\documentclass[11pt]{article}
[NO-DEFAULT-PACKAGES]
%WARNING: This class requires org-latex-pdf-process to be set to xelatex
\\usepackage[margin=1in]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Variant=01]{Linux Libertine O}
\\usepackage{hyperref}
\\usepackage{xunicode}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("cotagenda" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\setromanfont[Mapping=tex-text]{Dustismo Roman}
\\usepackage{multicol}
\\usepackage{sectsty}
\\sectionfont{\\centering\\large}
\\subsectionfont{\\bfseries\\normalsize}
\\subsubsectionfont{\\normalfont\\normalsize\\hspace*{6pt}}
\\usepackage{agentime}\\setcounter{agmin}{420}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\pretitle{\\begin{center}\\huge}
\\posttitle{\\end{center}\\vspace{-18pt}}
\\preauthor{\\begin{center}\\Large}
\\postauthor{\\end{center}\\vspace{-18pt}}
\\predate{\\begin{center}\\Large}\\postdate{\\normalfont\\par\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("worship" "\\documentclass[12pt]{article}
\\usepackage[hmargin=1in, vmargin=.70in]{geometry}
\\usepackage{sectsty}
\\usepackage{setspace}
\\usepackage{palatino}
\\newcommand{\\zaph}{\\usefont{OT1}{pzc}{m}{n}}
\\sectionfont{\\centering\\zaph}
\\subsectionfont{\\bfseries\\normalsize}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\pretitle{\\begin{center}\\zaph\\huge}
\\posttitle{\\end{center}\\vspace{-18pt}}
\\preauthor{\\begin{center}\\zaph\\Large}
\\postauthor{\\end{center}\\vspace{-18pt}}
\\predate{\\begin{center}\\zaph\\Large}\\postdate{\\normalfont\\par\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("cv" "\\documentclass[10pt]{article}
\\usepackage[margin=.75in]{geometry}
%\\usepackage{palatino}
\\usepackage{sectsty}
\\sectionfont{\\normalsize\\selectfont\\itshape}
\\usepackage[T1]{fontenc}
\\usepackage{libertine}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}"
      ("\\section{}" . "\\section*{}"))
     ("profile" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
"
      ("\\section{}" . "\\section*{}")
      ("\\subsection{}" . "\\subsection*{}")))))
 '(org-latex-default-packages-alist
(quote
 (("" "graphicx" t)
  ("" "grffile" t)
  ("" "longtable" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "amssymb" t)
  ("" "capt-of" nil)
  ("breaklinks=true" "hyperref" nil))))
'(org-latex-pdf-process
(quote
 ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-list-allow-alphabetical t)
 '(org-log-repeat nil)
 '(org-mobile-agendas (quote ("b")))
 '(org-mobile-directory "~/Dropbox/MobileOrg")
'(org-mobile-files
(quote
 ("~/Dropbox/GTD/personal.org" "~/Dropbox/GTD/life.org" "~/Dropbox/GTD/trinity.org" "~/Dropbox/GTD/ci.org")))
 '(org-mobile-files-exclude-regexp "sabbatical")
 '(org-mobile-inbox-for-pull "~/Dropbox/GTD/from-mobile.org")
 '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info)))
 '(org-odt-preferred-output-format "doc")
 '(org-outline-path-complete-in-steps t)
 '(org-password-manager-scope nil)
'(org-refile-targets
(quote
 ((org-agenda-files :todo . "CURRENT")
  (org-agenda-files :todo . "NOW"))))
 '(org-refile-use-outline-path t)
 '(org-special-ctrl-a/e t)
 '(org-speed-commands-user (quote (("i" . dmg/clockin))))
'(org-stuck-projects
(quote
 ("PROJECT+LEVEL=2/-WRAPPED-DROPPED"
  ("TODO" "NEXT" "DONE" "MAYB")
  nil "")))
'(org-tag-alist
(quote
 ((:startgroup "Tool Contexts")
  ("@EMAIL" . 101)
  ("@PHONE" . 102)
  ("@ONLINE" . 110)
  ("@THINK" . 116)
  ("@READ" . 114)
  ("@WRITE" . 119)
  ("@DRIVE" . 100)
  (:endgroup)
  (:startgroup "People Contexts")
  ("Jana" . 106)
  ("Alexis" . 97)
  ("Tammi" . 84)
  (:endgroup)
  ("PROJECT" . 80))))
 '(org-tag-faces nil)
 '(org-tags-match-list-sublevels t)
'(org-time-clocksum-format
(quote
 (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(org-time-stamp-rounding-minutes (quote (5 5)))
'(org-todo-keyword-faces
(quote
 (("TODO" :foreground "medium blue" :weight bold)
  ("WAIT" :foreground "light blue")
  ("CURRENT" :foreground "medium blue" :weight bold)
  ("PERHAPS" :foreground "light blue" :weight bold))))
'(org-todo-keywords
(quote
 ((sequence "MAYB(m)" "TODO(t)" "|" "WAIT(w!)" "DONE(d!)" "DROP(R!)")
  (sequence "PERHAPS(p)" "BREWING(b)" "CURRENT(c)" "|" "WRAPPED(f!)" "CANCELLED(x!)")
  (sequence "MUL(l)" "NOW(n)" "|" "OLD(o!)" "XXX(X!)")
  (sequence "REPEATING(r)"))))
 '(org-use-speed-commands t)
 '(orgstruct-heading-prefix-regexp ";;;; *")
 '(outline-minor-mode-hook nil t)
'(package-archives
(quote
 (("gnu" . "https://elpa.gnu.org/packages/")
  ("melpa" . "https://melpa.org/packages/")
  ("org" . "http://orgmode.org/elpa/"))))
'(package-selected-packages
(quote
 (org-plus-contrib solarized-theme dismal bind-key persistent-scratch use-package web-mode pretty-lambdada gnuplot ledger-mode smex magit outshine olivetti dracula-theme counsel outlined-elisp-mode selectric-mode emmet-mode org-mobile-sync dictionary multi-term paradox dired+ dired-sort dired-sort-menu bookmark+ org-password-manager mode-icons wc-mode twittering-mode twilight-theme syslog-mode svg-clock soothe-theme sentence-highlight remember-theme rainbow-mode rainbow-delimiters php-mode paredit org2blog org-bullets oauth2 nyan-mode naquadah-theme monokai-theme moe-theme minimap lua-mode lorem-ipsum less-css-mode ido-ubiquitous google-maps google gandalf-theme find-file-in-project dired-details diminish deft csv-mode conkeror-minor-mode col-highlight birds-of-paradise-plus-theme auctex anti-zenburn-theme)))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(pretty-control-l-mode t)
 '(preview-auto-cache-preamble t)
'(preview-default-option-list
(quote
 ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels")))
'(recentf-exclude
(quote
 ("classis.org" "diss.org" "rsa.org" "/home/dan/diary" "newsrc" "diary.ics" "/home/dan/.bbdb" "nntp" "tammidiary" ".*\\.log" "/tmp/.*" ".*~")))
 '(recentf-max-menu-items 25)
 '(recentf-max-saved-items 200)
 '(recentf-menu-before "New File...")
 '(reftex-default-bibliography (quote ("dissbib" "barth")))
'(rs-gnus-summary-line-content-type-alist
(quote
 (("^text/plain" " ")
  ("^text/html" "h")
  ("^message/rfc822" "f")
  ("^multipart/mixed" "m")
  ("^multipart/alternative" " ")
  ("^multipart/related" "r")
  ("^multipart/signed" "s")
  ("^multipart/encrypted" "e")
  ("^multipart/report" "t")
  ("^application/" "A")
  ("^image/" "I"))))
'(safe-local-variable-values
(quote
 ((org-time-stamp-custom-formats "%b %d" . "<%m/%d/%y %a %H:%M>")
  (org-latex-inactive-timestamp-format . "%s")
  (major-mode . emacs-lisp-mode)
  (org-time-stamp-custom-formats "<%b %d>" . "<%I:%M%p>")
  (org-time-stamp-custom-formats "<%b %d>" . "<%H:%M>")
  (org-time-stamp-custom-formats "<%b %d>" . "<%m/%d/%y %a %H:%M>")
  (org-clock-total-time-cell-format . "%s")
  (LaTeX-default-options . "pulpit")
  (LaTeX-default-style . "sermon")
  (TeX-master . t)
  (TeX-engine . xetex)
  (TeX-close-quote . "”")
  (TeX-open-quote . "“")
  (tex-engine . xetex)
  (TeX-master . exambook)
  (TeX-master quote shared))))
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(shadow-noquery t)
 '(show-paren-mode t nil (paren))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(solarized-distinct-fringe-background t)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-variable-pitch nil)
 '(sort-fold-case t t)
 '(sudoku-download-method "wget")
 '(sudoku-level "evil")
 '(sudoku-puzzle-source "built-in")
 '(sudoku-wget-process "wget" t)
 '(synonyms-cache-file "/home/dan/.mthesaur.txt.cache")
 '(synonyms-file "/home/dan/.mthesaur.txt")
 '(tab-always-indent (quote complete))
 '(tail-hide-delay 3)
 '(tail-volatile nil)
'(term-bind-key-alist
(quote
 (("C-c C-c" . term-interrupt-subjob)
  ("C-p" . previous-line)
  ("C-n" . next-line)
  ("C-s" . isearch-forward)
  ("C-r" . isearch-backward)
  ("C-m" . term-send-raw)
  ("C-y" . term-paste)
  ("M-f" . term-send-forward-word)
  ("M-b" . term-send-backward-word)
  ("M-o" . term-send-backspace)
  ("M-p" . term-send-up)
  ("M-n" . term-send-down)
  ("M-M" . term-send-forward-kill-word)
  ("M-N" . term-send-backward-kill-word)
  ("M-r" . term-send-reverse-search-history)
  ("M-," . term-send-input)
  ("M-." . comint-dynamic-complete)
  ("M-DEL" . dmg-term-send-backward-kill-word))))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tex-alt-dvi-print-command "lp")
 '(tex-dvi-print-command "lp")
 '(time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %s %u")
 '(tls-checktrust t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-default-method "scp")
'(tramp-default-method-alist
(quote
 (("" "%" "smb")
  ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
  ("\\`ftp\\." "" "ftp")
  ("\\`localhost\\'" "\\`root\\'" "su")
  ("home.rochester.rr.com" "" "ftp")
  ("isp-direct.com" "" "ftp"))))
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(user-full-name "Daniel M. Griswold")
 '(user-mail-address "dgriswol@rochester.rr.com")
 '(vc-annotate-background "#2B2B2B")
'(vc-annotate-color-map
(quote
 ((20 . "#BC8383")
  (40 . "#CC9393")
  (60 . "#DFAF8F")
  (80 . "#D0BF8F")
  (100 . "#E0CF9F")
  (120 . "#F0DFAF")
  (140 . "#5F7F5F")
  (160 . "#7F9F7F")
  (180 . "#8FB28F")
  (200 . "#9FC59F")
  (220 . "#AFD8AF")
  (240 . "#BFEBBF")
  (260 . "#93E0E3")
  (280 . "#6CA0A3")
  (300 . "#7CB8BB")
  (320 . "#8CD0D3")
  (340 . "#94BFF3")
  (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t)
 '(visual-line-fringe-indicators (quote (nil nil)))
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
'(weechat-color-list
(quote
 (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
'(xterm-color-names
["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
'(xterm-color-names-bright
["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbdb-name ((t (:foreground "blue" :weight bold))))
 '(cfw:face-regions ((t :foreground "#366060")) t)
 '(italic ((((class color) (min-colors 89)) (:background nil :foreground "#a40000" :italic t :underline nil))))
 '(minibuffer-prompt ((t (:background "gray85" :foreground "orange red"))))
 '(minimap-font-face ((t (:height 30 :family "Bitstream Vera Sans Mono"))) t)
 '(muse-link-face ((t (:foreground "blue" :underline "red" :weight bold))) t)
 '(variable-pitch ((t (:height 1.1 :family "Century Schoolbook L")))))


;; * Display settings

(cond
 ((string-equal system-name  "alto3880")
  (set-face-attribute 'default nil :font "Inconsolata-16")
  (setq initial-frame-alist
	'((top . 28) (left . 270) (width . 81) (height . 32))
	default-frame-alist
	'((top . 28) (width . 81) (height . 30))
	)
  (set-frame-size (selected-frame) 81 32)
  (set-frame-position (selected-frame) 270 18)
  (load-theme 'misterioso)
  )
 ((string-equal system-name "cantor")
  (set-face-attribute 'default nil :font "InputMono" :height 136)
  (setq initial-frame-alist
	'((top . 0) (left . 19) (width . 80) (height . 63)))
  (setq default-frame-alist
	'((width . 80) (height . 40) ))
  (load-theme 'gandalf)
  )
  ((string-equal system-name "prediger")
   (set-face-attribute 'default nil :font "DejaVu Sans Mono-14" :height 136)
   (setq initial-frame-alist
	 '((top . 19) (left . 270) (width . 81) (height . 45))))
 )

(global-set-key (kbd "C-a") 'beginning-of-visual-line)
(global-set-key (kbd "C-e") 'end-of-visual-line)

(mode-icons-mode)

;; sets cursor color blue for regular, red for overwrite, black for read-only
(setq hcz-set-cursor-color-color ""
      hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "black"
	   (if overwrite-mode "red"
	     "blue"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
;(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

(diminish 'abbrev-mode)
(diminish 'auto-fill-function)

(use-package ido
  :init
  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map [down] 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map [up] 'ido-prev-match)
    )
  (add-hook 'ido-setup-hook 'ido-define-keys)

  :config (ido-mode t)
  )


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
  (shell-command (concat "zgrep " code " /usr/share/doc/miscfiles/na.phone.gz"))
  )

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
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
			       (modify-frame-parameters nil `((alpha . 100)))))

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

(require 'dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(setq completion-ignored-extensions
      (append '(".log" ".xdv" ".dvi") completion-ignored-extensions))
(require 'dired-details)
(dired-details-install)
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(require 'dired+)


;; * Editing

;; ** General editing prefs

(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/.myabbrevs")
(setq abbrev-file-name "~/Dropbox/.myabbrevs")
(setq save-abbrevs t)

(add-hook 'text-mode-hook 
	  '(lambda ()
	     (flyspell-mode t)
	     ))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(pending-delete-mode 1)

(global-font-lock-mode t)

(set-fontset-font "fontset-default"
                  'greek-iso8859-7
                  "Galatia SIL")

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f00)
			(decode-char 'ucs #x1fef))
		  "Galatia SIL")

(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x0590)
			(decode-char 'ucs #x05f4))
                  "Ezra SIL")


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
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(add-to-list 'auto-mode-alist '("\\.DOC\\'" . no-word))


(defun no-word ()
  "Run antiword on the entire buffer."
  (shell-command-on-region (point-min) (point-max) "antiword - " t t))

(setq auto-mode-alist (cons '("\\.pl$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . cperl-mode) auto-mode-alist))

(defalias 'perl-mode 'cperl-mode)


(use-package midi-input)


(defun dmg-insert-footnote ()
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
	     dictionary-default-dictionary))))
  )

; smb-mode
(autoload 'smb-mode "smb-mode" nil t)
(setq auto-mode-alist (append '(("smb\\.conf$" . smb-mode))
			      auto-mode-alist))

; long lines for blog edits
(setq auto-mode-alist (append '(("www\\.chicagoinvitation\\.org/.*\\.txt"
				. longlines-mode))
			      auto-mode-alist))

; fix copy/paste clipboard
(setq x-select-enable-primary t)  ; makes killing/yanking interact
				  ;with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact
				    ; with clipboard X11 selection

(defun dmg-term-send-backward-kill-word ()
  "Kill just the previous sub-word, not the whole blasted thing"
  (interactive)
  (term-send-raw-string "\e\C-H"))



(use-package wc-mode)

(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package outlined-elisp-mode)
 
(setq ad-redefinition-action 'accept)


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
  (bind-key [(control tab)] 'other-window org-mode-map)
  (bind-key [f12] 'dmg-agenda-export org-mode-map)
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (add-hook 'org-mode-hook
	    (lambda()
	      (flyspell-mode -1)
;	      (iimage-mode)
	      (when (string= "agendas.org" (buffer-name))
		(message-box "Press <F12> for exporting agenda at location"))
	      (org-password-manager-key-bindings)
	      (column-number-mode)
	      ))

  (if (string-equal system-name "prediger")
      (progn
	(setq org-agenda-files
	      my/work-agenda-files
	      ))
    (progn
      (setq org-agenda-files
	    my/personal-agenda-files
	    ))
    )
  
  (defvar my/org-basic-task-template "* TODO %^{Task}	%^g
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|0:30|0:05|0:15|0:45|1:00|1:30}
:CREATED: %U
:END:
%?" "Basic task data")

  (setq org-capture-templates
	`(("t" "Kinds of TODOs")
	  ("tw" "Work TODO" entry
	   (file+headline "~/Dropbox/GTD/trinity.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("tp" "Personal TODO" entry
	   (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1 :empty-lines-after 2)
	  ("tl" "Life TODO" entry
	   (file+headline "~/Dropbox/GTD/life.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1 :empty-lines-after 2)
	  ("tc" "Classis TODO" entry
	   (file+headline "~/Dropbox/GTD/classis.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("tr" "RSA TODO" entry
	   (file+headline "~/Dropbox/GTD/rsa.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("to" "Coaching TODO" entry
	   (file+headline "~/Dropbox/GTD/coaching.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("ti" "CI TODO" entry
	   (file+headline "~/Dropbox/GTD/ci.org" "Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("tt" "Theology Commission TODO" entry
	   (file+headline "~/Dropbox/GTD/cot.org" "My Tasks")
	   ,my/org-basic-task-template :empty-lines 1)
	  ("a" "Ask someone about something")
	  ("aj" "Discuss with Jana" entry ;; Jana is my secretary
	   (file+headline "~/Dropbox/GTD/trinity.org" "Tasks")
	   "* TODO Ask Jana %^{Thing for Jana}  :Jana:\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
	  ("at" "Tammi" entry             ;; Tammi is my wife
	   (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
	   "* TODO Ask Tammi %^{Thing for Tammi}%? :Tammi:\n  %^u\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
	  ("aa" "Alexis" entry            ;; Alexis is my organist
	   (file+headline "~/Dropbox/GTD/trinity.org" "Tasks")
	   "* TODO Ask Alexis %^{Thing for Alexis}%? :Alexis:\n  %^u\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
	  ("am" "Mike" entry            ;; Mike is my coach
	   (file+headline "~/Dropbox/GTD/coaching.org" "Tasks")
	   "* TODO Ask Mike %^{Thing for Mike}%? :Mike:\n  %^u\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
	  ("p" "New Project")
	  ("pw" "Church Project" entry
	   (file+headline "~/Dropbox/GTD/trinity.org" "Unassigned Projects")
	   "* CURRENT %^{Project Name}%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :jump-to-captured t :empty-lines 1)
	  ("pp" "Personal Project" entry
	   (file+headline "~/Dropbox/GTD/personal.org" "Projects")
	   "* CURRENT %^{Project Name}%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :jump-to-captured t :empty-lines 1)
	  ("x" "Journal" entry
	   (file+datetree "~/Dropbox/GTD/journal.org")
	   "* %?")
	  ("w" "Capture template" entry
	   (file+headline "~/Dropbox/GTD/life.org" "Web items to read")
	   "* TODO Read [[%:link][%:description]]	:@READ:\n:PROPERTIES:\n:CREATED: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE")
	  ))

  (defun my/org-agenda-done (&optional arg)
    "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
    (interactive "P")
    (org-agenda-todo "DONE"))
  ;; Override the key definition for org-exit
  (define-key org-agenda-mode-map "x" 'my/org-agenda-done)

  )

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



(defun dmg-export-worship-sermon-planning ()
  "Export an html version of my sermon planning sheet for Jana"
  (interactive)
  (if (string= (buffer-name) "wsprep.org")
      (progn
	(org-html-export-to-html)
	(org-odt-export-to-odt)
	(kill-buffer "wsprep.odt")
	(start-process "Export Planning Sheet" nil "~/Software/expwplan")
	)
    )
)

(add-hook 'after-save-hook 'dmg-export-worship-sermon-planning )

(defun my-odt-filter-pagerefs (text backend info)
       "Make page references, not textual references in ODT export."
       (when (org-export-derived-backend-p backend 'odt)
             (replace-regexp-in-string "text:reference-format=\"text\"" "text:reference-format=\"page\"" text)))

(defun dmg-update-tasks ()
  "Update the orgwidget on the Awesome wm bottom box"
  (interactive)
  (if (assoc-string (abbreviate-file-name (buffer-file-name)) org-agenda-files)
      (save-excursion
	(setq ogf-tmp org-agenda-files)
	(setq org-agenda-files my/agenda-files-for-update-tasks)
	(org-agenda nil "a" nil)
	(set-buffer "*Org Agenda*")
	(end-of-buffer)
	(let ((items (concat "echo '" "orgwidget:set_text(\" " (number-to-string (- (string-to-number (format-mode-line "%l")) 3)) " \")" "'" "| awesome-client" )))
	  (shell-command items))
	(org-agenda-quit)
	(setq org-agenda-files ogf-tmp)
	)
      ))

(add-hook 'after-save-hook 'dmg-update-tasks)


(use-package org-mobile)
(defvar monitor-attributes nil "Cached file attributes to be monitored.")
(defun install-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (let ((att (elt (file-attributes f) 5)))
       (unless (or (null monitor-attributes) (equalp monitor-attributes att))
         (org-mobile-pull)
                 (org-mobile-push)
                 )
       (setq monitor-attributes att)))
   file secs))
(defvar monitor-timer (install-monitor (concat org-mobile-directory "/mobileorg.org") 5)   "Check if MobileOrg/mobileorg.org is changed every 5s.")


(defun dmg-new-journal-entry ()
  "Set up a new entry in journal.org"
  (interactive)
  (find-file "~/Dropbox/GTD/journal.org")
  (org-datetree-find-date-create (org-date-to-gregorian
				  (format-time-string "%Y-%m-%d")))
  (next-line)
  (let ((lev (org-current-level)))
    (unless (= lev 4)
      (insert-file-contents "jtemplate.org")))
  )

(defun dmg-agenda-export ()
  (interactive)
  (message "Export starting ...")
  (save-excursion
    (org-up-heading-all 4)
    (org-latex-export-to-pdf nil t)
    )
  (message "Export completed")
  )


(defun dmg/clockin ()
  (interactive)
  (let ((entry (org-entry-get nil "worklog" t)))
    (if entry
	(progn
	  (org-open-link-from-string entry t)
	  (with-current-buffer "worklog.org"
	    (org-clock-in))
	  (other-window 1)
	  (delete-other-windows)
	  )
      (message "No worklog entry here"))) 
  )

(defun dmg/agenda-clockin ()
  (interactive)
  (org-agenda-switch-to)
  (dmg/clockin)
  (switch-to-buffer "*Org Agenda*")
  )

(bind-key "i" 'dmg/agenda-clockin org-agenda-mode-map)



;; ** LaTeX
(use-package tex-site
  :init
  (defun dmg-emph (&optional arg)
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
	(insert "'" char)
	))
    )
  :config
  (use-package latex
    :defer t
    :config
    (bind-key "'" 'dmg-emph LaTeX-mode-map)
    (bind-key [f6] 'dmg-sermonsizes LaTeX-mode-map)
    (bind-key [S-return] "\\\\\n" LaTeX-mode-map)
    (add-hook 'LaTeX-mode-hook (lambda () (variable-pitch-mode t)))
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-olivetti-mode)
    (setq reftex-plug-into-AUCTeX t
	  LaTeX-section-hook
	  '(LaTeX-section-heading
	    LaTeX-section-title
	    LaTeX-section-toc
	    LaTeX-section-section
	    )
	  LaTeX-letter-sender-address
	  "69 Stratton Road\\\\Rochester, NY 14610"
	  )
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

    )
  
  (defun dmg-latex-tfb ()
    (if (eq major-mode 'latex-mode)
	(progn
	  (TeX-fold-buffer)
	  )))
  (add-hook 'after-save-hook 'dmg-latex-tfb )

  )



(defun dmg-reftex-buf ()
  "Conditionally turn on reftex"
  (if (buffer-file-name)
      (if (or (string= (file-name-directory buffer-file-name)
		       "/home/dan/Wp/Dissertation/")
	      (string= (file-name-directory buffer-file-name)
		       "/home/dan/Dissertation/"))
	  (reftex-mode)))
  )

;; (add-hook 'LaTeX-mode-hook 'dmg-reftex-buf)

;; (add-hook 'LaTeX-mode-hook 'local-set-key (quote [(control tab)])
;; 	  (quote other-window))


(global-set-key "\377" (quote backward-kill-word))

(defun convert-quotes ()
  (interactive)
  "Convert regular quotes to LaTeX quotes."
  (when (eq major-mode 'latex-mode)
    (push-mark)
    (while (re-search-forward " \"" nil t)
      (replace-match "``" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "\\([^ ]\\)\"" nil t)
      (replace-match "\\1''" nil nil))
    (pop-mark)
    (exchange-point-and-mark))
)

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
	 (TeX-arg-string "Bible 2")
	 ))
      ))
  )


(defun dmg-sermonsizes ()
  "Display a list of the tex files in the current
directory with word count, reverse sorted by size"
  (interactive)
  (shell-command "find . -name \"*.tex\" -print0 | xargs -n1 -0 -Ixxx sh -c 'printf \"%17s: %5s\n\" `basename xxx .tex`  `untex -aoe xxx |grep -v document | wc -w`' |sort -k 2 -n"
		 "*Sermon Sizes*")
  (other-window 1)
  (switch-to-buffer "*Sermon Sizes*")
  (view-mode t)
  (setq view-exit-action
	(lambda (buffer)
	  (kill-buffer "*Sermon Sizes*")
	  (other-window 1)
	  (delete-other-windows)))
)

(defun dmg-narrow-latex ()
  (interactive)
  (narrow-to-region (+ 18 (string-match "begin{document}"(buffer-string)))
		    (- (string-match "end{document}"(buffer-string)) 1)
		    )
  )

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

(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)


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
      (set-face-font 'variable-pitch "Galatia SIL-16"))
    )
)


(setq font-latex-quote-list
      '(("“" "”")
	("``" "''")))


(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(global-set-key (kbd "<f12>") 'unfill-paragraph)


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
	       (emmet-mode)))
  )




;; ** magit
(use-package magit
  :bind
  ("\C-xg" . magit-status)
  )

  
;; * BBDB

(use-package bbdb-loaddefs
  :load-path "/usr/local/share/emacs/site-lisp/bbdb/bbdb-loaddefs.el"
  :config
  (setq bbdb-ignore-some-messages-alist
	'(
	  ("From" . "noreply-comment")
	  ("From" . "menswearhouse.chtah.com")
	  )
	)
  (setq bbdb-print-omit-fields
	'(omit tex-name aka mail-alias
	       gnus-private notes creation-date timestamp))
  (setq bbdb-print-require 'phone)
  (setq bbdb-print-full-alist
	'((columns . 2)
	  (separator . 2)
	  (include-files "bbdb-print" "bbdb-cols")
	  (hsize . "5.5in")
	  (vsize . "8.5in")
	  )
	)
  )

;; ---------------
;; BBDB-print vars
;; ---------------

;(add-hook 'bbdb-load-hook (function (lambda () (require 'bbdb-print))))

(setq bbdb-print-omit-fields '(omit tex-name aka mail-alias gnus-private notes creation-date timestamp))

(setq bbdb-print-require 'phone)
(setq bbdb-print-full-alist
      '((columns . 2)
	(separator . 2)
	(include-files "bbdb-print" "bbdb-cols")
	(hsize . "5.5in")
	(vsize . "8.5in")
	)
      )


(use-package google-maps)
(defun dmg-display-map ()
  (interactive)
  (google-maps-static-show
   :zoom 11
   :markers
   (mapcar
    (lambda (address-entry)
      `((,(concat
	   (mapconcat
	    'identity
	    (elt address-entry 1) ", ") ", "
	    (elt address-entry 2) ", "
	    (elt address-entry 3) ", "
	    (elt address-entry 4)))))
    (bbdb-record-address (bbdb-current-record))
    )
   )
  )



;; * final setup



(load "~/.emacs.d/o2b.el")
(recentf-mode 1)

(server-start)

(if (string-equal system-name "prediger")
    (progn
      (find-file "~/Dropbox/GTD/worklog.org")
      (setq last-nonmenu-event nil)
      (if (y-or-n-p "Open the Prayer List?")
	  (progn
	    (find-file "~/Dropbox/Org_other/prayerlist.org.gpg")
	    (org-mode)))))

(if (y-or-n-p "Do a little journaling?")
    (save-excursion
      (org-open-link-from-string "[[file:~/Dropbox/GTD/worklog.org::*Journaling][Journaling]]" t)
      (with-current-buffer "worklog.org"
	(org-clock-in))
      (dmg-new-journal-entry))
  )


;; Local Variables:
;; eval: (outlined-elisp-mode)
;; End:
