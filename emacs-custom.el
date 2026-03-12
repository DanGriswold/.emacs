(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   '(("\\`fontspec\\'" "xelatex %S%(PDFout)")
     ("" "%(PDF)%(latex) %S%(PDFout)")))
 '(LaTeX-default-options '("12pt"))
 '(LaTeX-item-indent 0)
 '(LaTeX-section-hook
   '(LaTeX-section-heading LaTeX-section-title LaTeX-section-toc
			   LaTeX-section-section))
 '(LaTeX-style-list
   '(("article") ("beamer") ("book") ("letter") ("minimal") ("prosper")
     ("report") ("sermon")))
 '(LilyPond-command-alist
   '(("LilyPond" "lilypond %s" "%s" "%l" "View")
     ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
     ("2png" "lilypond -f png %s" "%s" "%n" "ViewPNG")
     ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
     ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
     ("View" "evince %f") ("ViewPDF" "evince %f")
     ("ViewPS" "gv --watch %p") ("Midi" "") ("MidiAll" "")))
 '(LilyPond-expand-alist
   '(("%s" . ".ly") ("%t" . ".tex") ("%d" . ".dvi") ("%f" . ".pdf")
     ("%p" . ".ps") ("%l" . ".tex") ("%x" . ".tely") ("%m" . ".midi")
     ("%n" . ".png")))
 '(LilyPond-gv-command "gv --watch")
 '(LilyPond-pdf-command "evince")
 '(TeX-auto-save t)
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode) :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode) :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX
      nil (ams-tex-mode) :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX
      nil (context-mode) :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode) :help "Run ConTeXt until completion")
     ("BibTeX" "%(bibtex) %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help
      "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help
      "Generate PostScript file")
     ("Index" "%(makeindex) %s" TeX-run-command nil t :help
      "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help
      "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t
      :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help
      "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help
      "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Webify" "webify %s" TeX-run-command nil t :help
      "Upload this sermon's files to the web site")))
 '(TeX-fold-env-spec-list '(("[comment]" ("comment"))))
 '(TeX-fold-macro-spec-list
   '(("[f]" ("footnote")) ("[c]" ("cite")) ("[l]" ("label"))
     ("[r]" ("ref" "pageref")) ("[i]" ("index")) ("*" ("item"))
     ("..." ("ldots"))
     (1
      ("part" "chapter" "section" "subsection" "subsubsection"
       "paragraph" "subparagraph" "part*" "chapter*" "section*"
       "subsection*" "subsubsection*" "paragraph*" "subparagraph*"
       "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
       "textbf" "textsc" "textup" "textsuperscript"))
     ("                        ❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄" ("jump"))
     ("⭢" ("quad")) ("¿" ("textquestiondown"))
     ("¡" ("textexclamdown")) (1 ("title")) ("%" ("%"))
     ("⦃" ("noindent"))))
 '(TeX-master nil)
 '(TeX-newline-function 'newline-and-indent)
 '(TeX-output-view-style
   '(("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$"
      "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$"
      "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
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
     ("^html?$" "." "netscape %o")))
 '(TeX-parse-self t)
 '(TeX-print-command "%(o?)dvips -r -P%p %r %s")
 '(TeX-printer-list
   '(("Xerox" "dvips -f %s | lp -d Xerox_5135 -o Collate=true -t %s"
      "lpq -P Xerox_5135")
     ("XeroxDoubleSided"
      "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexNoTumble -t %s"
      "lpq -P Xerox_5135")
     ("Xerox landscape"
      "dvips -t landscape -f %s | lp -d Xerox_5135 -o Collate=true -t %s")
     ("XeroxDoubleL"
      "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexTumble -t %s")))
 '(TeX-style-private '("\\home\\dan\\tex\\"))
 '(TeX-view-program-selection
   '(((output-pdf style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Evince") (output-html "xdg-open")))
 '(TeX-view-style
   '(("^a4\\(?:dutch\\|paper\\|wide\\)?\\|sem-a4$"
      "%(o?)xdvi %dS -paper a4 %d")
     ("^a5\\(?:comb\\|paper\\)?$" "%(o?)xdvi %dS -paper a5 %d")
     ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^letterpaper$" "%(o?)xdvi + %dS -paper us %d")
     ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("." "%(o?)xdvi + %dS %d")))
 '(abbrev-file-name "~/Dropbox/.myabbrevs")
 '(async-shell-command-display-buffer nil)
 '(auto-revert-verbose nil)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.saves")))
 '(before-save-hook '(time-stamp))
 '(bibtex-maintain-sorted-entries t)
 '(blink-matching-paren-distance 55600)
 '(blink-matching-paren-on-screen t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bookmark-fontify nil)
 '(browse-kill-ring-quit-action 'save-and-restore)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-args nil)
 '(browse-url-generic-program "x-www-browser")
 '(browse-url-new-window-flag t)
 '(browse-url-temp-dir "/home/dan/.tmp/")
 '(calendar-christian-all-holidays-flag t)
 '(calendar-initial-window-hook nil)
 '(calendar-latitude 42.8)
 '(calendar-location-name "Zeeland, MI")
 '(calendar-longitude -86.0)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(calendar-today-visible-hook '(calendar-mark-today))
 '(canlock-password "4964a429d11273d9eb5886f7ab2e92a669c9da5e" t)
 '(case-fold-search t)
 '(circadian-after-load-theme-hook '(dmg-reset-frame-params))
 '(circadian-themes
   '((:sunrise . solo-jazz) ("12:00" . solarized-dark)
     (:sunset . rebecca)))
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(connection-local-criteria-alist
   '(((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile
      tramp-flatpak-connection-local-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile (eshell-path-env-list))
     (tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
			 "/usr/bin" "/sbin" "/usr/sbin"
			 "/usr/local/bin" "/usr/local/sbin"
			 "/local/bin" "/local/freeware/bin"
			 "/local/gnu/bin" "/usr/freeware/bin"
			 "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
			 "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
			       (tramp-kubernetes--container
				(car tramp-current-connection))
			       104
			       (tramp-kubernetes--pod
				(car tramp-current-connection))
			       120
			       (tramp-kubernetes--context-namespace
				(car tramp-current-connection))))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
			 "/usr/bin" "/sbin" "/usr/sbin"
			 "/usr/local/bin" "/usr/local/sbin"
			 "/local/bin" "/local/freeware/bin"
			 "/local/gnu/bin" "/usr/freeware/bin"
			 "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
			 "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "state=abcde" "-o"
					"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number) (comm . 52)
					  (state . 5) (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime . tramp-ps-time)
					  (pcpu . number)
					  (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
					"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o" "stat=abcde" "-o"
					"ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (user . string)
					  (group . string) (comm . 52)
					  (state . 5) (ppid . number)
					  (pgrp . number)
					  (ttname . string)
					  (time . tramp-ps-time)
					  (nice . number)
					  (etime . tramp-ps-time)
					  (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
					"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
					"-o"
					"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
					  (euid . number)
					  (user . string)
					  (egid . number)
					  (group . string) (comm . 52)
					  (state . string)
					  (ppid . number)
					  (pgrp . number)
					  (sess . number)
					  (ttname . string)
					  (tpgid . number)
					  (minflt . number)
					  (majflt . number)
					  (time . tramp-ps-time)
					  (pri . number)
					  (nice . number)
					  (vsize . number)
					  (rss . number)
					  (etime . number)
					  (pcpu . number)
					  (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(csv-separators '("," "\11"))
 '(cua-global-mark-cursor-color "#41c7b9")
 '(cua-normal-cursor-color "#adbcbc")
 '(cua-overwrite-cursor-color "#dbb32d")
 '(cua-read-only-cursor-color "#75b938")
 '(current-language-environment "ASCII")
 '(custom-enabled-themes '(solarized-light))
 '(custom-safe-themes
   '("2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "e36b78ef2b29a76c8487061af440de56e2b8481e6c9ef8cdc2a72cfd9d2475d2"
     "9113a2a0e6f13b8fe851c6c5a9b2a1a9608b9aae28b411c81211315b2e312007"
     "cffbae32e5e3859f671c4b1dc2a0d95a4a6f2d071f7d9b9adbe66aaf1a865008"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "36d4b9573ed57b3c53261cb517eef2353058b7cf95b957f691f5ad066933ae84"
     "a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031"
     "9d5a33a0097c43f44759530c846e1adf9c40171f232a4b2ae561feccc99a03c4"
     "849cd3e525b89a28ad04ed2c52fc33eebe46f18bd7e8b79910fd575cb7b19e47"
     "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077"
     "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448"
     "3d94d6d1a1c23113a60c8496c9aed094dbc2695f219e8127bb168d17b1e6dab3"
     "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b"
     "762ecc514c33ffe89f19235e4240b755a8c33dbb03bb54315f478c921f861315"
     "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9"
     "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe"
     "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4"
     "b60b55ecd22db6cf1072d72532cbc87174edd81c18419962deb4b8ba48f6b49d"
     "28d61ac6f26030e3c649e9f75b6ebd93dbf7f5f7b2f13e14cb1fe101e8cf4737"
     "e071222c11229ae5719a78ad27c6bd55371a546aef5cfe43b747128fea90faeb"
     "dedd42edff4429616c6a2072861066d685ff12ec132b2e97fdfd5361b9aacdae"
     "4320a92406c5015e8cba1e581a88f058765f7400cf5d885a3aa9b7b9fc448fa7"
     "80b3188b9c08cb62af83638319f8300012efd0886fecb9b04e75a200e4be1d84"
     "4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73"
     "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db"
     "dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358"
     "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98"
     "ba9c91bc43996f2fa710e4b5145d9de231150103e142acdcf24adcaaf0db7a17"
     "e983c35ab806b2b6e442e7b45032694b303a2abde663fee376c2d809fd5ea210"
     "f5e72fe6414e887b88794ff535a861ffe9caaf4ac36e070b2bbd6ec4465dbb93"
     "6b53aaf34b52b70c8949f5a4557db14965627abf4518064f425ed7a6c04588c8"
     "2dcc74add831e59721d7ff36a4e38782f764575c02b9ca51c1ab3004592cb384"
     "8efcada22b66d8feb37a98020c53647fa9fd6ebe1a7bf87f470ab98d754d1573"
     "f5353b5f08a069941f6e935905eec174ebaf386ef3cbaa06a154770abd59e051"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "265f68939a70832a73137ef621b14882f83643882b1f0dfa2cd35b91b95afbcc"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "762f6a25cb0e3870f20299828d172738e7c6d16be1ec6ac58cd2eba51ba07be2"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "4e6ec38d7940398caef89b1f653a7f88d909f58a2837d6504edc573b063919df"
     "b70b1895b418f61a89fe39771703f0b1b2da17c6b112967e13a55f235b01200f"
     "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f"
     "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277"
     "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f"
     "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8"
     "fdf291d1953988fc04f44a1f39821fc38243b7395c1d25e8e12adca065674cf2"
     "aed1e83914919e1fd0eb1e4c6cdb52ca664e262a82fd8fa99f6ff6ea77c6a6b3"
     "383dc6271131e5a5169c6a0e029ae4e2c1d71ce91cd985fbc78ff02230e30655"
     "c8e076f0e2df414c02fdb46b09b735628e73c73f72f9d78392edf99de7d86977"
     "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3"
     "3f7b4c736ffe0a373b06ce3d97c26b9e559bbc4f9b2e50e4b53143f0b0d7eb2c"
     "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb"
     "cba5ebfabc6456e4bbd68e0394d176161e1db063c6ca24c23b9828af0bdd7411"
     "61e7d5ae9201449b640aa0376f9bfe7eb0b19c97c7c0631a137a404ebf8140da"
     "0a7e543a12cf83c56ee92b799983ed6e3d36e9a0f7f3d87d0342ae7b466d8ed6"
     "6339e18e32734507d5b70817fbd490cdf1761826d7445153215ad7ee63ee3931"
     "8b4d8679804cdca97f35d1b6ba48627e4d733531c64f7324f764036071af6534"
     "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62"
     "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8"
     "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15"
     "c5ad91387427abc66af38b8d6ea74cade4e3734129cbcb0c34cc90985d06dcb3"
     "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e"
     "a11808699b77d62f5d10dd73cd474af3057d84cceac8f0301b82ad3e4fb0433e"
     "cf9f20cab61999609e47b969f6d7a89c148e16f94ae3f2f127fecfc27dc878d3"
     "c19e5291471680e72d8bd98f8d6e84f781754a9e8fc089536cda3f0b7c3550e3"
     "6973f93f55e4a6ef99aa34e10cd476bc59e2f0c192b46ec00032fe5771afd9ad"
     "7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef"
     "d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748"
     "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf"
     "5419aade6b81139dbdc5ff7f924c34113f7b069a47729fa510868fa50d599618"
     "93dc869357db37fb1f3d4e3e8f0bd88c430c5919b17edfb0936f2c21c1821526"
     "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358"
     "b3697d12fb7c087e1337432be92026b5fd218e7e43277918c0fce680d573a90c"
     "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f"
     "19af2877d17fc4749ef3c860e84e8a9c177133a35c4daf6626d8e981c8eae2c7"
     "fd50a938921687443697065dd2c810d3e87c3bcaf81a33280d06a974c6697170"
     "5e515425f8a5ce00097d707742eb5eee09b27cebc693b8998c734305cbdce1f5"
     "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731"
     "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347"
     "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4"
     "14de8f58ad656af5be374086ae7ab663811633fc1483a02add92f7a1ff1a8455"
     "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b"
     "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f"
     "99b2fdc7de612b74fcb76eb3a1962092cf729909223434f256c7007d490d787a"
     "7444cf597389d4e0096c2cbc92ec154bf8526629a5fa6533886a3dfff00f2e0b"
     "fe16a59cc8d28255a61c701b032950a4785cc60708afebd352ed5960fcbc0e68"
     "96872a5b9e9a6b092df1e4bd034699c606c28f675869a8ff3ada1ca5f4d16ebf"
     "d64b20a5b3c0abc22a5f0945a4e4aa7dd25f971e587a760316a73ca851d7e82f"
     "94554072dc495e8344cf3c1b0d9047205a14b052756c20b55dfce4af26ffc7bc"
     "203fe0858c2018058526eff9887b06facf5044a94cf8af4dbf66bd16057d28f1"
     "d88c43fe03ac912e35963695caf0ae54bc6ce6365c3a42da434ef639f7a37399"
     "980f0adf3421c25edf7b789a046d542e3b45d001735c87057bccb7a411712d09"
     "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8"
     "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311"
     "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3"
     "f633d825e380caaaefca46483f7243ae9a663f6df66c5fad66d4cab91f731c86"
     "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02"
     "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09"
     "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333"
     "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "9e9b6898d063668bfa73ee0536ea2bcdf95ef1d6438eb88594a32b5b4abd5022"
     "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf"
     "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4"
     "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882"
     "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a"
     "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3"
     "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53"
     "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd"
     "b2db1708af2a7d50cac271be91908fffeddb04c66cb1a853fff749c7ad6926ae"
     "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec"
     "6f8a2eb434dc66f3b77e3ee80b66952106eb201786a273d12ab14ba7d1bd0c49"
     "4b16156b5172d8735ec06ec650ed01c74b327f1a5a75a79d93ac28481b819d93"
     "1a504c62038a2a10503dcd27a0b74ff7dfdc63da1e4f4fbef50bfc5612e27c6d"
     "cf28bfffbf8726a31989e662986065b5319670902ac1af0e63fb8e773c119488"
     "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab"
     "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b"
     "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0"
     "b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114"
     "aab598c4d024d544b4e8b356a95ca693afa9de000b154bd2f86eed68c9e75557"
     "a0bbe4dc3513cbd049eb95f79c467b6f19dc42979fec27a0481bb6980bd8d405"
     "9864c2e956c25b3098fbc935ba0969e333dd74ecd7a1013c8dd39a6c171e1cca"
     "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369"
     "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060"
     "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4"
     "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f"
     "cf6d8127339c76d2a4b8165492a2bee417ccd3741d292a80015e95f6e9f8769f"
     "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa"
     "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf"
     "b7e38c2b835e8b46cb51beb222ec85310a2b63135d4abbca44ecf533706fa6aa"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "21e37baa0460d29970b6e2eabd562a509c2a72cb1f328edba4d51419ed66e0e8"
     "6f5dc752ca593ab14b2e0ad33a1cfea69c11397dfd641b08fdf61b779d37e858"
     "f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728"
     "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade"
     "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a"
     "d0e97afdf241e6931af47ebe03bace80524f56bd6a2668204d33db47f728f484"
     "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f"
     "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6"
     "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c"
     "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0"
     "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0"
     "25f330cb050c7e7ec402af1b60243e8185a7837b455af0fa026593d4f48a78b2"
     "d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad"
     "a2c537c981b4419aa3decac8e565868217fc2995b74e1685c5ff8c6d77b198d6"
     "31bfef452bee11d19df790b82dea35a3b275142032e06c6ecdc98007bf12466c"
     "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb"
     "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f"
     "3ad55e40af9a652de541140ff50d043b7a8c8a3e73e2a649eb808ba077e75792"
     "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a"
     "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e"
     "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30"
     "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8"
     "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "27470eddcaeb3507eca2760710cc7c43f1b53854372592a3afa008268bcf7a75"
     "e85dd0d1b43cc1d725db627298c2753b0c3e90dc0b195e80f09f97a4e1e5660c"
     "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8"
     "0f8f704ffc80ef1f511e7a7b54977d11cbb32d772511a3f351aeb239c7d96b33"
     "98ad28b9f7df3e53b85b4f8fcc300c353aeeac097016c5ac897e870501d87be9"
     "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa"
     "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19"
     "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd"
     "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c"
     "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d"
     "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9"
     "1a7b620db388c2e4ae288794bbe7ed3b1e279cf67e8a51b6a678e4853467c748"
     "9ed7382aeb47f94ad0712ad57959354d03aa5df9"
     "f4c4f3eb70bd3dc14bbcca8a24d96719089fdd89"
     "207bb5b99ebc26b45e2d575342724c10236acd74"
     "ce4d82359c6d47de03485db52f9e1b44141666f7"
     "2bee775c3a3640f7c6f2c123d4ccaeab55f89962"
     "2eb734da07dcd6095f66709b0e85319e2624ef16"
     "549e06b318bd90ab10065db84e240f733b5af7fa"
     "64b170bd7204fe8e9c45b8f4f445dfb5f52d12ac"
     "2bfbc800988b899e101edd59f601ab530ea97686"
     "517aecb1202bfa31fd3c44473d72483c5166124d" default))
 '(debug-on-error nil)
 '(default-input-method "greek-ibycus4")
 '(deft-directory "/home/dan/Dropbox/GTD/")
 '(delete-old-versions t)
 '(diary-display-function 'diary-fancy-display)
 '(diary-list-entries-hook '(diary-include-other-diary-files diary-sort-entries))
 '(diary-mark-entries-hook '(diary-mark-included-diary-files))
 '(dictionary-default-dictionary "*")
 '(dictionary-server "localhost")
 '(dictionary-tooltip-dictionary "wn")
 '(dired-du-size-format t)
 '(dired-dwim-target t)
 '(dired-free-space nil)
 '(dired-guess-shell-alist-user
   '(("\\.pdf$" "mupdf") ("\\.docx?$" "xdg-open") ("\\.doc?$" "xdg-open")
     ("\\.aup?$" "audacity") ("\\.pptx?$" "xdg-open")
     ("\\.odf$" "xdg-open") ("\\.odt$" "xdg-open")
     ("\\.odt$" "xdg-open") ("\\.ods$" "xdg-open")
     ("\\.odp$" "xdg-open") ("\\.xls$" "xdg-open")
     ("\\.xlsx$" "xdg-open") ("\\.html$" "w3m")
     ("\\.jpe?g$" "swayimg") ("\\.png$" "swayimg")
     ("\\.gif$" "swayimg") ("\\.psd$" "gimp") ("\\.xcf" "gimp")
     ("\\.odt$" "xdg-open") ("\\.xo$" "unzip") ("\\.3gp$" "vlc")
     ("\\.mp3$" "vlc") ("\\.flac$" "vlc") ("\\.avi$" "vlc")
     ("\\.wm[va]$" "vlc") ("\\.flv$" "vlc") ("\\.mov$" "vlc")
     ("\\.divx$" "vlc") ("\\.mp4$" "vlc") ("\\.mkv$" "vlc")
     ("\\.mpe?g$" "vlc") ("\\.m4[av]$" "vlc") ("\\.mp2$" "vlc")
     ("\\.pp[st]$" "xdg-open") ("\\.ogg$" "vlc") ("\\.ogv$" "vlc")
     ("\\.rtf$" "xdg-open") ("\\.ps$" "gv") ("\\.mp3$" "play")
     ("\\.wav$" "vlc") ("\\.rar$" "unrar x")))
 '(dired-icon-image-size 32)
 '(dired-kill-when-opening-new-dired-buffer nil)
 '(dired-listing-switches "-AlhG --group-directories-first")
 '(dired-omit-files
   "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$\\|auto\\$\\|~lock\\|*agenda*tex")
 '(dired-omit-verbose nil)
 '(dired-open-extensions
   '(("docx" . "mimeopen -n") ("doc" . "mimeopen -n")
     ("odp" . "mimeopen -n") ("ods" . "mimeopen -n")
     ("xls" . "mimeopen -n") ("pdf" . "evince")
     ("odt" . "mimeopen -n") ("mscz" . "musescore")
     ("flac" . "clementine") ("mp3" . "clementine")
     ("ogg" . "clementine") ("avi" . "vlc")
     ("mp4" . "mpv --player-operation-mode=pseudo-gui -- ")
     ("wma" . "vlc") ("flv" . "vlc") ("xlsx" . "mimeopen -n")
     ("htm" . "firefox") ("html" . "firefox")))
 '(diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
 '(diredp-hide-details-initially-flag nil)
 '(diredp-image-preview-in-tooltip 'full)
 '(dirvish-reuse-session t)
 '(display-time-interval 30)
 '(display-time-mail-face 'mode-line-emphasis)
 '(display-time-mode t)
 '(display-time-string-forms
   '((if (and (not display-time-format) display-time-day-and-date)
	 (format-time-string "%a %b %e " now)
       "")
     (format-time-string
      (or display-time-format
	  (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
      now)
     (if mail
	 (concat " "
		 (propertize "Mail" 'display
			     `(when
				  (and display-time-use-mail-icon
				       (display-graphic-p))
				,@display-time-mail-icon
				,@(list :background (face-attribute display-time-mail-face :background)))
			     'help-echo "mouse-2: Read mail"
			     'local-map
			     (make-mode-line-mouse-map 'mouse-2
						       read-mail-command)))
       "")))
 '(display-time-use-mail-icon t)
 '(display-wttr-format "%c+%t+%h+%w")
 '(display-wttr-locations '("42.787654,-86.109164"))
 '(el-fetch-custom-info-device nil)
 '(el-fetch-custom-info-emacs-processes nil)
 '(elegant-agenda-font "Roboto")
 '(elegant-agenda-header-preference 'thin)
 '(emacs-lisp-mode-hook '(eldoc-mode goto-address-mode))
 '(emojify-inhibit-functions '(emojify-in-org-tags-p))
 '(emojify-inhibit-in-buffer-functions '(emojify-helm-buffer-p))
 '(emojify-inhibit-major-modes
   '(dired-mode doc-view-mode debugger-mode pdf-view-mode image-mode
		help-mode ibuffer-mode ert-results-mode
		compilation-mode proced-mode mu4e-headers-mode
		locate-mode))
 '(emojify-user-emojis
   '((":blue-info:" ("name" . "blue-info")
      ("image" . "/home/dan/Dropbox/Stuff/blue-info.png")
      ("style" . "github"))))
 '(erc-autojoin-channels-alist
   '(("freenode.net" "#org-mode" "#emacs") ("oftc.net" "#awesome")))
 '(erc-hide-list '("JOIN" "PART" "QUIT"))
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu
	      move-to-prompt netsplit networks noncommands readonly
	      ring smiley sound stamp track))
 '(erc-nick "kc5gmr")
 '(erc-prompt
   (lambda nil
     (if (and (boundp 'erc-default-recipients) (erc-default-target))
	 (erc-propertize (concat (erc-default-target) ">") 'read-only
			 t 'rear-nonsticky t 'front-nonsticky t)
       (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t
		       'front-nonsticky t))))
 '(erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353"
     "477"))
 '(erc-track-position-in-mode-line t)
 '(erc-track-shorten-cutoff 7)
 '(find-ls-option '("-ls" . "-dlsb"))
 '(flyspell-mode-hook '(flyspell-buffer))
 '(font-latex-match-reference-keywords
   '(("cite" "[{") ("cites" "[{}]") ("autocite" "[{") ("footcite" "[{")
     ("footcites" "[{") ("parencite" "[{") ("textcite" "[{")
     ("fullcite" "[{") ("citetitle" "[{") ("citetitles" "[{")
     ("headlessfullcite" "[{")))
 '(garbage-collection-messages nil)
 '(global-eldoc-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(grep-use-headings t)
 '(highlight-symbol-colors
   '("#4e3b57c24752" "#1c5d5c5162eb" "#58ac47cc4aec" "#3add4f876dec"
     "#316958f94870" "#53c94f1e4a56" "#1e6c515d7099"))
 '(highlight-symbol-foreground-color "#cad8d9")
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(holiday-bahai-holidays nil)
 '(holiday-christian-holidays
   '((if calendar-christian-all-holidays-flag
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
	 (holiday-julian 12 25 "Eastern Orthodox Christmas"))))
 '(holiday-general-holidays
   '((holiday-fixed 1 1 "New Year's Day")
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
     (holiday-float 11 4 4 "Thanksgiving")))
 '(holiday-islamic-holidays nil)
 '(holiday-oriental-holidays nil)
 '(ibuffer-modified-char 61735)
 '(ibuffer-never-show-predicates '("\\.org_archive$") nil (ibuf-ext))
 '(ibuffer-read-only-char 61475)
 '(ibuffer-saved-filter-groups
   '(("default"
      ("Dailies"
       (and (not (name . "*scratch*")) (directory . "daily")
	    (mode . org-mode)))
      ("LaTeX" (mode . latex-mode))
      ("Org"
       (and (not (name . "*scratch*"))
	    (or (mode . org-agenda-mode) (mode . org-mode))))
      ("PDF" (mode . pdf-view-mode))
      ("Lilypond" (mode . LilyPond-mode))
      ("Programming"
       (or (mode . html-mode) (mode . web-mode)
	   (mode . php-html-helper-mode) (mode . css-mode)
	   (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)
	   (mode . lua-mode)))
      ("Dired" (mode . dired-mode)) ("Customize" (mode . Custom-mode))
      ("Finding Stuff" (or (mode . grep-mode) (mode . locate-mode)))
      ("Org Agendas" (name . "*Org Agenda"))
      ("Help"
       (or (name . "*Help*") (name . "*Apropos*") (name . "*info*"))))))
 '(ibuffer-saved-filters
   '(("programming"
      (or (derived-mode . prog-mode) (mode . ess-mode)
	  (mode . compilation-mode)))
     ("text document"
      (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX"
      (or (derived-mode . tex-mode) (mode . latex-mode)
	  (mode . context-mode) (mode . ams-tex-mode)
	  (mode . bibtex-mode)))
     ("web"
      (or (derived-mode . sgml-mode) (derived-mode . css-mode)
	  (mode . javascript-mode) (mode . js2-mode)
	  (mode . scss-mode) (derived-mode . haml-mode)
	  (mode . sass-mode)))
     ("gnus"
      (or (mode . message-mode) (mode . mail-mode)
	  (mode . gnus-group-mode) (mode . gnus-summary-mode)
	  (mode . gnus-article-mode)))))
 '(ibuffer-show-empty-filter-groups nil)
 '(icalendar-import-format "%s")
 '(ido-cr+-disable-list
   '(read-file-name-internal read-buffer internal-complete-buffer
			     todo-add-category
			     gnus-emacs-completing-read
			     gnus-iswitchb-completing-read
			     grep-read-files
			     magit-builtin-completing-read
			     ess-completing-read Info-read-node-name
			     tmm-prompt org-tags-completion-function
			     ffap-read-file-or-url
			     ffap-read-file-or-url-internal
			     sly-read-symbol-name
			     org-olpath-completing-read
			     execute-extended-command org-refile))
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil)
 '(ido-file-extensions-order '(".org" ".tex"))
 '(ido-ignore-buffers '("\\` " "\\*Messages\\*"))
 '(ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"))
 '(ido-ubiquitous-mode nil)
 '(ido-use-filename-at-point 'guess)
 '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
 '(ido-vertical-show-count t)
 '(iimage-mode-image-search-path '("/home/dan/"))
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(initial-scratch-message "# Happy Hacking!\12\12\12")
 '(insert-directory-program "/bin/ls")
 '(ispell-check-comments nil)
 '(ispell-program-name "hunspell")
 '(kept-new-versions 200)
 '(latex-run-command "latex -src-specials")
 '(line-spacing 0.2)
 '(list-directory-verbose-switches "-lh")
 '(locate-command "locate")
 '(lpr-command "evince")
 '(lpr-printer-switch "-d")
 '(lsp-ui-doc-border "#cad8d9")
 '(magit-define-global-key-bindings t)
 '(magit-diff-use-overlays nil)
 '(magit-repository-directories '(("~/Software/Git/" . 1)))
 '(mastodon-active-user "DMGriswold")
 '(mastodon-instance-url "https://mstdn.social")
 '(menu-bar-mode nil)
 '(message-cite-function 'message-cite-original-without-signature)
 '(message-log-max 200)
 '(message-send-mail-partially-limit nil)
 '(midi-input-device "/dev/midi4")
 '(midi-input-shift-key 0)
 '(midi-input-shift-velocity 100)
 '(mixed-pitch-fixed-pitch-faces
   '(font-latex-math-face font-latex-sedate-face font-latex-warning-face
			  font-latex-sectioning-5-face
			  font-lock-builtin-face
			  font-lock-comment-delimiter-face
			  font-lock-comment-face
			  font-lock-constant-face font-lock-doc-face
			  font-lock-function-name-face
			  font-lock-keyword-face
			  font-lock-negation-char-face
			  font-lock-preprocessor-face
			  font-lock-regexp-grouping-backslash
			  font-lock-regexp-grouping-construct
			  font-lock-string-face
			  font-lock-variable-name-face
			  font-lock-warning-faceorg-block
			  markdown-code-face
			  markdown-gfm-checkbox-face
			  markdown-inline-code-face
			  markdown-language-info-face
			  markdown-language-keyword-face
			  markdown-math-face message-header-name
			  message-header-to message-header-cc
			  message-header-newsgroups
			  message-header-xheader
			  message-header-subject message-header-other
			  mu4e-header-key-face mu4e-header-value-face
			  mu4e-link-face mu4e-contact-face
			  mu4e-compose-separator-face
			  mu4e-compose-header-face org-block
			  org-block-begin-line org-block-end-line
			  org-code org-latex-and-related org-meta-line
			  org-table org-verbatim))
 '(mixed-pitch-variable-pitch-cursor '(bar . 4))
 '(noaa-latitude 42.7913)
 '(noaa-longitude -86.0362)
 '(org-agenda-category-icon-alist
   '(("mine" "~/Multimedia/OrgIcons/icon6.png" nil nil :ascent center)
     ("theo" "~/Multimedia/OrgIcons/library-icon.png" nil nil :ascent
      center)
     ("clerk" "~/Multimedia/OrgIcons/face-with-monocle_1f9d0.png" nil
      nil :ascent center :width 18)
     ("coach" "~/Multimedia/OrgIcons/coach-icon-9717.png" nil nil
      :ascent center :width 22)
     ("Diary" "~/Multimedia/OrgIcons/calendar_icon.png" nil nil
      :ascent center)
     ("cal" "~/Multimedia/OrgIcons/calendar_icon.png" nil nil :ascent
      center :width 20)
     ("recur" "~/Multimedia/OrgIcons/repeat.png" nil nil :ascent
      center :width 20)
     ("both" "~/Multimedia/OrgIcons/double-arrow-svgrepo-com.svg" nil
      nil :ascent center :width 20)))
 '(org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 1 :formula %))
 '(org-agenda-custom-commands
   '(("g" "Aims (goals)" todo "ACTIVE"
      ((org-agenda-sorting-strategy '(deadline-up))
       (org-agenda-prefix-format
	"%i  %-22(org-entry-get nil \"DEADLINE\") %s")))
     ("r" . "Reviews")
     ("rw" "Weekly Review"
      ((agenda ""
	       ((org-agenda-overriding-header "This Week's stuff")
		(org-agenda-span 'week)))
       (todo "ACTIVE"
	     ((org-agenda-overriding-header "Active Aims")
	      (org-agenda-sorting-strategy '(deadline-up))))
       (todo "NOW"
	     ((org-agenda-overriding-header "Now Objectives")
	      (org-agenda-sorting-strategy '(deadline-up))))
       (tags "TODO=\"CURRENT\""
	     ((org-agenda-overriding-header "Current Projects")
	      (org-agenda-sorting-strategy '(deadline-up))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Aims")
	       (org-stuck-projects
		'("/+ACTIVE" ("NOW" "CURRENT" "NEXT") nil ""))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Objectives")
	       (org-stuck-projects
		'("/+NOW" ("CURRENT" "NEXT" "WAIT") nil ""))))
       (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Items I'm Waiting On")
	      (org-agenda-sorting-strategy '(scheduled-up))
	      (org-agenda-prefix-format "%i %-7:c% s")))
       (tags "TODO=\"MUL\""
	     ((org-agenda-overriding-header "Staged Objectives")
	      (org-agenda-sorting-strategy '(deadline-up))))
       (tags "TODO=\"BREWING\""
	     ((org-agenda-overriding-header "Staged Projects")
	      (org-agenda-sorting-strategy '(deadline-up))))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Unscheduled Next Actions")
	      (org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
      ((org-agenda-start-with-log-mode '(closed clock state))
       (org-enforce-todo-dependencies nil)
       (org-super-agenda-groups nil)))
     ("rm" "Monthly Review"
      ((stuck ""
	      ((org-agenda-overriding-header "Stuck Objectives")
	       (org-stuck-projects
		'("/+NOW" ("CURRENT" "NEXT") nil ""))))
       (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Items I'm Waiting On")
	      (org-agenda-sorting-strategy '(scheduled-up))
	      (org-agenda-prefix-format "%i %-7:c% s")))
       (tags "TODO=\"BREWING\"|TODO=\"MUL\""
	     ((org-agenda-overriding-header
	       "Staged Projects and Objectives")))
       (tags "TODO=\"MAYB\"|TODO=\"PERHAPS\""
	     ((org-agenda-overriding-header "Someday/Maybe Items")
	      (org-agenda-sorting-strategy
	       '(category-keep todo-state-up alpha-up)))))
      ((org-agenda-archives-mode nil)
       (org-enforce-todo-dependencies nil)
       (org-super-agenda-groups nil))
      nil)
     ("d" "Agenda with completed items" agenda ""
      ((org-agenda-overriding-header "Agenda with Completed items")
       (org-agenda-files
	'("~/Dropbox/GTD/" "~/Dropbox/Management/cal.org"))
       (org-agenda-start-with-log-mode '(closed state))
       (org-agenda-archives-mode t)
       (org-agenda-skip-function
	'(org-agenda-skip-entry-if 'todo '("CURRENT" "NOW")))
       (org-agenda-sorting-strategy '(urgency-down alpha-up))))
     ("w" "Work-only schedule, tasks only" agenda ""
      ((org-agenda-overriding-header "Work Tasks")
       (org-deadline-warning-days 0)
       (org-agenda-files
	'("~/Dropbox/GTD/slpc.org" "~/Dropbox/Management/cal.org"))
       (org-agenda-skip-function
	'(org-agenda-skip-entry-if 'todo
				   '("SOON" "WAIT" "NOW" "CURRENT")))))
     ("p" "Personal (non work) schedule, tasks only" agenda ""
      ((org-agenda-overriding-header "Personal Tasks")
       (org-deadline-warning-days 0)
       (org-agenda-files
	'("~/Dropbox/GTD/personal.org" "~/Dropbox/GTD/scholarship.org"))
       (org-agenda-skip-function
	'(org-agenda-skip-entry-if 'todo '("WAIT" "NOW" "CURRENT")))))
     ("O" "Open Loops"
      ((stuck ""
	      ((org-agenda-overriding-header "Stuck Objectives")
	       (org-stuck-projects
		'("/+NOW" ("CURRENT" "NEXT") nil ""))))
       (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Items I'm Waiting On")
	      (org-agenda-sorting-strategy '(scheduled-up))
	      (org-agenda-prefix-format "%i %-7:c% s"))))
      nil nil)))
 '(org-agenda-diary-file "~/Dropbox/GTD/events.org")
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-agenda-exporter-settings
   '((ps-number-of-columns 1) (ps-landscape-mode nil)
     (htmlize-output-type 'css)))
 '(org-agenda-files
   '("~/Dropbox/GTD/slpc.org" "/home/dan/Dropbox/GTD/books.org"
     "/home/dan/Dropbox/GTD/personal.org"
     "/home/dan/Dropbox/GTD/scholarship.org"
     "/home/dan/Dropbox/Management/cal.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-insert-diary-strategy 'top-level)
 '(org-agenda-log-mode-items '(closed clock))
 '(org-agenda-prefix-format
   '((agenda . " %i %-7:c%?-12t% s") (timeline . "  % s")
     (todo . " %i  %-7:c%-8(my/org-todo-monthyr)")
     (tags . " %i  %-7:c") (search . " %i %-12:c")))
 '(org-agenda-restore-windows-after-quit nil)
 '(org-agenda-scheduled-leaders '("Scheduled: " "Schd.%3dx: "))
 '(org-agenda-show-inherited-tags nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda urgency-down time-up timestamp-up user-defined-up alpha-up)
     (todo urgency-down category-keep)
     (tags urgency-down category-keep) (search category-keep)))
 '(org-agenda-span 'day)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-sticky t)
 '(org-agenda-timegrid-use-ampm nil)
 '(org-agenda-use-time-grid nil)
 '(org-archive-location "archives/%s_archive::")
 '(org-archive-mark-done nil)
 '(org-babel-load-languages
   '((emacs-lisp . t) (lilypond . t) (gnuplot . t) (shell . t)))
 '(org-babel-tangle-remove-file-before-write nil)
 '(org-capture-templates
   '(("n" "NEXT actions")
     ("nw" "Work NEXT action" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("np" "Personal NEXT action" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("ns" "Scholarship NEXT action" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("p" "New Project")
     ("pw" "Work Project" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Unallied Projects")
      (file "~/Dropbox/Emacs_git/cptTmpProj.org") :jump-to-captured t)
     ("pp" "Personal Project" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Unallied Projects")
      (file "~/Dropbox/Emacs_git/cptTmpProj.org") :jump-to-captured t)
     ("ps" "Scholarship Project" entry
      (file+headline "~/Dropbox/GTD/scholarship.org"
		     "Unallied Projects")
      (file "~/Dropbox/Emacs_git/cptTmpProj.org") :jump-to-captured t)
     ("o" "New Objective")
     ("ow" "Work Objective" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpObj.org") :jump-to-captured t)
     ("op" "Personal Objective" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpObj.org") :jump-to-captured t)
     ("os" "Scholarship Objective" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpObj.org") :jump-to-captured t)
     ("a" "Ask someone about something")
     ("aw" "Work Ask" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} %^{Thing to Ask}%? :EMAIL:\12SCHEDULED: %^t\12:PROPERTIES:\12:effort: 0:05\12:CREATED: %U\12:END:\12")
     ("ap" "Personal Ask" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} %^{Thing to Ask}%? :EMAIL:\12SCHEDULED: %^t\12:PROPERTIES:\12:effort: 0:05\12:CREATED: %U\12:END:\12")
     ("as" "Scholarship Ask" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} %^{Thing to Ask}%? :EMAIL:\12SCHEDULED: %^t\12:PROPERTIES:\12:effort: 0:05\12:CREATED: %U\12:END:")
     ("m" "Ask someone to meet")
     ("mw" "Work meetup" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Visits planned")
      "* NEXT [#B] ❓ %^{Person to Ask} to meet%? :EMAIL:\12SCHEDULED: %^t\12:PROPERTIES:\12:effort: 0:05\12:CREATED: %U\12:END:\12")
     ("mp" "Personal visit" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Friends blessed")
      "* NEXT [#B] ❓ %^{Person to Ask} to meet%? :EMAIL:\12SCHEDULED: %^t\12:PROPERTIES:\12:effort: 0:05\12:CREATED: %U\12:END:\12")
     ("S" "Setup")
     ("Ss" "Session meeting setup" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Meetings")
      (file "~/Dropbox/Emacs_git/cptSessionMeeting.org")
      :jump-to-captured t)
     ("Sm" "Meeting setup" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Meetings")
      (file "~/Dropbox/Emacs_git/cptCmteMeeting.org")
      :jump-to-captured t)
     ("D" "brain Dump (\"What's on my mind now?\")" item
      (file+olp+datetree "~/Dropbox/Management/braindump.org") ""
      :jump-to-captured t)
     ("t" "Think about something")
     ("tw" "Work thought" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpThink.org"))
     ("tp" "Personal thought" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpThink.org"))
     ("ts" "Scholarship thought" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpThink.org"))
     ("r" "Research something")
     ("rp" "Research Personal" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] Rsrch %^{Item} :NET:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:\12"
      :immediate-finish t)
     ("rw" "Research Work" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Tasks")
      "* NEXT [#B] Rsrch %^{Item} :NET:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:\12")
     ("b" "Buy something")
     ("bl" "Buy local" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] 🛒 %^{Item} :CAR:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:\12"
      :immediate-finish t)
     ("bo" "Buy online" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] 🛒 %^{Item} :NET:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:\12"
      :immediate-finish t)
     ("c" "calls")
     ("cw" "work Call" entry
      (file+headline "~/Dropbox/GTD/slpc.org" "Tasks")
      "* NEXT [#B] ☎ %^{Call}        :PHONE:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:"
      :immediate-finish t :jump-to-captured t)
     ("cp" "personal Call" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Friends blessed")
      "* NEXT [#B] ☎ %^{Call}        :PHONE:\12SCHEDULED: %^t\12:PROPERTIES:\12:CREATED: %U\12:END:"
      :immediate-finish t :jump-to-captured t)
     ("Q" "Quote" entry (file "~/Dropbox/Brain/quotes.org")
      "* %^{Summary}\12  :PROPERTIES:\12  :CREATED: %u\12  :Author:   %^{Author}\12  :Title:    %^{Title}\12  :pages:    %^{pages}\12  :END:\12%?")
     ("z" "Protocol quote" entry
      (file+headline "~/Dropbox/Brain/links.org" "Inbox")
      "* %^{Title}\12Source: %u, %c\12 #+BEGIN_QUOTE\12%i\12#+END_QUOTE\12\12\12%?")
     ("L" "Protocol Link" entry
      (file+headline "~/Dropbox/Brain/links.org" "Inbox")
      "* %?[[%:link][%:description]] \12Captured On: %U")))
 '(org-capture-templates-contexts
   '(("L" "" ((not-in-file . "clerk.org") (not-in-buffer . "*Agenda*")))))
 '(org-clock-clocked-in-display 'mode-line)
 '(org-clock-history-length 10)
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total 'today)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist nil)
 '(org-clock-persist-file "~/Dropbox/Org_other/org-clock-save.el")
 '(org-clock-sound nil)
 '(org-clock-today-mode t)
 '(org-columns-default-format "%45ITEM %5CATEGORY(Cat) %5Effort(e){:} %TAGS %15SCHEDULED")
 '(org-completion-use-ido t)
 '(org-crypt-key "554097E85710005B")
 '(org-cycle-hook
   '(org-cycle-hide-archived-subtrees org-cycle-hide-drawers
				      org-cycle-show-empty-lines))
 '(org-deadline-warning-days 7)
 '(org-directory "~/Dropbox/GTD/")
 '(org-ditaa-eps-jar-path "/home/dan/.emacs.d/elpa/contrib/scripts/DitaaEps.jar")
 '(org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
 '(org-duration-format '((special . h:mm)))
 '(org-eldoc-breadcrumb-separator "⋯")
 '(org-ellipsis "⮷")
 '(org-enforce-todo-dependencies t)
 '(org-entities-user '(("space" "~" nil "&nbsp;" " " " " " ")))
 '(org-export-backends '(html latex odt pandoc))
 '(org-export-date-timestamp-format "%B %e, %Y")
 '(org-export-invisible-backends '(ascii org))
 '(org-export-latex-date-format "%B %d, %Y")
 '(org-export-with-section-numbers nil)
 '(org-export-with-smart-quotes t)
 '(org-export-with-toc nil)
 '(org-file-apps
   '(("\\.odt\\'" . "xdg-open %s") ("\\.docx?\\'" . "xdg-open %s")
     (auto-mode . emacs) (directory . emacs) ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "evince %s")))
 '(org-fold-show-context-detail
   '((agenda . local) (bookmark-jump . lineage) (isearch . lineage)
     (default . local)))
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-footnote-auto-adjust t)
 '(org-global-properties
   '(("Effort_ALL"
      . "0:05 0:10 0:15 0:20 0:25 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("Ink_ALL"
      . "Blue \"Blue Black\" Black Antietem Burgundy \"Purple Martin\" Beaver \"Apricot Orange\" \"Forest Green\" \"Quick Lime\" \"Nantucket Blue\" EMPTY")))
 '(org-goto-interface 'outline-path-completion)
 '(org-habit-following-days 1)
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-hide-macro-markers nil)
 '(org-html-postamble 'auto)
 '(org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")
 '(org-icalendar-include-body nil)
 '(org-icalendar-include-sexps t)
 '(org-icalendar-timezone "America/New_York")
 '(org-icalendar-use-scheduled '(event-if-not-todo event-if-todo todo-start))
 '(org-indent-indentation-per-level 1)
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-latex-classes
   '(("minutes"
      "\\documentclass[12pt]{article}\12\\usepackage[margin=.75in]{geometry}\12\\usepackage{sectsty}\12\\usepackage{titling}\12\\usepackage{fontspec}\12\\defaultfontfeatures{Ligatures=TeX}\12\\setmainfont{Liberation Serif}\12\\setsansfont[Color={0019D4}]{Lato}\12\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}\12\\renewcommand{\\maketitlehooka}{\\huge\\bfseries\\sffamily}\12\\sectionfont{\\sffamily}\12\\subsectionfont{\\sffamily}\12\\pretitle{\\begin{center}\\huge}\12\\posttitle{\\end{center}\\vspace{-14pt}}\12\\setlength{\\droptitle}{-5em}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\renewcommand{\\thesubsection}{\\Alph{subsection}.}\12\\usepackage{titlesec}\12\\titlespacing*{\\subsection}{\\parindent}{1ex}{1em}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article"
      "\\documentclass[12pt]{article}\12\\usepackage[margin=1in]{geometry}\12\\usepackage{fontspec}\12\\usepackage[doublespacing]{setspace}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Scale=1.1]{ETBookOT}\12\\usepackage[it,small]{titlesec}\12\\usepackage{graphicx}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\renewcommand{\\thesubsection}{\\Alph{subsection}.}\12\\usepackage{sectsty}\12\\usepackage{calc}\12\\usepackage{xcolor}\12\\newcounter{hours}\\newcounter{minutes}\\newcounter{agmin}\12\\setcounter{agmin}{420}\12\\setlength{\\marginparsep}{12pt}\\setlength{\\marginparwidth}{25pt}\12\\subsectionfont{\\emph\\normalsize\\hspace*{.1pt}}\12\\subsubsectionfont{\\emph\\normalsize\\hspace*{3pt}}\12\\reversemarginpar{}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("bylaws"
      "\\documentclass[12pt]{article}\12\\usepackage[margin=1in]{geometry}\12\\usepackage{fontspec}\12\\usepackage[singlespacing]{setspace}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Scale=1.05]{Cardo}\12\\usepackage{titlesec}\12\\titleformat{\\section}[block]{\\bf\\em\\Huge}{Part \\thechapter}{0pt}{\\hspace{.25in}}{}\12\\usepackage{graphicx}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\usepackage{sectsty}\12\\subsectionfont{\\bfseries\\normalsize\\hspace*{.1pt}}\12\\subsubsectionfont{\\normalsize\\it\\hspace*{3pt}}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("book"
      "\\documentclass[12pt, twoside]{book}\12[NO-DEFAULT-PACKAGES]\12\\usepackage{hyperref}\12\\usepackage[onehalfspacing]{setspace}\12\\usepackage{relsize,etoolbox}\12\\AtBeginEnvironment{quote}{\\smaller}\12\\expandafter\\def\\expandafter\\quote\\expandafter{\\quote\\singlespacing}\12\\usepackage{fancyhdr}\12\\pagestyle{fancy}\12\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{\\chaptername #1}}\12\\fancyhf{}\12\\renewcommand{\\headrulewidth}{0pt}\12\\fancyfoot[CE,CO]{\\thepage}\12\\fancyhead[RO]{\\texthdrtitle{\\hfill CALLING IN COUNTERPOINT}} % odd pages: book title\12\\fancyhead[LE]{\\texthdrtitle{\\leftmark}}   % even pages: chapter title\12\\pagenumbering{roman}\12\\usepackage{titlesec}\12\\titleformat{\\chapter}[display]{\\large\\centering}{\\LARGE\\texthdrtitle{\\thechapter}}{1ex}{\\rule{2em}{.5pt}\\\\\\vspace{2ex}\\LARGE}{}\12\\titleformat{\\section}[block]{\\hdrtitle\\centering}{}{0pt}{\\ }{}\12\\renewcommand{\\thesection}{\\Alph{section}.}\12\\titleformat{\\subsection}[block]{\\hdrtitle\\bfseries}{}{0pt}{\\ }{}\12\\usepackage[margin=1.25in, headheight=16pt]{geometry}\12\\usepackage{fontspec}\12\\usepackage{xcolor}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont{Cardo}\12\\newfontfamily\\hdrtitle{Lato}\12\\DeclareTextFontCommand{\\texthdrtitle}{\\hdrtitle\\addfontfeature{Color=gray}}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\usepackage{graphicx}\12\\usepackage{grffile}\12\\usepackage{longtable}\12\\usepackage[normalem]{ulem}\12\\usepackage{textcomp}\12\\usepackage{capt-of}\12\\usepackage{titletoc}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("agenda"
      "\\documentclass[12pt]{article}\12\\usepackage[margin=1in]{geometry}\12\\usepackage[T1]{fontenc}\12\\usepackage{multicol}\12\\usepackage{fontspec}\12\\setromanfont[Mapping=tex-text]{Baskervald ADF Std} %Cardo} % Dustismo Roman}\12\\setsansfont[Mapping=tex-text]{Noto Sans}\12\\newfontfamily{\\artsyfont}{Comfortaa}\12\\DeclareTextFontCommand{\\zaph}{\\artsyfont}\12\\usepackage{sectsty}\12\\sectionfont{\\centering\\zaph}\12\\subsectionfont{\\bfseries\\normalsize}\12\\subsubsectionfont{\\normalfont\\normalsize\\hspace*{6pt}}\12\\usepackage{xcolor}\12\\usepackage{calc}\12\\usepackage{ifthen}\12\\newcounter{hours}\\newcounter{minutes}\\newcounter{agmin}\12\\setcounter{agmin}{420}\12\\setlength{\\marginparsep}{12pt}\\setlength{\\marginparwidth}{25pt}\12\\newcommand{\\at}[1]{%\12  \\ifthenelse{\\value{agmin}>779}{\\setcounter{agmin}{\\value{agmin}-720}}{}%\12  \\setcounter{hours}{\\value{agmin}/60}%\12  \\setcounter{minutes}{\\value{agmin}-\\value{hours}*60}%\12  \\marginpar{\\rmfamily\\thehours:%\12    \\ifthenelse{\\value{minutes}<10}{0}{}\\theminutes}%\12    \\setcounter{agmin}{\\value{agmin}+#1}%\12  }\12\\reversemarginpar{}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\pretitle{\\begin{center}\\huge\\zaph}\12\\posttitle{\\end{center}\\vspace{-19pt}}\12\\preauthor{\\begin{center}\\Large\\zaph}\12\\postauthor{\\end{center}\\vspace{-14pt}}\12\\predate{\\begin{center}\\Large\\zaph}\\postdate{\\normalfont\\par\\end{center}}\12\\newcommand{\\resp}[1]{\\hfill\\normalfont\\emph{#1}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s} ")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("ssagenda"
      "\\documentclass[12pt]{article}\12\\usepackage[margin=1in]{geometry}\12\\usepackage{fontspec}\12\\usepackage[onehalfspacing]{setspace}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont{Baskervald ADF Std}\12\\usepackage[it,small]{titlesec}\12\\usepackage{graphicx}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\renewcommand{\\thesubsection}{\\Alph{subsection}.}\12\\usepackage{sectsty}\12\\usepackage{calc}\12\\usepackage{xcolor}\12\\usepackage{atime}\12\\newcounter{hours}\\newcounter{minutes}\\newcounter{agmin}\12\\setlength{\\marginparsep}{12pt}\\setlength{\\marginparwidth}{25pt}\12\\newcommand{\\resp}[1]{\\hfill\\normalfont\\small\\textit{#1}}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\posttitle{\\par\\end{center}\\vspace{-1.5in}}\12\\subsectionfont{\\emph\\normalsize\\hspace*{.1pt}}\12\\reversemarginpar{}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("notes"
      "\\documentclass[12pt]{article}\12\\usepackage{palatino}\12[NO-DEFAULT-PACKAGES]\12\\usepackage{sectsty}\12\\usepackage{multicol}\12\\usepackage{fullpage}\12\\sectionfont{\\centering}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-8em}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("lecture"
      "\\documentclass[12pt]{article}\12\\usepackage[left=1.5in,vmargin=1in]{geometry}\12\\usepackage{fontspec}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Scale=1.25]{Cardo}\12\\usepackage[doublespacing]{setspace}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\usepackage[rm]{titlesec}\12\\titleformat*{\\section}{\\center\\Large\\bfseries}\12\\titleformat*{\\subsection}{\\large\\itshape}\12% \\titleformat{name=\\subsection,numberless}{\\noindent\\large\\itshape} {}{0.5em}{}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("syllabus"
      "\\documentclass[12pt]{article}\12\\usepackage{palatino}\12\\usepackage[margin=1in]{geometry}\12\\setcounter{secnumdepth}{5}\12\\usepackage[T1]{fontenc}\12\\newcommand{\\zaph}{\\usefont{OT1}{pzc}{m}{n}}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\let\\temp\\author\12\\let\\author\\date\12\\let\\date\\temp\12\\pretitle{\\begin{center}\\em\\Large}\12\\posttitle{\\end{center}\\vspace{-14pt}}\12\\preauthor{\\begin{center}\\large}\12\\postauthor{\\end{center}}%\\vspace{-18pt}}\12\\predate{\\noindent\\normalfont}\12\\postdate{\\par\12  \\vspace{3pt}\\hrule}\12\\usepackage{titlesec}\12\\titleformat{\\section}[block]{\\mdseries\\upshape\\large\\hspace{-4pt}}{}{0pt}{\\ }{}\12\\titleformat{\\subsection}[block]{\\normalfont\\bfseries\\hspace{-4pt}}{}{0pt}{\\ }{}\12\\titlespacing{\\subsection}{12pt}{6pt}{3pt}\12\\titlespacing{\\paragraph}{24pt}{0pt}{3pt}\12\\renewcommand{\\theparagraph}{\\arabic{paragraph}.}\12\\newcommand{\\Unit}[1]{\\begin{center}\12\\bfseries \\em Unit #1\12\\end{center}}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("proposal"
      "\\documentclass[11pt]{article}\12[NO-DEFAULT-PACKAGES]\12%WARNING: This class requires org-latex-pdf-process to be set to xelatex\12\\usepackage[margin=1in]{geometry}\12\\usepackage[T1]{fontenc}\12\\usepackage{fontspec}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Variant=01]{Linux Libertine O}\12\\usepackage{xunicode}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("worshipls"
      "\\documentclass[12pt]{article}\12\\special{landscape}\12\\usepackage[print,1to1]{booklet}\12\\usepackage[singlespacing]{setspace}\12\\expandafter\\def\\expandafter\\quote\\expandafter{\\quote\\onehalfspacing}\12\\setlength{\\textheight}{8.25in}\12\\setlength{\\topmargin}{.5in}\12\\setlength{\\headsep}{0in}\12\\setlength{\\headheight}{0in}\12\\setlength{\\topskip}{0in}\12\\setlength{\\textwidth}{5.35in}\12\\usepackage{sectsty}\12\\usepackage{fontspec}\12\\setmainfont{TeX Gyre Schola}\12\\setmainfont[Scale=1.15]{Cardo}\12\\newfontfamily\\zaph[Scale=1.25]{Anaktoria}\12\\sectionfont{\\centering\\zaph}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\pretitle{\\begin{center}\\zaph\\huge}\12\\posttitle{\\end{center}\\vspace{-18pt}}\12\\preauthor{\\begin{center}\\zaph\\Large}\12\\postauthor{\\end{center}\\vspace{-18pt}}\12\\predate{\\begin{center}\\zaph\\Large}\\postdate{\\normalfont\\par\\end{center}}\12\\target{\\magstepminus1}{\\paperheight}{\\paperwidth}\12"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("worship"
      "\\documentclass[12pt]{article}\12\\usepackage[hmargin=1in, vmargin=.70in]{geometry}\12\\usepackage{sectsty}\12\\usepackage{setspace}\12\\usepackage{fontspec}\12\\setmainfont[Scale=1.15]{Accanthis ADF Std No3}\12\\newfontfamily\\zaph[Scale=1.25]{Anaktoria}\12\\sectionfont{\\centering\\zaph}\12\\sectionfont{\\centering\\zaph}\12\\subsubsectionfont{\\hspace{1em}\\itshape}\12\\usepackage{titling}\12\\setlength{\\droptitle}{-5em}\12\\pretitle{\\begin{center}\\zaph\\huge}\12\\posttitle{\\end{center}\\vspace{-18pt}}\12\\preauthor{\\begin{center}\\zaph\\Large}\12\\postauthor{\\end{center}\\vspace{-18pt}}\12\\predate{\\begin{center}\\zaph\\Large}\\postdate{\\normalfont\\par\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("cv"
      "\\documentclass[10pt]{article}\12\\usepackage[margin=.75in]{geometry}\12\\usepackage{sectsty}\12\\usepackage{titling}\12\\usepackage{fontspec}\12\\usepackage{xcolor}\12\\setmainfont[Scale=1.15]{Liberation Serif}\12\\setsansfont[Color={0019D4}]{Lato}\12\\sectionfont{\\normalsize\\selectfont\\itshape}\12\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}\12\\renewcommand{\\maketitlehooka}{\\huge\\bfseries\\sffamily}\12\\sectionfont{\\sffamily}\12\\subsectionfont{\\sffamily}\12\\usepackage[backend=biber,\12            style=authortitle,\12\11    url=false,\12\11    doi=true,\12\11    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("profile"
      "\\documentclass[11pt]{article}\12[NO-DEFAULT-PACKAGES]\12%WARNING: This class requires org-latex-pdf-process to be set to xelatex\12\\usepackage[margin=1in]{geometry}\12\\usepackage{hyperref}\12\\usepackage[T1]{fontenc}\12\\usepackage{fontspec}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Variant=01]{Linux Libertine O}\12\\usepackage{xunicode}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("script"
      "\\documentclass[12pt]{article}\12[NO-DEFAULT-PACKAGES]\12\\special{landscape}\12\\usepackage[print,1to1]{booklet}\12\\usepackage[singlespacing]{setspace}\12    \\setlength{\\textheight}{8.25in}\12    \\setlength{\\topmargin}{.5in}\12    \\setlength{\\headsep}{0in}\12    \\setlength{\\headheight}{0in}\12    \\setlength{\\topskip}{0in}\12    \\setlength{\\textwidth}{4.5in}\12    \\setlength{\\oddsidemargin}{0.75in}\12%    \\setlength\\evensidemargin{0.57in}\12\\usepackage{fontspec}\12\\defaultfontfeatures{Mapping=tex-text}\12\\setmainfont[Scale=1.35]{Linux Libertine O}\12\\usepackage[it,small]{titlesec}\12\\usepackage{graphicx}\12\\renewcommand{\\thesection}{\\arabic{section}.}\12\\usepackage{sectsty}\12\\usepackage[normalem]{ulem}\12\\target{\\magstepminus1}{\\paperheight}{\\paperwidth}\12\\subsectionfont{\\mdseries\\itshape}\12\\subsubsectionfont{\\mdseries\\itshape}\12\\sectionfont{\\centering\\fontspec[Scale=1.15]{Lato}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-latex-default-packages-alist
   '(("" "graphicx" t nil) ("" "grffile" t nil) ("" "longtable" nil nil)
     ("" "wrapfig" nil nil) ("" "rotating" nil nil)
     ("normalem" "ulem" t nil) ("" "amsmath" t nil)
     ("" "textcomp" t nil) ("" "amssymb" t nil) ("" "capt-of" nil nil)
     ("" "titletoc" nil nil) ("" "hyperref" nil nil)))
 '(org-latex-hyperref-template
   "\\hypersetup{\12 colorlinks=true,\12 urlcolor=blue,\12 urlbordercolor=blue,\12 pdfborder={1 1 1},\12 pdfborderstyle={/S/U/W 1},\12 pdfauthor={%a},\12 pdftitle={%t},\12 pdfkeywords={%k},\12 pdfsubject={%d},\12 pdfcreator={%c}, \12 pdflang={%L}}\12")
 '(org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"))
 '(org-link-abbrev-alist '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
 '(org-link-elisp-confirm-function 'y-or-n-p)
 '(org-link-elisp-skip-confirm-regexp "dmg-weather")
 '(org-link-email-description-format "%s (%c)")
 '(org-list-allow-alphabetical t)
 '(org-log-done nil)
 '(org-log-done-with-time t)
 '(org-log-into-drawer t)
 '(org-log-repeat nil)
 '(org-modern-priority '((65 . "❗") (66 . "🔹") (67 . "⬇")))
 '(org-modern-star '("◉" "◉" "○" "○" "◈" "◈" "◇" "◇" "✳" "✳") nil nil "adjusted to accommodate org-odd-levels-only")
 '(org-odd-levels-only t)
 '(org-odt-preferred-output-format "docx")
 '(org-outline-path-complete-in-steps nil)
 '(org-pandoc-menu-entry
   '((52 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (36 "as html5." org-pandoc-export-as-html5)
     (53 "to html5-pdf and open."
	 org-pandoc-export-to-html5-pdf-and-open)
     (37 "to html5-pdf." org-pandoc-export-to-html5-pdf)
     (108 "to latex-pdf and open."
	  org-pandoc-export-to-latex-pdf-and-open)
     (76 "to latex-pdf." org-pandoc-export-to-latex-pdf)
     (111 "to odt and open." org-pandoc-export-to-odt-and-open)
     (79 "to odt." org-pandoc-export-to-odt)
     (120 "to docx and open." org-pandoc-export-to-docx-and-open)
     (88 "to docx." org-pandoc-export-to-docx)))
 '(org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 12 1")
 '(org-password-manager-scope '("~/Dropbox/Org_other/passwords.org.gpg"))
 '(org-pretty-tags-global-mode t)
 '(org-pretty-tags-surrogate-strings
   '(("imp" . "☆") ("idea" . "💡") ("money" . "$$$") ("easy" . "₰")
     ("music" . "♬") ("open" . "🌈") ("clsd" . "🚪") ("deep" . "🌊")
     ("shlw" . "🏖") ("notstuck" . "∉𝔖") ("CAR" . "🚗")
     ("PHONE" . "📞") ("NET" . "🖧") ("TEXT" . "💬") ("EMAIL" . "✉")
     ("HOME" . "🏠")))
 '(org-priority-faces '((67 . "tan")))
 '(org-refile-targets
   '((nil :todo . "ACTIVE") (nil :todo . "NOW") (nil :todo . "CURRENT")
     (org-agenda-files :todo . "ASSIGNED")
     (nil :regexp . "/Maybe/ Items")))
 '(org-refile-use-outline-path 'file)
 '(org-return-follows-link t)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :unnarrowed t :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		 "#+title: ${title}\12#+date: %U\12"))
     ("c" "Conversation" plain "%?" :jump-to-captured t :unnarrowed t
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		 "#+title: ${title}\12# A conversation\12#+date: %U\12%?\12* Context\12\12* Main Points\12"))
     ("t" "theological thought" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		 "#+title: ${title}\12#+date: %U\12#+filetags: theology\12"))
     ("Q" "Quotes to use in my book" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
		 "#+title: ${title}\12#+date: %U\12#+filetags: quotesCC\12"))))
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "%[~/Dropbox/Org_other/jtemplate_dailies.org]"
      :target
      (file+head "%<%Y-%m-%d>.org"
		 "#+title: %<%Y-%m-%d %A>\12#+filetags: dailies\12"))))
 '(org-roam-directory "~/Dropbox/Org_Roam/")
 '(org-show-notification-handler "xcowsay")
 '(org-special-ctrl-a/e t)
 '(org-speed-commands
   '(("Outline Navigation")
     ("n" org-speed-move-safe 'org-next-visible-heading)
     ("p" org-speed-move-safe 'org-previous-visible-heading)
     ("f" org-speed-move-safe 'org-forward-heading-same-level)
     ("b" org-speed-move-safe 'org-backward-heading-same-level)
     ("F" . org-next-block) ("B" . org-previous-block)
     ("u" org-speed-move-safe 'outline-up-heading) ("j" . org-goto)
     ("g" org-refile '(4)) ("Outline Visibility") ("c" . org-cycle)
     ("C" . org-shifttab) (" " . org-display-outline-path)
     ("s" . org-toggle-narrow-to-subtree) ("k" . org-cut-subtree)
     ("=" . org-columns) ("Outline Structure Editing")
     ("U" . org-metaup) ("D" . org-metadown) ("r" . org-metaright)
     ("l" . org-metaleft) ("R" . org-shiftmetaright)
     ("L" . org-shiftmetaleft)
     ("i" progn (forward-char 1)
      (call-interactively 'org-insert-heading-respect-content))
     ("^" . org-sort) ("w" . org-refile)
     ("a" . org-archive-subtree-default-with-confirmation)
     ("@" . org-mark-subtree) ("#" . org-toggle-comment)
     ("Clock Commands") ("I" . org-clock-in) ("O" . org-clock-out)
     ("Meta Data Editing") ("t" . org-todo) ("d" org-todo "DONE")
     ("," org-priority) ("0" org-priority 32) ("1" org-priority 65)
     ("2" org-priority 66) ("3" org-priority 67)
     (":" . org-set-tags-command) ("e" . org-set-effort)
     ("E" . org-inc-effort)
     ("W" lambda (m) (interactive "sMinutes before warning: ")
      (org-entry-put (point) "APPT_WARNTIME" m))
     ("Agenda Views etc") ("v" . org-agenda) ("/" . org-sparse-tree)
     ("Misc") ("o" . org-open-at-point) ("?" . org-speed-command-help)
     ("<" org-agenda-set-restriction-lock 'subtree)
     (">" org-agenda-remove-restriction-lock)
     ("q" . org-cycle-agenda-files)))
 '(org-speed-commands-user '(("d" org-todo "DONE")))
 '(org-startup-folded t)
 '(org-startup-indented t)
 '(org-stuck-projects '("/+CURRENT" ("NEXT" "WAIT") ("persist") ""))
 '(org-super-agenda-groups
   '((:name "Today" :deadline today :face)
     (:name "Waiting" :todo "WAIT" :order 9)
     (:name "Important" :priority "A" :order 1)
     (:name "Passed deadline" :and (:deadline past :todo ("CURRENT"))
	    :order 5)
     (:name "Items in Projects"
	    (:ancestor-with-todo ("CURRENT" :limit 4)) :order 2)
     (:name "Recurring Tasks" :category "recur" :order 3)))
 '(org-superstar-item-bullet-alist '((42 . 10038) (43 . 10148) (45 . 8226)))
 '(org-tag-faces nil)
 '(org-tags-column -80)
 '(org-tags-exclude-from-inheritance '("crypt"))
 '(org-tags-match-list-sublevels t)
 '(org-tidy-properties-style 'inline)
 '(org-tidy-protect-overlay nil)
 '(org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 '(org-timestamp-rounding-minutes '(5 5))
 '(org-todo-keywords
   '((sequence "MAYB(m)" "SOON(s)" "NEXT(n)" "WAIT(w!)" "|" "DONE(d!)"
	       "DROP(D@)")
     (sequence "PERHAPS(p)" "BREWING(b)" "CURRENT(c!)" "|"
	       "WRAPPED(r!)" "CANCELED(x@)")
     (sequence "ASSIGNED(a@/@)")
     (sequence "MUL(M)" "NOW(N!)" "|" "END(E!)" "ZAP(Z@)")
     (sequence "PONDER(P)" "ACTIVE(D)" "|" "FINISH(f!)" "REWORK(R@)")))
 '(org-todo-repeat-to-state "NEXT")
 '(org-use-property-inheritance '("worklog"))
 '(org-use-speed-commands
   (lambda nil (and (looking-at org-outline-regexp) (looking-back "^**"))))
 '(org-variable-pitch-fixed-faces
   '(org-block org-block-begin-line org-block-end-line org-code
	       org-document-info-keyword org-done org-formula
	       org-indent org-meta-line org-special-keyword org-table
	       org-todo org-verbatim org-date org-drawer org-tag))
 '(outline-minor-mode-hook nil)
 '(outlined-elisp-startup-folded nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-hidden-regexps
   '("\\`names" "\\`guess-language" "\\`excorporate" "\\`eglot"
     "\\`org-readme" "\\`show-marks" "^company-" "^consult-"
     "^counsel-" "^evil-" "flycheck" "flymake" "^helm-" "^ido-"
     "^ivy-" "^nix" "^ob-" "^osx-" "projectile" "realgud" "sql" "^ac-"
     "^ace-" "^clojure"))
 '(package-selected-packages
   '(all-the-icons all-the-icons-completion all-the-icons-dired
		   all-the-icons-ibuffer auctex backup-walker bind-key
		   casual circadian csv-mode dictionary diminish
		   dired-du dired-icon dired-open dired-toggle-sudo
		   dirvish easy-find easy-theme-preview
		   elegant-agenda-mode ghub gnuplot hydra info-colors
		   intellij-theme interleave kaolin-themes kkp
		   marginalia minions mixed-pitch modus-themes
		   nerd-icons-dired nord-theme olivetti orderless
		   org-autolist org-fancy-priorities org-hide-drawers
		   org-mime org-modern org-noter org-password-manager
		   org-pdftools org-pretty-tags org-roam org-roam-ui
		   org-super-agenda org-superstar org-variable-pitch
		   ox-pandoc password-store pdf-meta-edit pdf-tools
		   persistent-scratch powerthesaurus rainbow-mode
		   rebecca-theme scratch-plus seq show-font
		   smartparens solarized-theme solo-jazz-theme
		   sudo-edit sudo-utils titlecase use-package valign
		   vertico web-mode writeroom-mode yaml-mode))
 '(password-store-url-field "URL")
 '(pdf-misc-print-program "/usr/bin/evince" t)
 '(persistent-scratch-what-to-save '(major-mode))
 '(powerline-default-separator 'utf-8)
 '(preview-auto-cache-preamble t)
 '(preview-default-option-list
   '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes"
     "showlabels"))
 '(read-file-name-completion-ignore-case t)
 '(recentf-exclude '("/home/dan/diary" ".*\\.log" "Dropbox/GTD/*" "elpa"))
 '(recentf-max-menu-items 25)
 '(recentf-max-saved-items 300)
 '(recentf-menu-before "New File...")
 '(reftex-cite-format
   '((13 . "\\cite[]{%l}") (116 . "\\textcite{%l}")
     (97 . "\\autocite[]{%l}") (112 . "\\parencite{%l}")
     (102 . "\\footcite[][]{%l}") (70 . "\\fullcite[]{%l}")
     (120 . "[]{%l}") (88 . "{%l}")))
 '(reftex-cite-prompt-optional-args nil)
 '(reftex-default-bibliography '("~/Wp/Scholarship/big.bib"))
 '(safe-local-variable-values
   '((backup-inhibited . 1) (org-modern-timestamp "%e %b, %y" . "%H:%I")
     (dired-omit-extensions ".tex" ".log" ".xdv" ".dvi" ".o" "~"
			    ".aux")
     (eval add-to-list 'dired-omit-extensions ".tex")
     (eval progn (org-modern-mode -1) (save-place-local-mode -1))
     (eval progn (org-modern-mode nil) (save-place-local-mode -1))
     (eval add-hook 'after-save-hook
	   (lambda nil (widen) (org-babel-tangle)) nil t)
     (time-stamp-active . t) (org-odd-levels-only)
     (org-latex-image-default-width . "\\linewidth")
     (org-latex-subtitle-format
      . "\\vspace{48pt} \\newline \\normalfont \\small %s")
     (eval variable-pitch-mode nil)
     (org-time-stamp-custom-formats "%B %e, %Y"
				    . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %e, %Y>"
				    . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%b %d, %y>"
				    . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %-e, %y>"
				    . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %e, %y>"
				    . "<%m/%d/%y %a %H:%M>")
     (eval add-hook 'after-save-hook (lambda nil (org-babel-tangle))
	   nil t)
     (eval org-content 2) (org-latex-hyperref-template . "")
     (org-odt-preferred-output-format . doc) (org-latex-with-hyperref)
     (org-latex-hyperref-template)
     (org-todo-keyword-faces ("INPROCESS" . "dark orange"))
     (org-image-actual-width 800)
     (org-time-stamp-custom-formats "%b %d" . "<%m/%d/%y %a %H:%M>")
     (org-latex-inactive-timestamp-format . "%s")
     (major-mode . emacs-lisp-mode)
     (org-time-stamp-custom-formats "<%b %d>" . "<%I:%M%p>")
     (org-time-stamp-custom-formats "<%b %d>" . "<%H:%M>")
     (org-time-stamp-custom-formats "<%b %d>" . "<%m/%d/%y %a %H:%M>")
     (org-clock-total-time-cell-format . "%s")
     (LaTeX-default-options . "pulpit")
     (LaTeX-default-style . "sermon") (TeX-master . t)
     (TeX-engine . xetex) (TeX-close-quote . "”")
     (TeX-open-quote . "“") (tex-engine . xetex)
     (TeX-master . exambook) (TeX-master quote shared)
     (buffer-auto-save-file-name)))
 '(save-place-mode t nil (saveplace))
 '(scratch-plus-force-restore nil)
 '(scratch-plus-restore-type t)
 '(scratch-plus-save-directory "")
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(sentence-end-double-space nil)
 '(shadow-noquery t)
 '(show-font-pangram 'wizards)
 '(show-paren-mode t nil (paren))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#75b938" "#184956" 0.2))
 '(solarized-distinct-fringe-background t)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil)
 '(sort-fold-case t)
 '(suggest-key-bindings nil)
 '(tab-always-indent 'complete)
 '(tail-hide-delay 3)
 '(tail-volatile nil)
 '(term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob) ("C-p" . previous-line)
     ("C-n" . next-line) ("C-s" . isearch-forward)
     ("C-r" . isearch-backward) ("C-m" . term-send-raw)
     ("C-y" . term-paste) ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word) ("M-o" . term-send-backspace)
     ("M-p" . term-send-up) ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input) ("M-." . comint-dynamic-complete)
     ("M-DEL" . dmg-term-send-backward-kill-word)))
 '(term-default-bg-color "#103c48")
 '(term-default-fg-color "#adbcbc")
 '(tex-alt-dvi-print-command "lp")
 '(tex-dvi-print-command "lp")
 '(time-stamp-format "%:y-%02m-%02d %02H:%02M %s")
 '(titlecase-style 'chicago)
 '(tls-checktrust t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-default-method "scp")
 '(tramp-default-method-alist
   '(("" "%" "smb") ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
     ("\\`ftp\\." "" "ftp") ("\\`localhost\\'" "\\`root\\'" "su")
     ("home.rochester.rr.com" "" "ftp") ("isp-direct.com" "" "ftp")))
 '(tramp-syntax 'default nil (tramp))
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(user-full-name "Daniel M. Griswold")
 '(user-mail-address "kc5gmr@gmail.com")
 '(vc-annotate-background "#04c4c7")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#778ca3") (40 . "#00afef") (60 . "#778ca3") (80 . "#778ca3")
     (100 . "#778ca3") (120 . "#009c9f") (140 . "#778ca3")
     (160 . "#778ca3") (180 . "#778ca3") (200 . "#778ca3")
     (220 . "#009c9f") (240 . "#005cc5") (260 . "#fa1090")
     (280 . "#778ca3") (300 . "#005cc5") (320 . "#778ca3")
     (340 . "#009c9f") (360 . "#778ca3")))
 '(vc-annotate-very-old-color "#778ca3")
 '(version-control t)
 '(vertico-cycle t)
 '(visible-bell t)
 '(visual-line-fringe-indicators '(nil nil))
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
 '(warning-suppress-types '((comp)))
 '(wttrin-default-cities '("Zeeland")))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(muse-link-face ((t (:foreground "blue" :underline "red" :weight bold))) t))
