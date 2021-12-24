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
   '(LaTeX-section-heading LaTeX-section-title LaTeX-section-toc LaTeX-section-section))
 '(LaTeX-style-list
   '(("article")
     ("beamer")
     ("book")
     ("letter")
     ("minimal")
     ("prosper")
     ("report")
     ("sermon")))
 '(LilyPond-command-alist
   '(("LilyPond" "lilypond %s" "%s" "%l" "View")
     ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
     ("2png" "lilypond -f png %s" "%s" "%n" "ViewPNG")
     ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
     ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
     ("View" "evince %f")
     ("ViewPDF" "evince %f")
     ("ViewPS" "gv --watch %p")
     ("Midi" "")
     ("MidiAll" "")))
 '(LilyPond-expand-alist
   '(("%s" . ".ly")
     ("%t" . ".tex")
     ("%d" . ".dvi")
     ("%f" . ".pdf")
     ("%p" . ".ps")
     ("%l" . ".tex")
     ("%x" . ".tely")
     ("%m" . ".midi")
     ("%n" . ".png")))
 '(LilyPond-gv-command "gv --watch")
 '(LilyPond-pdf-command "evince")
 '(TeX-auto-save t)
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
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
     ("BibTeX" "%(bibtex) %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "%(makeindex) %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Webify" "webify %s" TeX-run-command nil t :help "Upload this sermon's files to the web site")))
 '(TeX-fold-env-spec-list '(("[comment]" ("comment"))))
 '(TeX-fold-macro-spec-list
   '(("[f]"
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
     ("                        ❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄"
      ("jump"))
     ("⭢"
      ("quad"))
     ("¿"
      ("textquestiondown"))
     ("¡"
      ("textexclamdown"))
     (1
      ("title"))
     ("%"
      ("%"))
     ("⦃"
      ("noindent"))))
 '(TeX-master nil)
 '(TeX-newline-function 'newline-and-indent)
 '(TeX-output-view-style
   '(("^dvi$"
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
     ("^html?$" "." "netscape %o")))
 '(TeX-parse-self t)
 '(TeX-print-command "%(o?)dvips -r -P%p %r %s")
 '(TeX-printer-list
   '(("Xerox" "dvips -f %s | lp -d Xerox_5135 -o Collate=true -t %s" "lpq -P Xerox_5135")
     ("XeroxDoubleSided" "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexNoTumble -t %s" "lpq -P Xerox_5135")
     ("Xerox landscape" "dvips -t landscape -f %s | lp -d Xerox_5135 -o Collate=true -t %s")
     ("XeroxDoubleL" "dvips -f %s | lp -d Xerox_5135 -o Duplex=DuplexTumble -t %s")))
 '(TeX-style-private '("\\home\\dan\\tex\\"))
 '(TeX-view-program-selection
   '(((output-pdf style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open")))
 '(TeX-view-style
   '(("^a4\\(?:dutch\\|paper\\|wide\\)?\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^a5\\(?:comb\\|paper\\)?$" "%(o?)xdvi %dS -paper a5 %d")
     ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^letterpaper$" "%(o?)xdvi + %dS -paper us %d")
     ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("." "%(o?)xdvi + %dS %d")))
 '(abbrev-file-name "~/Dropbox/.myabbrevs")
 '(ag-highlight-search t)
 '(airline-display-directory nil)
 '(all-the-icons-dired-monochrome nil)
 '(ange-ftp-try-passive-mode t)
 '(annotate-annotation-position-policy :margin)
 '(annotate-file "~/Dropbox/Org_other/annotations")
 '(annotate-use-echo-area t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(appt-display-duration 6)
 '(appt-display-format 'echo)
 '(async-shell-command-display-buffer nil)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.saves")))
 '(bbdb-accept-name-mismatch 0)
 '(bbdb-default-area-code 585)
 '(bbdb-default-country "")
 '(bbdb-default-label-list '("Home" "Office" "Mobile" "Other"))
 '(bbdb-mail-user-agent 'gnus-user-agent)
 '(bbdb-message-all-addresses t)
 '(bbdb-message-pop-up t)
 '(bbdb-mua-pop-up t)
 '(bbdb-phone-label-list
   '("Home" "Office" "Mobile" "Other" "Work" "Cell" "Pager" "FAX"))
 '(bbdb-pop-up-layout 'one-line)
 '(bbdb-pop-up-window-size 1)
 '(before-save-hook '(time-stamp))
 '(bibtex-maintain-sorted-entries t)
 '(blink-matching-paren-distance 55600)
 '(blink-matching-paren-on-screen t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bookmark-fontify nil)
 '(browse-kill-ring-quit-action 'save-and-restore)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-firefox-arguments '("-new-window"))
 '(browse-url-firefox-program "conkeror")
 '(browse-url-galeon-arguments '("-n"))
 '(browse-url-galeon-startup-arguments '("-x"))
 '(browse-url-generic-args nil)
 '(browse-url-generic-program "firefox")
 '(browse-url-netscape-program "netscoop")
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
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(csv-separators '("," "	"))
 '(cua-global-mark-cursor-color "#41c7b9")
 '(cua-normal-cursor-color "#adbcbc")
 '(cua-overwrite-cursor-color "#dbb32d")
 '(cua-read-only-cursor-color "#75b938")
 '(current-language-environment "ASCII")
 '(custom-enabled-themes '(rebecca))
 '(custom-safe-themes
   '("57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "265f68939a70832a73137ef621b14882f83643882b1f0dfa2cd35b91b95afbcc" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "762f6a25cb0e3870f20299828d172738e7c6d16be1ec6ac58cd2eba51ba07be2" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "4e6ec38d7940398caef89b1f653a7f88d909f58a2837d6504edc573b063919df" "b70b1895b418f61a89fe39771703f0b1b2da17c6b112967e13a55f235b01200f" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "fdf291d1953988fc04f44a1f39821fc38243b7395c1d25e8e12adca065674cf2" "aed1e83914919e1fd0eb1e4c6cdb52ca664e262a82fd8fa99f6ff6ea77c6a6b3" "383dc6271131e5a5169c6a0e029ae4e2c1d71ce91cd985fbc78ff02230e30655" "c8e076f0e2df414c02fdb46b09b735628e73c73f72f9d78392edf99de7d86977" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "3f7b4c736ffe0a373b06ce3d97c26b9e559bbc4f9b2e50e4b53143f0b0d7eb2c" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" "cba5ebfabc6456e4bbd68e0394d176161e1db063c6ca24c23b9828af0bdd7411" "61e7d5ae9201449b640aa0376f9bfe7eb0b19c97c7c0631a137a404ebf8140da" "0a7e543a12cf83c56ee92b799983ed6e3d36e9a0f7f3d87d0342ae7b466d8ed6" "6339e18e32734507d5b70817fbd490cdf1761826d7445153215ad7ee63ee3931" "8b4d8679804cdca97f35d1b6ba48627e4d733531c64f7324f764036071af6534" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "c5ad91387427abc66af38b8d6ea74cade4e3734129cbcb0c34cc90985d06dcb3" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "a11808699b77d62f5d10dd73cd474af3057d84cceac8f0301b82ad3e4fb0433e" "cf9f20cab61999609e47b969f6d7a89c148e16f94ae3f2f127fecfc27dc878d3" "c19e5291471680e72d8bd98f8d6e84f781754a9e8fc089536cda3f0b7c3550e3" "6973f93f55e4a6ef99aa34e10cd476bc59e2f0c192b46ec00032fe5771afd9ad" "7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" "d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "5419aade6b81139dbdc5ff7f924c34113f7b069a47729fa510868fa50d599618" "93dc869357db37fb1f3d4e3e8f0bd88c430c5919b17edfb0936f2c21c1821526" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "b3697d12fb7c087e1337432be92026b5fd218e7e43277918c0fce680d573a90c" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "19af2877d17fc4749ef3c860e84e8a9c177133a35c4daf6626d8e981c8eae2c7" "fd50a938921687443697065dd2c810d3e87c3bcaf81a33280d06a974c6697170" "5e515425f8a5ce00097d707742eb5eee09b27cebc693b8998c734305cbdce1f5" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "14de8f58ad656af5be374086ae7ab663811633fc1483a02add92f7a1ff1a8455" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "99b2fdc7de612b74fcb76eb3a1962092cf729909223434f256c7007d490d787a" "7444cf597389d4e0096c2cbc92ec154bf8526629a5fa6533886a3dfff00f2e0b" "fe16a59cc8d28255a61c701b032950a4785cc60708afebd352ed5960fcbc0e68" "96872a5b9e9a6b092df1e4bd034699c606c28f675869a8ff3ada1ca5f4d16ebf" "d64b20a5b3c0abc22a5f0945a4e4aa7dd25f971e587a760316a73ca851d7e82f" "94554072dc495e8344cf3c1b0d9047205a14b052756c20b55dfce4af26ffc7bc" "203fe0858c2018058526eff9887b06facf5044a94cf8af4dbf66bd16057d28f1" "d88c43fe03ac912e35963695caf0ae54bc6ce6365c3a42da434ef639f7a37399" "980f0adf3421c25edf7b789a046d542e3b45d001735c87057bccb7a411712d09" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "f633d825e380caaaefca46483f7243ae9a663f6df66c5fad66d4cab91f731c86" "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "9e9b6898d063668bfa73ee0536ea2bcdf95ef1d6438eb88594a32b5b4abd5022" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "b2db1708af2a7d50cac271be91908fffeddb04c66cb1a853fff749c7ad6926ae" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "6f8a2eb434dc66f3b77e3ee80b66952106eb201786a273d12ab14ba7d1bd0c49" "4b16156b5172d8735ec06ec650ed01c74b327f1a5a75a79d93ac28481b819d93" "1a504c62038a2a10503dcd27a0b74ff7dfdc63da1e4f4fbef50bfc5612e27c6d" "cf28bfffbf8726a31989e662986065b5319670902ac1af0e63fb8e773c119488" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "133222702a3c75d16ea9c50743f66b987a7209fb8b964f2c0938a816a83379a0" "b61c55259c639a54628f91452b060b99c550a1269eb947e372321b806b68f114" "aab598c4d024d544b4e8b356a95ca693afa9de000b154bd2f86eed68c9e75557" "a0bbe4dc3513cbd049eb95f79c467b6f19dc42979fec27a0481bb6980bd8d405" "9864c2e956c25b3098fbc935ba0969e333dd74ecd7a1013c8dd39a6c171e1cca" "51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "cf6d8127339c76d2a4b8165492a2bee417ccd3741d292a80015e95f6e9f8769f" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "b7e38c2b835e8b46cb51beb222ec85310a2b63135d4abbca44ecf533706fa6aa" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "21e37baa0460d29970b6e2eabd562a509c2a72cb1f328edba4d51419ed66e0e8" "6f5dc752ca593ab14b2e0ad33a1cfea69c11397dfd641b08fdf61b779d37e858" "f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" "d0e97afdf241e6931af47ebe03bace80524f56bd6a2668204d33db47f728f484" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "25f330cb050c7e7ec402af1b60243e8185a7837b455af0fa026593d4f48a78b2" "d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad" "a2c537c981b4419aa3decac8e565868217fc2995b74e1685c5ff8c6d77b198d6" "31bfef452bee11d19df790b82dea35a3b275142032e06c6ecdc98007bf12466c" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "3ad55e40af9a652de541140ff50d043b7a8c8a3e73e2a649eb808ba077e75792" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "27470eddcaeb3507eca2760710cc7c43f1b53854372592a3afa008268bcf7a75" "e85dd0d1b43cc1d725db627298c2753b0c3e90dc0b195e80f09f97a4e1e5660c" "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "951e10f17de57de1e0c9cbeb44fcdda1b6c6d26beab40c3bd0abbfc38dd5c9c8" "0f8f704ffc80ef1f511e7a7b54977d11cbb32d772511a3f351aeb239c7d96b33" "98ad28b9f7df3e53b85b4f8fcc300c353aeeac097016c5ac897e870501d87be9" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "1a7b620db388c2e4ae288794bbe7ed3b1e279cf67e8a51b6a678e4853467c748" "9ed7382aeb47f94ad0712ad57959354d03aa5df9" "f4c4f3eb70bd3dc14bbcca8a24d96719089fdd89" "207bb5b99ebc26b45e2d575342724c10236acd74" "ce4d82359c6d47de03485db52f9e1b44141666f7" "2bee775c3a3640f7c6f2c123d4ccaeab55f89962" "2eb734da07dcd6095f66709b0e85319e2624ef16" "549e06b318bd90ab10065db84e240f733b5af7fa" "64b170bd7204fe8e9c45b8f4f445dfb5f52d12ac" "2bfbc800988b899e101edd59f601ab530ea97686" "517aecb1202bfa31fd3c44473d72483c5166124d" default))
 '(dashboard-set-file-icons t)
 '(dashboard-set-heading-icons t)
 '(debug-on-error nil)
 '(default-input-method "greek-ibycus4")
 '(deft-directory "/home/dan/Dropbox/GTD/")
 '(delete-old-versions t)
 '(diary-display-function 'diary-fancy-display)
 '(diary-list-entries-hook '(diary-include-other-diary-files diary-sort-entries))
 '(diary-mark-entries-hook '(diary-mark-included-diary-files))
 '(dictionary-default-dictionary "*")
 '(dictionary-server "dict.org")
 '(dictionary-tooltip-dictionary "gcide")
 '(dired-dwim-target t)
 '(dired-guess-shell-alist-user
   '(("\\.pdf$" "evince")
     ("\\.docx?$" "libreoffice")
     ("\\.aup?$" "audacity")
     ("\\.pptx?$" "libreoffice")
     ("\\.odf$" "libreoffice")
     ("\\.odt$" "libreoffice")
     ("\\.odt$" "libreoffice")
     ("\\.ods$" "libreoffice")
     ("\\.odp$" "libreoffice")
     ("\\.xls$" "libreoffice")
     ("\\.xlsx$" "libreoffice")
     ("\\.html$" "w3m")
     ("\\.jpe?g$" "gpicview")
     ("\\.png$" "gpicview")
     ("\\.gif$" "gpicview")
     ("\\.psd$" "gimp")
     ("\\.xcf" "gimp")
     ("\\.odt$" "libreoffice")
     ("\\.xo$" "unzip")
     ("\\.3gp$" "vlc")
     ("\\.mp3$" "vlc")
     ("\\.flac$" "vlc")
     ("\\.avi$" "vlc")
     ("\\.wm[va]$" "vlc")
     ("\\.flv$" "vlc")
     ("\\.mov$" "vlc")
     ("\\.divx$" "vlc")
     ("\\.mp4$" "vlc")
     ("\\.mkv$" "vlc")
     ("\\.mpe?g$" "vlc")
     ("\\.m4[av]$" "vlc")
     ("\\.mp2$" "vlc")
     ("\\.pp[st]$" "libreoffice")
     ("\\.ogg$" "vlc")
     ("\\.ogv$" "vlc")
     ("\\.rtf$" "libreoffice")
     ("\\.ps$" "gv")
     ("\\.mp3$" "play")
     ("\\.wav$" "vlc")
     ("\\.rar$" "unrar x")))
 '(dired-icon-image-size 32)
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$\\|auto$")
 '(dired-open-extensions
   '(("docx" . "mimeopen -n")
     ("doc" . "mimeopen -n")
     ("odp" . "mimeopen -n")
     ("ods" . "mimeopen -n")
     ("xls" . "mimeopen -n")
     ("pdf" . "evince")
     ("odt" . "mimeopen -n")
     ("mscz" . "musescore")
     ("flac" . "clementine")
     ("mp3" . "clementine")
     ("ogg" . "clementine")
     ("avi" . "vlc")
     ("mp4" . "vlc")
     ("wma" . "vlc")
     ("flv" . "vlc")
     ("xlsx" . "mimeopen -n")
     ("htm" . "firefox")
     ("html" . "firefox")))
 '(diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)
 '(diredp-hide-details-initially-flag nil)
 '(diredp-image-preview-in-tooltip 'full)
 '(display-time-interval 30)
 '(display-time-mail-face 'mode-line-emphasis)
 '(display-time-mode t)
 '(display-time-string-forms
   '((if
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
		 (propertize "Mail" 'display
			     `(when
				  (and display-time-use-mail-icon
				       (display-graphic-p))
				,@display-time-mail-icon ,@(list :background
								 (face-attribute display-time-mail-face :background)))
			     'help-echo "mouse-2: Read mail" 'local-map
			     (make-mode-line-mouse-map 'mouse-2 read-mail-command)))
       "")))
 '(display-time-use-mail-icon t)
 '(elegant-agenda-header-preference 'thin)
 '(emms-player-list '(emms-setup-default-player-list))
 '(emms-source-file-default-directory "~/Multimedia/Music/")
 '(emojify-inhibit-functions '(emojify-in-org-tags-p))
 '(emojify-inhibit-in-buffer-functions '(emojify-helm-buffer-p))
 '(emojify-inhibit-major-modes
   '(dired-mode doc-view-mode debugger-mode pdf-view-mode image-mode help-mode ibuffer-mode ert-results-mode compilation-mode proced-mode mu4e-headers-mode locate-mode))
 '(emojify-user-emojis
   '((":blue-info:"
      ("name" . "blue-info")
      ("image" . "/home/dan/Dropbox/Stuff/blue-info.png")
      ("style" . "github"))))
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
			 (:color "#c0c0c0"))
     (implicitParams :underline
		     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000")))
 '(erc-autojoin-channels-alist
   '(("freenode.net" "#org-mode" "#emacs")
     ("oftc.net" "#awesome")))
 '(erc-hide-list '("JOIN" "PART" "QUIT"))
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring smiley sound stamp track))
 '(erc-nick "kc5gmr")
 '(erc-prompt
   (lambda nil
     (if
	 (and
	  (boundp 'erc-default-recipients)
	  (erc-default-target))
	 (erc-propertize
	  (concat
	   (erc-default-target)
	   ">")
	  'read-only t 'rear-nonsticky t 'front-nonsticky t)
       (erc-propertize
	(concat "ERC>")
	'read-only t 'rear-nonsticky t 'front-nonsticky t))))
 '(erc-track-exclude-types
   '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))
 '(erc-track-position-in-mode-line t t)
 '(erc-track-shorten-cutoff 7)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#dedede")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(font-latex-match-reference-keywords
   '(("cite" "[{")
     ("cites" "[{}]")
     ("autocite" "[{")
     ("footcite" "[{")
     ("footcites" "[{")
     ("parencite" "[{")
     ("textcite" "[{")
     ("fullcite" "[{")
     ("citetitle" "[{")
     ("citetitles" "[{")
     ("headlessfullcite" "[{")))
 '(font-lock-maximum-size 1048576)
 '(garbage-collection-messages nil)
 '(global-font-lock-mode t nil (font-lock))
 '(global-magit-file-mode t)
 '(highlight-changes-colors '("#f275be" "#af88eb"))
 '(highlight-symbol-colors
   '("#4e3b57c24752" "#1c5d5c5162eb" "#58ac47cc4aec" "#3add4f876dec" "#316958f94870" "#53c94f1e4a56" "#1e6c515d7099"))
 '(highlight-symbol-foreground-color "#cad8d9")
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-bg-colors
   '("#ac8a0c" "#b45b24" "#b42e2a" "#b3478d" "#7255b7" "#0068bb" "#00a195" "#489615"))
 '(hl-fg-colors
   '("#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48" "#103c48"))
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#41c7b9" "#dbb32d" "#4695f7" "#af88eb" "#75b938"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#ba29eb")
     ("NEXT" . "#ba29eb")
     ("THEM" . "#09b8be")
     ("PROG" . "#00bfa5")
     ("OKAY" . "#0082c9")
     ("DONT" . "#ff3d00")
     ("FAIL" . "#b0151a")
     ("DONE" . "#22a54e")
     ("NOTE" . "#ffb627")
     ("KLUDGE" . "#fb6107")
     ("HACK" . "#4d10a5")
     ("TEMP" . "#7a7b75")
     ("FIXME" . "#ba29eb")
     ("XXX" . "#0d47a1")
     ("XXXX" . "#0d47a1")
     ("\\?\\?\\?+" . "#0d47a1")))
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
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-never-show-predicates '("\\.org_archive$") nil (ibuf-ext))
 '(ibuffer-saved-filter-groups
   '(("work"
      ("LaTeX"
       (mode . latex-mode))
      ("Org"
       (or
	(mode . org-agenda-mode)
	(mode . org-mode)))
      ("PDF"
       (mode . pdf-view-mode))
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
      ("Org Agendas"
       (name . "*Org Agenda"))
      ("Help"
       (or
	(name . "*Help*")
	(name . "*Apropos*")
	(name . "*info*"))))
     ("home"
      ("Dailies"
       (directory . "daily"))
      ("LaTeX"
       (mode . latex-mode))
      ("Org"
       (or
	(mode . org-agenda-mode)
	(mode . org-mode)))
      ("PDF"
       (mode . pdf-view-mode))
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
      ("Org Agendas"
       (name . "*Org Agenda"))
      ("Help"
       (or
	(name . "*Help*")
	(name . "*Apropos*")
	(name . "*info*"))))))
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
	(starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(icalendar-import-format "%s")
 '(ido-cr+-disable-list
   '(read-file-name-internal read-buffer internal-complete-buffer todo-add-category gnus-emacs-completing-read gnus-iswitchb-completing-read grep-read-files magit-builtin-completing-read ess-completing-read Info-read-node-name tmm-prompt org-tags-completion-function ffap-read-file-or-url ffap-read-file-or-url-internal sly-read-symbol-name org-olpath-completing-read execute-extended-command org-refile))
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order '(".org" ".tex"))
 '(ido-ignore-buffers '("\\` " "\\*Messages\\*"))
 '(ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"))
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point 'guess)
 '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
 '(ido-vertical-show-count t)
 '(iimage-mode-image-search-path '("/home/dan/"))
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(ispell-check-comments nil)
 '(kept-new-versions 200)
 '(latex-run-command "latex -src-specials")
 '(line-spacing 0.2)
 '(list-directory-verbose-switches "-lh")
 '(locate-command "locate")
 '(lpr-command "evince")
 '(lpr-printer-switch "-d")
 '(lsp-ui-doc-border "#cad8d9")
 '(magit-diff-use-overlays nil)
 '(magit-repository-directories '(("~/Software/Git/" . 1)))
 '(message-cite-function 'message-cite-original-without-signature)
 '(message-log-max 200)
 '(message-send-mail-partially-limit nil)
 '(midi-input-device "/dev/midi4")
 '(midi-input-shift-key 0)
 '(midi-input-shift-velocity 100)
 '(mixed-pitch-fixed-pitch-faces
   '(font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-comment-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-variable-name-face font-lock-warning-faceorg-block markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-code org-latex-and-related org-meta-line org-table org-verbatim))
 '(mixed-pitch-variable-pitch-cursor '(bar . 4))
 '(mode-icons
   '(("\\`CSS\\'" "css" xpm)
     ("\\`Coffee\\'" "coffee" xpm-bw)
     ("\\`Compilation\\'" "compile" xpm)
     ("\\`Dart\\'" "dart" xpm)
     ("\\`Flutter\\'" "flutter" xpm)
     ("\\`Elixir\\'" "elixir" xpm)
     ("\\`Erlang\\'" "erlang" xpm)
     ("\\`Emacs-Lisp\\'" "emacs" xpm)
     ("\\`Lisp Interaction\\'" "emacs" xpm)
     ("\\`HTML\\'" "html" xpm)
     ("\\`Haml\\'" "haml" xpm)
     ("\\`Image\\[imagemagick\\]\\'" "svg" xpm)
     ("\\`Inf-Ruby\\'" "infruby" xpm)
     ("\\`Java[Ss]cript\\'" "js" xpm)
     ("\\`Lisp\\'" "cl" xpm)
     ("\\`Lua\\'" "Lua-Logo_16x16" png)
     ("\\`nXML\\'" "xml" xpm)
     ("\\`Org\\'" "org" xpm)
     ("\\`PHP\\(\\|/.*\\)\\'" "php" xpm)
     ("\\`Projectile Rails Server\\'" "rails" xpm)
     ("\\`Python\\'" "python" xpm)
     ("\\` Emmet\\'" "emmet" xpm)
     ("\\`RJSX\\'" "react" xpm)
     ("\\`Ruby\\'" "ruby" xpm)
     ("\\`Rust\\'" "rust" xpm)
     ("\\`EnhRuby\\'" "ruby" xpm)
     ("\\`ESS\\[S\\]\\'" "R" xpm)
     ("\\`ESS\\[SAS\\]\\'" "sas" xpm)
     ("\\`ESS\\[BUGS\\]\\'" 61832 FontAwesome)
     ("\\`iESS\\'" "R" xpm)
     ("\\`SCSS\\'" "sass" xpm)
     ("\\`Sass\\'" "sass" xpm)
     ("\\`Scheme" "scheme" xpm-bw)
     ("\\`Shell-script" "bash" xpm-bw)
     ("\\`Slim" "slim" xpm-bw)
     ("\\`Snippet" "yas" xpm)
     ("\\`Term\\'" "term" xpm)
     ("\\`Web\\'" "html" xpm)
     ("\\`XML\\'" "xml" xpm)
     ("\\`YAML\\'" "yaml" xpm)
     ("\\` ?YASnippet\\'" "yas" xpm)
     ("\\` ?yas\\'" "yas" xpm)
     ("\\` ?hs\\'" "hs" xpm)
     ("\\`Markdown\\'" 61641 github-octicons)
     ("\\`GFM\\'" 61641 github-octicons)
     ("\\`Scala\\'" 61787 font-mfizz)
     ("\\`Magit\\'" 61906 FontAwesome)
     ("\\` Pulls\\'" 61586 FontAwesome)
     ("\\`Zip-Archive\\'" 61894 FontAwesome)
     ("\\` ARev\\'" 61473 FontAwesome)
     ("\\`Calc\\(ulator\\)?\\'" 61932 FontAwesome)
     ("\\`Debug.*\\'" 61832 FontAwesome)
     ("\\`Debug.*\\'" 61832 FontAwesome)
     ("\\`Calendar\\'" 61555 FontAwesome)
     ("\\`Help\\'" 61529 FontAwesome)
     ("\\`WoMan\\'" 61530 FontAwesome)
     ("\\`C\\(/.*\\|\\)\\'" "c" xpm)
     ("\\`Custom\\'" 61459 FontAwesome)
     ("\\`Go\\'" "go" xpm)
     ("\\` ?Rbow\\'" "rainbow" xpm)
     ("\\` ?ivy\\'" "ivy" xpm)
     ("\\` ?ICY\\'" "icy" xpm)
     ("\\` ?Golden\\'" "golden" xpm-bw)
     ("\\`BibTeX\\'\\'" "bibtex" xpm-bw)
     ("\\`C[+][+]\\(/.*\\|\\)\\'" "cpp" xpm)
     ("\\`C[#]\\(/.*\\|\\)\\'" "csharp" xpm)
     ("\\`Haskell\\'" 61734 font-mfizz)
     ("\\`Clojure\\'" 61707 font-mfizz)
     ("\\`Java\\(/.*\\|\\)\\'" 61739 font-mfizz)
     ("\\`C?Perl\\'" 61768 font-mfizz)
     ("\\`Octave\\'" "octave" xpm)
     ("\\`AHK\\'" "autohotkey" xpm)
     ("\\`Info\\'" 61530 FontAwesome)
     ("\\` ?Narrow\\'" 61542 FontAwesome)
     ("\\`Dockerfile\\'" "docker" xpm)
     ("\\`Spacemacs buffer\\'" "spacemacs" png)
     ("\\` ?emoji\\'" "emoji" png)
     ("\\`Org-Agenda" 61510 FontAwesome)
     ("\\`PS\\'" "powershell" xpm)
     (mode-icons-powershell-p "powershell" xpm)
     (mode-icons-cmd-p "cmd" xpm-bw)
     (mode-icons-msys-p "msys" xpm)
     (mode-icons-cygwin-p "cygwin" xpm)
     (read-only 61475 FontAwesome)
     (writable 61596 FontAwesome)
     (save 61639 FontAwesome)
     (saved "" nil)
     (modified-outside 61553 FontAwesome)
     (steal 61979 FontAwesome)
     (apple 60095 IcoMoon-Free)
     (apple 61817 FontAwesome)
     (win 61818 FontAwesome)
     (unix 61700 font-mfizz)
     (unix 61820 FontAwesome)
     (undecided 61736 FontAwesome)
     ("Text\\'" 61686 FontAwesome)
     ("\\` ?company\\'" 61869 FontAwesome)
     ("\\` ?AC\\'" 61838 FontAwesome)
     ("\\` ?Fly\\'" 59922 IcoMoon-Free)
     ("\\` ?SP\\(/s\\)?\\'" "smartparens" xpm)
     ("\\` ?Ergo" 61724 FontAwesome)
     ("\\` ?drag\\'" 61511 FontAwesome)
     ("\\` ?Helm\\'" "helm" xpm-bw)
     ("\\`Messages\\'" 62075 FontAwesome)
     ("\\`Conf" 61918 FontAwesome)
     ("\\`Fundamental\\'" 61462 FontAwesome)
     ("\\`Javascript-IDE\\'" "js" xpm)
     ("\\` Undo-Tree\\'" ":palm_tree:" emoji)
     ("\\`LaTeX\\'" "tex" ext)
     ("\\`Image\\[xpm\\]\\'" "xpm" ext)
     ("\\`Image\\[png\\]\\'" "png" ext)
     ("\\` ?AI\\'" 61500 FontAwesome)
     ("\\` ?Isearch\\'" 61442)
     (default 61529 FontAwesome)
     ("\\` ?\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil)))
 '(mode-icons-desaturate-active t)
 '(mode-icons-generate-emoji-xpms t)
 '(mode-icons-generate-font-xpms t)
 '(mode-icons-prefer-xpm-over-font t)
 '(mode-icons-use-default-icon t)
 '(noaa-latitude 42.7913)
 '(noaa-longitude -86.0362)
 '(nrepl-message-colors
   '("#00afef" "#778ca3" "#009c9f" "#778ca3" "#005cc5" "#fa1090" "#009c9f" "#778ca3"))
 '(ocpf-frame-parameters
   '((name . "org-capture-pop-frame")
     (width . 80)
     (height . 17)
     (tool-bar-lines . 0)
     (menu-bar-lines . 1)))
 '(org-agenda-category-icon-alist
   '(("mine" "~/Multimedia/OrgIcons/icon6.png" nil nil :ascent center)
     ("theo" "~/Multimedia/OrgIcons/library-icon.png" nil nil :ascent center)
     ("clerk" "~/Multimedia/OrgIcons/face-with-monocle_1f9d0.png" nil nil :ascent center :width 18)
     ("Diary" "~/Multimedia/OrgIcons/calendar_icon.png" nil nil :ascent center)
     ("recur" "~/Multimedia/OrgIcons/repeat.png" nil nil :ascent center :width 20)))
 '(org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 1 :formula %))
 '(org-agenda-custom-commands
   '(("g" "Goals" todo "ACTIVE"
      ((org-agenda-sorting-strategy
	'(deadline-up))
       (org-agenda-prefix-format "%i  %-22(org-entry-get nil \"DEADLINE\") %s")))
     ("r" . "Reviews")
     ("rw" "Weekly Review"
      ((agenda ""
	       ((org-agenda-overriding-header "Summary of Last Week")
		(org-agenda-start-with-log-mode
		 '(closed clock state))
		(org-agenda-archives-mode t)
		(org-agenda-start-day "-2Sun")
		(org-agenda-span 'week)
		(org-agenda-skip-function
		 '(org-agenda-skip-entry-if 'todo
					    '("NEXT" "CURRENT")))
		(org-agenda-prefix-format
		 '((agenda . " %?-12t% s")))))
       (agenda ""
	       ((org-agenda-overriding-header "This Week's stuff")
		(org-agenda-span 'week)))
       (todo "ACTIVE"
	     ((org-agenda-overriding-header "Active Goals")
	      (org-agenda-sorting-strategy
	       '(deadline-up))))
       (todo "NOW"
	     ((org-agenda-overriding-header "Now Objectives")
	      (org-agenda-sorting-strategy
	       '(deadline-up))))
       (tags "TODO=\"CURRENT\""
	     ((org-agenda-overriding-header "Current Projects")
	      (org-agenda-sorting-strategy
	       '(deadline-up))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Goals")
	       (org-stuck-projects
		'("/+ACTIVE"
		  ("NOW" "CURRENT" "NEXT")
		  nil ""))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Objectives")
	       (org-stuck-projects
		'("/+NOW"
		  ("CURRENT" "NEXT" "WAIT")
		  nil ""))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Projects")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Items I'm Waiting On")
	      (org-agenda-sorting-strategy
	       '(scheduled-up))
	      (org-agenda-prefix-format "%i %-7:c% s")))
       (tags "TODO=\"MUL\""
	     ((org-agenda-overriding-header "Staged Objectives")
	      (org-agenda-sorting-strategy
	       '(deadline-up))))
       (tags "TODO=\"BREWING\""
	     ((org-agenda-overriding-header "Staged Projects")
	      (org-agenda-sorting-strategy
	       '(deadline-up))))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Unscheduled Next Actions")
	      (org-agenda-skip-function
	       '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
      ((org-agenda-start-with-log-mode
	'(closed clock state))))
     ("rm" "Monthly Review"
      ((stuck ""
	      ((org-agenda-overriding-header "Stuck Objectives")
	       (org-stuck-projects
		'("/+NOW"
		  ("CURRENT" "NEXT")
		  nil ""))))
       (stuck ""
	      ((org-agenda-overriding-header "Stuck Projects")))
       (todo "WAIT"
	     ((org-agenda-overriding-header "Items I'm Waiting On")
	      (org-agenda-sorting-strategy
	       '(scheduled-up))
	      (org-agenda-prefix-format "%i %-7:c% s")))
       (tags "TODO=\"BREWING\"|TODO=\"MUL\""
	     ((org-agenda-overriding-header "Staged Projects and Goals")))
       (tags "TODO=\"MAYB\"|TODO=\"PERHAPS\""
	     ((org-agenda-overriding-header "Someday/Maybe Items")
	      (org-agenda-sorting-strategy
	       '(category-keep todo-state-up alpha-up)))))
      ((org-agenda-archives-mode nil))
      nil)
     ("d" "Agenda with completed items" agenda ""
      ((org-agenda-start-with-log-mode
	'(closed state))
       (org-agenda-archives-mode t)
       (org-agenda-skip-function
	'(org-agenda-skip-entry-if 'todo
				   '("CURRENT" "NOW")))
       (org-agenda-sorting-strategy
	'(priority-down alpha-up))
       (org-agenda-category-filter-preset
	'("+clerk" "+recur"))))
     ("w" "Work-only schedule, tasks only" agenda ""
      ((org-agenda-skip-function
	'(org-agenda-skip-entry-if 'todo
				   '("CURRENT" "NOW")))
       (org-agenda-view-columns-initially t)
       (org-agenda-category-filter-preset
	'("+clerk" "+recur"))))))
 '(org-agenda-diary-file "~/Dropbox/GTD/events.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-exporter-settings
   '((ps-number-of-columns 1)
     (ps-landscape-mode nil)
     (htmlize-output-type 'css)))
 '(org-agenda-files
   '("/home/dan/Dropbox/GTD/personal.org" "/home/dan/Dropbox/GTD/scholarship.org" "/home/dan/Dropbox/Brain/books.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-insert-diary-extract-time t)
 '(org-agenda-insert-diary-strategy 'top-level)
 '(org-agenda-log-mode-items '(closed clock))
 '(org-agenda-prefix-format
   '((agenda . " %i %-7:c%?-12t% s")
     (timeline . "  % s")
     (todo . " %i  %-7:c%-8(my/org-todo-monthyr)")
     (tags . " %i  %-7:c")
     (search . " %i %-12:c")))
 '(org-agenda-restore-windows-after-quit nil)
 '(org-agenda-scheduled-leaders '("Scheduled: " "Schd.%3dx: "))
 '(org-agenda-show-inherited-tags nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   '((agenda priority-down timestamp-up alpha-up)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
 '(org-agenda-span 'day)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-sticky t)
 '(org-agenda-timegrid-use-ampm nil)
 '(org-agenda-use-time-grid nil)
 '(org-archive-location "archives/%s_archive::")
 '(org-archive-mark-done nil)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (lilypond . t)
     (gnuplot . t)
     (shell . t)))
 '(org-capture-templates
   '(("n" "NEXT actions")
     ("nw" "Work NEXT action" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("np" "Personal NEXT action" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("nc" "Coaching NEXT action" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNextCoach.org"))
     ("ns" "Scholarship NEXT action" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Tasks")
      (file "~/Dropbox/Emacs_git/cptTmpNext.org"))
     ("d" "Draft & send item for committee" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
      "** NEXT [#B] Draft & send %^{Committee Name} %^{Item|minutes|agenda}        :shlw:clsd:
SCHEDULED: %^t
:PROPERTIES:
:Effort: 0:15
:CREATED: %U
:END:")
     ("p" "New Project")
     ("pw" "Work Project" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Unassigned Projects")
      (file "~/Dropbox/Emacs_git/cptTmpProj.org")
      :jump-to-captured t)
     ("pp" "Personal Project" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Unassigned Projects")
      (file "~/Dropbox/Emacs_git/cptTmpProj.org")
      :jump-to-captured t)
     ("o" "New Objective")
     ("ow" "Work Objective" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpGoal.org")
      :jump-to-captured t)
     ("op" "Personal Objective" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpGoal.org")
      :jump-to-captured t)
     ("os" "Scholarship Objective" entry
      (file+headline "~/Dropbox/GTD/scholarship.org" "Objectives")
      (file "~/Dropbox/Emacs_git/cptTmpGoal.org")
      :jump-to-captured t)
     ("a" "Ask someone about something")
     ("aw" "Work Ask" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} %^{Thing to Ask}%? :EMAIL:
SCHEDULED: %^t
:PROPERTIES:
:effort: 0:05
:CREATED: %U
:END:
")
     ("ap" "Personal Ask" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} %^{Thing to Ask}%? :EMAIL:
SCHEDULED: %^t
:PROPERTIES:
:effort: 0:05
:CREATED: %U
:END:
")
     ("m" "Ask someone to meet" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
      "* NEXT [#B] ❓ %^{Person to Ask} to meet%? :EMAIL:
SCHEDULED: %^t
:PROPERTIES:
:effort: 0:05
:CREATED: %U
:END:
")
     ("M" "Meeting setup" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Unassigned Projects")
      (file "~/Dropbox/Emacs_git/cptCmteMeeting.org")
      :jump-to-captured t)
     ("t" "Think about something")
     ("tw" "Work thought" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
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
      "* NEXT [#B] Rsrch %^{Item} :NET:
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :immediate-finish t)
     ("rw" "Research Work" entry
      (file+headline "~/Dropbox/GTD/clerk.org" "Tasks")
      "* NEXT [#B] Rsrch %^{Item} :NET:
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
")
     ("b" "Buy something")
     ("bl" "Buy local" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] 🛒 %^{Item} :CAR:
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :immediate-finish t)
     ("bo" "Buy online" entry
      (file+headline "~/Dropbox/GTD/personal.org" "Tasks")
      "* NEXT [#B] 🛒 %^{Item} :NET:
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :immediate-finish t)
     ("Q" "Quote" entry
      (file "~/Dropbox/Brain/quotes.org")
      "* %^{Summary}
  :PROPERTIES:
  :CREATED: %u
  :Author:   %^{Author}
  :Title:    %^{Title}
  :pages:    %^{pages}
  :END:
%?")))
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
 '(org-crypt-key nil)
 '(org-cycle-hook
   '(org-cycle-hide-archived-subtrees org-cycle-hide-drawers org-cycle-show-empty-lines))
 '(org-deadline-warning-days 7)
 '(org-directory "~/Dropbox/GTD/")
 '(org-ditaa-eps-jar-path "/home/dan/.emacs.d/elpa/contrib/scripts/DitaaEps.jar")
 '(org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
 '(org-duration-format '((special . h:mm)))
 '(org-eldoc-breadcrumb-separator "⋯")
 '(org-ellipsis "⤵" nil nil "Customized with use-package org-bullets")
 '(org-enforce-todo-dependencies t)
 '(org-entities-user '(("space" "~" nil "&nbsp;" " " " " " ")))
 '(org-export-backends '(latex odt))
 '(org-export-date-timestamp-format "%B %e, %Y")
 '(org-export-invisible-backends '(ascii org))
 '(org-export-latex-date-format "%B %d, %Y")
 '(org-export-with-section-numbers nil)
 '(org-export-with-smart-quotes t)
 '(org-export-with-toc nil)
 '(org-fancy-priorities-list '("❗" "·" "⬇"))
 '(org-file-apps
   '(("docx?" . "libreoffice %s")
     (auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")))
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-footnote-auto-adjust t)
 '(org-global-properties
   '(("Effort_ALL" . "0:05 0:10 0:15 0:20 0:25 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
     ("Ink_ALL" . "Blue \"Blue Black\" Black Antietem Burgundy \"Purple Martin\" Beaver \"Apricot Orange\" \"Forest Green\" \"Quick Lime\" \"Nantucket Blue\" EMPTY")))
 '(org-goto-interface 'outline-path-completion)
 '(org-habit-following-days 1)
 '(org-habit-show-habits-only-for-today nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-postamble 'auto)
 '(org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")
 '(org-icalendar-include-body nil)
 '(org-icalendar-include-sexps t)
 '(org-icalendar-timezone "America/New_York")
 '(org-icalendar-use-scheduled '(event-if-not-todo event-if-todo todo-start))
 '(org-indent-indentation-per-level 1)
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-latex-classes
   '(("minutes" "\\documentclass[12pt]{article}
\\usepackage[margin=.75in]{geometry}
\\usepackage{sectsty}
\\usepackage{titling}
\\usepackage{fontspec}
\\setmainfont{Liberation Serif}
\\setsansfont[Color={0019D4}]{Lato}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}
\\renewcommand{\\maketitlehooka}{\\huge\\bfseries\\sffamily}
\\sectionfont{\\sffamily}
\\subsectionfont{\\sffamily}
\\pretitle{\\begin{center}\\huge}
\\posttitle{\\end{center}\\vspace{-14pt}}
\\setlength{\\droptitle}{-5em}
\\renewcommand{\\thesection}{\\arabic{section}.}
\\renewcommand{\\thesubsection}{\\Alph{subsection}.}
\\usepackage{titlesec}
\\titlespacing*{\\subsection}{\\parindent}{1ex}{1em}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{fontspec}
\\usepackage[onehalfspacing]{setspace}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Scale=1.1]{Cardo}
\\usepackage[it,small]{titlesec}
\\usepackage{graphicx}
\\renewcommand{\\thesection}{\\arabic{section}.}
\\renewcommand{\\thesubsection}{\\alph{subsection}.}
\\usepackage{sectsty}
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
\\subsectionfont{\\emph\\normalsize\\hspace*{.1pt}}
\\subsubsectionfont{\\normalsize\\it\\hspace*{3pt}}
\\reversemarginpar{}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("bylaws" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage{fontspec}
\\usepackage[singlespacing]{setspace}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Scale=1.05]{Cardo}
\\usepackage{titlesec}
\\titleformat{\\section}[block]{\\bf\\em\\Huge}{Part \\thechapter}{0pt}{\\hspace{.25in}}{}
\\usepackage{graphicx}
\\renewcommand{\\thesection}{\\arabic{section}.}
\\usepackage{sectsty}
\\subsectionfont{\\bfseries\\normalsize\\hspace*{.1pt}}
\\subsubsectionfont{\\normalsize\\it\\hspace*{3pt}}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("book" "\\documentclass[12pt, twoside]{book}
[NO-DEFAULT-PACKAGES]
\\usepackage{hyperref}
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
\\fancyhead[RO]{\\texthdrtitle{\\hfill CALLING IN COUNTERPOINT}} % odd pages: book title
\\fancyhead[LE]{\\texthdrtitle{\\leftmark}}   % even pages: chapter title
\\pagenumbering{roman}
\\usepackage{titlesec}
\\titleformat{\\chapter}[display]{\\large\\centering}{\\LARGE\\texthdrtitle{\\thechapter}}{1ex}{\\rule{2em}{.5pt}\\\\\\vspace{2ex}\\LARGE}{}
\\titleformat{\\section}[block]{\\hdrtitle\\centering}{}{0pt}{\\ }{}
\\renewcommand{\\thesection}{\\Alph{section}.}
\\titleformat{\\subsection}[block]{\\hdrtitle\\bfseries}{}{0pt}{\\ }{}
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
\\usepackage{titletoc}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("agenda" "\\documentclass[12pt]{article}
\\usepackage[margin=1in]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{multicol}
\\usepackage{fontspec}
\\setromanfont[Mapping=tex-text]{Cardo} % Dustismo Roman}
\\setsansfont[Mapping=tex-text]{Inconsolata}
\\newfontfamily{\\artsyfont}{Comfortaa}
\\DeclareTextFontCommand{\\zaph}{\\artsyfont}
\\usepackage{sectsty}
\\sectionfont{\\centering\\zaph}
\\subsectionfont{\\bfseries\\normalsize}
\\subsubsectionfont{\\normalfont\\normalsize\\hspace*{6pt}}
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
\\posttitle{\\end{center}\\vspace{-19pt}}
\\preauthor{\\begin{center}\\Large\\zaph}
\\postauthor{\\end{center}\\vspace{-14pt}}
\\predate{\\begin{center}\\Large\\zaph}\\postdate{\\normalfont\\par\\end{center}}
\\newcommand{\\resp}[1]{\\hfill\\normalfont\\emph{#1}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s} ")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("notes" "\\documentclass[12pt]{article}
\\usepackage{palatino}
[NO-DEFAULT-PACKAGES]
\\usepackage{sectsty}
\\usepackage{multicol}
\\usepackage{fullpage}
\\sectionfont{\\centering}
\\usepackage{titling}
\\setlength{\\droptitle}{-8em}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("lecture" "\\documentclass[12pt]{article}
\\usepackage[left=1.5in,vmargin=1in]{geometry}
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Scale=1.05]{Cardo}
\\usepackage[onehalfspacing]{setspace}
\\usepackage{sectsty}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\usepackage[rm]{titlesec}
\\renewcommand{\\thesection}{\\Roman{section}.}
\\titleformat{\\section}[display]{\\bf\\center\\Large}{Session \\Roman{section}}{24pt}{}{}
\\renewcommand{\\thesubsection}{\\Alph{subsection}.}
\\renewcommand{\\thesubsubsection}{\\hspace{48pt}\\arabic{subsubsection}.}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}"))
     ("syllabus" "\\documentclass[12pt]{article}
\\usepackage{palatino}
\\usepackage[margin=1in]{geometry}
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
\\end{center}}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
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
\\usepackage{xunicode}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("cotagenda" "\\documentclass[12pt]{article}
\\usepackage[margin=1.5in]{geometry}
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
     ("worshipls" "\\documentclass[12pt]{article}
\\special{landscape}
\\usepackage[print,1to1]{booklet}
\\usepackage[singlespacing]{setspace}
\\expandafter\\def\\expandafter\\quote\\expandafter{\\quote\\onehalfspacing}
\\setlength{\\textheight}{8.25in}
\\setlength{\\topmargin}{.5in}
\\setlength{\\headsep}{0in}
\\setlength{\\headheight}{0in}
\\setlength{\\topskip}{0in}
\\setlength{\\textwidth}{5.35in}
\\usepackage{sectsty}
\\usepackage{fontspec}
\\setmainfont{TeX Gyre Schola}
\\setmainfont[Scale=1.15]{Cardo}
\\newfontfamily\\zaph[Scale=1.25]{Anaktoria}
\\sectionfont{\\centering\\zaph}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\pretitle{\\begin{center}\\zaph\\huge}
\\posttitle{\\end{center}\\vspace{-18pt}}
\\preauthor{\\begin{center}\\zaph\\Large}
\\postauthor{\\end{center}\\vspace{-18pt}}
\\predate{\\begin{center}\\zaph\\Large}\\postdate{\\normalfont\\par\\end{center}}
\\target{\\magstepminus1}{\\paperheight}{\\paperwidth}
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("worship" "\\documentclass[12pt]{article}
\\usepackage[hmargin=1in, vmargin=.70in]{geometry}
\\usepackage{sectsty}
\\usepackage{setspace}
\\usepackage{fontspec}
\\setmainfont[Scale=1.15]{Accanthis ADF Std No3}
\\newfontfamily\\zaph[Scale=1.25]{Anaktoria}
\\sectionfont{\\centering\\zaph}
\\sectionfont{\\centering\\zaph}
\\subsubsectionfont{\\hspace{1em}\\itshape}
\\usepackage{titling}
\\setlength{\\droptitle}{-5em}
\\pretitle{\\begin{center}\\zaph\\huge}
\\posttitle{\\end{center}\\vspace{-18pt}}
\\preauthor{\\begin{center}\\zaph\\Large}
\\postauthor{\\end{center}\\vspace{-18pt}}
\\predate{\\begin{center}\\zaph\\Large}\\postdate{\\normalfont\\par\\end{center}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("cv" "\\documentclass[10pt]{article}
\\usepackage[margin=.75in]{geometry}
\\usepackage{sectsty}
\\usepackage{titling}
\\usepackage{fontspec}
\\setmainfont[Scale=1.15]{Liberation Serif}
\\setsansfont[Color={0019D4}]{Lato}
\\sectionfont{\\normalsize\\selectfont\\itshape}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}
\\renewcommand{\\maketitlehooka}{\\huge\\bfseries\\sffamily}
\\sectionfont{\\sffamily}
\\subsectionfont{\\sffamily}
\\usepackage[backend=biber,
            style=authortitle,
	    url=false,
	    doi=true,
	    eprint=false]{biblatex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("profile" "\\documentclass[11pt]{article}
[NO-DEFAULT-PACKAGES]
%WARNING: This class requires org-latex-pdf-process to be set to xelatex
\\usepackage[margin=1in]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Variant=01]{Linux Libertine O}
\\usepackage{xunicode}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("script" "\\documentclass[12pt]{article}
[NO-DEFAULT-PACKAGES]
\\special{landscape}
\\usepackage[print,1to1]{booklet}
\\usepackage[singlespacing]{setspace}
    \\setlength{\\textheight}{8.25in}
    \\setlength{\\topmargin}{.5in}
    \\setlength{\\headsep}{0in}
    \\setlength{\\headheight}{0in}
    \\setlength{\\topskip}{0in}
    \\setlength{\\textwidth}{4.5in}
    \\setlength{\\oddsidemargin}{0.75in}
%    \\setlength\\evensidemargin{0.57in}
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont[Scale=1.35]{Linux Libertine O}
\\usepackage[it,small]{titlesec}
\\usepackage{graphicx}
\\renewcommand{\\thesection}{\\arabic{section}.}
\\usepackage{sectsty}
\\usepackage[normalem]{ulem}
\\target{\\magstepminus1}{\\paperheight}{\\paperwidth}
\\subsectionfont{\\mdseries\\itshape}
\\subsubsectionfont{\\mdseries\\itshape}
\\sectionfont{\\centering\\fontspec[Scale=1.15]{Lato}}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-latex-default-packages-alist
   '(("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("" "titletoc" nil nil)
     ("" "hyperref" nil nil)))
 '(org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
 '(org-link-abbrev-alist '(("gmail" . "https://mail.google.com/mail/u/0/#all/%s")))
 '(org-link-email-description-format "%s (%c)")
 '(org-list-allow-alphabetical t)
 '(org-log-done-with-time nil)
 '(org-log-into-drawer t)
 '(org-log-repeat nil)
 '(org-mode-hook
   '(org-password-manager-key-bindings org-tempo-setup
				       #[0 "\300\301\302\303\304$\207"
					   [add-hook before-save-hook org-encrypt-entries nil t]
					   5]
				       org-variable-pitch-minor-mode emojify-mode prettify-symbols-mode org-fancy-priorities-mode org-clock-load org-autolist-mode org-superstar-mode visual-line-mode org-babel-result-hide-spec org-babel-hide-all-hashes))
 '(org-odd-levels-only t)
 '(org-odt-preferred-output-format "docx")
 '(org-outline-path-complete-in-steps t)
 '(org-pandoc-menu-entry
   '((52 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (36 "as html5." org-pandoc-export-as-html5)
     (53 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
     (37 "to html5-pdf." org-pandoc-export-to-html5-pdf)
     (61 "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
     (45 "to ms-pdf." org-pandoc-export-to-ms-pdf)
     (104 "to html4 and open." org-pandoc-export-to-html4-and-open)
     (72 "as html4." org-pandoc-export-as-html4)
     (108 "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
     (76 "to latex-pdf." org-pandoc-export-to-latex-pdf)
     (111 "to odt and open." org-pandoc-export-to-odt-and-open)
     (79 "to odt." org-pandoc-export-to-odt)
     (114 "to rtf and open." org-pandoc-export-to-rtf-and-open)
     (82 "as rtf." org-pandoc-export-as-rtf)
     (120 "to docx and open." org-pandoc-export-to-docx-and-open)
     (88 "to docx." org-pandoc-export-to-docx)))
 '(org-password-manager-default-pwgen-command "pwgen --secure --symbols --capitalize --numerals 12 1")
 '(org-password-manager-scope '("~/Dropbox/Org_other/passwords.org.gpg"))
 '(org-pretty-tags-global-mode t)
 '(org-pretty-tags-surrogate-images
   '(("@EMAIL" . "/home/dan/Dropbox/Stuff/email.png")
     ("@HOME" . "/home/dan/Dropbox/Stuff/home.png")))
 '(org-pretty-tags-surrogate-strings
   '(("imp" . "☆")
     ("idea" . "💡")
     ("money" . "$$$")
     ("easy" . "₰")
     ("music" . "♬")
     ("open" . "🌈")
     ("clsd" . "🚪")
     ("deep" . "🌊")
     ("shlw" . "🏖")
     ("OFFICE" . "🏢")
     ("notstuck" . "∉𝔖")
     ("CAR" . "🚗")
     ("PHONE" . "📞")
     ("NET" . "🖧")
     ("TEXT" . "💬")
     ("EMAIL" . "✉")
     ("HOME" . "🏠")))
 '(org-priority-faces '((67 . "tan")))
 '(org-refile-targets
   '((org-agenda-files :todo . "ACTIVE")
     (org-agenda-files :todo . "CURRENT")
     (nil :todo . "NOW")
     (nil :todo . "MUL")))
 '(org-refile-use-outline-path t)
 '(org-return-follows-link t)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :unnarrowed t :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
#+date: %U
"))
     ("c" "Conversation" plain "%?" :jump-to-captured t :unnarrowed t :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Conversation with ${title}
#+date: %U
* Context

* Main Points
- "))))
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "%[~/Dropbox/Org_other/jtemplate_dailies.org]" :if-new
      (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d %A>
#+filetags: dailies
"))))
 '(org-show-context-detail
   '((agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . local)))
 '(org-show-notification-handler "xcowsay")
 '(org-special-ctrl-a/e t)
 '(org-speed-commands
   '(("Outline Navigation")
     ("n" org-speed-move-safe 'org-next-visible-heading)
     ("p" org-speed-move-safe 'org-previous-visible-heading)
     ("f" org-speed-move-safe 'org-forward-heading-same-level)
     ("b" org-speed-move-safe 'org-backward-heading-same-level)
     ("F" . org-next-block)
     ("B" . org-previous-block)
     ("u" org-speed-move-safe 'outline-up-heading)
     ("j" . org-goto)
     ("g" org-refile
      '(4))
     ("Outline Visibility")
     ("c" . org-cycle)
     ("C" . org-shifttab)
     (" " . org-display-outline-path)
     ("s" . org-toggle-narrow-to-subtree)
     ("k" . org-cut-subtree)
     ("=" . org-columns)
     ("Outline Structure Editing")
     ("U" . org-metaup)
     ("D" . org-metadown)
     ("r" . org-metaright)
     ("l" . org-metaleft)
     ("R" . org-shiftmetaright)
     ("L" . org-shiftmetaleft)
     ("i" progn
      (forward-char 1)
      (call-interactively 'org-insert-heading-respect-content))
     ("^" . org-sort)
     ("w" . org-refile)
     ("a" . org-archive-subtree-default-with-confirmation)
     ("@" . org-mark-subtree)
     ("#" . org-toggle-comment)
     ("Clock Commands")
     ("I" . org-clock-in)
     ("O" . org-clock-out)
     ("Meta Data Editing")
     ("t" . org-todo)
     ("d" org-todo "DONE")
     ("," org-priority)
     ("0" org-priority 32)
     ("1" org-priority 65)
     ("2" org-priority 66)
     ("3" org-priority 67)
     (":" . org-set-tags-command)
     ("e" . org-set-effort)
     ("E" . org-inc-effort)
     ("W" lambda
      (m)
      (interactive "sMinutes before warning: ")
      (org-entry-put
       (point)
       "APPT_WARNTIME" m))
     ("Agenda Views etc")
     ("v" . org-agenda)
     ("/" . org-sparse-tree)
     ("Misc")
     ("o" . org-open-at-point)
     ("?" . org-speed-command-help)
     ("<" org-agenda-set-restriction-lock 'subtree)
     (">" org-agenda-remove-restriction-lock)))
 '(org-speed-commands-user '(("d" org-todo "DONE")))
 '(org-src-block-faces 'nil)
 '(org-startup-folded t)
 '(org-startup-indented t)
 '(org-stuck-projects '("/+CURRENT" ("NEXT" "WAIT") ("persist") ""))
 '(org-superstar-item-bullet-alist '((42 . 10038) (43 . 10148) (45 . 8226)))
 '(org-tag-alist
   '((:startgroup . "Location  ")
     ("OFFICE" . 79)
     ("HOME" . 104)
     ("CAR" . 67)
     (:endgroup)
     (:startgroup . "Tool      ")
     ("EMAIL" . 101)
     ("PHONE" . 102)
     ("TEXT" . 116)
     ("NET" . 110)
     (:endgroup)
     (:startgroup . "Attention ")
     ("shlw" . 115)
     ("deep" . 100)
     (:endgroup)
     (:startgroup . "Creativity")
     ("open" . 111)
     ("clsd" . 99)
     (:endgroup)))
 '(org-tag-faces nil)
 '(org-tags-exclude-from-inheritance '("crypt"))
 '(org-tags-match-list-sublevels t)
 '(org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 '(org-time-stamp-rounding-minutes '(5 5))
 '(org-todo-keywords
   '((sequence "MAYB(m)" "NEXT(n)" "|" "WAIT(w!)" "DONE(d!)" "DROP(D@)")
     (sequence "PERHAPS(p)" "BREWING(b)" "CURRENT(c!)" "|" "WRAPPED(r!)" "CANCELED(x@)")
     (sequence "MUL(M)" "NOW(N!)" "|" "END(E!)" "ZAP(Z@)")
     (sequence "PONDER(P)" "ACTIVE(a)" "|" "FINISH(f!)")))
 '(org-todo-repeat-to-state "NEXT")
 '(org-use-property-inheritance '("worklog"))
 '(org-use-speed-commands
   (lambda nil
     (and
      (looking-at org-outline-regexp)
      (looking-back "^**"))))
 '(org-variable-pitch-fixed-faces
   '(org-block org-block-begin-line org-block-end-line org-code org-document-info-keyword org-done org-formula org-indent org-meta-line org-special-keyword org-table org-todo org-verbatim org-date org-drawer org-tag))
 '(outline-minor-mode-hook nil)
 '(outlined-elisp-startup-folded nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-hidden-regexps
   '("\\`names" "\\`guess-language" "\\`excorporate" "\\`eglot" "\\`org-readme" "\\`show-marks"))
 '(package-selected-packages
   '(magit annotate org org-roam-ui all-the-icons-dired elegant-agenda-mode simple-httpd websocket deft emmet-mode web-mode org-roam undersea-theme helpful holy-books sudo-utils default-font-presets solo-jazz-theme org-superstar i3wm-config-mode journalctl-mode which-key writeroom-mode sudo-edit pdf-tools celestial-mode-line rainbow-mode solarized-theme lavenderless-theme flucui-themes olivetti org-fancy-priorities info-colors all-the-icons diredfl org-variable-pitch org-pretty-tags hydra mode-icons nord-theme smartparens auctex use-package dired-hacks-utils dired-open poet-theme lab-themes ghub persistent-scratch org2blog ox-pandoc powerthesaurus rebecca-theme smex intellij-theme bind-key csv-mode emojify ido-completing-read+ mixed-pitch org-autolist org-password-manager seq flatui-dark-theme flatui-theme midi-kbd dired-icon dired-toggle-sudo interleave ido-vertical-mode gnuplot dictionary diminish))
 '(paperless-capture-directory "~/Downloads")
 '(paperless-root-directory "~/Documents")
 '(paradox-automatically-star t)
 '(pdf-misc-print-program "/usr/bin/evince" t)
 '(pdf-view-midnight-colors '("#232629" . "#f8f8f2"))
 '(persistent-scratch-what-to-save '(major-mode))
 '(pos-tip-background-color "#184956")
 '(pos-tip-foreground-color "#cad8d9")
 '(powerline-default-separator 'utf-8)
 '(preview-auto-cache-preamble t)
 '(preview-default-option-list
   '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
 '(recentf-exclude
   '("/home/dan/diary" "newsrc" "diary.ics" "/home/dan/.bbdb" "nntp" ".*\\.log" "/tmp/.*" ".*~" "Dropbox/GTD/*" "personal.org" "HC/.*\\.tex" "elpa"))
 '(recentf-max-menu-items 25)
 '(recentf-max-saved-items 200)
 '(recentf-menu-before "New File...")
 '(reftex-cite-format
   '((13 . "\\cite[]{%l}")
     (116 . "\\textcite{%l}")
     (97 . "\\autocite[]{%l}")
     (112 . "\\parencite{%l}")
     (102 . "\\footcite[][]{%l}")
     (70 . "\\fullcite[]{%l}")
     (120 . "[]{%l}")
     (88 . "{%l}")))
 '(reftex-cite-prompt-optional-args nil)
 '(reftex-default-bibliography '("~/Wp/Scholarship/big.bib"))
 '(safe-local-variable-values
   '((org-odd-levels-only)
     (org-latex-image-default-width . "\\linewidth")
     (org-latex-subtitle-format . "\\vspace{48pt} \\newline \\normalfont \\small %s")
     (eval variable-pitch-mode nil)
     (org-time-stamp-custom-formats "%B %e, %Y" . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %e, %Y>" . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%b %d, %y>" . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %-e, %y>" . "<%m/%d/%y %a %H:%M>")
     (org-time-stamp-custom-formats "<%B %e, %y>" . "<%m/%d/%y %a %H:%M>")
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (org-babel-tangle))
	   nil t)
     (eval org-content 2)
     (org-latex-hyperref-template . "")
     (org-odt-preferred-output-format . doc)
     (org-latex-with-hyperref)
     (org-latex-hyperref-template)
     (org-todo-keyword-faces
      ("INPROCESS" . "dark orange"))
     (org-image-actual-width 800)
     (org-time-stamp-custom-formats "%b %d" . "<%m/%d/%y %a %H:%M>")
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
     (TeX-master quote shared)))
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(sentence-end-double-space nil)
 '(shadow-noquery t)
 '(show-paren-mode t nil (paren))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#75b938" "#184956" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(solarized-distinct-fringe-background t)
 '(solarized-scale-org-headlines nil)
 '(solarized-use-variable-pitch nil)
 '(sort-fold-case t t)
 '(tab-always-indent 'complete)
 '(tail-hide-delay 3)
 '(tail-volatile nil)
 '(term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
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
     ("M-DEL" . dmg-term-send-backward-kill-word)))
 '(term-default-bg-color "#103c48")
 '(term-default-fg-color "#adbcbc")
 '(tex-alt-dvi-print-command "lp")
 '(tex-dvi-print-command "lp")
 '(time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %s")
 '(tls-checktrust t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-default-method "scp")
 '(tramp-default-method-alist
   '(("" "%" "smb")
     ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")
     ("\\`ftp\\." "" "ftp")
     ("\\`localhost\\'" "\\`root\\'" "su")
     ("home.rochester.rr.com" "" "ftp")
     ("isp-direct.com" "" "ftp")))
 '(tramp-syntax 'default nil (tramp))
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(user-full-name "Daniel M. Griswold")
 '(user-mail-address "kc5gmr@gmail.com")
 '(vc-annotate-background "#04c4c7")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#778ca3")
     (40 . "#00afef")
     (60 . "#778ca3")
     (80 . "#778ca3")
     (100 . "#778ca3")
     (120 . "#009c9f")
     (140 . "#778ca3")
     (160 . "#778ca3")
     (180 . "#778ca3")
     (200 . "#778ca3")
     (220 . "#009c9f")
     (240 . "#005cc5")
     (260 . "#fa1090")
     (280 . "#778ca3")
     (300 . "#005cc5")
     (320 . "#778ca3")
     (340 . "#009c9f")
     (360 . "#778ca3")))
 '(vc-annotate-very-old-color "#778ca3")
 '(version-control t)
 '(visible-bell t)
 '(visual-line-fringe-indicators '(nil nil))
 '(w3m-default-display-inline-images t)
 '(w3m-use-cookies t)
 '(weechat-color-list
   '(unspecified "#103c48" "#184956" "#b42e2a" "#fa5750" "#489615" "#75b938" "#ac8a0c" "#dbb32d" "#0068bb" "#4695f7" "#b3478d" "#f275be" "#00a195" "#41c7b9" "#adbcbc" "#103c48"))
 '(wttrin-default-cities '("Zeeland"))
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :slant normal :weight normal :height 158 :width normal))))
 '(bookmark-face ((t (:background "gray29" :foreground "white"))))
 '(fixed-pitch ((t (:family "Fantasque Sans Mono"))))
 '(muse-link-face ((t (:foreground "blue" :underline "red" :weight bold))) t)
 '(org-indent ((t (:inherit (org-hide fixed-pitch))))))

