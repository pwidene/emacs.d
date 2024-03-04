
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(fixed-pitch ((t (:family "Hack"))))
 '(fringe ((t :background "#000000")))
 '(header-line ((t :box (:line-width 4 :color "#2b2b2b" :style nil))))
 '(header-line-highlight ((t :box (:color "#d3d3d3"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#000000")))
 '(mode-line ((t :background "#000000" :overline "#4c83ff" :box (:line-width 4 :color "#000000" :style nil))))
 '(mode-line-active ((t :background "#000000" :overline "#4c83ff" :box (:line-width 4 :color "#000000" :style nil))))
 '(mode-line-highlight ((t :box (:color "#d3d3d3"))))
 '(mode-line-inactive ((t :background "#000000" :overline "#333333" :box (:line-width 4 :color "#000000" :style nil))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit variable-pitch :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.5))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.25))))
 '(org-level-3 ((t (:inherit variable-pitch :height 1.1))))
 '(org-level-4 ((t (:inherit variable-pitch :height 1.05))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch :underline t))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(variable-pitch ((t (:family "Trebuchet MS"))))
 '(vertical-border ((t :background "#000000" :foreground "#000000")))
 '(window-divider ((t (:background "#000000" :foreground "#000000"))))
 '(window-divider-first-pixel ((t (:background "#000000" :foreground "#000000"))))
 '(window-divider-last-pixel ((t (:background "#000000" :foreground "#000000")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   '(("Latexmk" "/opt/local/bin/latexmk -pdf %t" TeX-run-command nil t)
     ("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
      (plain-tex-mode latex-mode doctex-mode context-mode texinfo-mode ams-tex-mode)
      :help "Run BibTeX")
     ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run Biber")
     ("Texindex" "texindex %s.??" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texindex")
     ("Texi2dvi" "%(PDF)texi2dvi %t" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texi2dvi or Texi2pdf")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx -o %(O?pdf) %d" TeX-run-dvipdfmx nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %(d-dir) %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeglossaries to create glossary file")
     ("Index" "makeindex %(O?idx)" TeX-run-index nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeindex to create index file")
     ("upMendex" "upmendex %(O?idx)" TeX-run-index t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(ispell-program-name "/opt/local/bin/aspell")
 '(org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(websocket json-navigator json-process-client json-reformat json-rpc json-snatcher jsonl jsonrpc rjsx-mode org-ql org-gcal org-super-agenda org-superstar org-sync org-sync-snippets org-tag-beautify gist transient org-roam-server zonokai-theme yasnippet-snippets yaml-mode web-mode use-package-ensure-system-package undo-tree underwater-theme ujelly-theme tronesque-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme telephone-line tangotango-theme tango-plus-theme sublime-themes subatomic-theme spaceline spacegray-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smex smartparens smart-mode-line shell-pop seti-theme rustic rust-playground rainbow-mode racer project-explorer plantuml-mode persp-projectile paradox org-roam org-journal org-bullets oldlace-theme obsidian-theme nlinum night-owl-theme niflheim-theme neotree nav naquadah-theme nameframe-projectile nameframe-perspective mustang-theme multiple-cursors monokai-theme monochrome-theme molokai-theme moe-theme minimap magit-svn lsp-rust light-soap-theme json-mode js2-mode ivy-posframe ir-black-theme inkpot-theme idomenu ido-vertical-mode ido-ubiquitous icicles htmlize highlight hexrgb heroku-theme hc-zenburn-theme haskell-mode gruvbox-theme gnuplot-mode gnuplot git-messenger git-gutter gcmh gandalf-theme flymake-rust flycheck-rust flx-ido flatui-theme flatland-theme firebelly-theme espresso-theme display-theme dired+ dimmer diminish-buffer diminish deft darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme counsel color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern color-theme-buffer-local color-theme-approximate cmake-mode clues-theme centaur-tabs cargo busybee-theme bubbleberry-theme bookmark+ bind-map bind-chord autofit-frame auto-complete auctex-latexmk apples-mode anzu anti-zenburn-theme ample-zen-theme ample-theme all-the-icons afternoon-theme))
 '(plantuml-default-exec-mode 'executable)
 '(plantuml-executable-path "/opt/local/bin/plantuml")
 '(safe-local-variable-values '((TeX-master . main.tex)))
 '(warning-suppress-types '((comp) (comp))))

		    
