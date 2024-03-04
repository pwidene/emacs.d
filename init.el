;;
;;  Eat a good breakfast
;;

;; Get site-and-host-specific customizations
(let ((sitestart (concat user-emacs-directory (convert-standard-filename "site-start.el"))))
  (if (file-exists-p sitestart)
      (load-file sitestart)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

;; ;;;
;; ;;; start package system
;; ;;;
;; (require 'package)

;; (customize-set-variable 'package-enable-at-startup nil)
;; (customize-set-variable 'package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;; 					    ("melpa" . "http://melpa.org/packages/")
;; 					    ))
;; (package-initialize)

;; ;; Configure the package system
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package))

;;
;; Configure straight.el package management
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'diminish)
(straight-use-package 'bind-key)
;;(straight-use-package 'use-package-core)

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package system-packages
  :custom
  (system-packages-noconfirm t))


;;
;;  Emacs-server
;;
;;;; the following lets you kill emacsclient buffers with C-x k instead of C-x #
;; (add-hook 'server-switch-hook
;; 	  (lambda ()
;; 	    (when (current-local-map)
;; 	      (use-local-map (copy-keymap (current-local-map))))
;; 	    (when server-buffer-clients
;; 	      (local-set-key (kbd "C-x k") 'server-edit))))

;; so we can talk about the OS
(use-package sysinfo
  :straight (:host github :repo "p-kolacz/sysinfo")
  :config
  (defconst platform-linux-p
    (eq sysinfo-os-type 'Linux)
    "Are we running on Linux?")
  (defconst platform-macos-p
    (eq sysinfo-os-family 'macOS)
    "Are we running on macOS?")
  (defconst platform-wsl-p
    (eq sysinfo-os-type 'WSL)
    "Are we running under WSL?")
  (defconst platform-cygwin-p
    (eq sysinfo-os-type 'cygwin)
    "Are we running under cygwin?")
  (defconst platform-linux-x-p
    (and
     platform-linux-p
     (eq window-system 'x))
    "Are we running X Window System on Linux?")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some useful constants
;; init that should precede most packages
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (if (display-graphic-p)
		(unless (server-running-p)
		  (server-start)))))
(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 scroll-step 1
 auto-window-vscroll nil
 ;;
 ;; fix for weird emacs 28.2 / Apple compiler issue
 native-comp-driver-options (when (eq system-type 'darwin) '("-Wl,-w"))
 
 )
(setq-default cursor-type 'bar)
;; faces / fonts
(when (display-graphic-p)
  (setq pmw/default-font "Source Code Pro")
  (when platform-macos-p
    (setq pmw/variable-pitch-font "Trebuchet MS"))
  (when platform-linux-x-p
    (setq pmw/variable-pitch-font "DejaVu Sans"))
  
  (set-face-attribute 'default nil
		      :family pmw/default-font
		      :height (if (<= (display-pixel-width) 2000) 100 120)
		      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
		      :family (when (member pmw/variable-pitch-font (font-family-list)) pmw/variable-pitch-font)
		      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
		      :family pmw/default-font
		      :weight 'regular)

  (use-package sublime-themes )
  (use-package cyberpunk-theme )
  (use-package color-theme-modern 
    :config
    (load-theme 'cyberpunk t)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Everything from here on should be some form of use-package invocation
;;;

(use-package cursor-chg
  :config
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1)
  (curchg-change-cursor-when-idle-interval 5)
  )
  

(use-package saveplace
  
  :config
  (save-place-mode 1)
  )

(use-package vc
  :custom
  (vc-follow-symlinks t)
  )

(use-package rainbow-mode
  :custom
  (rainbow-x-colors nil)
  :hook prog-mode
  )

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  )

(use-package tex
  :straight auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil)
  :hook (LaTeX-mode . (lambda ()
			(auto-fill-mode)
			'turn-on-reftex
			(visual-line-mode)
			(set-fill-column 105)))
  
  )

(use-package auctex-latexmk
  :requires auctex
  )


(use-package f )
(use-package popup )
  
(use-package autofit-frame
  :custom
  (autofit-frames-flag nil)
  :config
  (add-hook 'after-make-frame-functions 'fit-frame)
  )

(use-package hexrgb
  :if (display-graphic-p)
  )

(use-package multiple-cursors )

;;(use-package counsel )
;;(use-package swiper )
(use-package ivy
  :disabled t
  :after (counsel swiper)
  
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (ivy-wrap t)
  (ivy-extra-directories nil)
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep))
  :config
  (ivy-set-actions
   `counsel-find-file
   `(("j" find-file-other-frame "other frame")
     ("d" delete-file "delete")
     ("r" counsel-find-file-as-root "open as root")))
  (ivy-set-actions
   `ivy-switch-buffer
   `(("j" switch-to-buffer-other-frame "other frame")
     ("k" kill-buffer "kill")
     ("r" ivy--rename-buffer-action "rename")))
  (ivy-mode 1)
  )


(use-package ivy-posframe
  :disabled t
  :after (ivy counsel swiper)
  
  :custom
  ;; (setq ivy-posframe-display-functions-alist `((t . ivy-posframe-display))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  :config
  (ivy-posframe-mode 1)
  )

(use-package vertico)
(use-package vertico-posframe
  :after (vertico posframe)
  :custom
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :config
  (vertico-posframe-mode 1)
  )
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (matching-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )
(use-package embark)
(use-package marginalia)

(use-package spacious-padding
  :custom
  (spacious-padding-subtle-mode-line
   `( :mode-line-active 'default
      :mode-line-inactive vertical-border))
  ;; these are default values from the package documentation
  (spacious-padding-widths
   `( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 4
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8))
  :config
  (spacious-padding-mode 1)
  )


;; minimap
(use-package minimap
  :custom
  (minimap-window-location 'right)
  )

(use-package all-the-icons
  :if (display-graphic-p)
  )

(use-package centaur-tabs
  :disabled t
  :demand
  
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward))
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  (centaur-tabs-height 24)
  (centaur-tabs-set-modified-marker t)
  :config
  (centaur-tabs-mode t)
  ;;  (centaur-tabs-headline-match)
  ;;  (centaur-tabs-change-fonts "arial" 120)
  (centaur-tabs-group-by-projectile-project)
  )


(use-package smartparens
  
  :config
  ;;;(smartparens-global-mode f)
  )

(use-package electric-case
  :disabled t
  )
(use-package electric-operator
  :disabled t
  )

;; modeline / line number stuff
(use-package telephone-line
  
  :config
  (telephone-line-mode 1)
  )

(use-package gnuplot-mode
  :mode 
  ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)
  )

;; dired+ / bookmark+ / desktop
(use-package dired+
  :custom
  (diredp-hide-details-initially-flag nil)
  )

(use-package bookmark+
  :disabled t
  :custom
  (bmkp-desktop-jump-save-before-flag t)
  :config
  (add-hook 'kill-emacs (progn
			  (bmkp-desktop-save-as-last)))
  )


;;
;;  magit
;;
(use-package magit
  :bind
  ("C-c g" . magit-status)
  :config
  (add-hook 'magit-mode #'magit-load-config-extensions)
  )

(use-package git-messenger )
(use-package git-gutter
  
  :custom
  (git-gutter:added-sign "+ ")
  (git-gutter:modified-sign "* ")
  (git-gutter:deleted-sign "- ")
  (git-gutter:lighter " GG")
  :config
  (global-git-gutter-mode 1)
  (git-gutter:linum-setup)
  )

(use-package nameframe
  :disabled t
  )


(use-package persp-mode
  :custom
  (persp-autokill-buffer-on-remove 'kill-weak)
  (persp-state-default-file (concat user-emacs-directory (convert-standard-filename ".emacs-perspective-save")))
  :hook
  (window-setup-hook . (lambda () (persp-mode 1)))
  ;(kill-emacs . persp-state-save)
)  



(use-package shell-pop
  
  :custom
  (shell-pop-term-shell "/opt/local/bin/zsh")
  (shell-pop-full-span t)
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-universal-key "C-t")
  )
   
  
  
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  )

(use-package treemacs-persp
  :after treemacs persp-mode
  
  :config (treemacs-set-scope-type 'Perspectives))

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode))
  :init
  (defun cmake-rename-buffer ()
    "Renames a CMakeLists.txt buffer to cmake-<directory name>."
    (interactive)
					;(print (concat "buffer-filename = " (buffer-file-name)))
					;(print (concat "buffer-name     = " (buffer-name)))
    (when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
					;(setq file-name (file-name-nondirectory (buffer-file-name)))
      (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
					;(print (concat "parent-dir = " parent-dir))
      (setq new-buffer-name (concat "cmake-" parent-dir))
					;(print (concat "new-buffer-name= " new-buffer-name))
      (rename-buffer new-buffer-name t)
      )
    )
  :hook (cmake-mode . cmake-rename-buffer)
  )

(use-package easy-jekyll
  
  :custom
  (easy-jekyll-basedir "~/Documents/pwidene.github.io/")
  (easy-jekyll-url "https://pwidene.github.io")
  (easy-jekyll-image-directory "assets")
  (easy-jekyll-default-picture-directory "~/Desktop")
  :bind
  ("C-c C-e" . easy-jekyll)
  )

  
(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config
  (show-smartparens-mode 1)
  :hook
  (c-mode-common . display-line-numbers-mode)
  )

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))
(use-package js2-mode
  :mode "\\.js\\'")
(use-package rjsx-mode :mode "\\.jsx\\'")
(use-package json-mode
  :mode "\\.json\\'")
(use-package plantuml-mode
  :mode "\\.uml\\'")
(use-package yaml-mode
  :mode "\\.yaml\\'")
(use-package haskell-mode
  :mode "\\.hs\\'")
(use-package web-mode
  :mode ("\\.\\(html\\|htm\\)\\'" . wevb-mode))

(use-package deft
  :disabled t
  :custom
  (deft-directory "~/Documents/repo/notes")
  (deft-use-file-name-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-extension "org")
  (deft-default-extension "org")
  (deft-text-mode 'org-mode)
  )



(use-package epa-file
  :straight (:type built-in)
  :config
  (epa-file-enable)
  )

(use-package org-bullets
  :disabled f
  :after org
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  )

(use-package org-super-agenda
:disabled f
  :custom
  (org-super-agenda-groups
   '(
     (:name "Radar"
	    :todo ("ACTIVITY" "RADAR")
	    :deadline nil)
     (:name "Deadlines"
	    :deadline t)
     (:name "Undated"
	    :todo ("TODO")
	    :deadline nil)
     )))
			   
(use-package org
  :disabled f
  :after (epa-file org-super-agenda)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :custom
  (org-directory "~/.org")
  (org-agenda-files (quote ("~/.org/journal.org")))
  (org-agenda-show-all-dates nil)
  (org-agenda-prefix-format '((agenda . " %i %?-12t% s")
			      (todo . " %?-12t% s")
			      (tags . " %i %-12:c")
			      (search . " %i %-12:c")))
  (org-crypt-key nil)
  (org-hide-emphasis-markers t)
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-indirect-buffer-display 'current-window)
  (org-log-into-drawer t)
  ;; set ditaa JAR path to work with org/babel (http://sourceforge.net/projects/ditaa/)
  (org-ditaa-jar-path "/opt/local/share/java/ditaa0_9.jar")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  (org-todo-keywords '((sequence "TODO" "|" "DONE")
		       (sequence "ACTIVITY" "PAUSED" "|" "COMPLETED" "ABANDONED")))
  (org-tag-alist '(("abet" . ?a)
		   ("h4s" . ?h)
		   ("pmem" . ?p)
		   ("sandreport" . ?s)
		   ("storage" . nil)
		   ("hpda" . nil)
		   ("rvma" . nil)
		   ("sampra" . nil)
		   ("data-mgt" . nil)
		   ("faodel" . ?f)
		   ("recruiting" . nil)))
  (org-capture-templates
   '(("t" "New TODO" entry (file+olp+datetree "journal.org")
      "* TODO %? %^g\n  %i\n  %a")
     ("j" "New journal" entry (file+olp+datetree "journal.org")
      "* %? %^g\n dated %t\n  %i\n  %a")))
  (org-agenda-custom-commands
   '(("P" "Agenda and radars"
      ((agenda ""
       ((org-agenda-span 15)))
       (todo "ACTIVITY")
       ))))
  ;; invoking this externally with
  ;;   /opt/local/bin/emacs -batch -l ~/.emacs.d/init.el -Q -eval '(org-batch-agenda "P")'
  :init
  (add-to-list 'org-export-backends 'md)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (gnuplot . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (ruby . t)
     (python . t)
     (emacs-lisp . t)
     (plantuml . t)
     ))
  ;;
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-mode . org-super-agenda-mode)
  :custom-face
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info ((t (:foreground "dark orange"))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-document-title ((t (:inherit variable-pitch :height 2.0 :underline nil))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-level-1 ((t (:inherit variable-pitch :height 1.5))))
  (org-level-2 ((t (:inherit variable-pitch :height 1.25))))
  (org-level-3 ((t (:inherit variable-pitch :height 1.1))))
  (org-level-4 ((t (:inherit variable-pitch :height 1.05))))
  (org-link ((t (:foreground "royal blue" :underline t))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-value ((t (:inherit fixed-pitch :underline t))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  )

(use-package org-roam
  :disabled t
  :hook
  ((after-init . org-roam-setup)
   (org-roam-backlinks-mode . visual-line-mode))
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.org/")
  (org-roam-tag-sources '(prop all-directories))
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-ui
  :disabled t
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :requires org-mode
  ;;:hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  )

(use-package org-roam-server
  :disabled t
  :custom
  (org-roam-server-host "127.0.0.1")
  (org-roam-server-port 8080)
  (org-roam-server-authenticate nil)
  (org-roam-server-export-inline-images t)
  (org-roam-server-serve-files nil)
  (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
  (org-roam-server-network-poll t)
  (org-roam-server-network-arrows nil)
  (org-roam-server-network-label-truncate t)
  (org-roam-server-network-label-truncate-length 60)
  (org-roam-server-network-label-wrap-length 20)
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))


(defvar org-journal--date-location-scheduled-time nil)

(defun org-journal-date-location (&optional scheduled-time)
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
    (setq org-journal--date-location-scheduled-time scheduled-time)
    (org-journal-new-entry t (org-time-string-to-time scheduled-time))
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max))))

(use-package org-journal
  :disabled f
  :custom
  (org-icalendar-store-UID t)
  (org-icalendar-include-todo "all")
  (org-icalendar-combined-agenda-file "~/org-journal.ics")
  )

(use-package tablist)

(use-package pdf-tools
  :after (tablist)
  :config
  (pdf-loader-install)
  )

(use-package biblio
  :custom
  (biblio-crossref-user-email-address "widenerpm@ornl.gov")
  )
(use-package citar
  :custom
  (citar-bibliography '("~/work/bib/references.bib" "~/Documents/cv/pubs/pubs.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  )
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  )
(use-package org-noter)

(use-package htmlize
  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-htmlize-font-prefix "org-"))

(use-package autoinsert
  :hook (find-file . auto-insert)
  )

(use-package yasnippet
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  :config
  (yas-reload-all)
  :hook
  (prog-mode  . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  )

(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :config
  (recentf-mode 1)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))


(use-package gist)

(use-package mwheel
  :straight (:type built-in)
  :custom
  (mouse-wheel-scroll-amount '(1
			       ((shift) . 5)
			       ((control))))
  (mouse-wheel-progressive-speed nil))

(use-package pixel-scroll
  :straight (:type built-in)
  :config
  (pixel-scroll-mode))

(use-package cus-edit
  :straight (:type built-in)
  :custom
  (custom-file (concat user-emacs-directory (convert-standard-filename "custom.el")))
  :config
  (load-file custom-file)
  )

(use-package apples-mode
  :straight (:host github :repo "rprimus/apples-mode")
  )



;;
;; end of init.el
;;

