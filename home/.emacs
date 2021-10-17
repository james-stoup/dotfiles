(prefer-coding-system 'utf-8)

;;-------------------------------------------------------------------------------------------
;; MELPA & GNU
;;-------------------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)



;;-------------------------------------------------------------------------------------------
;; SYSTEM SETUP
;;-------------------------------------------------------------------------------------------
;;;; Bootstrap use-package system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(setq tramp-default-method "ssh")

(use-package paredit
  :ensure t)

;;-------------------------------------------------------------------------------------------
;; HELM
;;-------------------------------------------------------------------------------------------
(use-package helm
  :ensure t
  :after (company)
  
  :bind (("M-x" . helm-M-x)
         :map ac-complete-mode-map
         ("C-:" . ac-complete-with-helm)
         :map company-mode-map
         ("C-:" . helm-company)
         :map company-active-map
         ("C-:" . helm-company)
         )
  )


;;-------------------------------------------------------------------------------------------
;; PROJECTILE
;;-------------------------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  )


;;-------------------------------------------------------------------------------------------
;; HELM
;;-------------------------------------------------------------------------------------------
(use-package helm-projectile
  :ensure t)

(use-package ac-helm
  :ensure t)


;;-------------------------------------------------------------------------------------------
;; SEEING IS BELIEVING
;;-------------------------------------------------------------------------------------------
(use-package seeing-is-believing
  :ensure t)


;;-------------------------------------------------------------------------------------------
;; RUBY
;;-------------------------------------------------------------------------------------------
(use-package ruby-test-mode
  :ensure t)

(use-package ac-inf-ruby
  :ensure t)


;;-------------------------------------------------------------------------------------------
;; DEFAULTS 
;;-------------------------------------------------------------------------------------------
;; better defaults
(require 'better-defaults)
(menu-bar-mode t) 

(setq column-number-mode t
      initial-scratch-message nil
      inhibit-startup-screen t
      visible-bell t
      show-paren-mode 1)

;; show line numbers
(global-linum-mode)

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

;; Symbol highlighting
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(require 'highlight-parentheses)

(global-set-key (kbd "C-?") 'flymake-show-diagnostics-buffer)

;; Auto correct spelling mistakes
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; this doesn't want for ruby so doing it manually
(add-hook 'ruby-mode-hook #'flyspell-prog-mode)
;;(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)



;;-------------------------------------------------------------------------------------------
;; CUSTOM
;;-------------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(beacon-color "#c82829")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (material-light)))
 '(custom-safe-themes
   (quote
    ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "66e417dfa3a329de0531c9fefa0410e43b350de1fc5bba73e5b7fb2e986d10c8" "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e" "bbaceb6cb7847bc84727128b1b5e514d5775b8d94d46846fe1c4bc37db9ed4a2" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "89885317e7136d4e86fb842605d47d8329320f0326b62efa236e63ed4be23c58" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "1bdc49116a77e52aaea740cd8e54b93e0bae6c0895dcc36d5c8d1a493e89c78d" "9b88b8c64dc30188514f19d1be732ee71cc905b04b0c2c7eb1194528fcebbea4" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "bf10bd6d21928bf87bc3032b498c62cb9d48c54c06d217c8b00bef8090e539f7" "0f1da577aee469f092b24417fe8c13d0b5b76c40c8e4484ff40c609d8da61f69" "9fcc7f1f4c90b6cd8507984c1628061d6c3f7cf0307777da25aa1ef4b11e1d91" "413ba24c4f8a0d187a43d69dc7cbfd8b1d8782739422ba2368eb5f0893f0642a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "aa6638f0cd2ba2c68be03220ea73495116dc6f0b625405ede34087c1babb71ae" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "387b487737860e18cbb92d83a42616a67c1edfd0664d521940e7fbf049c315ae" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-color "#1fb3b3")
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#14151E")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")))
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (0blayout acme-theme afternoon-theme ahungry-theme alect-themes ample-zen-theme apropospriate-theme auto-complete auto-complete-rst autumn-light-theme better-defaults blacken butler color-theme-sanityinc-tomorrow elpy enh-ruby-mode flycheck-plantuml flycheck-pycheckers format-all hemisu-theme hybrid-reverse-theme immaterial-theme importmagic indent-tools jedi jenkins jenkinsfile-mode magit material-theme pippel plantuml-mode projectile pyimpsort python-black python-docstring python-mode rbtagger rubocop rubocopfmt ruby-electric ruby-extra-highlight ruby-test-mode ruby-tools rufo seeing-is-believing soft-stone-theme sphinx-doc sphinx-mode twilight-anti-bright-theme twilight-bright-theme twilight-theme use-package which-key yaml-mode yaml-tomato yard-mode)))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(show-paren-mode t)
 '(tabbar-background-color "#ffffff")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))



;;-------------------------------------------------------------------------------------------
;; ORG MODE
;;-------------------------------------------------------------------------------------------
(require 'org)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;; Shortcuts
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Agenda hacks
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-start-day "-1d")

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "\n==================================================================\n\nHigh-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 3)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "\n==================================================================\n\nALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))
;; End Agenda hacks

;; Must do this so org knows where to look
(setq org-agenda-files '("~/org"))

(setq org-log-done t)

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "VERIFY(v!)" "|" "DONE(d!)" "OBE(o!)" "BLOCKED(b@)")))

;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@story" . ?s) ("@bug" . ?b) ("@task" . ?t) ("@subtask" . ?u)
                      (:endgroup . nil)
                      ("random" . ?r) ("HR" . ?h) ("backend" . ?k) ("frontend" . ?f) ("QA" . ?q) ("planning" . ?p)))

;; All org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)



;; journal settings
(setq org-capture-templates
      '(    
        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO %?\n %i\n %a"
         :empty-lines 1)

        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO %?\n"
         :empty-lines 1)
        
        ("j" "Work Journal Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)
        
        ("m" "Meeting"
         entry (file "~/org/meetings.org")
         "* %? %^g\n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n- [ ] %^G"         
         :empty-lines 1)
        
        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 1)

        ("t" "Ticket"
         entry (file "~/org/tickets.org" )
         "* TODO %?\nStarted: %T\n** Jira Link: \n** Notes\n** Status\n - [ ] Research\n - [ ] PR\n - [ ] Verify\n** Subtasks"
         :empty-lines 1)

        ))



;;-------------------------------------------------------------------------------------------
;; SOLARGRAPH
;;-------------------------------------------------------------------------------------------
(require 'lsp-mode)
(add-hook 'ruby-mode-hook #'lsp)
(global-set-key (kbd "C-c h h") 'lsp-describe-thing-at-point)

;; which-key integration with LSP
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Redefine the 'super' key to be "C-c C-c" for LSP
;;(setq lsp-keymap-prefix "C-c C-c")  ;; OLD WAY
(define-key lsp-mode-map (kbd "C-c C-c") lsp-command-map) ;; NEW WAY



;;-------------------------------------------------------------------------------------------
;; HELM
;;-------------------------------------------------------------------------------------------
;; (require 'ac-helm) ;; Not necessary if using ELPA package

;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-:") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;; ;;(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))



;;-------------------------------------------------------------------------------------------
;; PROJECTILE
;;-------------------------------------------------------------------------------------------
;; (require 'helm-projectile)
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



;;-------------------------------------------------------------------------------------------
;; RUBY
;;-------------------------------------------------------------------------------------------
(setq ruby-insert-encoding-magic-comment nil)
(setq package-list '(
		     better-defaults
                     ruby-electric
                     seeing-is-believing
		     rubocopfmt
		     ruby-test-mode
		     ))

(require 'rubocopfmt)
(require 'yaml-mode)
(require 'seeing-is-believing)
(require 'ruby-test-mode)

;; RVM
(rvm-use-default)
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

(add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;; Make sure yard-mode starts when ruby-mode does
(add-hook 'ruby-mode-hook #'yard-mode)
(add-hook 'ruby-mode-hook #'eldoc-mode)
(add-hook 'ruby-mode-hook #'which-key-mode)
(add-hook 'ruby-mode-hook #'fic-mode) ;; Highlights BUG, TODO, and FIXME

;; ruby hooks
;; (eval-after-load "ruby-mode"
;;   '(progn
;;      '(add-hook 'ruby-mode-hook 'ruby-electric-mode)         
;;      '(add-hook 'ruby-mode-hook 'ruby-extra-highlight-mode) 
;; ;;     '(add-hook 'ruby-mode-hook #'ruby-extra-highlight-mode) 
;;      '(add-hook 'ruby-mode-hook 'seeing-is-believing)
;;      '(add-hook 'ruby-mode-hook 'ruby-test-mode)
;;      )
;; )


(setq rubocopfmt-use-bundler-when-possible nil) ;; rubocop
(setq seeing-is-believing-prefix "C-.")         ;; SiB

;; yaml files
(add-to-list 'auto-mode-alist
	     '("\\.yml\\'" . yaml-mode))

;; treat ruby files that don't end in .rb like ruby anyway
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; INF
;;(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;;(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(global-set-key (kbd "C-c r r") 'inf-ruby)

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map
     (kbd "C-c C-s") 'inf-ruby-console-auto))

;; auto complete
(require 'ac-inf-ruby) ;; when not installed via package.el
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; auto complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-ignore-case nil)
;; (add-to-list 'ac-modes 'enh-ruby-mode)

;; Optionally bind auto-complete to TAB in inf-ruby buffers:
(eval-after-load 'inf-ruby '
  '(define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

;; syntax checking in ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Smart Parens
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))


;; Flyspell (spell check)
(require 'flyspell)
(setq flyspell-issue-message-flg nil)
(add-hook 'enh-ruby-mode-hook
          (lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
          (lambda () (flyspell-prog-mode)))
;; flyspell mode breaks auto-complete mode without this.
(ac-flyspell-workaround)

(require 'flyspell-correct-helm)
;;(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)


;; Custom definition to insert this stupid string literal comment
;; # frozen_string_literal: true
;; # Copyright © 2009-2021 by Metova Federal, LLC.  All rights reserved
(fset 'insert-metova-copyright
   (kmacro-lambda-form [escape ?< ?# ?  ?f ?r ?o ?z ?e ?n ?_ ?s ?t ?r ?i ?n ?g ?_ ?l ?i ?t ?e ?r ?a ?l ?: ?  ?t ?r ?u ?e return return ?# ?  ?C ?o ?p ?y ?r ?i ?g ?h ?t ?  ?© ?  ?2 ?0 ?0 ?9 ?- ?2 ?0 ?2 ?1 ?  ?b ?y ?  ?M ?e ?t ?o ?v ?a ?  ?F ?e ?d ?e ?r ?a ?l ?, ?  ?L ?L ?C ?. ?  ?A backspace ?  ?A ?l ?l ?  ?r ?i ?g ?h ?t ?s ?  ?r ?e ?s ?e ?r ?v ?e ?d return return] 0 "%d"))




;;-------------------------------------------------------------------------------------------
;; PYTHON 
;;-------------------------------------------------------------------------------------------
(add-hook 'python-mode-hook 'blacken-mode)
(require 'pyimpsort)
(eval-after-load 'python
  '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer))

;; Import Magic
(add-hook 'python-mode-hook 'importmagic-mode)

(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    ;;material-theme                  ;; Theme
    blacken                         ;; Black formatting on save
    magit                           ;; Git integration
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;; Enable elpy
;;(elpy-enable)
;;(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(elpy-enable)  
(setq elpy-rpc-backend "jedi")  
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; pipenv
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))


;;-------------------------------------------------------------------------------------------
;; PLANT UML
;;-------------------------------------------------------------------------------------------
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

;; Sample jar configuration
(setq plantuml-jar-path "/opt/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; Sample executable configuration
(setq plantuml-executable-path "/usr/local/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

;; (setq plantuml-java-args (list "-Djava.awt.headless=true" "-jar"))

;; (use-package plantuml-mode
;;   :init
;;     (setq plantuml-default-exec-mode 'jar)
;;     (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
;;     (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
;;     (setq org-startup-with-inline-images t)
;;     (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))



;;-------------------------------------------------------------------------------------------
;; Treemacs
;;-------------------------------------------------------------------------------------------
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-width                         40
          treemacs-width-is-initially-locked     nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)



;;-------------------------------------------------------------------------------------------
;; JAVA
;;-------------------------------------------------------------------------------------------



;;-------------------------------------------------------------------------------------------
;; GOLANG
;;-------------------------------------------------------------------------------------------



;;-------------------------------------------------------------------------------------------
;; install the missing packages
;;-------------------------------------------------------------------------------------------
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


