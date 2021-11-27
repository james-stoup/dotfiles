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
;; MODELINE
;;-------------------------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;;-------------------------------------------------------------------------------------------
;; MODELINE
;;-------------------------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-buffer-file-name-style 'file-name) 

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

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)


;;-------------------------------------------------------------------------------------------
;; CUSTOM
;;-------------------------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


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

                    (org-agenda-overriding-header "ALL normal priority tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority Unfinished tasks:")))          
          )
         ((org-agenda-compact-blocks nil)))))

;; Agenda View "g"
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("gp" "Planning" tags-todo "planning")
        ("gb" "Backend" tags-todo "backend")
        ("G" "GTD Block Agenda"
         ((todo "IN-PROGRESS")
          )
         ((org-agenda-prefix-format "[ ] %T: ")
          (org-agenda-with-colors t)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
          (ps-landscape-mode t))
         ;;nil                      ;; i.e., no local settings
         ("~/next-actions.txt"))
        )
      )
>>>>>>> Stashed changes
;; End Agenda hacks

;; Must do this so org knows where to look
(setq org-agenda-files '("~/org"))

(setq org-log-done t)

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "|" "DONE(d!)" "OBE(o!)" )))

;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@short_term" . ?s) ("@long_term" . ?l) ("@immediate" . ?i) 
                      (:endgroup . nil)
                      ("maggie" . ?m) ("james" . ?j) ("house" . ?h) ("kids" . ?k) ("wood-working" . ?w) ("code" . ?c)))

;; All org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)



;; journal settings
(setq org-capture-templates
      '(    
        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %?\n:Created: %T\n%i\n%a\nResolution: "
         :empty-lines 1)

        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n\nResolution: "
         :empty-lines 1)
        
        ("j" "Journal Entry"
         entry (file+datetree "~/org/journal.org")
         "* %?"
         :empty-lines 0)
        
        ("m" "Meeting"
         entry (file "~/org/meetings.org")
         "* %? %^g\n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n- [ ]"         
         :empty-lines 1)

        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 1)

        ("t" "TO DO"
         entry (file "~/org/todos.org" )
         "* TODO %? %^G\nCreated: %T\n** Details"
         :empty-lines 1)

        ("p" "Projects"
         entry (file "~/org/projects.org" )
         "* TODO %?\nCreated: %T\n** Details"
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


(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
