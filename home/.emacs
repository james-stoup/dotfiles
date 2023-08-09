(prefer-coding-system 'utf-8)
(require 'package)

;;-------------------------------------------------------------------------------------------
;; MELPA & GNU
;;-------------------------------------------------------------------------------------------
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

(use-package helm-projectile
  :ensure t)

(use-package ac-helm
  :ensure t)

(use-package seeing-is-believing
  :ensure t)

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
;;(global-linum-mode)

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

(which-key-mode)
(comment-tags-mode)

;; Make things colorful
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Replace package manager with something better
(require 'paradox)
(paradox-enable)
(setq paradox-github-token "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")

;; Adding pretty icons
;;(mode-icons-mode) ;; don't like the icon for ruby, change that if I decide to keep this

;; Auto format on save
(format-all-mode)


;;-------------------------------------------------------------------------------------------
;; LOOK AND FEEL
;;-------------------------------------------------------------------------------------------




;;-------------------------------------------------------------------------------------------
;; CUSTOM
;;-------------------------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;-------------------------------------------------------------------------------------------
;; ELisp
;;-------------------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)


;;-------------------------------------------------------------------------------------------
;; ORG MODE
;;-------------------------------------------------------------------------------------------
(require 'org)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
(setq jiralib-url "https://cybercents.atlassian.net/")

;; Must do this so org knows where to look
(setq org-agenda-files '("~/org"))
;;(setq org-log-done t)
(setq org-log-done 'time)

(setq org-return-follows-link  t)

;; All org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap the change priority keys
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Shortcuts
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")



;;;; Agenda hacks ;;;;

;; Agenda View "d"
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

(setq org-agenda-skip-deadline-if-done t)

(setq org-agenda-custom-commands
      '(
        ;; Daily Agenda - most used
        ("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 7)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                   (air-org-skip-subtree-if-priority ?C)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
          )
         ((org-agenda-compact-blocks nil)))

        ;; Weekly Review - look at the week
        ("w" "Weekly review"
         agenda ""
         ((org-agenda-span 7)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-log-mode '(closed))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE "))))

        ;; Closed Items - see what I accomplished
        ("c" "Closed items"
         agenda ""
         ((org-agenda-span 7)
          (org-agenda-start-on-weekday 1)
                                        ;(org-agenda-start-with-log-mode '(closed))
          (org-agenda-log-mode-items '(closed))
          ))

        ;; James's Super View
        ("j" "James's Super View"
         (
          (agenda ""
                  (
                   (org-agenda-remove-tags t)                                       
                   (org-agenda-span 7)
                   ;;(org-agenda-prefix-format "  %t  s")
                   )
                  )

          (alltodo ""
                   (
                    (org-agenda-remove-tags t)
                    (org-agenda-prefix-format "  %t  %s")                    
                    (org-agenda-overriding-header "CURRENT STATUS")
                    (org-super-agenda-groups
                     '(
                       (:name "Critical Tasks"
                              :tag "CRITICAL"
                              :order 0
                              )
                       (:name "Currently Working"
                              :todo "IN-PROGRESS"
                              :order 1
                              )
                       (:name "Planning Next Steps"
                              :todo "PLANNING"
                              :order 2
                              )
                       (:name "Problems & Blockers"
                              :todo "BLOCKED"
                              :tag "obstacle"                              
                              :order 3
                              )
                       (:name "Tickets to Create"
                              :tag "@write_future_ticket"
                              :order 4
                              )
                       (:name "Research Required"
                              :tag "@research"
                              :order 7
                              )
                       (:name "Meeting Action Items"
                              :and (:tag "meeting" :priority "A")
                              :order 8
                              )
                       (:name "Other Important Items"
                              :and (:todo "TODO" :priority "A" :not (:tag "meeting"))
                              :order 9
                              )
                       (:name "General Backlog"
                              :and (:todo "TODO" :priority "B")
                              :order 10
                              )
                       (:name "Non Critical"
                              :priority<= "C"
                              :order 11
                              )
                       (:name "Currently Being Verified"
                              :todo "VERIFYING"
                              :order 20
                              )
                       )
                     )
                    )
                   )
          ))
        
        ;; Current progress - see what I'm currently doing
        ("i" "Items Currently Working"
         (
          (todo "IN-PROGRESS"
                (
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\[#[ABC]\\]"))
                 (org-agenda-overriding-header "Currently Working:")
                 )
                )

          (tags "meeting"
                (
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\[#[ABC]\\]" 'todo '("IN-PROGRESS" "DONE" "OBE" "WONT-DO")))
                 (org-agenda-overriding-header "Meeting Action Items:")
                 )
                )
          
          (agenda "" ((org-agenda-span 1)))
          )
         ((org-agenda-compact-blocks nil)))
        
        ))

;; Agenda View "g"
;; (setq org-agenda-custom-commands
;;       '(("g" . "GTD contexts")
;;         ("gp" "Planning" tags-todo "planning")
;;         ("gb" "Backend" tags-todo "backend")
;;         ("G" "GTD Block Agenda"
;;          ((todo "IN-PROGRESS")
;;           ;(tags-todo "URGENT")
;;           ;(todo "NEXT")
;;           )
;;          ((org-agenda-prefix-format "[ ] %T: ")
;;           (org-agenda-with-colors t)
;;           (org-agenda-compact-blocks t)
;;           (org-agenda-remove-tags t)
;;           (ps-number-of-columns 2)
;;           (ps-landscape-mode t))
;;          ;;nil                      ;; i.e., no local settings
;;          ("~/next-actions.txt"))
;;         )
;;       )

;; End Agenda hacks


;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
        ))

;; TODO colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
        ("BLOCKED" . (:foreground "Red" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("OBE" . (:foreground "LimeGreen" :weight bold))
        ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
        ))

;; Tags
(setq org-tag-alist '(
                      ;; Ticket types
                      (:startgroup . nil)
                      ("@bug" . ?b)
                      ("@feature" . ?u)
                      ("@spike" . ?j)                      
                      (:endgroup . nil)

                      ;; Ticket flags
                      ("@write_future_ticket" . ?w)
                      ("@emergency" . ?e)
                      ("@research" . ?r)

                      ;; Meeting types
                      (:startgroup . nil)
                      ("big_sprint_review" . ?i)
                      ("cents_sprint_retro" . ?n)
                      ("dsu" . ?d)
                      ("grooming" . ?g)
                      ("sprint_retro" . ?s)
                      (:endgroup . nil)

                      ;; Code TODOs tags
                      ("QA" . ?q)
                      ("backend" . ?k)
                      ("broken_code" . ?c)
                      ("frontend" . ?f)

                      ;; Special tags
                      ("CRITICAL" . ?x)
                      ("obstacle" . ?o)
                      
                      ;; Meeting tags
                      ("HR" . ?h)
                      ("general" . ?l)
                      ("meeting" . ?m)
                      ("misc" . ?z)
                      ("planning" . ?p)

                      ;; Work Log Tags
                      ("accomplishment" . ?a)
                      ))

;; Tag colors
(setq org-tag-faces
      '(
        ("planning" . (:foreground "mediumPurple1" :weight bold))
        ("backend" . (:foreground "royalblue1" :weight bold))
        ("frontend" . (:foreground "forest green" :weight bold))
        ("QA" . (:foreground "sienna" :weight bold))
        ("meeting" . (:foreground "yellow1" :weight bold))
        ("CRITICAL" . (:foreground "red1" :weight bold))
        )
      )

;; journal settings
(setq org-capture-templates
      '(
        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %? :broken_code:%^g\n:Created: %T\n%i\n%a\n"
         :empty-lines 0)

        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %? :general:%^g\n:Created: %T\n"
         :empty-lines 0)

        ("j" "Work Journal Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)

        ("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)

        ("s" "Scheduled Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \nMeetingTime: %^{MeetingTime}T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :empty-lines 0)
        
        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)

        ("t" "Ticket"
         entry (file+headline "~/org/tickets.org" "Tickets")
         "* TODO [#B] %? %^g\nCreated: %T\n** Jira Link: \n** Notes\n** Status\n - [ ] Research\n - [ ] PR\n - [ ] Verifying\n** Subtasks"
         :empty-lines 0)

        ("p" "Sprint"
         entry (file "~/org/sprints.org" )
         "** TODO Sprint %?\n:Created: %T\nSCHEDULED: %T\nDEADLINE: %T\n*** Workload\n- [ ]\n*** Points ")
        ))

;; Make org look better
(setq org-hide-emphasis-markers t)

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))))

(add-hook 'org-mode-hook 'visual-line-mode)

;; (custom-theme-set-faces
;;  'user
;;  '(org-block ((t (:inherit fixed-pitch))))
;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-document-info ((t (:foreground "dark orange"))))
;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;  '(org-link ((t (:foreground "royal blue" :underline t))))
;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))



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
;; GIT
;;-------------------------------------------------------------------------------------------
;; Git Gutter
;(require 'git-gutter)

;; If you enable global minor mode
;(global-git-gutter-mode t)

;; If you enable git-gutter-mode for some modes
;(add-hook 'ruby-mode-hook 'git-gutter-mode)
;(add-hook 'python-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)


;; Mo Git Blame
; add config here

;;-------------------------------------------------------------------------------------------
;; PYTHON
;;-------------------------------------------------------------------------------------------
;; LSP Pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=120"))

;; Blacken
(add-hook 'python-mode-hook 'blacken-mode)

;; Import sorter
(require 'pyimpsort)
(eval-after-load 'python
  '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer))

;; Import Magic
(add-hook 'python-mode-hook 'importmagic-mode)


;; (defvar myPackages
;;   '(better-defaults                 ;; Set up some better Emacs defaults
;;     elpy                            ;; Emacs Lisp Python Environment
;;     flycheck                        ;; On the fly syntax checking
;;     ;;material-theme                  ;; Theme
;;     blacken                         ;; Black formatting on save
;;     magit                           ;; Git integration
;;     )
;;   )

;; ;; Scans the list in myPackages
;; ;; If the package listed is not already installed, install it
;; (mapc #'(lambda (package)
;;           (unless (package-installed-p package)
;;             (package-install package)))
;;       myPackages)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)                      ; optional
;; (setq jedi:complete-on-dot t)                 ; optional

;; ;; Enable elpy
;; ;;(elpy-enable)
;; ;;(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; (elpy-enable)
;; (setq elpy-rpc-backend "jedi")
;; (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

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
;; C#
;;-------------------------------------------------------------------------------------------
;; (use-package omnisharp
;;   :after company
;;   :config
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-to-list 'company-backends 'company-omnisharp))

;; (add-hook 'csharp-mode-hook 'flycheck-mode)


;;-------------------------------------------------------------------------------------------
;; install the missing packages
;;-------------------------------------------------------------------------------------------
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
