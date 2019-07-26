;; .emacs


;; enable visual feedback on selections
;;(setq transient-mark-mode t)

;;; Packages ;;;
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; default tab width
(setq-default tab-width 4)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (whiteboard)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
	(
     ace-window
     ac-html
     ac-html-csswatcher
     anaconda-mode
     async
     blacken
     elpy
     exec-path-from-shell
     flycheck
     flycheck-gradle
     flymake-go
     flymd
     format-all
     gh-md
     go-autocomplete
     go-dlv
     go-gopath
     go-mode
     govet
     gradle-mode
     groovy-mode
     helm
     importmagic
     indium
     isortify
     javadoc-lookup
     java-file-create
     javaimp
     java-imports
     javap-mode
     jedi
     json-reformat
     markdown-mode
     markdown-mode+
     markdown-preview-eww
     markdown-preview-mode
     meghanada
     multiple-cursors
     mvn
     mvn-help
     pyenv-mode
     pyenv-mode-auto
     pyflakes
     pyimport
     py-import-check
     requirejs
     rjsx-mode
     thread-dump
     web-beautify)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))


;; Require JS ;;
;;(setq requirejs-require-base "~/path/to/your/project")
;;(requirejs-add-alias "jquery" "$" "path/to/jquery-<version>.js")

(add-hook 'js2-mode-hook
          '(lambda ()
             (local-set-key [(super a) ?s ?r ] 'requirejs-sort-require-paths)
             (local-set-key [(super a) ?a ?r ] 'requirejs-add-to-define)
             (local-set-key [(super a) ?r ?j ] 'requirejs-jump-to-module)
             ))

(setq requirejs-define-header-hook
      '(lambda ()
         (insert
          (format "// (c) Copyright %s ACME, Inc.  All rights reserved.\n"
                  (format-time-string "%Y")))))
;; end Require JS ;;


;;(when window-system (set-exec-path-from-shell-PATH))


;;; Multiple Cursors ;;;
(global-set-key (kbd "C-x C-j") 'mc/mark-all-like-this-dwim)

;;; Flycheck ;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; js beautify ;;;
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(add-hook 'js-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'json-mode
  '(add-hook 'json-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(eval-after-load 'sgml-mode
  '(add-hook 'html-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))


;;; Python stuff ;;;
(add-hook 'python-mode-hook 'blacken-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional


;; json formatter
(defun json-format ()
  (interactive)
  (save-excursion
	(shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
	)
  )

;; preview markdown files
(defalias 'pmd 'flymd-flyit)


;; Handle pane navigation
(global-set-key (kbd "M-o") 'ace-window)


