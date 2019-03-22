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
	(pyflakes pyimport flymd markdown-mode markdown-mode+ markdown-preview-eww async elpy format-all importmagic jedi json-reformat py-import-check groovy-mode flycheck-gradle gradle-mode ac-html ac-html-csswatcher rjsx-mode anaconda-mode pyenv-mode pyenv-mode-auto helm indium java-file-create meghanada thread-dump java-imports javadoc-lookup javap-mode mvn mvn-help requirejs javaimp flycheck flymake-go go-dlv exec-path-from-shell web-beautify multiple-cursors govet go-mode go-gopath go-autocomplete)))
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


(when window-system (set-exec-path-from-shell-PATH))


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
(require 'pyimpsort)
(eval-after-load 'python
  '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer))
			 
(eval-after-load 'python
    '(add-hook 'python-mode-hook
			 (lambda ()
			   (add-hook 'before-save-hook #'pyimpsort-buffer t t))))



;; json formatter
(defun json-format ()
  (interactive)
  (save-excursion
	(shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
	)
  )

;; preview markdown files
(defalias 'pmd 'flymd-flyit)


