(require 'package)

;;-------------------------------------------------------------------------------------------
;; MELPA
;;-------------------------------------------------------------------------------------------
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;;-------------------------------------------------------------------------------------------
;; DEFAULTS 
;;-------------------------------------------------------------------------------------------
;; better defaults
(require 'better-defaults)

(setq column-number-mode t)
(show-paren-mode 1)
(setq visible-bell t)

;;; Multiple Cursors ;;;
(global-set-key (kbd "C-x C-j") 'mc/mark-all-like-this-dwim)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")




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
    (use-package better-defaults hemisu-theme 0blayout hybrid-reverse-theme immaterial-theme material-theme acme-theme afternoon-theme ahungry-theme alect-themes ample-zen-theme apropospriate-theme autumn-light-theme color-theme-sanityinc-tomorrow soft-stone-theme twilight-anti-bright-theme twilight-bright-theme twilight-theme magit auto-complete flycheck-plantuml plantuml-mode auto-complete-rst sphinx-doc sphinx-mode blacken elpy flycheck-pycheckers importmagic jedi pippel pyimpsort python-black python-docstring python-mode yaml-tomato indent-tools yaml-mode ruby-extra-highlight yard-mode enh-ruby-mode format-all rbtagger rubocop rubocopfmt ruby-tools rufo)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))


;;-------------------------------------------------------------------------------------------
;; RUBY
;;-------------------------------------------------------------------------------------------
(require 'rubocopfmt)
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)
(setq rubocopfmt-use-bundler-when-possible nil)

;; ruby highlighting
(add-hook 'ruby-mode-hook #'ruby-extra-highlight-mode)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;;-------------------------------------------------------------------------------------------
;; PYTHON 
;;-------------------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

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
    material-theme                  ;; Theme
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
;; JAVASCRIPT
;;-------------------------------------------------------------------------------------------
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


;;-------------------------------------------------------------------------------------------
;; GOLANG
;;-------------------------------------------------------------------------------------------
(setenv "GOPATH" "~/go")
(add-to-list 'exec-path "~/go/bin")

;;; Go Docs ;;;
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


;; Go autocomplete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; Call Gofmt before saving
(add-hook 'before-save-hook 'gofmt-before-save)

(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go vet"))

  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-[") 'godef-describe)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)


;;-------------------------------------------------------------------------------------------
;; JSON
;;-------------------------------------------------------------------------------------------
;; json formatter 
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
    )
  )
