(require 'package)
(customize-set-variable 'custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;;; Minimize garbage collection during startup to reduce startup time
(setq gc-cons-threshold most-positive-fixnum)
;;; Set it back to reduce long freezing during GC
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 100))))

(defvar emacs-temp-dir "~/.emacs.d/temp"
  "The dumpster for all backup-related stuff")
(customize-set-variable 'backup-directory-alist `(("." . ,emacs-temp-dir)))
(customize-set-variable 'lock-file-name-transforms
                        `(("\\`/.*/\\([^/]+\\)\\'" ,emacs-temp-dir t)))
(customize-set-variable 'auto-save-file-name-transforms
                        `((".*" ,emacs-temp-dir t)))


(customize-set-variable 'inhibit-startup-echo-area-message "andrey")
(customize-set-variable 'global-auto-revert-mode t)
(customize-set-variable 'global-eldoc-mode nil)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'line-number-mode nil)
(pixel-scroll-precision-mode)
(cua-mode t)


(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)


(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


;;; Making sure that emacs inherits same environment variable as shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (setenv "SHELL" "/usr/share/zsh")
  (exec-path-from-shell-initialize))


(line-number-mode t)
(column-number-mode t)

(use-package display-line-numbers
  :ensure t
  :config
  (global-display-line-numbers-mode 1)
  :custom
  (display-line-numbers-width-start t))


(use-package modus-themes
  :ensure t
  :config
  ; Set the background color first, since it's weird with daemon mode
  (set-face-attribute 'default nil :background "#0d0e1c" :height 120)
  (load-theme 'modus-vivendi-tinted t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ; Set the background color first, since it's weird with daemon mode
;;   (set-face-attribute 'default nil :background "#1c1e1f" :height 120)
;;   ; https://github.com/jonathanchu/atom-one-dark-theme/issues/50
;;   (defun my/theme-init ()
;;     (load-theme 'doom-molokai t)
;;     (set-face-attribute 'highlight nil :distant-foreground 'unspecified :foreground 'unspecified :background 'unspecified :underline '(:color foreground-color :style line))
;;     (set-face-attribute 'link nil :foreground 'unspecified)
;;     (set-face-attribute 'font-lock-comment-face nil :foreground "#999999")
;;     (set-face-attribute 'font-lock-doc-face nil :foreground "#CCBB77"))
;;   (my/theme-init))


(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-def-modeline 'personal-mode-line
    '(buffer-info remote-host buffer-position)
    '(misc-info major-mode process vcs check))
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'personal-mode-line 'default)))
  (doom-modeline-mode 1)
  (setq mode-line-percent-position nil)
  :custom
  (doom-modeline-env-version nil))


(use-package nyan-mode
  :custom
  (nyan-minimum-window-width 16)
  (nyan-animate-nyancat t)
  (nyan-bar-length 12)
  (nyan-wavy-trail t)
  :config
  (nyan-mode t)
  (setq nyan-cat-image (create-image nyan-cat-face-image 'xpm nil :scale 2 :ascent 'center)
        nyan-animation-frames
        (mapcar
         (lambda (id)
           (create-image (concat nyan-directory (format "img/nyan-frame-%d.xpm" id))
                         'xpm nil :scale 2 :ascent 95))
         '(1 2 3 4 5 6))))


(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'server-after-make-frame-hook
            (lambda () (if (string= (buffer-name) "*dashboard*") (revert-buffer))))
  (if (daemonp)
      (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
  :custom
  (dashboard-startup-banner "~/.emacs.d/emacs.svg")
  (dashboard-banner-logo-title nil)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-center-content t)
  (dashboard-recentf-show-base t)
  (dashboard-recentf-item-format "%s")
  (dashboard-projects-backend 'project-el)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator nil)
  (dashboard-items '((recents  . 10) (projects . 0)))
  (dashboard-footer-messages
   '("Mitochondria is the powerhouse of the cell"
     "I showed you my source code, pls respond"
     "Monads are just monoids in the category of endofunctors"
     "Proof checker? I hardly know'er!"
     "I use Arch btw")))


(use-package recentf
  :custom
  (recentf-max-saved-items 20)
  (recentf-max-menu-items 20)
  (recentf-exclude '("*.aux" "*.log" "*.bcf" "*.org" "*.run.xml" "*~"))
  :config
  (recentf-mode 1))


(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  :custom
  (ibuffer-use-other-window t))


(use-package which-key
  :config (which-key-mode))


(use-package vertico
  :init (vertico-mode)
  :custom (vertico-count 6))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
;; (defun company-completion-styles (capf-fn &rest args)
;;   (let ((completion-styles '(basic partial-completion)))
;;     (apply capf-fn args)))
;; (advice-add 'company-capf :around #'company-completion-styles)


(use-package marginalia
  :init (marginalia-mode))


(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))


(use-package ethan-wspace
  :config
  ;; Turn off `mode-require-final-newline' since ethan-wspace supersedes `require-final-newline'.
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1) ; Enable ethan-wspace globally
  (add-hook 'makefile-mode-hook ; Keep TAB's on makefile mode
            #'(lambda()
                (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))
  (add-hook 'diff-mode-hook ; Disable in diff-mode
            #'(lambda()
                (ethan-wspace-mode -1)))
  ;; Ignore no trailing newline error
  (setq-default ethan-wspace-errors (remove 'no-nl-eof ethan-wspace-errors)))


;; <---------------------- ORG ----------------------------->

;;; Used to set org-mode buffer margins
;; (defun org-set-margins ()
;;   "Set margins in current buffer."
;;   (setq left-margin-width 3)
;;   (setq right-margin-width 3))

(use-package org
  :mode (("\\.org$" . org-mode)) ; Config doesn't run without this
  :hook
  (org-mode . flyspell-mode)
  (org-mode . company-mode)
  (org-mode . org-indent-mode)
  (org-mode . org-fragtog-mode)
  :config
  ;; (add-hook 'org-mode-hook 'org-set-margins)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-startup-truncated nil)
  ; From the package ov
  (defun ov-at (&optional point)
    "Get an overlay at POINT.
  POINT defaults to the current `point'."
    (or point (setq point (point)))
    (car (overlays-at point)))
  ; https://www.reddit.com/r/emacs/comments/169keg7/comment/jzierha/?utm_source=share&utm_medium=web2x&context=3
  (defun org-justify-fragment-overlay (beg end image &optional imagetype)
    "Only equations at the beginning and also end of a line are justified."
    (if
     (and (= beg (line-beginning-position)) (= end (line-end-position)))
     (let* ((ov (ov-at))
            (disp (overlay-get ov 'display)))
       (overlay-put ov 'line-prefix `(space :align-to (- center (0.5 . ,disp)))))))
  (advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)
  :custom
  (org-support-shift-select t)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-fontify-whole-heading-line t)
  (org-confirm-babel-evaluate nil)
  (org-cite-global-bibliography '("~/Documents/Roam/zotero.bib"))
  (org-babel-load-languages '((python . t) (emacs-lisp . t) (C . t) (ocaml . t) (shell . t) (R . t)))
  (org-babel-python-command "~/Documents/Roam/.venv/bin/python") ;; virtual env
  (org-latex-packages-alist '(("AUTO" "mathpartir" t ("pdflatex"))("AUTO" "mathtools" t ("pdflatex"))))
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.3))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-2 :height 1.1))))
  (org-level-4 ((t (:inherit outline-2 :height 1.1))))
  (org-level-5 ((t (:inherit outline-2 :height 1.1))))
  (org-link
   ((t (:foreground unspecified :inherit (link)))))
  (org-block-begin-line
   ((t
     (:box (:line-width (2 . 4) :color "brown" :style released-button)
      :foreground "gray50" :background "brown" :extend t :inherit (org-block))))))
; (setq org-hide-block-startup t)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t) ; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename (concat (getenv "HOME") "/Documents/Roam/")))
  (org-roam-capture-templates
   `(("d" "default" plain "%?" :target (file+head "${slug}.org" "#+TAGS:\n#+TITLE: ${title}") :unnarrowed t))) ; Gets rid of timestamp in Roam file names
  :config
  (org-roam-db-autosync-enable)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)))
   ; How to insert node with different label: https://github.com/org-roam/org-roam/issues/2147


(use-package citar
  :after org
  :bind
  (("C-c r e" . #'citar-open-entry)
   ("C-c r l" . #'citar-open-links)
   ("C-c r n" . #'citar-open-notes)
   ("C-c r i" . #'citar-insert-citation))
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-follow-processor 'citar))


(use-package citar-org-roam
  :after (citar org-roam)
  :custom
  (citar-org-roam-mode t)
  ;; NOTE: This was the original default subdir, prior to https://github.com/emacs-citar/citar-org-roam/issues/36
  (citar-org-roam-subdir "references"))


(use-package org-fragtog
  :defer t
  :ensure t)

;; <----------------------- END ORG ------------------------->

;; <----------------------- PROGRAM ------------------------->
(use-package treemacs
  :defer t
  :custom
  (treemacs-width 20)
  (treemacs-indentation 1)
  (treemacs-hide-gitignored-files-mode t)
  :config
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))


(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(use-package treesit
  :defer t
  :custom
  (treesit-font-lock-level 4) ; Highest level
  (major-mode-remap-alist
   '((yaml-mode . yaml-ts-mode)
     (bash-mode . bash-ts-mode)
     (js2-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (css-mode . css-ts-mode)
     (python-mode . python-ts-mode)
     (rustic-mode . rust-ts-mode))))


(use-package eglot
  :hook
  ((c-mode          ; clangd
    c++-mode        ; clangd
    java-mode       ; IDK, some server for java
    typescript-mode ; ts-ls (tsserver wrapper)
    python-mode     ; pyright
    rust-ts-mode    ; rust-analyzer
    tuareg-mode     ; ocaml-lsp-server
    haskell-mode)   ; haskell-language-server
   . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  (add-to-list
   'eglot-server-programs
   '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  (add-hook 'eglot-managed-mode-hook #'company-mode)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode))


(use-package eldoc-box
  :after (eglot)
  :custom
  (eldoc-box-max-pixel-width 1500)
  (eldoc-box-max-pixel-height 1000))


(use-package yasnippet
  :hook (LaTeX-mode . yas-minor-mode))


(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))


(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2))


;;;;********************** Python 🐍 *************************
(use-package python
  :defer t
  :custom
  (python-indent-offset 4))


;;;;********************** OCaml 🐪 **************************
(defun ocamllsp-setup ()
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(tuareg-mode)
    :priority -6
    :server-id 'ocamllsp)))

(use-package tuareg
  :defer t
  :config
  ;; (with-eval-after-load 'company
  ;;   (add-to-list 'company-backends 'merlin-company-backend))
  (with-eval-after-load 'eglot-mode
    (ocamllsp-setup))
  (lambda ()
    (make-local-variable 'ac-ignores)
    (setq ac-ignores
          (append '("and" "as" "assert" "begin" "class"
                    "constraint" "do" "done" "downto"
                    "else" "end" "exception" "external"
                    "false" "for" "fun" "function"
                    "functor" "if" "in" "include"
                    "inherit" "initializer" "lazy" "let"
                    "match" "method" "module" "mutable"
                    "new" "object" "of" "open" "or"
                    "private" "rec" "sig" "struct"
                    "then" "to" "true" "try" "type"
                    "val" "virtual" "when" "while"
                    "with" "mod" "land" "lor" "lxor"
                    "lsl" "lsr" "asr")
                  ac-ignores)))
  :hook
  (tuareg-mode . eglot-ensure));  (tuareg-mode . merlin-mode))


;;;;************************ LaTeX ***************************
(use-package tex
  :ensure auctex
  :ensure reftex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (TeX-mode . display-line-numbers-mode)
  (TeX-mode . prettify-symbols-mode)
  (TeX-mode . eglot-ensure)
  (LaTeX-mode . turn-on-reftex)
  :custom
  (TeX-view-program-selection '((output-pdf "Okular")))
  (TeX-source-correlate-start-server t)
  (TeX-save-query nil)
  (reftex-plug-into-AUCTeX t)
  :config
  (add-hook 'TeX-after-compilation-finished-hook
            #'TeX-revert-document-buffer))


;;;;*********************** Coq 🐓 ***************************
(use-package proof-general
  :defer t
  :custom
  (proof-splash-enable nil))


(use-package company-coq
  :defer t
  :hook (coq-mode . company-coq-mode)
  :custom (company-coq-live-on-the-edge t))


;;;;********************** Rust 🦀 ***************************
(use-package rust-ts
  :defer t)


;;;;************************ C++ *****************************
(use-package c++
  :defer t)


;;;;********************** Type script ***********************
(use-package typescript
  :defer t)


;;;;************************ Haskell *************************
(use-package haskell
  :defer t)


;;;;************************* ELisp **************************
(use-package emacs-lisp
  :hook
  (emacs-lisp-mode . company-mode))

(use-package web
  :defer t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-markdown-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package :css
  :defer t
  :custom
  (css-indent-offset 2))


;; https://medium.com/@jrmjrm/configuring-emacs-and-eglot-to-work-with-astro-language-server-9408eb709ab0
;; ASTRO
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))
