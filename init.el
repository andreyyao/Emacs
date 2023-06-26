(require 'package)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


(use-package esup
  :init (setq esup-depth 0))


;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Set it back to reduce long freezing during GC
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 100))))


(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 3)
  (setq right-margin-width 3))


(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))


(setq global-auto-revert-mode t)
(setq global-eldoc-mode nil)
(setq indent-tabs-mode nil)
(setq line-number-mode nil)
(cua-mode t)


(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)


(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


;; Making sure that emacs inherits same environment variable as shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (setenv "SHELL" "/usr/share/zsh")
  (exec-path-from-shell-initialize))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-monokai-pro t)
  (set-face-attribute 'highlight nil :foreground 'unspecified :background 'unspecified :underline '(:color foreground-color :style line)))


(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-def-modeline 'personal-mode-line
    '(buffer-info remote-host buffer-position)
    '(misc-info major-mode process vcs checker))
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'personal-mode-line 'default)))
  (doom-modeline-mode 1)
  (setq mode-line-percent-position nil))


(use-package nyan-mode
  :custom
  (nyan-minimum-window-width 16)
  (nyan-animate-nyancat t)
  (nyan-bar-length 12)
  (nyan-wavy-trail t)
  :config
  (nyan-mode t)
  (setq nyan-cat-image
        (create-image nyan-cat-face-image 'xpm nil :scale 2 :ascent 'center))
  (setq nyan-animation-frames
        (mapcar
	 (lambda (id)
           (create-image (concat nyan-directory (format "img/nyan-frame-%d.xpm" id))
                         'xpm nil :scale 2 :ascent 95))
         '(1 2 3 4 5 6))))


(use-package dashboard
  :init
  (if (daemonp)
      (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook 'my-set-margins)
  (setq dashboard-navigator-buttons
	`((("🌲" "Treemacs" "Open Treemacs" (lambda (&rest _) (treemacs)))
	   ("🦄" "Roam" "Open Org Roam" (lambda (&rest _) (org-roam-node-find)))
	   ("🔧" "init.el" "Settings" (lambda (&rest _) (open-init-file))))))
  :custom-face
  (dashboard-navigator ((t nil)))
  :custom
  (dashboard-startup-banner ".emacs.d/banner.txt")
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-banner-logo-title nil)
  (dashboard-set-heading-icons t)
  (dashboard-center-content t)
  (dashboard-recentf-show-base t)
  (dashboard-recentf-item-format "%s")
  (dashboard-projects-backend 'project-el)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents  . 10) (projects . 5))))


(use-package recentf
  :custom
  (recentf-max-saved-items 20)
  (recentf-max-menu-items 20)
  (recentf-exclude '("*.aux" "*.log" "*.bcf" "*.org" "*.run.xml" "*~"))
  :config
  (recentf-mode 1))


(use-package which-key
  :config (which-key-mode))


(use-package vertico
  :init (vertico-mode)
  :custom (vertico-count 4))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'company-completion-styles)


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
  ;; Turn off `mode-require-final-newline' since ethan-wspace
  ;; supersedes `require-final-newline'.
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

(use-package org
  :mode (("\\.org$" . org-mode)) ;; Config doesn't run without this
  :defer t
  :hook
  (org-mode . company-mode)
  (org-mode . org-indent-mode)
  :config
  (add-hook 'org-mode-hook 'my-set-margins)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  :custom
  (org-support-shift-select t)
  (org-startup-with-inline-images t)
  (org-fontify-whole-heading-line t)
  (org-confirm-babel-evaluate nil)
  (org-cite-global-bibliography '("~/Documents/Roam/zotero.bib"))
  (org-babel-load-languages '((python . t) (emacs-lisp . t) (C . t) (ocaml . t) (shell . t) (R . t)))
  (org-babel-python-command "~/Documents/Roam/.venv/bin/python") ;; virtual env
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.5))))
  (org-level-2 ((t (:inherit outline-2 :height 1.4))))
  (org-level-3 ((t (:inherit outline-2 :height 1.3))))
  (org-level-4 ((t (:inherit outline-2 :height 1.2))))
  (org-level-5 ((t (:inherit outline-2 :height 1.1))))
  (org-block-begin-line
   ((t
     (:box (:line-width (2 . 4) :color "brown20" :style released-button)
      :foreground "gray50" :background "brown25" :extend t :inherit (org-block))))))
  ;(setq org-hide-block-startup t)))


(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t) ; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename (concat (getenv "HOME") "/Documents/Roam/")))
  (org-roam-capture-templates
   `(("d" "default" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}") :unnarrowed t))) ; Gets rid of timestamp in Roam file names
  :config
  (org-roam-setup)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)))


;; (use-package org-modern
;;   :defer t
;;   :custom
;;   ;(org-modern-hide-stars t) ; adds extra indentation
;;   (org-modern-table nil)
;;   (org-modern-list
;;    '(;; (?- . "-")
;;      (?* . "•")
;;      (?+ . "‣")))
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-agenda-finalize . org-modern-agenda))


;; (use-package org-modern-indent
;;   :load-path "~/.emacs.d/org-modern-indent"
;;   :defer t
;;   :config ; add late to hook
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


;; (use-package org-fragtog
;;   :hook (org-mode . org-fragtog-mode))

;; <----------------------- END ORG ------------------------->

;; <----------------------- PROGRAM ------------------------->
(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode))


(use-package treemacs
  :defer t
  :custom
  (treemacs-width 20)
  (treemacs-hide-gitignored-files-mode t)
  :config
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))


(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)


(use-package eglot
  :defer t
  :hook
  ((c-mode          ; clangd
    c++-mode         ; clangd
    typescript-mode ; ts-ls (tsserver wrapper)
    python-mode     ; pyright
    rustic-mode     ; rust-analyzer
    tuareg-mode     ; ocaml-lsp-server
    haskell-mode)   ; haskell-language-server
   . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  (add-to-list 'eglot-server-programs
	       '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  (add-hook 'eglot-managed-mode-hook #'company-mode t))


(use-package eldoc-box
  :custom
  (eldoc-box-max-pixel-width 1500)
  (eldoc-box-max-pixel-height 1000))


;; (use-package lsp-treemacs
;;   :defer t
;;   :after (lsp treemacs)
;;   :config
;;   (treemacs-load-theme "nerd-icons"))


;; (use-package lsp-mode
;;   :defer t
;;   :hook
;;   ((c-mode          ; clangd
;;     c++-mode         ; clangd
;;     typescript-mode ; ts-ls (tsserver wrapper)
;;     python-mode     ; pyright
;;     rustic-mode     ; rust-analyzer
;;     tuareg-mode     ; ocaml-lsp-server
;;     haskell-mode)   ; haskell-language-server
;;    . lsp-deferred)
;;   :custom
;;   (read-process-output-max (* 1024 1024)) ; 1MB
;;   (gc-cons-threshold 100000000) ; lsp-mode speedup
;;   (lsp-diagnostics-provider :flymake)
;;   (lsp-enable-imenu nil)
;;   (lsp-enable-on-type-formatting nil)
;;   (lsp-enable-snippet nil)
;;   (lsp-enable-symbol-highlighting t)
;;   (lsp-headerline-breadcrumb-segments '(project file symbols))
;;   (lsp-idle-delay 0.5)
;;   (lsp-lens-enable nil)
;;   (lsp-log-io nil)
;;   (lsp-modeline-code-actions-enable nil)
;;   (lsp-modeline-diagnostics-enable nil)
;;   (lsp-semantic-tokens-enable nil)
;;   (lsp-signature-auto-activate nil)
;;   (lsp-signature-render-documentation nil)
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-sideline-enable nil)
;;   :commands lsp)


;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :commands lsp-ui-mode)


(use-package yasnippet
  :hook (latex-mode . yas-minor-mode))


(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; (use-package flycheck
;;   :defer t
;;   :custom
;;   (flycheck-check-syntax-automatically '(save new-line))
;;   (flycheck-deferred-syntax-check nil)
;;   (flycheck-display-errors-delay 0.2)
;;   (flycheck-display-errors-function nil)
;;   (flycheck-highlighting-mode 'symbols)
;;   (flycheck-indication-mode 'left-margin)
;;   (flycheck-standard-error-navigation t))


(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2))


;;;;********************** Python 🐍 *************************
(use-package python
  :defer t
  :custom
  (python-indent-offset 2))

;; (use-package lsp-pyright
;;   :defer t
;;   :hook
;;   (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp)))
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)))


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
(use-package AucTeX
  :hook
  (LaTeX-mode . display-line-numbers-mode)
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . eglot-ensure)
  :custom
  (TeX-view-program-selection '((output-pdf "Okular")))
  (TeX-source-correlate-start-server t)
  (TeX-electric-math (cons "$" "$"))
  (TeX-save-query nil)
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
(use-package rustic
  :defer t
  :custom (rustic-lsp-client 'eglot))


;;;;************************ C++ *****************************
(use-package c++
  :defer t)


;;;;********************** Type script ***********************
(use-package typescript
  :defer t)


;;;;************************ Haskell *************************
(use-package haskell
  :defer t
  :bind ("C-c C-c" . 'haskell-compile))


;;;;************************* ELisp **************************
(use-package emacs-lisp
  :hook
  (emacs-lisp-mode . company-mode))
