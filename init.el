(require 'package)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq esup-depth 0)
(use-package esup
  :ensure t)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))


(setq global-auto-revert-mode t)
(setq global-eldoc-mode nil)
(setq indent-tabs-mode nil)
(set-scroll-bar-mode nil)
(set-fringe-mode '(0 . 0))
;; (menu-bar-mode -1) ; Modify ~/.Xresources to prevent flashing
;; (tool-bar-mode -1)
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
  :config
  (load-theme 'doom-one)
  (set-face-attribute 'highlight nil :foreground 'unspecified :background "#404040"))


(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-minor-modes t)
  (minions-mode 1)
  (doom-modeline-def-modeline 'personal-mode-line
    '(buffer-info remote-host buffer-position)
    '(misc-info minor-modes major-mode process vcs checker))
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'personal-mode-line 'default)))
  (doom-modeline-mode 1)
  (setq mode-line-percent-position nil))


(use-package nyan-mode
  :custom
  (nyan-minimum-window-width 20)
  (nyan-animate-nyancat t)
  (nyan-bar-length 16)
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
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-icon-type 'nerd-icons)
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (when (string= (buffer-name) "*dashboard*") (revert-buffer))))
  (setq dashboard-navigator-buttons
	`((("🌲" "Treemacs" "Open Treemacs" (lambda (&rest _) (treemacs)))
	   ("🦄" "Roam" "Open Org Roam" (lambda (&rest _) (org-roam-node-find)))
	   (,(nerd-icons-codicon "nf-cod-settings_gear" :face 'nerd-icons-dsilver) "init.el" "Settings" (lambda (&rest _) (open-init-file))))))
  :custom
  (dashboard-startup-banner "~/.emacs.d/emacs.png")
  (dashboard-display-icons-p t)
  (dashboard-banner-logo-title nil)
  (dashboard-set-heading-icons t)
  (dashboard-recentf-show-base t)
  (dashboard-recentf-item-format "%s")
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 15) (projects . 5))))


(use-package recentf
  :custom
  (recentf-max-saved-items 20)
  (recentf-max-menu-items 20)
  (recentf-exclude '("*.aux" "*.log" "*.bcf" "*.org" "*.run.xml" "*~"))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))


(use-package which-key
  :config (which-key-mode))


(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package vertico
  :init (vertico-mode)
  :custom (vertico-count 4))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package marginalia
  :init (marginalia-mode))


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

(use-package org-mode
  :hook (org-mode . company-mode)
  :custom-face
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  :config (setq org-hide-block-startup t))


(use-package org-roam
  :init
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  (setq org-directory (concat (getenv "HOME") "/Documents/OrgRoam/"))
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))


;; (use-package org-superstar
;;   :hook (org-mode org-roam-mode)
;;   :config (org-superstar-configure-like-org-bullets))


(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

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
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(use-package lsp-mode
  :hook
  ((c-mode          ; clangd
    c++-mode        ; clangd
    c-or-c++-mode   ; clangd
    typescript-mode ; ts-ls (tsserver wrapper)
    python-mode     ; pyright
    rustic-mode     ; rust-analyzer
    tuareg-mode     ; ocaml-lsp-server
    haskell-mode)   ; haskell-language-server
   . lsp-deferred)
  :custom
  (read-process-output-max (* 1024 1024)) ; 1MB
  (gc-cons-threshold 100000000) ; lsp-mode speedup
  (lsp-diagnostics-provider :flycheck)
  (lsp-enable-imenu nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-idle-delay 0.5)
  (lsp-lens-enable nil)
  (lsp-log-io nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  :commands lsp)


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)


(use-package yasnippet
  :hook (latex-mode . yas-minor-mode))


(use-package lsp-treemacs
  :after (lsp-mode treemacs))


(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save new-line))
  (flycheck-deferred-syntax-check nil)
  (flycheck-display-errors-delay 0.2)
  (flycheck-display-errors-function nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode 'left-margin)
  (flycheck-standard-error-navigation t))


(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1))


;;;;********************** Python 🐍 *************************
;; (use-package python-mode
;;   :defer t
;;   :hook
;;   (pyvenv-mode company-mode)
;;   :custom
;;   (python-indent-offset 2))

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
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  (with-eval-after-load 'lsp-mode
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
  (tuareg-mode . lsp)
  (tuareg-mode . merlin-mode))


;;;;************************ LaTeX ***************************
(use-package latex
  :hook
  (LaTeX-mode . display-line-numbers-mode)
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . lsp)
  :custom
  (LaTeX-electric-left-right-brace t)
  (TeX-view-program-selection '((output-pdf "Okular")))
  (TeX-source-correlate-start-server t)
  (TeX-save-query nil)
  :config
  (setq TeX-electric-math (cons "$" "$"))
  (add-hook 'TeX-after-compilation-finished-hook
            #'TeX-revert-document-buffer))


;;;;*********************** Coq 🐓 ***************************
(use-package proof-general
  :after all-the-icons
  :init
  ;; all-the-icons thinks ".v" files are verilog.
  (add-to-list 'all-the-icons-extension-icon-alist '("v" all-the-icons-fileicon "coq" :height 1.0 :v-adjust -0.2 :face all-the-icons-yellow))
  :defer t
  :custom
  (proof-splash-enable nil))


(use-package company-coq
  :defer t
  :hook (coq-mode . company-coq-mode)
  :custom (company-coq-live-on-the-edge t))


;;;;********************** Rust 🦀 ***************************
(use-package rustic
  :defer t)


;;;;************************ C++ *****************************
(use-package c++-mode
  :defer t
  :hook
  (c++-mode . modern-c++-font-lock-mode))


;;;;********************** Type script ***********************
(use-package typescript-mode
  :defer t)


;;;;************************ Haskell *************************
(use-package lsp-haskell
  :defer t
  :config
  (setf lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))


;;;;************************* ELisp **************************
(use-package emacs-lisp-mode
  :hook
  (emacs-lisp-mode . company-mode))
