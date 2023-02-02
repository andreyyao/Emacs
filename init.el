(require 'package)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(setq global-auto-revert-mode t)
(setq global-eldoc-mode nil)
(setq indent-tabs-mode nil)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(set-scroll-bar-mode nil)
(set-fringe-mode '(0 . 0))
(cua-mode t)


(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


;; Makes emacs transparent in terminal mode
;; https://stackoverflow.com/q/19054228
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)


;; Making sure that emacs inherits same environment variable as shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (setenv "SHELL" "/usr/share/zsh")
  (exec-path-from-shell-initialize))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))


(use-package recentf
  :custom
  (recentf-max-saved-items 20)
  (recentf-max-menu-items 20)
  (recentf-exclude '("*.aux" "*.log" "*.bcf" "*.run.xml" "*~"))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))


;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-faicon "tree" :height 1.1 :v-adjust 0.0)
          "Treemacs"
          "Open Treemacs"
          (lambda (&rest _) (treemacs)))
	 ("⚙" "init.el" "Configure Emacs" (lambda (&rest _) (open-init-file))))))

(use-package dashboard
  :after all-the-icons
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-item-generators  '(treemacs . dashboard-insert-custom))
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (when (string= (buffer-name) "*dashboard*") (revert-buffer))))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 15))))


(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-count 4))

(use-package marginalia
  :init (marginalia-mode))

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

(use-package ethan-wspace
  :diminish ethan-wspace-mode
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


;;;;********************** PROGRAM ***************************
(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode))


(use-package treemacs
  :defer t
  :custom (treemacs-width 20)
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
  :diminish yas-minor-mode
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
  :diminish company-mode
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
  (LaTeX-mode . lsp)
  :custom
  (TeX-view-program-selection '((output-pdf "Okular")))
  (TeX-source-correlate-start-server t)
  :config
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
  :config
  (setf lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))


;;;;************************* ELisp **************************
(use-package emacs-lisp-mode
  :hook
  (emacs-lisp-mode . company-mode))
