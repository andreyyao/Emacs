(require 'package)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(global-auto-revert-mode t)
(delete-selection-mode 1) ; Inserting characters replaces selected region
(global-eldoc-mode nil)
(indent-tabs-mode nil)
(scroll-bar-mode nil)
(menu-bar-mode nil)
(tool-bar-mode nil)

(setq use-package-compute-statistics t)

;; Makes emacs transparent in terminal mode
;; https://stackoverflow.com/q/19054228
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

(use-package all-the-icons
  :if (display-graphic-p))

;; Making sure that emacs inherits same environment variable as shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (setenv "SHELL" "/usr/share/zsh")
  (exec-path-from-shell-initialize))

(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))

(use-package mini-frame
  :init (mini-frame-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package which-key-posframe
  :init (which-key-posframe-mode 1))

(use-package eldoc-mode
  :defer t
  :diminish)

(use-package selectrum
  :init (selectrum-mode +1))

(use-package marginalia
  :init (marginalia-mode))

(use-package pdf-tools
   :defer t
   :config
   (pdf-tools-install))

(defun contextual-nyan (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (nyan-mode (if (display-graphic-p frame)
                 t -1)))

(add-hook 'after-make-frame-functions 'contextual-nyan)

(use-package nyan-mode
  :defer t
  :config
  (nyan-mode t))

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
  (prog-mode . linum-mode))

(use-package treemacs
  :defer t
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
    tuareg-mode)    ; ocaml-lsp-server
   . lsp-deferred)
  :custom
  (read-process-output-max (* 1024 1024)) ; 1MB
  (gc-cons-threshold 100000000) ; lsp-mode speedup
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
  (flycheck-display-errors-function nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-standard-error-navigation t)
  (flycheck-deferred-syntax-check nil))

(use-package flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package company
  :diminish company-mode
  :custom
  (company-minimum-prefix-length 3)
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
    (setq lsp-enabled-clients '(ocamllsp))
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
  :defer t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server nil)
  (add-hook 'TeX-after-compilation-finished-hook
            #'TeX-revert-document-buffer))


;;;;*********************** Coq 🐓 ***************************

(use-package proof-general
  :defer t
  :custom (proof-splash-enable nil))

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

;;;;*********************Type script ***********************

(use-package typescript-mode
  :defer t)
