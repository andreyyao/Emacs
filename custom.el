(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-default-adjust 0)
 '(all-the-icons-scale-factor 1)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "03336a06e95a977ce3d67f849b93d492736d6bd11d61dddc424c81d7819c8ad1" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(eldoc-echo-area-use-multiline-p nil)
 '(flycheck-display-errors-delay 0.2)
 '(flycheck-indication-mode 'left-margin)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice '(lambda nil (get-buffer-create "*dashboard*")))
 '(lsp-diagnostics-provider :flycheck)
 '(lsp-enable-imenu nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-snippet nil)
 '(lsp-enable-symbol-highlighting t)
 '(lsp-headerline-breadcrumb-segments '(project file symbols))
 '(lsp-idle-delay 0.2)
 '(lsp-lens-enable nil)
 '(lsp-log-io nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-semantic-tokens-enable nil)
 '(lsp-signature-auto-activate nil)
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-sideline-enable nil)
 '(mode-line-format
   '("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line-buffer-id
            (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
           (mode-line
            (if active 'mode-line 'mode-line-inactive))
           (face0
            (if active 'powerline-active0 'powerline-inactive0))
           (face1
            (if active 'powerline-active1 'powerline-inactive1))
           (face2
            (if active 'powerline-active2 'powerline-inactive2))
           (separator-left
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (cdr powerline-default-separator-dir))))
           (lhs
            (list
             (powerline-raw "%*" face0 'l)
             (when powerline-display-buffer-size
               (powerline-buffer-size face0 'l))
             (when powerline-display-mule-info
               (powerline-raw mode-line-mule-info face0 'l))
             (powerline-buffer-id
              `(mode-line-buffer-id ,face0)
              'l)
             (when
                 (and
                  (boundp 'which-func-mode)
                  which-func-mode)
               (powerline-raw which-func-format face0 'l))
             (powerline-raw " " face0)
             (funcall separator-left face0 face1)
             (when
                 (and
                  (boundp 'erc-track-minor-mode)
                  erc-track-minor-mode)
               (powerline-raw erc-modified-channels-object face1 'l))
             (powerline-major-mode face1 'l)
             (powerline-process face1)
             (powerline-minor-modes face1 'l)
             (powerline-narrow face1 'l)
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (when
                 (bound-and-true-p nyan-mode)
               (powerline-raw
                (list
                 (nyan-create))
                face2 'l))))
           (rhs
            (list
             (powerline-raw global-mode-string face2 'r)
             (funcall separator-right face2 face1)
             (unless window-system
               (powerline-raw
                (char-to-string 57505)
                face1 'l))
             (powerline-vc face1 'r)
             (funcall separator-right face1 face0)
             (powerline-raw "%4l" face0 'l)
             (powerline-raw ":" face0 'l)
             (powerline-raw "%3c" face0 'r))))
        (concat
         (powerline-render lhs)
         (powerline-fill face2
                         (powerline-width rhs))
         (powerline-render rhs))))))
 '(mode-line-percent-position nil)
 '(nyan-animate-nyancat t)
 '(nyan-bar-length 20)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   '(dashboard pdf-tools caml lsp-ui tuareg auto-highlight-symbol which-key-posframe marginalia mini-frame powerline selectrum flycheck-posframe lsp-mode nyan-mode diminish treemacs-all-the-icons ethan-wspace lsp-pyright typescript-mode which-key which-keyc markdown-preview-mode rustic vscode-dark-plus-theme modern-cpp-font-lock company-coq exec-path-from-shell csv-mode merlin-company magit company-lsp auctex proof-general lsp-ocaml lsp-treemacs company use-package treemacs pyvenv projectile flycheck))
 '(powerline-default-separator 'utf-8)
 '(selectrum-max-window-height 4)
 '(treemacs-width 20)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp)))
 '(which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "ADBO" :family "JetBrains Mono"))))
 '(compilation-error ((t (:inherit nil :foreground "red" :underline (:color "light pink" :style wave)))))
 '(flycheck-posframe-error-face ((t (:foreground "red" :inherit flycheck-posframe-face))))
 '(flycheck-posframe-info-face ((t (:foreground "green" :inherit flycheck-posframe-face))))
 '(flycheck-posframe-warning-face ((t (:foreground "gold" :inherit flycheck-posframe-face))))
 '(mode-line ((t (:background "dark cyan" :foreground "white" :weight normal))))
 '(mode-line-buffer-id ((t (:foreground "gold" :weight normal))))
 '(mode-line-inactive ((t (:background "dark cyan" :foreground "gray" :weight normal))))
 '(powerline-active1 ((t (:foreground "white" :background "grey11" :inherit mode-line))))
 '(powerline-active2 ((t (:foreground "white" :background "grey20" :inherit mode-line))))
 '(scroll-bar ((t nil))))
