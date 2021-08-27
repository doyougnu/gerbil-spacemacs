;;; packages.el --- gerbil layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <doyougnu@7thChamber>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `gerbil-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `gerbil/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `gerbil/pre-init-PACKAGE' and/or
;;   `gerbil/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst gerbil-packages '(auto-highlight-symbol
                            eldoc
                            evil-cleverparens
                            parinfer
                            ggtags
                            counsel-gtags
                            helm-gtags
                            rainbow-identifiers
                            rainbow-delimiters
                            smartparens
                            company
                            (gambit-mode :location local)
                            (gerbil-mode :location local :requires comint)
                            ))


(defun gerbil/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'gerbil-mode)))


(defun gerbil/post-init-linum ()
  (when gerbil-enable-linum
      (add-hook 'gerbil-mode #'linum-mode)))

(defun gerbil/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'gerbil-mode)))

(defun gerbil/post-init-evil ()
  (defadvice scheme-send-region (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it)))

(defun gerbil/post-init-rainbow-identifiers ()
  (add-hook 'gerbil-mode-hook #'colors//rainbow-identifiers-ignore-keywords))

(defun gerbil/post-init-rainbow-delimiters()
  (add-hook 'gerbil-mode-hook #'rainbow-delimiters-mode))

(defun gerbil/post-init-parinfer ()
  (add-hook 'gerbil-mode-hook 'parinfer-mode))

(defun gerbil/post-init-smartparens ()
  (add-hook 'gerbil-mode-hook      #'spacemacs//activate-smartparens)
  (add-hook 'gerbil-repl-mode-hook #'spacemacs//activate-smartparens)
  (with-eval-after-load 'smartparens
    (sp-local-pair 'gerbil-mode  "`" nil :actions :rem)
    (sp-local-pair 'gerbil-mode  "'" nil :actions :rem)))

(defun gerbil/post-init-eldoc ()
  (add-hook 'gerbil-mode-hook 'eldoc-mode)
  (add-hook 'gerbil-repl-mode-hook 'eldoc-mode)
  (add-hook 'gerbil-interaction-mode-hook 'eldoc-mode))

(defun gerbil/init-gambit-mode ()
  (use-package gambit-mode
    :defer t
    :config
    (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
    (add-hook 'scheme-mode-hook (function gambit-mode))))

(defun gerbil/init-gerbil-mode ()
  (use-package gerbil-mode
    :defer t
    :mode (("\\.ss\\'"  . gerbil-mode)
           ("\\.scm\\'" . gerbil-mode)
           ("\\.pkg\\'" . gerbil-mode))
    :config
    (progn
      ;; dispatch the backend
      (add-hook 'scheme-mode-local-vars-hook #'spacemacs-gerbil//setup-gerbil)

      ;; tags
      ;; (let ((tags (locate-dominating-file default-directory "TAGS")))
      ;;   (when tags (visit-tags-table tags)))
      ;; (visit-tags-table gerbil-src-tags-location)
      ;; (visit-tags-table gerbil-pkg-tags-location)

      ;; keys
      (spacemacs/set-leader-keys-for-major-mode 'gerbil-mode
        "'"  'spacemacs//gerbil-spawn-repl

        "sr" 'scheme-send-region
        "sd" 'scheme-send-definition
        "sD" 'scheme-send-definition-and-go
        "se" 'scheme-send-last-sexp
        "sc" 'clear-comint-buffer
        "sp" 'comint-previous-input
        "sn" 'comint-next-input
        "sK" 'restart-scheme
        "ss" 'gerbil/switch-to-repl

        "ad" 'gerbil/jump-to-enclosing-repl
        "at" 'gerbil/jump-to-top-repl
        "ab" 'gerbil/show-backtrace
        "ae" 'gerbil/show-environment
        "an" 'gerbil/crawl-backtrace-newer
        "ap" 'gerbil/crawl-backtrace-older

        "bl" 'scheme-load-file
        "bL" 'gerbil/load-current-buffer
        "bi" 'gerbil-import-current-buffer
        "br" 'gerbil-reload-current-buffer
        "bc" 'gerbil-compile-current-buffer
        "bb" 'gerbil-build
        "bb" 'gerbil-build-directory)

      (dolist (prefix '(("m'" . "gerbil-repl")
                        ("ma" . "gambit")
                        ("ms" . "repl")
                        ("mb" . "build")
                        ("mh" . "help")
                        ("mg" . "navigation")))
        (spacemacs/declare-prefix-for-mode 'gerbil-mode (car prefix) (cdr prefix))))))

(defun gerbil/post-init-ggtags ()
  (add-hook 'scheme-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun gerbil/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'gerbil-mode))

(defun gerbil/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'gerbil-mode))

(defun gerbil/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes
    gerbil-mode))


;;; packages.el ends here
