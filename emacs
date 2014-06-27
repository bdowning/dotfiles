;;;; -*- mode: emacs-lisp -*-
;;;; bdowning's .emacs

(require 'cl)

(setq confirm-kill-emacs 'yes-or-no-p)

(defvar local-library-directory "~/.emacs.d/lib")
(defun libdir (more)
  (concatenate 'string local-library-directory "/" more))

;;; load paths
(add-to-list 'load-path local-library-directory)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; start server
(server-start)

;;; some "big switch" parameters
(setq using-ispell t)
(setq ispell-program-name "aspell")
(setq using-tramp t)
(setq using-parenface t)
(setq using-slime t)
(setq inferior-lisp-program "sbcl")

(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-auto-revert-mode 1)
(setq require-final-newline 'query)

(global-unset-key "\C-z") ; iconify-or-deiconify-frame (C-x C-z)

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines nil)

;;; parenface
(when using-parenface
  (require 'parenface))

;;; trampage
(when using-tramp
  (require 'tramp))

;;; mutt files
(defvar mutt-file-pattern "mutt-[a-z]+-[0-9]+-[0-9]+\\'"
  "Regular expression which matches Mutt's temporary files.")
(defun mutt-edit-message ()
  (interactive)
  (message-mode)
  (flyspell-mode)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map (current-local-map))
                   map))
  (local-set-key "\C-c\C-c" (lambda ()
                              (interactive)
                              (save-buffer)
                              (server-edit))))
(unless (assq mutt-file-pattern auto-mode-alist)
  (setq auto-mode-alist
       (cons (cons mutt-file-pattern 'mutt-edit-message)
             auto-mode-alist)))

;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; attempt (ha!) to fix scrolling
(defun scroll-line-by-line ()
  (make-local-variable 'scroll-step)
  (make-local-variable 'scroll-conservatively)
  (setq scroll-up-aggressively 0.0
        scroll-down-aggressively 0.0
        scroll-step 1
        scroll-conservatively 100000))

(set-default 'scroll-up-aggressively 0.0)
(set-default 'scroll-down-aggressively 0.0)
(setq scroll-step 5
      scroll-conservatively 0)

;;; icomplete+
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;;; global keybindings
(global-unset-key "\C-l")
(global-set-key "\C-l\C-l" 'recenter)
(global-set-key "\C-x\C-b" 'ibuffer)

(defun resize-rows (rows)
  (interactive "nNew height:  ")
  (when (> rows 10)
    (modify-frame-parameters nil `((height . ,rows)))))
(global-set-key "\C-l\C-r" 'resize-rows)
(defun resize-columns (cols)
  (interactive "nNew width:  ")
  (when (> cols 10)
    (modify-frame-parameters nil `((width . ,cols)))))
(global-set-key "\C-l\C-c" 'resize-columns)
(defun new-font (font)
  (interactive "sNew font:  ")
  (modify-frame-parameters nil `((font . ,font))))
(global-set-key "\C-l\C-f" 'new-font)
(global-set-key "\C-l\C-g" 'goto-line)
(global-set-key "\C-l\C-s" (lambda () (interactive) (eshell t)))

(global-set-key "\C-l\C-p" (lambda () (interactive) (other-window -1)))
(global-set-key [?\C-l ?\C-,] 'other-window)
(global-set-key "\C-l," 'other-window)
(global-set-key [?\C-l ?\C-\;] 'other-frame)
(global-set-key "\C-l;" 'other-frame)

;;; Minor modes
(global-font-lock-mode 1)
(iswitchb-mode 1)
(column-number-mode 1)
(progn
  (winner-mode 1)
  (global-set-key [24 left] 'winner-undo)
  (global-set-key [24 right] 'winner-redo))

;; fill mode
(setq colon-double-space t)

;; Transient mark mode, but always use mark
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;;; disabled features
(put 'narrow-to-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;;; c-mode
(setq-default c-basic-offset 4)
(setq-default c-default-style '((java-mode . "java") (other . "K&R")))
(setq-default c-cleanup-list '(brace-else-brace
                               brace-elseif-brace
                               brace-catch-brace
                               defun-close-semi
                               list-close-comma
                               scope-operator))
(defun insert-c-arrow ()
  (interactive)
  (insert "->"))
(defun set-insert-c-arrow ()
  (local-set-key '[67108910] (quote insert-c-arrow)))
(add-hook 'c-mode-hook 'set-insert-c-arrow)
(add-hook 'c++-mode-hook 'set-insert-c-arrow)

;;; perl-mode
(add-hook 'perl-mode-hook 'set-insert-c-arrow)

;;; lisp
;; slime
(require 'eldoc)
(when using-slime
  (add-to-list 'load-path (libdir "slime"))
  (add-to-list 'load-path (libdir "slime/contrib"))
  (require 'slime)
  (require 'slime-fancy)
  (setf slime-net-coding-system 'utf-8-unix)
  (load-library "cl-indent"))

(modify-syntax-entry ?\[ "(]  " lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[  " lisp-mode-syntax-table)

(defun kill-sexp-up-tree (arg)
  "Kills one sexp up the tree, replacing with the arg sexps
   after the mark."
  (interactive "*p")
  (kill-sexp arg)
  (backward-up-list)
  (kill-sexp)
  (yank 2)
  (backward-sexp arg))
(defun move-past-close-and-space ()
  "bleh"
  (interactive)
  (backward-up-list)
  (forward-sexp)
  (insert " "))

(defun custom-lisp-keybindings ()
  (local-set-key "[" 'insert-parentheses)
  (local-set-key "]" 'move-past-close-and-reindent)
  (local-set-key "}" 'move-past-close-and-space)
  (local-set-key [\C-\M-backspace] 'backward-kill-sexp)
  (local-set-key "\C-l\C-k" 'kill-sexp-up-tree)
  (local-set-key "\M-[" (lambda () (interactive) (insert "[")))
  (local-set-key "\M-]" (lambda () (interactive) (insert "]"))))

(defun use-indentation (indent-function)
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function indent-function))
(defun use-elisp-indentation () (use-indentation 'lisp-indent-function))
(defun use-cl-indentation () (use-indentation 'common-lisp-indent-function))
(defun use-scheme-indentation () (use-indentation 'scheme-indent-function))

(defun basic-cl-setup ()
  (custom-lisp-keybindings)
  (use-cl-indentation))
(defun basic-elisp-setup ()
  (custom-lisp-keybindings)
  (use-elisp-indentation)
  (eldoc-mode 1))
(defun basic-scheme-setup ()
  (custom-lisp-keybindings)
  (use-scheme-indentation))

(add-hook 'lisp-mode-hook 'basic-cl-setup)
(add-hook 'inferior-lisp-mode-hook 'basic-cl-setup)
(when using-slime
  (dolist (map (list slime-mode-map 
                     slime-repl-mode-map))
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\C-c\C-i" 'slime-fuzzy-complete-symbol)
    (define-key map "\C-l\C-q" 'slime-reindent-defun))
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (add-hook 'slime-repl-mode-hook 'basic-cl-setup)
  (add-hook 'slime-repl-mode-hook 'scroll-line-by-line))
(add-hook 'emacs-lisp-mode-hook 'basic-elisp-setup)
(add-hook 'lisp-interaction-mode-hook 'basic-elisp-setup)
(add-hook 'ielm-mode-hook 'basic-elisp-setup)
(add-hook 'scheme-mode-hook 'basic-scheme-setup)

(setq common-lisp-hyperspec-root 
      "file:///usr/share/doc/hyperspec/")

;;; lua
(setq lua-indent-level 4)
(setq lua-electric-flag nil)
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;; rnc
(autoload 'rnc-mode "rnc-mode" "Relax NG Compact schema editing mode." t)

(setq auto-mode-alist
      (cons '("\\.rnc\\'" . rnc-mode)
            auto-mode-alist))

;;; miscellaneous configuration
;; I hate typing "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Changes in file saving policy:
;; * No backup files, but still use autosave.
;; * Try hard to break hard links.
(setq make-backup-files             nil
      file-precious-flag            t
      find-file-existing-other-name nil)

;; Prompt before evaluating local bits of lisp.  This stops people
;; putting things at the end of files which delete all your files!
(setq enable-local-variables t
      enable-local-eval      1)

;;; font setup
(modify-all-frames-parameters
 '((font . "Consolas:pixelsize=13")))

(autoload 'dtrt-indent-mode "dtrt-indent"
  "Adapt to foreign indentation offsets" t)
(add-hook 'c-mode-common-hook '(lambda () (dtrt-indent-mode 1)))

(add-to-list 'load-path (libdir "omake-mode"))
(load-library "omake")

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Livescript-mode
(add-to-list 'load-path (libdir "livescript-mode"))
(load-library "livescript-mode")

(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil nil (frame))
 '(blink-matching-paren-on-screen nil)
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "K&R") (other . "K&R"))))
 '(case-fold-search t)
 '(coffee-tab-width 2)
 '(cperl-indent-level 4)
 '(current-language-environment "English")
 '(custom-safe-themes (quote ("bf9d5728e674bde6a112979bd830cc90327850aaaf2e6f3cc4654f077146b406" default)))
 '(dns-mode-soa-auto-increment-serial nil)
 '(ecb-layout-window-sizes (quote (("left8" (0.3333333333333333 . 0.29577464788732394) (0.3333333333333333 . 0.23943661971830985) (0.3333333333333333 . 0.28169014084507044) (0.3333333333333333 . 0.16901408450704225)))))
 '(glasses-face nil)
 '(glasses-original-separator "")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(inferior-lisp-buffer nil t)
 '(inferior-lisp-load-command "(load \"%s\")
;; ")
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(iswitchb-buffer-ignore (quote ("^ " "^\\*tramp")))
 '(iswitchb-default-method (quote samewindow))
 '(iswitchb-max-to-show 10)
 '(js2-auto-indent-flag nil)
 '(js2-bounce-indent-flag nil)
 '(js2-enter-indents-newline nil)
 '(js2-mirror-mode nil)
 '(js2-rebind-eol-bol-keys nil)
 '(line-move-visual nil)
 '(lisp-indent-maximum-backtracking 5)
 '(lisp-loop-forms-indentation 6)
 '(lisp-loop-keyword-indentation 6)
 '(lisp-prefix-match-indentation t)
 '(make-backup-files nil)
 '(mouse-1-click-follows-link nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-progressive-speed nil)
 '(nxml-sexp-element-flag t)
 '(org-agenda-files (quote ("~/Documents/org/pomodoro.org" "~/projects/somat/docs/ng/planning-bs.org")))
 '(pascal-indent-level 2)
 '(prolog-program-name (quote (((getenv "EPROLOG") (eval (getenv "EPROLOG"))) (eclipse "eclipse") (mercury nil) (sicstus "sicstus") (swi "prolog") (gnu "gprolog") (t "prolog"))))
 '(rmail-summary-scroll-between-messages t)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 5)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-conservative-indentation nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-follow-symlinks t)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Arch)))
 '(w3m-key-binding (quote info)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "green"))))
 '(erc-nick-default-face ((t (:foreground "cyan" :weight bold))) t)
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "Red"))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray40" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face :foreground "gray40" :height 0.8))))
 '(message-cited-text-face ((((class color) (background dark)) (:foreground "green"))) t)
 '(paren-face ((((class color)) (:foreground "gray51"))))
 '(show-paren-match ((((class color)) (:background "darkgreen" :foreground "green"))))
 '(tex-verbatim ((t nil)) t))

;; (desktop-load-default)
;; (desktop-read)
