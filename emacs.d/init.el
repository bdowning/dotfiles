;; .emacs.d/init.el
;;
;; Parts from emacs-kicker (Dimitri Fontaine <dim@tapoueh.org>)
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

(require 'cl)                           ; common lisp goodies, loop

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move                   ; have to add your own keys
          :after (progn
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name magit                         ; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change              ; move pointer back to last change
          :after (progn
                   ;; when using AZERTY keyboard, consider C-x C-_
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; now set our own packages
(setq
 bd/el-get-packages
 '(el-get                               ; el-get is self-hosting
   color-theme                          ; nice looking emacs
   color-theme-zenburn
   column-enforce-mode
   dtrt-indent
   lua-mode
   paredit
   parenface
   switch-window                        ; takes over C-x o
   ))

(setq bd/el-get-packages
      (append
       bd/el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync bd/el-get-packages)
(el-get-cleanup bd/el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)
(line-number-mode 1)
(column-number-mode 1)
(load-theme 'zenburn t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Consolas:pixelsize=13"))

(global-hl-line-mode)

(setq scroll-conservatively 1000000)
(setq mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control))))

(show-paren-mode 1)
(setq show-paren-delay 0
      blink-matching-paren-on-screen nil)

(iswitchb-mode 1)

(add-hook 'prog-mode-hook 'column-enforce-mode)

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; C-l keybindings
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l C-l") 'recenter)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun bd/resize-rows (rows)
  (interactive "nNew height:  ")
  (when (> rows 10)
    (modify-frame-parameters nil `((height . ,rows)))))
(global-set-key (kbd "C-l C-r") 'bd/resize-rows)
(defun bd/resize-columns (cols)
  (interactive "nNew width:  ")
  (when (> cols 10)
    (modify-frame-parameters nil `((width . ,cols)))))
(global-set-key (kbd "C-l C-c") 'bd/resize-columns)
(defun bd/new-font (font)
  (interactive "sNew font:  ")
  (modify-frame-parameters nil `((font . ,font))))
(global-set-key (kbd "C-l C-f") 'bd/new-font)
(global-set-key (kbd "C-l C-g") 'goto-line)
(global-set-key (kbd "C-l C-s") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "C-l C-;") 'other-frame)
(global-set-key (kbd "C-l ;") 'other-frame)

;; fill-mode
(setq colon-double-space t)

;; Transient mark mode, but always use mark
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;; NO TABS
(setq indent-tabs-mode nil)

;; Disabled features
(put 'narrow-to-region 'disabled nil)

;;; C mode
(setq-default c-basic-offset 4)
(setq-default c-default-style '((java-mode . "java") (other . "K&R")))
(setq-default c-cleanup-list '(brace-else-brace
                               brace-elseif-brace
                               brace-catch-brace
                               defun-close-semi
                               list-close-comma
                               scope-operator))
(defun bd/insert-c-arrow ()
  (interactive)
  (insert "->"))
(defun bd/set-insert-c-arrow ()
  (local-set-key (kbd "C-.") 'bd/insert-c-arrow))
(add-hook 'c-mode-hook 'bd/set-insert-c-arrow)
(add-hook 'c++-mode-hook 'bd/set-insert-c-arrow)

;;; perl-mode
(add-hook 'perl-mode-hook 'bd/set-insert-c-arrow)

;;; lisp
(require 'eldoc)
(require 'paredit)
(require 'parenface)

(define-key paredit-mode-map (kbd "[") 'paredit-open-round)
(define-key paredit-mode-map (kbd "]") 'paredit-close-round-and-newline)
(define-key paredit-mode-map (kbd "}") 'paredit-close-round)
(define-key paredit-mode-map (kbd "M-}") (lambda () (interactive) (insert "}")))
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "(") 'paredit-open-square)
(define-key paredit-mode-map (kbd ")") 'paredit-close-square)
(define-key paredit-mode-map (kbd "C-M-<backspace>") 'bd/paredit-kill-backwards)
(defun bd/paredit-kill-backwards ()
  (interactive)
  (paredit-backward)
  (paredit-kill)
  (if (looking-at "\\\n")
      (paredit-kill)))

(defun bd/use-lisp-indentation (indent-function)
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function indent-function))
(defun bd/basic-lisp-setup ()
  (enable-paredit-mode))
(defun bd/basic-cl-setup ()
  (bd/basic-lisp-setup)
  (bd/use-lisp-indentation 'lisp-indent-function))
(defun bd/basic-elisp-setup ()
  (bd/basic-lisp-setup)
  (bd/use-lisp-indentation 'common-lisp-indent-function)
  (eldoc-mode 1))
(defun bd/basic-scheme-setup ()
  (bd/basic-lisp-setup)
  (bd/use-lisp-indentation 'scheme-indent-function))

(add-hook 'lisp-mode-hook 'bd/basic-cl-setup)
(add-hook 'inferior-lisp-mode-hook 'bd/basic-cl-setup)
(add-hook 'emacs-lisp-mode-hook 'bd/basic-elisp-setup)
(add-hook 'lisp-interaction-mode-hook 'bd/basic-elisp-setup)
(add-hook 'ielm-mode-hook 'bd/basic-elisp-setup)
(add-hook 'scheme-mode-hook 'bd/basic-scheme-setup)

;;; lua
(setq lua-indent-level 4)
(setq lua-electric-flag nil)
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))

;;; omake
(add-to-list 'load-path "~/.emacs.d/lib/omake-mode")
(require 'omake)

;;; miscellaneous configuration
;; I hate typing "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Changes in file saving policy:
;; * No backup files, but still use autosave.
;; * Try hard to break hard links.
(setq make-backup-files             nil
      file-precious-flag            t
      find-file-existing-other-name nil)
