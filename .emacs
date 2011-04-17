;;;; .emacs

;;; set w3m-command
(setq w3m-command "/usr/local/bin/w3m")

;;; add ~/elisp/ to load-path
(setq load-path
      (append
       (list
	(expand-file-name "~/.emacs.d/elisp")
	(expand-file-name "~/.emacs.d")
	(expand-file-name "/usr/share/emacs/site-lisp/w3m")
	)
       load-path))

;;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;;; enable visual feedback on selections
(setq transient-mark-mode t)

;;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;;; call compile command from key shortcat "c-c c".
(define-key mode-specific-map "c" 'compile)

;;;bind ctrl-h to BackSpace
(global-set-key "\C-h" 'delete-backward-char)

;;; flymake
(require 'flymake)
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" local-file))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))

;;; highlight ()
(show-paren-mode 1)

;;; hide menu bar
(menu-bar-mode -1)

;;; bind help command to C-x C-h
(define-key global-map "\C-x\C-h" 'help-command)

;;; bind dabbrev-expand (auto complete) to C-o
(global-set-key "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)

;;; php mode
(require 'php-mode)
(add-hook 'php-mode-hook
      '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;; setting of scheme mode using gauche environment
(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;;; hide menubar, fullscreen, for mac
(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq mac-autohide-menubar-on-maximize t)
              (set-frame-parameter nil 'fullscreen 'fullboth)
              )))

(if window-system (progn
  (setq initial-frame-alist '((width . 200)(height . 58)(top . 0)(left . 0)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")
))

(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;;; Carbon Emacs, hide menu, etc
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )

;;; color configurations
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 80)
   ))

;;; install-elisp
;; http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;;; auto-save-buffers.el 
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

;;; simple-hatena-mode.el
(require 'simple-hatena-mode)
(setq simple-hatena-bin "~/projects/pl/hatenaDiaryWriter/hw.pl")
(setq simple-hatena-use-timestamp-permalink-flag nil)
(setq simple-hatena-time-offset 6)

;;; in c mode, use flyspell only for comemnt and string
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; enable flyspell-prog-mode
             (flyspell-prog-mode)
))

;;; global (gtags) setting
;; (autoload 'gtags-mode "gtags" "" t)
;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-t" 'gtags-find-tag)
;;          (local-set-key "\M-r" 'gtags-find-rtag)
;;          (local-set-key "\M-s" 'gtags-find-symbol)
;;          (local-set-key "\C-t" 'gtags-pop-stack)
;;          ))
;; (add-hook 'c-mode-common-hook
;;           '(lambda()
;;              (gtags-mode 1)
;;              (gtags-make-complete-list)
;;              ))

;;; gdb-many-windows
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t) ; If you don't need "IO buffer", set it to nil.

;;; anything.el
;; bind anything to C-x b
(require 'info)

(require 'anything)
(require 'anything-config)
(setq anything-sources
      '(anything-c-source-buffers+
        anything-c-source-recentf
        anything-c-source-man-pages
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-bookmarks
        anything-c-source-info-pages
        anything-c-source-files-in-current-dir
        ))
(define-key global-map (kbd "C-x b") 'anything)

;;; compilation window size
(setq compilation-window-height 8)

;;; close compilation window if it ends with no problem
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

;;; emacs-w3m
(require 'w3m-load)

;;; cppref.el
(require 'cppref)
;(setq cppref-doc-dir "/Library/Perl/5.8.8/auto/share/dist/cppref") ;; doesn't end with "/"

;;; yasnippet
;; http://code.google.com/p/yasnippet/
(add-to-list 'load-path
	     "~/.emacs.d/elisp/yasnippet-0.6.1c")
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-o"))
;(setq yas/next-field-key (kbd "TAB"))
;; deactivate snippet if the line is in comment
(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
                             (get-char-property (point) 'face))
                    (string= "font-lock-string-face"
                             (get-char-property (point) 'face))))
           '(require-snippet-condition . force-in-comment)))
;; set my original snippet directory
(defvar my-snippet-directories
  (list (expand-file-name "~/.emacs.d/elisp/mySnippets")))
;; initialize
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets")

;;; remove beep
(setq ring-bell-function 'ignore)

;;; not use tab for indent
(setq-default indent-tabs-mode nil)

;;; cperl-mode
(defalias 'perl-mode 'cperl-mode)

;;; my eamil address, for yasnippet's "email" on text-mode
(setq user-mail-address "cou929@gmail.com")

;;; magit
(require 'magit)

;;; tab width, 2 spaces
(setq-default tab-width 2)

;;; js2.el
;;; from http://8-p.info/emacs-javascript.html
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default c-basic-offset 2)
(when (load "js2" t)
  (setq js2-bounce-indent-flag nil)

  (defun indent-and-back-to-indentation ()
    (interactive)
    (indent-for-tab-command)
    (let ((point-of-indentation
           (save-excursion
             (back-to-indentation)
             (point))))
      (skip-chars-forward "\s " point-of-indentation))))

;;; anything-c-yasnippet
;; http://d.hatena.ne.jp/IMAKADO/20080401/1206715770
;; (require 'anything-c-yasnippet)
;; (setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil
;; (global-set-key (kbd "C-c y") 'anything-c-yas-complete) ;C-c yで起動 (同時にお使いのマイナーモードとキーバインドがかぶるかもしれません)
;(add-to-list 'yas/extra-mode-hooks 'ruby-mode-hook)
;(add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

;;; escape making backup *~ file
(setq backup-inhibited t)

;;; multi-term
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/zsh"))

;;; change or make window with C-t
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)
(define-key term-mode-map (kbd "C-t") 'other-window-or-split)

;;; c-eldoc
(load "c-eldoc")
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-idle-delay) 0.20)
            (c-turn-on-eldoc-mode)
            ))

;;; unicode and language settings
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; dont make backup file
(setq backup-inhibited t)

;;; ChangeLog.txt
(defun memo ()
  (interactive)
    (add-change-log-entry 
     nil
     (expand-file-name "~/memo/ChangeLog.txt")))
(define-key ctl-x-map "M" 'memo)

;;; todo.txt
(defun todo ()
  (interactive)
  (expand-file-name "~/memo/todo.txt"))
(define-key ctl-x-map "T" 'todo)

;;; open-junk-file
(require 'open-junk-file)

;;; anything-c-source-info-localmdc
(defvar anything-c-info-localmdc nil)
(defvar anything-c-source-info-localmdc
  `((name . "Info localmdc")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-localmdc
                  (with-temp-buffer
                    (Info-find-node "localmdc" "top")
                    (setq anything-c-info-localmdc (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-localmdc
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "localmdc" "top")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (volatile)
    (requires-pattern . 2)))

;;; rst-mode
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(add-hook 'rst-mode-hook
          (lambda ()
            (setq rst-slides-program "firefox")
            ))

;;; color-moccur
(when (require 'color-moccur nil t)
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  (setq moccur-split-word t)
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (require 'moccur-edit nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nit t))
    (setq moccur-use-migemo t)))

;;; use option as meta-key
(setq mac-option-modifier 'meta)


;;; font setting
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))

;;; tramp
(require 'tramp)

;;; js2-mode.el by mooz
;;; https://github.com/mooz/js2-mode/
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; wdired
(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)
