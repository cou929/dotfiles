;;;; .emacs
;;; obsolute configures are in ~/memo/knowledge/dotemacs-obsolutes.md

;;; add ~/elisp/ to load-path
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elisp")
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

;;; bind ctrl-h to BackSpace
(global-set-key "\C-h" 'delete-backward-char)

;;; my eamil address, for yasnippet's "email" on text-mode
(setq user-mail-address "cou929@gmail.com")

;;; highlight ()
(show-paren-mode 1)

;;; hide menu bar
(menu-bar-mode -1)

;;; compilation window size
(setq compilation-window-height 8)

;;; remove beep
(setq ring-bell-function 'ignore)

;;; not use tab for indent
(setq-default indent-tabs-mode nil)

;;; tab width, 2 spaces
(setq-default tab-width 2)

;;; escape making backup *~ file
(setq backup-inhibited t)

;;; use option as meta-key
(setq mac-option-modifier 'meta)

;;; bind dabbrev-expand (auto complete) to C-o
(global-set-key "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)

;;; color configurations
(if window-system (progn
                    (set-background-color "Black")
                    (set-foreground-color "LightGray")
                    (set-cursor-color "Gray")
                    (set-frame-parameter nil 'alpha 80)
                    ))

;;; flymake
;; (require 'flymake)
;; (defun flymake-cc-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "g++" (list "-Wall" "-Wextra" local-file))))

;; (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)))

;;; install-elisp
;; http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;;; auto-save-buffers.el
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

;;; in c mode, use flyspell only for comemnt and string
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              ;; enable flyspell-prog-mode
;;              (flyspell-prog-mode)
;;              ))


;;; anything.el
;; bind anything to C-x b
(require 'info)

(require 'anything)
(require 'anything-config)
(setq anything-sources
      '(anything-c-source-buffers+
        ;; anything-c-source-git-project-for-modified
        ;; anything-c-source-git-project-for-all
        anything-c-source-recentf
        anything-c-source-man-pages
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-bookmarks
        anything-c-source-info-pages
        anything-c-source-files-in-current-dir
        ))
(define-key global-map (kbd "C-x b") 'anything)

;;; close compilation window if it ends with no problem
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

;;; cperl-mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-label-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-font-lock t)

;;; js2.el
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; espresso for indentation
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
                                        ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  ;; (define-key js2-mode-map "\C-\M-\\"
  ;;   '(lambda()
  ;;      (interactive)
  ;;      (insert "/* -----[ ")
  ;;      (save-excursion
  ;;        (insert " ]----- */"))
  ;;      ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
                                        ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
                                        ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;; unicode and language settings
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; ChangeLog.txt
(defun memo ()
  (interactive)
  (add-change-log-entry
   nil
   (expand-file-name "~/memo/ChangeLog.txt")))
(define-key ctl-x-map "M" 'memo)

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

;;; font setting
;(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
;(set-fontset-font "fontset-menlokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))

;;; some confs from
;;; http://jesselegg.com/archives/2010/02/25/emacs-python-programmers-part-1/
;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;; flymake for python
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "/Users/kosei/projects/utils/pycheckers"  (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (load-library "flymake-cursor")
;; (global-set-key [f10] 'flymake-goto-prev-error)
;; (global-set-key [f11] 'flymake-goto-next-error)

;;; php-mode
(require 'php-mode)

;;; flymake for perl
;; (defvar flymake-perl-err-line-patterns '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
;; (defconst flymake-allowed-perl-file-name-masks '(("\\.pl$" flymake-perl-init)
;;                                                  ("\\.pm$" flymake-perl-init)
;;                                                  ("\\.t$" flymake-perl-init)
;;                                                  ))

;; (defun flymake-perl-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "perl" (list "-wc" local-file))))

;; (defun flymake-perl-load ()
;;   (interactive)
;;   (set-perl5lib)
;;   (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;     (setq flymake-check-was-interrupted t))
;;   (ad-activate 'flymake-post-syntax-check)
;;   (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
;;   (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
;;   (flymake-mode t))

;; (add-hook 'cperl-mode-hook '(lambda () (flymake-perl-load)))

;; (defun next-flymake-error ()
;;   (interactive)
;;   (flymake-goto-next-error)
;;   (let ((err (get-char-property (point) 'help-echo)))
;;     (when err
;;       (message err))))
;; (global-set-key "\C-ce" 'next-flymake-error)

;;; perltidy
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))
;; (global-set-key "\C-ct" 'perltidy-region)
;; (global-set-key "\C-c\C-t" 'perltidy-defun)
(add-hook 'cperl-mode-hook '(lambda ()
                               (define-key cperl-mode-map "\C-ct" 'perltidy-region)))
(add-hook 'cperl-mode-hook '(lambda ()
                               (define-key cperl-mode-map "\C-c\C-t" 'perltidy-defun)))

;;; Visualize zenkaku-space, tab and line end white spaces
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "Red"))) nil)
(defface my-face-u-1 '((t (:background "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))) t)

;;; html-mode suffixes
(add-to-list 'auto-mode-alist '("\\.tt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.xt$" . html-mode))

;;; cperl-mode suffixes
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;;; ruby-mode suffixes
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . ruby-mode))

;;; pythontidy
(defun pytidy-whole-buffer ()
  (interactive)
  (let ((a (point)))
    (shell-command-on-region (point-min) (point-max) "PythonTidy.py" t)
    (goto-char a)))
(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map "\C-ct" 'pytidy-whole-buffer)))

;;; grep
(define-key global-map (kbd "C-x g") 'grep)
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;; rust-mode
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; gtags
(setq gtags-prefix-key "\C-c")
(require 'gtags)
(require 'anything-gtags)
(setq gtags-mode-hook
      '(lambda ()
         (define-key gtags-mode-map "\C-cs" 'gtags-find-symbol)
         (define-key gtags-mode-map "\C-cr" 'gtags-find-rtag)
         (define-key gtags-mode-map "\C-ct" 'gtags-find-tag)
         (define-key gtags-mode-map "\C-cf" 'gtags-parse-file)
         (define-key gtags-mode-map "\C-cb" 'gtags-pop-stack)))
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)))
(add-hook 'js2-mode-hook
          '(lambda()
             (gtags-mode 1)))
(add-hook 'cperl-mode-hook
          '(lambda()
             (gtags-mode 1)))
(add-hook 'ruby-mode-hook
          '(lambda()
             (gtags-mode 1)))


;;; anything with git project
;;; http://yaotti.hatenablog.com/entry/20101216/1292500323
;;; TODO: use this as anything-source

(defun anything-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "")
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

(defun anything-git-project ()
  (interactive)
  (let* ((pwd default-directory)
         (sources (anything-c-sources-git-project-for pwd)))
    (anything-other-buffer sources
     (format "*Anything git project in %s*" pwd))))
(define-key global-map (kbd "C-x n") 'anything-git-project)
