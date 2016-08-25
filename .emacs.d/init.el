;; afternoon-theme-20140104.1059
;; auto-complete-20150225.715
;; browse-kill-ring-20150104.1237
;; buffer-move-20141010.117
;; col-highlight-20150104.2134
;; color-theme-20080305.34
;; crosshairs-20150104.2139
;; darkburn-theme-20141205.226
;; evil-20150304.49
;; git-commit-mode-20141014.1634
;; git-rebase-mode-20150122.1114
;; goto-chg-20131228.1459
;; hl-line+-20150104.2222
;; idle-highlight-mode-20120920.948
;; magit-20150124.930
;; maxframe-20140916.754
;; popup-20150116.1223
;; smex-20141210.1422
;; undo-tree-20140509.522
;; vkill-20091203.1022
;; vline-20120108.1245
;; window-number-20140124.302

;; emacs-eclim-20140809.207
;; erlang-20150319.456
;; evil-20150313.1208
;; go-mode-20150817.2318
;; popup-20150315.612
;; s-20140910.334
;; scala-mode2-20150617.2350
;; yaml-mode-20141125.37

;; https://github.com/tkf/emacs-jedi

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'thingatpt)
(require 'imenu)
(require 'dired)

(require 'darkburn-theme)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(defun disable-scroll-margin ()
  (set (make-local-variable 'scroll-margin) 0))

(add-hook 'shell-mode-hook 'disable-scroll-margin)
(add-hook 'eshell-mode-hook 'disable-scroll-margin)
(add-hook 'gud-mode-hook 'disable-scroll-margin)
(add-hook 'magit-mode-hook 'disable-scroll-margin)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)
        (auto-complete-mode)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq window-min-width 120)
(setq window-min-height 80)

(setq border-width 0)
;; (set-face-foreground 'vertical-border "#000000")

(unless window-system
  (setq linum-format "%d "))

(add-hook 'before-make-frame-hook '(lambda () (tool-bar-mode -1)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-clipboard-coding-system 'utf-16le-dos)

(when (equal system-type 'darwin)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq eval-expression-print-length 1000)

(setq ring-bell-function 'ignore
      inhibit-startup-message t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      transient-mark-mode t
      color-theme-is-global t
      ;; shift-select-mode t
      delete-selection-mode t
      require-final-newline t
      ffap-machine-p-known 'reject
      truncate-partial-width-windows nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      x-select-enable-clipboard t
      save-place-file (concat dotfiles-dir "places"))

(when window-system
  (setq redisplay-dont-pause t))

(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position 1)

(setq fill-column 120)
(set-fill-column 120)
(setq split-height-threshold 60)

(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)

(random t)

(when (equal system-type 'darwin)
  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell
           (replace-regexp-in-string "[[:space:]\n]*$" ""
                                     (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)

  (set-exec-path-from-shell-PATH)
  (setq system-name (car (split-string system-name "\\.")))

  (when (and window-system (not (equal window-system 'x)))
    (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")

    (setq mac-command-modifier 'meta
          mac-option-modifier 'alt)

    (setq linum-format "  %d "))

  (set-face-attribute 'default nil :height 100))

(when (equal system-type 'windows-nt)
  (set-face-attribute 'default nil :height 80))

(when (equal system-type 'gnu/linux)
  (set-face-attribute 'default nil :height 80)

  (setq linum-format " %d ")

  (defun x-fullscreen ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  
  (defun x-maximize ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

(global-linum-mode t)
(setq line-number-mode t
      column-number-mode t)

(size-indication-mode t)
(auto-compression-mode t)
(global-font-lock-mode t)
(recentf-mode 1)
(show-paren-mode 1)

(defconst use-backup-dir t)
(setq backup-directory-alist `(("." . ,(expand-file-name (concat dotfiles-dir "backups"))))
      auto-save-file-directory (concat (expand-file-name (concat dotfiles-dir "autosaves")) "/")
      auto-save-file-name-transforms `((".*" ,auto-save-file-directory t))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

(unless (file-exists-p auto-save-file-directory)
  (make-directory auto-save-file-directory))

(when (equal system-type 'windows-nt)
  (setq delete-by-moving-to-trash t))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(setq diff-switches "-u -w")

(which-func-mode)

(set-face-background 'which-func "#000000")
(set-face-foreground 'which-func "#FFFFFF")

(setq which-func-format
      '((:propertize "[ " face which-func)
        (:propertize which-func-current
                     local-map ,which-func-keymap
                     face which-func)
        (:propertize " ] " face which-func)))

(setq-default mode-line-format
              '("%e"
                mode-line-mule-info ;;UUU: UU-(DOS)
                mode-line-client
                mode-line-modified
                mode-line-remote ;; -
                mode-line-frame-identification ;; -F1
                " "
                mode-line-buffer-identification
                " "
                mode-line-position
                (which-func-mode ("" which-func-format))
                " "
                (:eval (let ((v vc-mode)) (when (> (length v) 0) (list (replace-regexp-in-string "^ +" "" v) " "))))
                mode-line-modes
                (global-mode-string ("" global-mode-string))
                ))

(setq mode-line-position (list
                          '(:eval (let ((c (point))
                                        (s (- (point-max) (point-min)))
                                        (lc (line-number-at-pos))
                                        (ls (line-number-at-pos (point-max))))
                                    (concat
                                     "%l,%c,"
                                     (number-to-string c)
                                     " "
                                     (if (> ls 0)
                                       (number-to-string (/ (* 100 lc) ls))
                                       "-")
                                     "%% "
                                     (number-to-string s))))
                          " "))

(setq-default mode-line-modes (delete "--" mode-line-modes))
(setq-default mode-line-modified '(:eval (let ((s (format-mode-line "%*%+")))
  (cond ((equal s "--") "") ((equal s "**") "M") ((equal s "%%") "R") ((equal s "%*") "RM") (t s)))))
;; (setq-default mode-line-remote '("%@"))
(setq-default mode-line-buffer-identification (propertized-buffer-identification " %b "))
(setq mode-line-frame-identification '(:eval (chomp (mode-line-frame-control))))

(require 'server)
(setq server-socket-dir (concat dotfiles-dir "server"))
(defun server-ensure-safe-dir (dir) "Noop" t)
(let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
       (server-file (expand-file-name server-name server-dir)))
  (unless (file-exists-p server-dir)
    (make-directory server-dir))
  (unless (file-exists-p server-file)
    (server-start)))

(setq pidfile (concat dotfiles-dir "server/emacs-server.pid"))
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-temp-file pidfile
              (insert (number-to-string (emacs-pid))))))
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (file-exists-p pidfile)
              (delete-file pidfile))))

(require 'auto-complete)
(delq 'ac-source-filename ac-sources) ;; jesus fucking christ
(defun ac-filename-candidate () nil) ;; IT WONT DIE

;; tab display width of 4 columns by default
(setq-default indent-tabs-mode nil) ; Yelp uses spaces not tabs
(setq-default tab-width 4)  ; Normal emacs tab-width
(setq-default py-smart-indentation nil) ; Don't try to guess tab width
(setq-default py-indent-offset 4) ; emacs-for-python setting
(setq-default python-indent 4) ; emacs-for-python setting

(defun customize-py-tabs ()
    (setq indent-tabs-mode nil
        tab-width 4
        py-smart-indentation nil
        py-indent-offset 4
        python-indent 4
    )
 )

(add-hook 'python-mode-hook 'customize-py-tabs)

(define-derived-mode cheetah-mode html-mode "Cheetah"
  (set (make-local-variable 'sgml-basic-offset) 4)
  (make-face 'cheetah-variable-face)
  (setq tab-width 4
		indent-tabs-mode nil)
  (font-lock-add-keywords
   nil '(
		 ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|elif\\|end\\)+\\)\\>" 1 font-lock-type-face)
		 ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
		 ("\\(##.*\\)\n" 1 font-lock-comment-face)
		 ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face)))
  (font-lock-mode 1)
  (setq comment-start "##")
  (setq comment-end ""))

(setq auto-mode-alist (cons '("\\.tmpl\\'" . cheetah-mode) auto-mode-alist))

(ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input t
      comint-prompt-read-only t)

(defun clear-shell ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))

(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun st-on ()
  "Turns debugging on"
  (interactive)
  (setq stack-trace-on-error t))

(defun st-off ()
  "Turns debugging off"
  (interactive)
  (setq stack-trace-on-error nil))

(defun uncd ()
  "Return to the current buffer's directory"
  (interactive)
  (when buffer-file-name
	(cd (file-name-directory buffer-file-name))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
	(replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun filter (condp lst)
  (delq nil
		(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun kill-other-windows ()
  "Kill all other windows."
  (interactive)
  (mapc 'delete-window (delq (selected-window) (window-list))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir (concat dotfiles-dir "/scripts"))
		(generated-autoload-file autoload-file))
	(when (or force-regen
			  (not (file-exists-p autoload-file))
			  (some (lambda (f) (file-newer-than-file-p f autoload-file))
					(directory-files autoload-dir t "\\.el$")))
	  (message "Updating autoloads...")
	  (let (emacs-lisp-mode-hook)
		(update-directory-autoloads autoload-dir))))
  (load autoload-file))

;; source: xemacs 20.3
(defun count-words-region (start end)
  (interactive "r")
  (save-excursion
	(let ((n 0))
	  (goto-char start)
	  (while (< (point) end)
	(if (forward-word 1)
		(setq n (1+ n))))
	  (message "Region has %d words" n)
	  n)))

(defun count-lines-words-region (start end)
  "Print number of lines words and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d words, %d characters"
	   (count-lines start end)
	   (count-words-region start end)
	   (- end start)))

(defun query-string-swap (string-a string-b)
  "Swap A and B strings in current buffer."
  (interactive "sString A: \nsString B: ")
  (query-replace-regexp
   (concat "\\(\\(" string-a "\\)\\" string-b "\\)")
   `(replace-eval-replacement
	 replace-quote
	 (if (match-string 2) ,string-b ,string-a))
   nil
   (if (and transient-mark-mode mark-active) (region-beginning))
   (if (and transient-mark-mode mark-active) (region-end))))

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (replace-match ""))))

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "\n" nil t)
	  (replace-match "\r\n"))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
	(if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
	  (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(defun ts ()
  "Spit out the current time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun reload-buffer ()
  "revert-buffer without confirmation"
  (interactive)
  (revert-buffer t t))

(defun other-frame--1 ()
  (interactive)
  (other-frame -1))

(defun other-window--1 ()
  (interactive)
  (other-window -1))

(defun occurrences (regexp &rest ignore)
  "Show all matches for REGEXP in an `occur' buffer."
  ;; keep text covered by occur-prefix and match text-properties
  (interactive (occur-read-primary-args))
  (occur regexp)
  (with-current-buffer (get-buffer "*Occur*")
	(let ((inhibit-read-only t)
	  delete-from
	  pos)
	  (save-excursion
	(while (setq pos (next-property-change (point)))
	  (goto-char pos)
	  (if (not (or (get-text-property (point) 'occur-prefix)
			   (get-text-property (point) 'occur-match)))
		  (if delete-from
		  (delete-region delete-from (point))
		(setq delete-from (point)))
		(when delete-from
		  (delete-region delete-from (point))
		  (if (get-text-property (point) 'occur-prefix)
		  (insert "\n")
		(insert " ")))
		(setq delete-from nil)))))))

(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
 unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
	  (save-excursion
	(set-buffer (get-buffer "*Occur*"))
	(goto-char (point-min))
	(toggle-read-only 0)
	(if (looking-at "^[0-9]+ lines matching \"")
		(kill-line 1))
	(while (re-search-forward "^[ \t]*[0-9]+:"
				  (point-max)
				  t)
	  (replace-match "")
	  (forward-line 1)))
	(message "There is no buffer named \"*Occur*\".")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
	  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
	(error (message "Invalid expression")
		   (insert (current-kill 0)))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (concat dotfiles-dir "scripts/") 0))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
	  (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
	(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
		  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
		  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
		  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
		  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
		  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
		  "culpa qui officia deserunt mollit anim id est laborum."))

(defun dired-root-same-buffer ()
  "Moves to root (parent) directory in dired."
  (interactive)
  (find-alternate-file ".."))

(defun buf-move-either-horiz ()
  "Swap the current buffer and the buffer on the right or left of the split."
  (interactive)
  (let* ((other-win-left (windmove-find-other-window 'left))
		 (other-win-right (windmove-find-other-window 'right)))
	(if other-win-left
		(buf-move-left)
	  (if other-win-right
		  (buf-move-right)))))

(defun buf-move-either-vert ()
  "Swap the current buffer and the buffer on the down or up of the split."
  (interactive)
  (let* ((other-win-up (windmove-find-other-window 'up))
		 (other-win-down (windmove-find-other-window 'down)))
	(if other-win-up
		(buf-move-up)
	  (if other-win-down
		  (buf-move-down)))))

(defun split-4 ()
  "Split window 4 ways"
  (interactive)
  (let ((window-1 (selected-window))
		 (window-3 (split-window-horizontally)))
	(split-window-vertically)
	(select-window window-3)
	(split-window-vertically)))

;; make generic, maybe? lol
(defun split-3 ()
  "Split window 6 ways"
  (interactive)
  (split-window-horizontally-damn-it)
  (split-window-horizontally-damn-it)
  (balance-windows-damn-it))

;; make generic, maybe? lol
(defun split-6 ()
  "Split window 6 ways"
  (interactive)
  (let ((window-1 (selected-window))
		(window-3 (split-window-horizontally))
		(window-5 (split-window-horizontally)))
	(split-window-vertically)
	(select-window window-3)
	(split-window-vertically)
	(select-window window-5)
	(split-window-vertically)
	(select-window window-1)
	(balance-windows)))

(defun annotated-copy-region-as-kill (beg end)
  (interactive "r")
  (kill-new (concat
			 buffer-file-name
			 ":"
			 (number-to-string (line-number-at-pos))
			 "\n"
			 (filter-buffer-substring beg end)))
  (setq deactivate-mark t)
  nil)

(defun split-window-vertically-damn-it ()
  (interactive)
  (let ((previous-window-min-height window-min-height))
	(unwind-protect
		(progn
		  (setq window-min-height 4)
		  (split-window-vertically))
	  (setq window-min-height previous-window-min-height))))

(defun split-window-horizontally-damn-it ()
  (interactive)
  (let ((previous-window-min-width window-min-width))
	(unwind-protect
		(progn
		  (setq window-min-width 4)
		  (split-window-horizontally))
	  (setq window-min-width previous-window-min-width))))

(defun balance-windows-damn-it ()
  (interactive)
  (let (
		(previous-window-min-width window-min-width)
		(previous-window-min-height window-min-height))
	(unwind-protect
		(progn
		  (setq window-min-height 4)
		  (setq window-min-width 4)
		  (balance-windows))
	  (setq window-min-width previous-window-min-width)
	  (setq window-min-height previous-window-min-height))))

(require 'eshell)
(defun new-eshell ()
  (interactive)
  (eshell t))

(defun eshell-banner-initialize ())

(defun delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
	(if (not (bolp)) (forward-line 1))
	(delete-whitespace-rectangle (point) end nil)))
(setq eshell-cmpl-cycle-completions nil
      eshell-scroll-to-bottom-on-input t
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (eshell/export "TERM" "dumb")))
     (when (< emacs-major-version 23)
       (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
                 '(lambda ()
                    ;; move these to binds
                    (define-key eshell-mode-map "\C-l" 'eshell/clear)
                    (define-key eshell-mode-map "\C-a" 'eshell-bol)))
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(setq eshell-buffer-shorthand t)

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(setq evil-toggle-key "C-\\")
(setq evil-default-cursor '("#73d216" t))
(setq evil-mode-line-format 'before)

(require 'evil)
(evil-mode)

(mapc (lambda (mode)
        (add-to-list 'evil-emacs-state-modes mode))
      '(dired-mode Buffer-menu-mode package-menu-mode magit-status-mode magit-key-mode shell-mode eshell-mode))

(mapc (lambda (mode)
        (add-to-list 'evil-insert-state-modes mode))
      '(magit-log-edit-mode))

(defun evil-ex-call-current-command ()
  "Execute the given command COMMAND."
  (if (not evil-ex-current-cmd)
      (if (not evil-ex-current-range)
          (error "Invalid ex-command.")
        (let ((pos (car evil-ex-current-range)))
          (if pos (goto-line (car pos)))))
    (let ((binding (evil-ex-completed-binding evil-ex-current-cmd)))
      (if binding
          (with-current-buffer evil-ex-current-buffer
            (save-excursion
              (let ((range (evil-ex-get-current-range))
                    prefix-arg)
                (when (and (not range)
                           evil-ex-current-range
                           (car evil-ex-current-range)
                           (numberp (caar evil-ex-current-range)))
                  (setq prefix-arg (caar evil-ex-current-range)))
                (call-interactively binding))))
        (error "Unknown command %s" evil-ex-current-cmd)))))

(evil-define-motion evil-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (or count scroll-margin))
  (back-to-indentation))

(evil-define-motion evil-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (or count (+ 1 scroll-margin))))
  (back-to-indentation))

(defun evil-open-above-no-insert (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun evil-open-below-no-insert (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun evil-next-line-10 ()
  (interactive)
  (evil-next-line 10))
 
(defun evil-previous-line-10 ()
  (interactive)
  (evil-previous-line 10))

(defface evil-vi-state-id-face
  '((t (:background "#000000" :foreground "#4545FF")))
  "Vi state modeline face"
  :group 'evil-mode)
(defface evil-emacs-state-id-face
  '((t (:background "#000000" :foreground "#FF3333")))
  "Emacs state modeline face"
  :group 'evil-mode)
(defface evil-insert-state-id-face
  '((t (:background "#000000" :foreground "#33FF33")))
  "Insert state modeline face"
  :group 'evil-mode)
(defface evil-visual-state-id-face
  '((t (:background "#000000" :foreground "#AD1EC9")))
  "Visual state modeline face"
  :group 'evil-mode)
(defface evil-replace-state-id-face
  '((t (:background "#000000" :foreground "#FFFF33")))
  "Replace state modeline face"
  :group 'evil-mode)
(defface evil-motion-state-id-face
  '((t (:background "#000000" :foreground "#FF9500")))
  "Motion state modeline face"
  :group 'evil-mode)
(defface evil-operator-state-id-face
  '((t (:background "#000000" :foreground "#FF9500")))
  "Operator state modeline face"
  :group 'evil-mode)

(setq evil-normal-state-tag (propertize " <N> " 'face 'evil-vi-state-id-face))
(setq evil-emacs-state-tag (propertize " <E> " 'face 'evil-emacs-state-id-face))
(setq evil-visual-state-tag (propertize " <V> " 'face 'evil-visual-state-id-face))
(setq evil-insert-state-tag (propertize " <I> " 'face 'evil-insert-state-id-face))
(setq evil-motion-state-tag (propertize " <M> " 'face 'evil-motion-state-id-face))
(setq evil-replace-state-tag (propertize " <R> " 'face 'evil-replace-state-id-face))
(setq evil-operator-state-tag (propertize " <O> " 'face 'evil-operator-state-id-face))

(require 'window-number)

(window-number-mode)
(window-number-meta-mode)

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " -" (number-to-string (window-number)) "- ")
   'face
   'window-number-face))

(set-face-background 'window-number-face "#000000")

(when (equal system-type 'darwin)
  (window-number-define-keys window-number-meta-mode-map "A-"))

(defun window-number-select (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (if (= 10 number)
          (select-window (minibuffer-window))
        (let ((window (nth (1- number) (window-number-list))))
          (if (and window (or (not (window-minibuffer-p window))
                              (minibuffer-window-active-p window)))
              (select-window window)
            (error "No such window."))))))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'idle-highlight-mode)
(defun turn-on-idle-highlight-mode ()
  (interactive)
  (idle-highlight-mode t))
(add-hook 'after-change-major-mode-hook 'turn-on-idle-highlight-mode)

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(setq minor-mode-alist
      (filter
       '(lambda (x)
          (not (memq (car x) '(idle-highlight-mode
                               hi-lock-mode
                               auto-complete-mode
                               yas/minor-mode
                               window-number-meta-mode
                               window-number-mode))))
       minor-mode-alist))

(add-to-list 'mode-line-position
             '(:eval (window-number-string)) t 'eq)

(defvar symbol-replacement-list '())
(setq symbol-replacement-list
	  '(
		("lambda" . "λ")
		;; ("sum" . "∑")
		;; ("<=" . "≤")
		;; (">=" . "≥")
		;; ("!=" . "≠")
		;; ("sqrt" . "√")
		;; ("pi" . "π")
		;; ("tau" . "τ")
		;; ("phi" . "Φ")
		;; ("range" . "ℝ")
		;; ("xrange" . "ℤ")
		;; ("for" . "∀")
		;; ("in" . "∈")
		("not" . "¬")
		("and" . "∧")
		("or" . "∨")
		("xor" . "⊻")
		;; ("union" . "∪")
		;; ("any" . "∃")
		("True" . "Δ")
		("False" . "∇")
		("None" . "ε") ; ∅")
		;; ("assert" . "⊦")
		;; ("map" . "✈")
		("self" . "◊")
		;; ("return" . "←")
		;; ("yield" . "→")
		;; ("continue" . "↑")
		;; ("break" . "↓")
		))

(defun add-symbol-replacement-font-lock-keywords (replacement-list)
  (font-lock-add-keywords
   nil
   (mapcar
	(lambda (pair)
	  (let ((from (car pair)) (to (cdr pair)))
		`(,(concat "\\<" from "\\>")
		  (0 (progn (compose-region (match-beginning 0) (match-end 0) ,to) nil)))))
	replacement-list)))

(defun lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
	  (0 (progn (compose-region (match-beginning 0) (match-end 0)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

(add-hook 'python-mode-hook (lambda () (add-symbol-replacement-font-lock-keywords symbol-replacement-list)))

(defun python-insert-pdb-breakpoint ()
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands) ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'browse-kill-ring)
(global-set-key (kbd "C-x y") 'browse-kill-ring)

(require 'magit)
(when (equal system-type 'windows-nt)
  (let* ((gits (mapcar 'chomp (delete "" (split-string (shell-command-to-string "where git") "\n"))))
         (gitexes (delete nil (mapcar (lambda (x) (when (and (< 4 (length x))
                                                             (equal ".exe" (downcase (substring x -4))))
                                                    x)) gits))))
    (when gitexes
      (setq magit-git-executable (car gitexes)))))

(defadvice magit-key-mode
    (around magit-key-mode-damn-it activate compile)
  (let ((window-min-height 1))
    ad-do-it))

;; (require 'eval-expr)
;; (eval-expr-install)
;; (setq eval-expr-print-length 1000)

(when (equal system-type 'windows-nt)
  (setq cygwin-binary-dir
        (car (delete nil (mapcar (lambda (x) (when (file-exists-p x) x)) '("C:/cygwin/bin" "C:/bin")))))
  
  (defun cygwin-shell ()
    "Run cygwin bash in shell mode."
    (interactive)
    (let (
	  (explicit-shell-file-name (concat cygwin-binary-dir "/bash"))
	  (explicit-bash-args '("--noediting" "--login" "-i")))
      (call-interactively 'shell)))
  
  (setq cygwin-mount-cygwin-bin-directory cygwin-binary-dir)
  (require 'cygwin-mount)
  (cygwin-mount-activate))

(require 'maxframe)
(require 'buffer-move)

(if (or (equal system-type 'darwin) (equal system-type 'gnu/linux))
    (require 'vkill))

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [C-S-tab] 'next-buffer)
(global-set-key (kbd "M-p") 'goto-line)

;; DONT DO SHIT (tty)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-~") 'other-frame--1)
(global-set-key (kbd "A-`") 'other-frame)
(global-set-key (kbd "A-~") 'other-frame--1)
(global-set-key (kbd "C-M-q") 'other-frame)
(global-set-key (kbd "C-M-w") 'other-frame--1)

(global-set-key (kbd "C-x O") 'other-window--1)
(global-set-key (kbd "C-x g") 'whitespace-mode)
(global-set-key (kbd "C-x G") 'whitespace-cleanup)

(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-x [") 'buf-move-either-horiz)
(global-set-key (kbd "C-x ]") 'buf-move-either-vert)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x j") 'occur)
(global-set-key (kbd "C-x 8 s") "\\s-*")
(global-set-key (kbd "C-x C-0") 'kill-other-windows)
(global-set-key (kbd "C-x c") 'sunrise)

(when (boundp 'evil-mode)
  (defun evil-search-symbol-back ()
    (interactive)
    (evil-search-word nil nil t))
  
  (defun evil-search-symbol-forward ()
    (interactive)
    (evil-search-word t nil t))

  (define-key evil-replace-state-map "\C-l" 'evil-normal-state)
  (define-key evil-visual-state-map "\C-l" 'evil-normal-state)
  (define-key evil-operator-state-map "\C-l" 'evil-normal-state)
  ;; (define-key evil-motion-state-map "\C-l" 'evil-esc)

  (define-key evil-normal-state-map [(shift return)] 'evil-open-above-no-insert)
  (define-key evil-normal-state-map [(return)] 'evil-open-below-no-insert)
  (define-key evil-normal-state-map (kbd "C-w C-q") 'crosshairs)
  (define-key evil-normal-state-map (kbd "[") 'evil-backward-paragraph)
  (define-key evil-normal-state-map (kbd "]") 'evil-forward-paragraph)
  (define-key evil-normal-state-map (kbd "g s") 'sort-lines)

  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-l" 'evil-normal-state)
  (define-key evil-insert-state-map "\C-d" 'delete-char)
  (define-key evil-insert-state-map "\C-n" 'next-line)
  (define-key evil-insert-state-map "\C-p" 'previous-line)
  (define-key evil-insert-state-map "\C-f" 'forward-char)
  (define-key evil-insert-state-map "\C-b" 'backward-char)
  (define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)

  (define-key evil-normal-state-map " " 'evil-next-line-10)
  (define-key evil-motion-state-map " " 'evil-next-line-10)
  (define-key evil-motion-state-map (kbd "DEL") 'evil-previous-line-10)
  (define-key evil-normal-state-map (kbd "DEL") 'evil-previous-line-10)

  (define-key evil-normal-state-map "_" 'evil-previous-line-first-non-blank)
  (define-key evil-normal-state-map "+" 'evil-next-line-first-non-blank)
  (define-key evil-normal-state-map "=" 'evil-scroll-page-down)
  (define-key evil-normal-state-map "-" 'evil-scroll-page-up)
  (define-key evil-normal-state-map "\\" 'evil-indent)

  (define-key evil-normal-state-map "*" 'evil-search-symbol-forward)
  (define-key evil-normal-state-map "#" 'evil-search-symbol-back)

  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; (if scala-mode-map
;;     (define-key scala-mode-map [backspace] nil))
;; if(tuareg-mode-map
;;     (define-key tuareg-mode-map [backspace] nil))

(defun define-easy-scroll-keys (target-map)
  (define-key target-map "b" 'scroll-down)
  (define-key target-map "f" 'scroll-up)
  (define-key target-map "j" 'next-line)
  (define-key target-map "k" 'previous-line)
  (define-key target-map "=" 'evil-scroll-page-down)
  (define-key target-map "-" 'evil-scroll-page-up)
  (define-key target-map " " 'evil-next-line-10)
  (define-key target-map (kbd "DEL") 'evil-previous-line-10))

(defun define-easy-search-keys (target-map)
  (define-key target-map "/" 'evil-search-forward)
  (define-key target-map "?" 'evil-search-backward)
  (define-key target-map "n" 'evil-search-next)
  (define-key target-map "N" 'evil-search-previous))

(defun define-easy-scroll-and-search-keys (target-map)
  (define-easy-scroll-keys target-map)
  (define-easy-search-keys target-map))

(define-key dired-mode-map "z" 'dired-root-same-buffer)
(define-key dired-mode-map "c" 'dired-isearch-filenames)

;; (define-easy-scroll-and-search-keys completion-list-map)
(define-easy-scroll-and-search-keys dired-mode-map)
(define-easy-scroll-and-search-keys package-menu-mode-map)
(define-easy-scroll-and-search-keys Buffer-menu-mode-map)
(define-easy-scroll-and-search-keys occur-mode-map)

(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)
(define-key occur-mode-map "A-n" nil)
(define-key occur-mode-map "A-p" 'goto-line)

(define-key package-menu-mode-map "0" 'move-beginning-of-line)

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
                  t))

(let ((funclist ()))
  (mapatoms
   (lambda (x)
     (when (and (fboundp x) (string/starts-with (symbol-name x) "newsticker-"))
       (makunbound x)
       (fmakunbound x)))))

(set-face-background 'col-highlight (face-background 'hl-line))
(setq col-highlight-vline-face-flag t)

(defun eshell/docker-env (&rest args)
  (interactive)
  (let* ((cmd (mapconcat 'identity (append '("docker-machine" "env") args) " "))
         (output (shell-command-to-string cmd))
         (split (mapcar 'chomp (split-string output "[\n]+"))))
    (dolist (s split)
      (save-match-data
        (when (string-match "^export \\([[:ascii:]]+\\)=\"\\(.*\\)\"$" s)
          (let ((key (match-string-no-properties 1 s))
                (value (match-string-no-properties 2 s)))
            (setenv key value)))))))
