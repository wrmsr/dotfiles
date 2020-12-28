;;; TODO:
;;;  - fix C-r
;;;  - fix M-<#>
;;;  - package list up top, pin versions
;;;  - https://www.emacswiki.org/emacs/download/crosshairs.el


;; bootstrap

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(unless window-system
  (setq linum-format "%d "))

(setq

 ;; shift-select-mode t
 color-theme-is-global t
 delete-selection-mode t
 echo-keystrokes 0.1
 ediff-window-setup-function 'ediff-setup-windows-plain
 ffap-machine-p-known 'reject
 font-lock-maximum-decoration t
 inhibit-startup-message t
 require-final-newline t
 ring-bell-function 'ignore
 transient-mark-mode t
 truncate-partial-width-windows nil
 x-select-enable-clipboard t
 xterm-mouse-mode t
 eval-expression-print-length 1000

 )

(setq
 scroll-margin 7
 scroll-step 1
 scroll-conservatively most-positive-fixnum
 scroll-preserve-screen-position 1)

(setq fill-column 120)
(set-fill-column 120)
(setq split-height-threshold 60)

(random t)

(global-linum-mode t)
(setq line-number-mode t
      column-number-mode t)

(size-indication-mode t)
(auto-compression-mode t)
(global-font-lock-mode t)
(recentf-mode 1)
(show-paren-mode 1)


;; packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(let
    ((packages
      '(

	auto-compile
	browse-kill-ring
	buffer-move
	evil
	magit
	s
	undo-tree
	window-number
	zenburn-theme

	))
     (updated nil))
  (dolist (p packages)
    (unless (package-installed-p p)
      (unless updated
	(package-refresh-contents)
	(setq updated 't))
      (package-install p))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil zenburn-theme s window-number)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; functions

(require 's)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
       (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun filter (condp lst)
  (delq nil
    (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


;; zenbburn

(setq zenburn-override-colors-alist
      '(("zenburn-bg"    . "#111111")
	("zenburn-bg+05" . "#181818")
        ("zenburn-bg+1"  . "#202020")
        ("zenburn-bg+2"  . "#282828")
        ("zenburn-bg+3"  . "#303030")))

(load-theme 'zenburn t)


;; window-number

(require 'window-number)

(window-number-mode 1)

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

(add-to-list
 'mode-line-position
 '(:eval (window-number-string)) t 'eq)


;; buffer-move

(require 'buffer-move)


;; which-func

(which-func-mode)

(set-face-background 'which-func "#000000")
(set-face-foreground 'which-func "#FFFFFF")

(setq which-func-format
      '((:propertize "[ " face which-func)
	(:propertize which-func-current
		     local-map ,which-func-keymap
		     face which-func)
	(:propertize " ] " face which-func)))


;; evil

(evil-mode t)

(setq evil-toggle-key "C-\\")
(setq evil-default-cursor '("#73d216" t))
(setq evil-mode-line-format 'before)

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

(global-set-key (kbd "C-x g") 'whitespace-mode)
(global-set-key (kbd "C-x G") 'whitespace-cleanup)

(global-set-key (kbd "C-z") nil)

(global-set-key (kbd "C-x [") 'buf-move-either-horiz)
(global-set-key (kbd "C-x ]") 'buf-move-either-vert)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x j") 'occur)
(global-set-key (kbd "C-x 8 s") "\\s-*")
(global-set-key (kbd "C-x C-0") 'kill-other-windows)

(define-key evil-insert-state-map "\C-l" 'evil-normal-state)

(define-key evil-normal-state-map [(shift return)] 'evil-open-above-no-insert)
(define-key evil-normal-state-map [(return)] 'evil-open-below-no-insert)
; (define-key evil-normal-state-map (kbd "C-w C-q") 'crosshairs)
(define-key evil-normal-state-map (kbd "[") 'evil-backward-paragraph)
(define-key evil-normal-state-map (kbd "]") 'evil-forward-paragraph)
(define-key evil-normal-state-map (kbd "g s") 'sort-lines)

(define-key evil-normal-state-map " " 'evil-next-line-10)
(define-key evil-motion-state-map " " 'evil-next-line-10)
(define-key evil-motion-state-map (kbd "DEL") 'evil-previous-line-10)
(define-key evil-normal-state-map (kbd "DEL") 'evil-previous-line-10)

(define-key evil-normal-state-map "_" 'evil-previous-line-first-non-blank)
(define-key evil-normal-state-map "+" 'evil-next-line-first-non-blank)
(define-key evil-normal-state-map "=" 'evil-scroll-page-down)
(define-key evil-normal-state-map "-" 'evil-scroll-page-up)
(define-key evil-normal-state-map "\\" 'evil-indent)

(defun evil-next-line-10 ()
  (interactive)
  (evil-next-line 10))

(defun evil-previous-line-10 ()
  (interactive)
  (evil-previous-line 10))

(defface evil-vi-state-id-face '((t (:background "#000000" :foreground "#4545FF"))) "" :group 'evil)
(defface evil-emacs-state-id-face '((t (:background "#000000" :foreground "#FF3333"))) "" :group 'evil)
(defface evil-insert-state-id-face '((t (:background "#000000" :foreground "#33FF33"))) "" :group 'evil)
(defface evil-visual-state-id-face '((t (:background "#000000" :foreground "#AD1EC9"))) "" :group 'evil)
(defface evil-replace-state-id-face '((t (:background "#000000" :foreground "#FFFF33"))) "" :group 'evil)
(defface evil-motion-state-id-face '((t (:background "#000000" :foreground "#FF9500"))) "" :group 'evil)
(defface evil-operator-state-id-face '((t (:background "#000000" :foreground "#FF9500"))) "" :group 'evil)

(setq evil-normal-state-tag (propertize " <N> " 'face 'evil-vi-state-id-face))
(setq evil-emacs-state-tag (propertize " <E> " 'face 'evil-emacs-state-id-face))
(setq evil-visual-state-tag (propertize " <V> " 'face 'evil-visual-state-id-face))
(setq evil-insert-state-tag (propertize " <I> " 'face 'evil-insert-state-id-face))
(setq evil-motion-state-tag (propertize " <M> " 'face 'evil-motion-state-id-face))
(setq evil-replace-state-tag (propertize " <R> " 'face 'evil-replace-state-id-face))
(setq evil-operator-state-tag (propertize " <O> " 'face 'evil-operator-state-id-face))


;; modeline

(setq-default
 mode-line-format
 '(

   "%e"
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

(setq
 mode-line-position
 (list
  '(:eval
    (let
	((c (point))
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
(setq-default mode-line-buffer-identification (propertized-buffer-identification " %b "))
(setq mode-line-frame-identification '(:eval (chomp (mode-line-frame-control))))
