;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Donghai Ruan"
      user-mail-address "9737935@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


;; font



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (expand-file-name "notes" org-directory))
(defconst org-gtd-home (expand-file-name "gtd" org-directory))
(defconst org-gtd-inbox-file (expand-file-name "inbox.org" org-gtd-home))
(defconst org-gtd-calender-file (expand-file-name "calender.org" org-gtd-home))
(defconst org-gtd-draft-file (expand-file-name "draft.org" org-gtd-home))
(defconst org-gtd-trash-file (expand-file-name "trash.org" org-gtd-home))
(defconst org-gtd-favorite-file (expand-file-name "favorite.org" org-gtd-home))
(defconst org-gtd-history-file (expand-file-name "history.org" org-gtd-home))

(after! org
  (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭")
        org-ellipsis " ▼ "
        org-tags-column -77
        org-agenda-tags-column -77))

;; blog root
(defconst blog-root
  (if-let (IS-WINDOWS)
      "d:/git/blog"
   "~/Documents/git/www.lanhuzi.com1/blog"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org-cn". "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; set projectile global ignore files
(setq ignored-file-suffixes '(list ".obj" ".class"))

;; (defun semgilo/add-to-ignored-file-suffixes (name)
  ;; (add-to-list 'projectile-globally-ignored-file-suffixes name))
;; (mapcar 'semgilo/add-to-ignored-file-suffixes ignored-file-suffixes)
;; (add-to-list 'projectile-globally-ignored-file-suffixes '(".obj" ".class" ".tlog" ".log"))

;; exit and not ask.
(setq confirm-kill-emacs nil)

;; time format
(setq system-time-locale "C")

;; valign
(use-package! valign)
(add-hook 'org-mode-hook #'valign-mode)

;; show-in-finder
(defun show-in-explorer (path)
  "Show path in explorer (window platform)"
  (call-process-shell-command (format "explorer.exe %s" (replace-regexp-in-string "\/" "\\\\" path))))

(defun show-current-buffer-in-explorer ()
  (interactive)
  "Show current buff in explorer"
  (progn
    (setq path (file-name-directory (buffer-file-name)))
    (when IS-WINDOWS
      (show-in-explorer path))
    (when IS-MAC
      (message "todo show-in-finder"))
    ))

(map! :leader "C-o" #'show-current-buffer-in-explorer)

