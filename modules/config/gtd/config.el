;;; config/gtd/config.el -*- lexical-binding: t; -*-

(defun semgilo/create-gtd-file-if-not-exist (path &optional category tag subtitles)
  (when (not (file-exists-p path))
    (find-file path)
    (erase-buffer)
    (when category
      (insert (format "#+CATEGORY: %s\n" category)))
    (when tag
      (insert (format "#+FILETAGS: %s\n" tag)))
    (when subtitles
      (dolist (i subtitles)
        (insert (format "\* %s\n" i))))
    (save-buffer)
    (kill-current-buffer)
    ))

;; (dolist (i '("actions" "projects")) (insert i))

(after! org
  (semgilo/create-gtd-file-if-not-exist org-gtd-inbox-file "Inbox" "INBOX")
  (semgilo/create-gtd-file-if-not-exist org-gtd-calender-file "Calender" "CALENDER" '("Actions" "Projects"))
  (semgilo/create-gtd-file-if-not-exist org-gtd-history-file "History" "HISTORY")
  (semgilo/create-gtd-file-if-not-exist org-gtd-favorite-file "Favorite" "FAVORITE")
  (semgilo/create-gtd-file-if-not-exist org-gtd-trash-file "Trash" "TRASH"))

(after! org
  ;; To speed up startup, don't put to init section
  (setq org-agenda-files (setq  org-agenda-files (list org-gtd-home))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))

        org-todo-repeat-to-state "NEXT"

        org-todo-keyword-faces '(("NEXT" . warning)
                                 ("PROJECT" . "purple"))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t))

;; capture templates
(after! org
  (setq org-capture-templates
    `(("t" "todo" entry (file org-gtd-inbox-file) ; "" => `org-default-notes-file'
       "* TODO %?" :clock-resume t)
      ("i" "idea" entry (file+headline org-gtd-favorite-file "Ideas")
       "* %? :IDEA\n%T\n" :clock-resume t)
      ("l" "link" entry (file+headline org-gtd-favorite-file "Links")
       "* %? :LINK\n%T\n" :clock-resume t)
      ("n" "note" entry (file+headline org-gtd-favorite-file "Notes")
       "* %? :NOTE\n%T\n" :clock-resume t)
      )))

(after! org
  (setq org-refile-targets '((org-gtd-calender-file :level . 1)
                             (org-gtd-draft-file :maxlevel . 1)
                             (org-gtd-history-file :level . 1)
                             (org-gtd-trash-file :maxlevel . 3)
                             (org-gtd-favorite-file :maxlevel . 3))))

;;; Refiling
(defun semgilo/org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.
The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (with-current-buffer (current-buffer)
      (save-excursion
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    (save-buffer)
    (kill-current-buffer)
    (switch-to-buffer "*Org Agenda(g)*")
    (org-agenda-redo-all)
    )
  )

(defun semgilo/org-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))
    (save-buffer)
    (kill-buffer-if-not-modified (current-buffer))
    (switch-to-buffer "*Org Agenda(g)*")
    (org-agenda-redo-all)))

(defun semgilo/handle-outline-state-to-next ()
  "When todo keyword from todo to PROJECT/NEXT, refile outline to calender."
  (when (string= org-state "NEXT")
    (semgilo/org-refile org-gtd-calender-file "Actions"))
  (when (string= org-state "PROJECT")
    (semgilo/org-refile org-gtd-calender-file "Projects"))
  (when (or (string= org-state "DONE") (string= org-state "CANCEL"))
    (semgilo/org-refile-to-datetree org-gtd-history-file))
  )

(after! org
  (setq org-refile-use-cache nil
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))


(add-hook 'org-after-todo-state-change-hook 'semgilo/handle-outline-state-to-next)

;; (after-load 'org-agenda
;;             (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; ;; Exclude DONE state tasks from refile targets
;; (defun sanityinc/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets."
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
;; (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

;; (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
;;   "A version of `org-refile' which allows refiling to any subtree."
;;   (interactive "P")
;;   (let ((org-refile-target-verify-function))
;;     (org-refile goto default-buffer rfloc msg)))

;; (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
;;   "A version of `org-agenda-refile' which allows refiling to any subtree."
;;   (interactive "P")
;;   (let ((org-refile-target-verify-function))
;;     (org-agenda-refile goto rfloc no-update)))

(after! org
  (let ((active-project-match "-INBOX/PROJECT"))

    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))

    (setq org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          ;;org-agenda-filter-preset '("-FAVORITE")
          org-agenda-sorting-strategy
          '((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
          `(("N" "Notes"
             ((agenda "" nil)
              (tags "NOTE"
                    ((org-agenda-overriding-header "Notes")
                     (org-tags-match-list-sublevels t)))))
            ("g" "GTD"
             ((agenda "" nil)
              (tags "INBOX"
                    ((org-agenda-overriding-header "Inbox")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "CALENDER"
                         ((org-agenda-overriding-header "Calender")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "DRAFT"
                         ((org-agenda-overriding-header "Drafts")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "TRASH"
                         ((org-agenda-overriding-header "Trash")
                          (org-tags-match-list-sublevels nil)))
              (tags "HISTORY"
                    ((org-agenda-overriding-header "History")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-max-entries 5)
                     (org-agenda-skip-function
                      '(lambda ()
                         (org-agenda-skip-entry-if 'nottodo 'any)))

                     ))

              ))))))

(add-hook! org-mode (electric-indent-local-mode -1))


;; org-roam
(after! org-roam
  (setq org-roam-capture-templates
      '(
        ("d" "problem note" plain (function org-roam-capture--get-point)
         "\* 问题描述\n\n\* 问题分析\n\n\* 解决方案\n"
         :file-name "${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n\n"))))
