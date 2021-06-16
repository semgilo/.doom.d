;;; config/gtd/config.el -*- lexical-binding: t; -*-

;; hugo blog
(defun semgilo/hugo-new-share (name)
  (interactive "sInput post name: ")
  (setq fullpath
        (concat
         org-directory
         "/share/"
         name
         ".org"))
  (find-file fullpath)
  (yas-insert-snippet))

(defun semgilo/hugo-new-note (name)
  (interactive "sInput note name: ")
  (setq fullpath
        (concat
         org-directory
         "/notes/"
         name
         ".org"))
  (find-file fullpath)
  (yas-insert-snippet))


(defun semgilo/hugo-publish-posts (&optional blog-base-dir)
  (interactive "sInpup blog base dir: ")
  (if (equal blog-base-dir "")
      (setq blog-base-dir blog-root))

  (setq content-org-dir
        (concat
         blog-base-dir
         "/content-org"))

  (setq org-notes-dir (concat org-directory "/notes"))
  (setq org-share-dir (concat org-directory "/share"))
  (if (file-exists-p content-org-dir)
      (delete-directory content-org-dir t)
    )
  (make-directory content-org-dir)
  (copy-directory org-notes-dir (concat content-org-dir "/notes"))
  (copy-directory org-share-dir (concat content-org-dir "/share"))

  (dolist (file (directory-files-recursively content-org-dir ".org"))
    (when (or (string-match-p "/notes/" file)
              (string-match-p "/share/" file))
      (message file)
      (find-file file)
      (org-hugo-export-to-md)
      (kill-current-buffer))
  ))

(defun semgilo/insert-org-img-link (path)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s]]" path))))

(defun semgilo/capture-screenshot (filename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal filename "")
      (setq filename (format-time-string "%Y%m%d_%H%M%S")))

  (setq cur-org-file-name-base (file-name-base (buffer-file-name)))
  (setq output-path
        (concat (file-name-directory (buffer-file-name))
                "/../static/images/"
                cur-org-file-name-base
                "/"
                filename
                ".png"))

  (setq blog-relative-path
        (concat "/images/"
                cur-org-file-name-base
                "/"
                filename
                ".png"))
  (message output-path)
  (setq output-dir (file-name-directory output-path))

  (unless (file-directory-p output-dir)
    (make-directory output-dir t))


  (progn
    (call-process "screencapture" nil nil nil "-s" output-path)
    (call-process "open" nil nil nil output-path)
    (semgilo/insert-org-img-link blog-relative-path))

  (insert "\n"))


(defun semgilo/record-screencapture (filename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (call-process "/Applications/LICPcap.app" nil 0)
  )

(global-set-key [C-s-268632065] 'semgilo/capture-screenshot)
