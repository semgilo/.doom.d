;;; config/qc/config.el -*- lexical-binding: t; -*-
;; launch client
(defun qc/launch-client()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq bat-path (format "%s/client/runtime/win32/client_disableIDE.bat" root-path))
  (message bat-path)
  (call-process-shell-command bat-path)
  )
(map! :leader "C-c" #'qc/launch-client)

;; launch server
(defun qc/launch-server()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq server-path (format "%s/server" root-path))
  (call-process-shell-command (format "cd %s&start start_gs.bat" server-path))
  )
(map! :leader "C-s" #'qc/launch-server)

;; pack android apk
(defun qc/pack-android(&optional channel)
  (interactive)
  (let ((choice (completing-read-multiple "Select: " '("qc" "qc_official" "lt"))))
    (setq channel choice))
  (if (string= channel "")
      (setq channel "qc"))
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq android-build-root (format "%s/client/build/android" root-path))
  (call-process-shell-command (format "cd %s&start pack_%s.bat" android-build-root channel)))

(map! :map doom-leader-quit/session-map "p" #'qc/pack-android)

;; svn update
(defun qc/update-project()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (message root-path)
  (call-process-shell-command (format "TortoiseProc.exe /command:update /path:\"%s\" /closeonend:2" root-path))
  )
(map! :leader "C-u" #'qc/update-project)
