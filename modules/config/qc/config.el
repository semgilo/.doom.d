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
  (interactive "sChannel(default qc)):")
  (if (string= channel "")
      (setq channel "qc"))
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq bat-path (format "%s/client/build/android/pack_%s.bat" root-path channel))
  (message bat-path)
  (call-process-shell-command bat-path))

;; svn update
(defun qc/update-project()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (call-process-shell-command (format "TortoiseProc.exe /command:update:%s /closeonend:2" root-path))
  )
(map! :leader "C-u" #'qc/update-project)
