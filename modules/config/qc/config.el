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

;; launch server
(defun qc/launch-server()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq server-path (format "%s/server" root-path))
  (call-process-shell-command (format "cd %s&start start_gs.bat" server-path))
  )

;; pack android apk
(defun qc/pack-android(&optional channel)
  (interactive
   (list (completing-read "Choose: "
                          '("qc" "qc_official" "lt") nil t)))
  (message channel)
  (if (string= channel "")
      (setq channel "qc"))
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq android-build-root (format "%s/client/build/android" root-path))
  (call-process-shell-command (format "cd %s&start pack_%s.bat" android-build-root channel)))

;; build ios src
(defun qc/build-ios-src(&optional channel)
  (interactive
   (list (completing-read "Choose: "
                          '("qc" "qc_official" "lt") nil t)))
  (message channel)
  (if (string= channel "")
      (setq channel "qc"))
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (setq android-build-root (format "%s/client/build/ios" root-path))
  (call-process-shell-command (format "cd %s&start build_src_%s_with_new.bat" android-build-root channel)))

;; svn update
(defun qc/update-project()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (message root-path)
  (call-process-shell-command (format "TortoiseProc.exe /command:update /path:\"%s\" /closeonend:2" root-path))
  )

;; svn commit
(defun qc/commit-project()
  (interactive
   (list (completing-read "Choose: "
                          '("src" "src+runtime" "src+runtime+cocos2dx" "src+cocos2dx" "cocos2dx+runtime") nil t)))
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))

  (call-process-shell-command (format "TortoiseProc.exe /command:update /path:\"%s\" /closeonend:2" root-path))
  )

(defun qc/open-server-product-dir()
  (interactive)
  (setq path (file-name-directory (buffer-file-name)))
  (setq root-path (substring path 0 (string-match "[\w/\\]+?client" path)))
  (show-in-explorer (format "%s/server/server_scripts/product" root-path)))

(map! :leader
      (:prefix-map ("m" . "mine")
       :desc "qcplay pack android"             "a" #'qc/pack-android
       :desc "qcplay build ios src"            "i" #'qc/build-ios-src
       :desc "qcplay update project"           "u" #'qc/update-project
       :desc "qcplay launch client"            "c" #'qc/launch-client
       :desc "qcplay launch server"            "s" #'qc/launch-server
       (:prefix-map ("o" . "show dir in exploder")
        :desc "server product"   "p" #'qc/open-server-product-dir)))
