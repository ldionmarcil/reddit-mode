(defun reddit ()
  (interactive)
  (when (not
	 (get-buffer reddit-buffer-name))
    (with-current-buffer (get-buffer-create reddit-buffer-name)
      (reddit-mode)
      (switch-to-buffer reddit-buffer-name))))

    
(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'reddit-quit)
    (define-key map "a" 'reddit-all)
    map)
  "Keymap reddit-mode")

(defun reddit-all ()
  "fetch r/all"
  (interactive)
  (message "... loading"))


(defun reddit-quit ()
  "Commands to perform when reddit-mode is closed"
  (interactive)
  (kill-buffer reddit-buffer-name))

(define-derived-mode reddit-mode nil "reddit"
  "reddit mode"
  (use-local-map reddit-mode-map)
  (setq buffer-read-only t))

(defconst reddit-index-url "http://www.reddit.com/"
  "reddit's index page")

(defconst reddit-api-url "http://www.reddit.com/dev/api"
  "reddit's API access point")

(defconst reddit-version-string "Version 0.1"
  "reddit's API access point")

(defconst reddit-buffer-name "*reddit*"
  "reddit-mode main buffer name")

(defun reddit-version ()
  (interactive)
  (let ((version-string
	(format "%s" reddit-version-string)))
    (message version-string)))