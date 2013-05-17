(require 'json)
(require 'url)
(require 'cl)

(defun reddit ()
  (interactive)
  (when (not
	 (get-buffer reddit-buffer-name))
    (with-current-buffer (get-buffer-create reddit-buffer-name)
      (reddit-mode)
      (switch-to-buffer reddit-buffer-name))))

    
(defvar reddit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "q" 'reddit-quit)
    (define-key map "l" 'reddit-display-subreddit)
    map)
  "Keymap reddit-mode")

(defun reddit-fetch-subreddit (&optional dest)
  "Contacts API, fetches the JSON into a plist and returns"
  (interactive "P")
  (let ((buffer (url-retrieve-synchronously (concat reddit-index-url (if dest "r/" "") dest ".json")))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (reddit-parse-list (json-read)))))

(defun reddit-parse-list (list-data)
  "retrieves children nodes from a main data list"
  (let ((data (cdr (assoc 'data list-data))))
    (let ((children (cdr (assoc 'children data))))
      (loop for a across children
	    for n from 0 do
	    (reddit-show-post a n))
      (widget-setup))))

(defun reddit-show-post (alist n)
  "add a post to the reddit buffer"
  (let ((a (cdr (assoc 'data alist)))
	(inhibit-read-only t))
    (with-current-buffer reddit-buffer-name
      ;; (widget-create 'toggle :on "-" :off "+" :format "%[%v%] ")
      (widget-create (get-data (author score num_comments title subreddit)
			       a
			       (list 'reddit-post
				     :format "%vfoo foo %[click-me%] bar bar bar\n"
				     :notify (lambda (widget &rest ignore)
					       (interactive)
					       (message "clicked!"))
				     :post-id n
				     :reddit-author author
				     :reddit-score score
				     :reddit-comments num_comments
				     :reddit-title title
				     :reddit-subreddit subreddit
				     :buttons "foo"))))))

(define-widget 'reddit-post 'push-button
  "A widget representing a Reddit entry."
  :format-handler 'reddit-post-format
  :value-create 'reddit-value-create)

(defun reddit-value-create (widget)
  (message "trigger"))

(defvar reddit-post-format "%[%B. (%Cpts) %E (by %A, %D comments) r/%G%]\n")

(defmacro get-data (keys data &rest body)
	 `(let* (,@(loop for key in keys
			 collecting `(,key (assoc-default ',key ,data))))
	    ,@body))

(defun reddit-post-format (widget char)
  (case char
    (?A (insert (widget-get widget :reddit-author)))
    (?B (insert (format "%d" (widget-get widget :post-id))))
    (?C (insert (format "%d" (widget-get widget :reddit-score))))
    (?D (insert (format "%d" (widget-get widget :reddit-comments))))
    (?E (insert (widget-get widget :reddit-title)))
    (?G (insert (widget-get widget :reddit-subreddit)))
    (t (widget-default-format-handler widget char))))


(defun make-post ()
  (list ))

(defun reddit-display-subreddit (&optional subreddit-name)
  (interactive "P")
  (if subreddit-name
      (reddit-fetch-subreddit subreddit-name)
    (reddit-fetch-subreddit))
  (widget-setup))

(defun reddit-list-posts ()
  (interactive)
  (let ((inhibit-read-only t))
    (insert "test")))

  
(defun reddit-quit ()
  "Commands to perform when reddit-mode is closed"
  (interactive)
  (kill-buffer reddit-buffer-name))


(define-derived-mode reddit-mode nil "reddit"
  "reddit mode"
  (use-local-map reddit-mode-map)
  (setq buffer-read-only t))

;; (defconst reddit-index-url "http://www.reddit.com/"
;;   "reddit's index page")

(defconst reddit-index-url "http://localhost/"
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

(provide 'reddit)
