;;; muster-deck.el --- Muster bus UI in Emacs -*- lexical-binding: t; -*-

;; A minimal Emacs surface for the muster coordination bus.
;; Two buffers: *muster-compose* (write) and *muster-log* (tail).
;;
;; Bind under your preferred leader prefix, e.g.:
;;   (map! :leader
;;         :prefix ("y" . "muster")
;;         :desc "send"    "RET" #'muster-send
;;         :desc "compose" "m"   #'muster-compose
;;         :desc "log"     "l"   #'muster-log
;;         :desc "deck"    "d"   #'muster-deck)

;;; Commentary:

;; muster-deck replaces the muster-ws web UI with two Emacs buffers.
;; The log file remains the single source of truth; the log buffer is a
;; processed view that unescapes framed newlines so multi-line posts read
;; naturally.  Posts are sent via `muster post' and identity is kept in
;; sync with `muster-name' by calling `muster name' first.

;;; Code:

;;; Variables

(defgroup muster-deck nil
  "Emacs surface for the muster coordination bus."
  :group 'applications)

(defcustom muster-root "~/.config/muster"
  "Root directory for muster channels."
  :type 'string
  :group 'muster-deck)

(defcustom muster-channel "bus"
  "Default muster channel."
  :type 'string
  :group 'muster-deck)

(defcustom muster-name "captain"
  "Name to post as."
  :type 'string
  :group 'muster-deck)

(defcustom muster-compose-buffer-name "*muster-compose*"
  "Name of the compose buffer."
  :type 'string
  :group 'muster-deck)

(defcustom muster-log-buffer-name "*muster-log*"
  "Name of the log buffer."
  :type 'string
  :group 'muster-deck)

;;; Core helpers

(defun muster--expand-root ()
  "Expand `muster-root' to an absolute path."
  (expand-file-name muster-root))

(defun muster--log-file ()
  "Return path to the current channel log file."
  (expand-file-name (format "%s/log.md" muster-channel) (muster--expand-root)))

;;; Sending

;;;###autoload
(defun muster-send (begin end)
  "Send active region from BEGIN to END to muster.
If no region is active, send the whole buffer, but only when in
`muster-compose-mode' to avoid accidentally posting source files."
  (interactive "r")
  (let* ((use-region (use-region-p))
         (begin (if use-region begin (point-min)))
         (end (if use-region end (point-max))))
    (when (= begin end)
      (user-error "Nothing to send"))
    (unless (or use-region (derived-mode-p 'muster-compose-mode))
      (user-error "No active region; switch to muster-compose to send a full buffer"))
    (let ((text (string-trim (buffer-substring-no-properties begin end))))
      (when (string-empty-p text)
        (user-error "Nothing to send"))
      ;; muster resolves the sender from ~/.config/muster/.me. Keep that
      ;; in sync with `muster-name' so the Emacs config stays authoritative.
      ;; Force UTF-8 so circuit symbols and marks survive argv handoff.
      (let ((coding-system-for-write 'utf-8))
        (call-process "muster" nil nil nil "name" muster-name)
        (call-process "muster" nil nil nil "post" text)))
    (when (derived-mode-p 'muster-compose-mode)
      (erase-buffer))))

;;; Compose buffer

(defvar muster-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'muster-send)
    map)
  "Keymap for `muster-compose-mode'.")

(define-derived-mode muster-compose-mode text-mode "Muster-Compose"
  "Major mode for composing muster posts."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;;###autoload
(defun muster-compose ()
  "Open the muster compose buffer."
  (interactive)
  (pop-to-buffer muster-compose-buffer-name)
  (unless (derived-mode-p 'muster-compose-mode)
    (muster-compose-mode)))

;;; Log buffer

(defcustom muster-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable."
  :type 'number
  :group 'muster-deck)

(defvar muster-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar muster-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar muster-log-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `muster-log-mode'.")

(define-derived-mode muster-log-mode text-mode "Muster-Log"
  "Major mode for viewing muster logs."
  (add-hook 'kill-buffer-hook #'muster-log-stop-watch nil t))

(defun muster-log--read-file ()
  "Read the current channel log file as UTF-8 text."
  (with-temp-buffer
    (insert-file-contents-literally (muster--log-file))
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-string)))

(defun muster-log--unescape (s)
  "Turn escaped newlines in muster log frames into real newlines."
  (replace-regexp-in-string "\\\\n" "\n" s))

(defun muster-log-refresh ()
  "Refresh the log buffer from the file, preserving scroll position.
Skip if the file content is unchanged since the last refresh."
  (when-let ((buf (get-buffer muster-log-buffer-name)))
    (with-current-buffer buf
      (let* ((old-point (point))
             (at-end (= old-point (point-max)))
             (inhibit-read-only t)
             (raw (muster-log--read-file))
             (contents (muster-log--unescape raw)))
        (unless (string= contents (buffer-string))
          (erase-buffer)
          (insert contents)
          (goto-char (if at-end (point-max) old-point)))))))

(defun muster-log--refresh-soon ()
  "Schedule a log refresh after a short debounce window.
Multiple file-notify events in quick succession collapse into one refresh."
  (when muster-log--timer
    (cancel-timer muster-log--timer))
  (setq muster-log--timer
        (run-with-timer 0.05 nil #'muster-log-refresh)))

(defun muster-log-start-watch ()
  "Start watching the log file.
Prefer file-notify events; fall back to a polling timer if unavailable."
  (muster-log-stop-watch)
  (condition-case nil
      (setq muster-log--file-watch
            (file-notify-add-watch (muster--log-file)
                                   '(change)
                                   (lambda (_event)
                                     (muster-log--refresh-soon))))
    (error
     (setq muster-log--timer
           (run-with-timer 0 muster-log-refresh-interval #'muster-log-refresh)))))

(defun muster-log-stop-watch ()
  "Stop watching the log file."
  (when muster-log--timer
    (cancel-timer muster-log--timer)
    (setq muster-log--timer nil))
  (when muster-log--file-watch
    (file-notify-rm-watch muster-log--file-watch)
    (setq muster-log--file-watch nil)))

;;;###autoload
(defun muster-log ()
  "Open the muster log buffer and start tailing it."
  (interactive)
  (let ((log-file (muster--log-file)))
    (unless (file-readable-p log-file)
      (user-error "Log file not found: %s" log-file))
    (let ((buf (get-buffer-create muster-log-buffer-name)))
      (with-current-buffer buf
        (muster-log-mode)
        (setq buffer-read-only t)
        (muster-log-start-watch)
        (muster-log-refresh))
      (pop-to-buffer buf))))

;;; Deck

;;;###autoload
(defun muster-deck ()
  "Open both the muster log and compose buffers in a stacked layout.
Log on top (2/3), compose on bottom (1/3).  Reuses the selected window
instead of creating a new frame."
  (interactive)
  (let ((display-buffer-alist
         '(("\\*muster-\\(log\\|compose\\)\\*"
            (display-buffer-same-window)
            (inhibit-same-window . nil)))))
    (pop-to-buffer (get-buffer-create muster-log-buffer-name))
    (unless (derived-mode-p 'muster-log-mode)
      (muster-log-mode))
    (setq buffer-read-only t)
    (muster-log-start-watch)
    (muster-log-refresh)
    (split-window-below)
    (let ((compose-win (next-window)))
      (set-window-buffer compose-win (get-buffer-create muster-compose-buffer-name))
      (with-current-buffer (get-buffer muster-compose-buffer-name)
        (unless (derived-mode-p 'muster-compose-mode)
          (muster-compose-mode)))
      (select-window compose-win))))

;;;###autoload
(defun muster-deck-quit ()
  "Close muster deck buffers and stop the log watcher."
  (interactive)
  (muster-log-stop-watch)
  (dolist (buf (list muster-compose-buffer-name muster-log-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

(provide 'muster-deck)
;;; muster-deck.el ends here
