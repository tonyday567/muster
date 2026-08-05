;;; muster-deck.el --- Muster bus UI in Emacs -*- lexical-binding: t; -*-

;; A minimal Emacs surface for the muster coordination bus.
;; Two buffers: *muster-log* (tail) and *muster-post* (write).
;; The board is a regular file; open it with `muster-board'.
;;
;; Bind under your preferred leader prefix, e.g.:
;;   (map! :leader
;;         :prefix ("y" . "muster")
;;         :desc "send"  "RET" #'muster-send
;;         :desc "send"  "SPC" #'muster-send
;;         :desc "board" "b"   #'muster-board
;;         :desc "post"  "p"   #'muster-post
;;         :desc "log"   "l"   #'muster-log
;;         :desc "deck"  "d"   #'muster-deck)

;;; Commentary:

;; muster-deck replaces the muster-ws web UI with Emacs buffers.
;; The log file remains the single source of truth; the log buffer is a
;; processed view that renders JSONL messages as `sender: body' (or
;; `[ts] sender: body' when timestamps are on).  Multi-line posts read
;; naturally because the body is stored with real newlines.  Posts are
;; sent via `muster post' from the *muster-post* buffer.  The board file
;; (`loom/board.md' by default) is just a regular file; open it with
;; `muster-board'.

;;; Code:

(require 'filenotify)
(require 'json)

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

(defcustom muster-post-buffer-name "*muster-post*"
  "Name of the post buffer."
  :type 'string
  :group 'muster-deck)

(defcustom muster-log-buffer-name "*muster-log*"
  "Name of the log buffer."
  :type 'string
  :group 'muster-deck)

(defcustom muster-board-file "~/coffee/loom/board.md"
  "Path to the board markdown file.
This is a regular file; `muster-board' simply opens it."
  :type 'string
  :group 'muster-deck)

;;; Core helpers

(defun muster--expand-root ()
  "Expand `muster-root' to an absolute path."
  (expand-file-name muster-root))

(defun muster--log-file ()
  "Return path to the global log file."
  (expand-file-name "log.jsonl" (muster--expand-root)))

(defun muster--current-name ()
  "Return the current muster identity from ~/.config/muster/.me, if any."
  (let ((me-file (expand-file-name ".me" (muster--expand-root))))
    (when (file-readable-p me-file)
      (with-temp-buffer
        (insert-file-contents me-file)
        (string-trim (buffer-string))))))

;;; Sending

;;;###autoload
(defun muster-send (begin end)
  "Send active region from BEGIN to END to muster as `muster-name'.
If no region is active, send the whole buffer, but only when in
`muster-post-mode' to avoid accidentally posting source files.
The caller's existing identity in ~/.config/muster/.me is saved and
restored after posting so the deck doesn't drift the global identity."
  (interactive "r")
  (let* ((use-region (use-region-p))
         (begin (if use-region begin (point-min)))
         (end (if use-region end (point-max))))
    (when (= begin end)
      (user-error "Nothing to send"))
    (unless (or use-region (derived-mode-p 'muster-post-mode))
      (user-error "No active region; switch to muster-post to send a full buffer"))
    (let ((text (string-trim (buffer-substring-no-properties begin end)))
          (previous-name (muster--current-name))
          (coding-system-for-write 'utf-8))
      (when (string-empty-p text)
        (user-error "Nothing to send"))
      ;; Post as `muster-name', then restore the previous identity so the
      ;; deck doesn't hijack the global .me file.
      (unwind-protect
          (progn
            (call-process "muster" nil nil nil "name" muster-name)
            (call-process "muster" nil nil nil "--channel" muster-channel "post" text))
        (when (and previous-name (not (string-empty-p previous-name)))
          (call-process "muster" nil nil nil "name" previous-name))))
    (when (derived-mode-p 'muster-post-mode)
      (erase-buffer))))

;;; Post buffer

(defvar muster-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'muster-send)
    map)
  "Keymap for `muster-post-mode'.")

(define-derived-mode muster-post-mode text-mode "Muster-Post"
  "Major mode for composing muster posts."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;;###autoload
(defun muster-post ()
  "Open the muster post buffer."
  (interactive)
  (pop-to-buffer muster-post-buffer-name)
  (unless (derived-mode-p 'muster-post-mode)
    (muster-post-mode)))

;;; Board file

;;;###autoload
(defun muster-board ()
  "Open the board file as a regular file."
  (interactive)
  (find-file (expand-file-name muster-board-file)))

;;; Log buffer

(defcustom muster-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable."
  :type 'number
  :group 'muster-deck)

(defcustom muster-log-show-timestamps t
  "Whether to show timestamps in `*muster-log*'."
  :type 'boolean
  :group 'muster-deck)

(defvar muster-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar muster-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar muster-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'muster-log-toggle-timestamps)
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

(defun muster-log--format-line (line)
  "Format a raw log LINE for display.
Accepts stamped JSONL (`id', `ts', `from', `to', `thread', `body') and the
legacy bracket format `[ts] sender: body'.  Lines not addressed to the
current `muster-channel' are ignored.  Unparseable lines are returned as-is."
  (or (ignore-errors
        (let* ((obj (json-read-from-string line))
               (id (cdr (assoc 'id obj)))
               (ts (cdr (assoc 'ts obj)))
               (from (cdr (assoc 'from obj)))
               (to (cdr (assoc 'to obj)))
               (body (cdr (assoc 'body obj))))
          (when (and id ts from body
                     (seq-find (lambda (c) (string= c muster-channel)) to))
            (if muster-log-show-timestamps
                (format "[%s@%s] %s: %s" id ts from body)
              (format "%s: %s" from body)))))
      (when (string-match "^\[\([^@]+\)@\([^\]]+\)\] +\([^:]+\): +\(.*\)$" line)
        (let ((id (match-string 1 line))
              (ts (match-string 2 line))
              (from (match-string 3 line))
              (body (match-string 4 line)))
          (if muster-log-show-timestamps
              (format "[%s@%s] %s: %s" id ts from body)
            (format "%s: %s" from body))))
      (when (string-match "^\[\([^\]]+\)\] +\([^:]+\): +\(.*\)$" line)
        (let ((ts (match-string 1 line))
              (sender (match-string 2 line))
              (body (match-string 3 line)))
          (if muster-log-show-timestamps
              (format "[%s] %s: %s" ts sender body)
            (format "%s: %s" sender body))))
      line))

(defun muster-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
When FORCE is non-nil, re-render even if the file content appears
unchanged.  Keeps the tail in view for any window that was already at
the end of the log."
  (when-let ((buf (get-buffer muster-log-buffer-name)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (raw (muster-log--read-file))
             (contents (mapconcat #'muster-log--format-line
                                  (split-string raw "
")
                                  "
")))
        (when (or force (not (string= contents (buffer-string))))
          (let ((window-states
                 (mapcar (lambda (w)
                           (let ((pt (window-point w)))
                             (list w pt (= pt (point-max)))))
                         (get-buffer-window-list buf nil t))))
            (erase-buffer)
            (insert contents)
            (dolist (state window-states)
              (let* ((w (nth 0 state))
                     (old-pt (nth 1 state))
                     (was-at-end (nth 2 state))
                     (new-pt (if was-at-end (point-max) (min old-pt (point-max)))))
                (set-window-point w new-pt)))))))))

(defun muster-log--refresh-soon ()
  "Schedule a log refresh after a short debounce window.
Multiple file-notify events in quick succession collapse into one refresh."
  (when muster-log--timer
    (cancel-timer muster-log--timer))
  (setq muster-log--timer
        (run-with-timer 0.05 nil #'muster-log-refresh)))

(defun muster-log-toggle-timestamps ()
  "Toggle timestamp display in `*muster-log*' and force a re-render."
  (interactive)
  (setq muster-log-show-timestamps (not muster-log-show-timestamps))
  (message "Muster log timestamps %s"
           (if muster-log-show-timestamps "on" "off"))
  (muster-log-refresh t))

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
        (muster-log-refresh)
        (goto-char (point-max)))
      (pop-to-buffer buf))))

;;; Deck

;;;###autoload
(defun muster-deck ()
  "Open the full deck: log on top, post across the bottom."
  (interactive)
  (delete-other-windows)
  ;; Split into top (log) and bottom (post), so post spans full width.
  (split-window-below)
  (let* ((top-win (selected-window))
         (bottom-win (next-window))
         (log-buf (get-buffer-create muster-log-buffer-name))
         (post-buf (get-buffer-create muster-post-buffer-name)))
    ;; Top: log
    (set-window-buffer top-win log-buf)
    (with-current-buffer log-buf
      (muster-log-mode)
      (setq buffer-read-only t)
      (muster-log-start-watch)
      (muster-log-refresh)
      (goto-char (point-max)))
    (set-window-point top-win (with-current-buffer log-buf (point-max)))
    ;; Bottom: post
    (select-window bottom-win)
    (set-window-buffer bottom-win post-buf)
    (with-current-buffer post-buf
      (unless (derived-mode-p 'muster-post-mode)
        (muster-post-mode)))
    (select-window top-win)))

;;;###autoload
(defun muster-deck-quit ()
  "Close muster deck buffers and stop the log watcher."
  (interactive)
  (muster-log-stop-watch)
  (dolist (buf (list muster-post-buffer-name
                     muster-log-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

(provide 'muster-deck)
;;; muster-deck.el ends here
