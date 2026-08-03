;;; muster-deck.el --- Muster bus UI in Emacs -*- lexical-binding: t; -*-

;; A minimal Emacs surface for the muster coordination bus.
;; Three buffers: *muster-board* (top active loom cards),
;; *muster-log* (tail), and *muster-compose* (write).
;;
;; Bind under your preferred leader prefix, e.g.:
;;   (map! :leader
;;         :prefix ("y" . "muster")
;;         :desc "send"    "RET" #'muster-send
;;         :desc "board"   "b"   #'muster-board
;;         :desc "compose" "m"   #'muster-compose
;;         :desc "log"     "l"   #'muster-log
;;         :desc "deck"    "d"   #'muster-deck)

;;; Commentary:

;; muster-deck replaces the muster-ws web UI with Emacs buffers.
;; The log file remains the single source of truth; the log buffer is a
;; processed view that unescapes framed newlines so multi-line posts read
;; naturally.  The board pane reads the top active cards from loom/ and
;; will eventually retire loom/board.md as the canonical index.
;; Posts are sent via `muster post' and identity is kept in sync with
;; `muster-name' by calling `muster name' first.

;;; Code:

(require 'filenotify)
(require 'seq)

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

(defcustom muster-board-buffer-name "*muster-board*"
  "Name of the board buffer."
  :type 'string
  :group 'muster-deck)

(defcustom muster-board-file "~/coffee/loom/board.md"
  "Path to the board markdown file.
This is a transitional index. The long-term goal is for *muster-board*
to derive its view directly from the loom/ cards, retiring this file."
  :type 'string
  :group 'muster-deck)

(defcustom muster-board-current "deck.md"
  "Current active card to show in the board buffer.
When non-nil, only this card is shown instead of the top N active items."
  :type 'string
  :group 'muster-deck)

(defcustom muster-board-show-progress t
  "Whether to render each card's `### progress' section in *muster-board*."
  :type 'boolean
  :group 'muster-deck)

;;; Core helpers

(defun muster--expand-root ()
  "Expand `muster-root' to an absolute path."
  (expand-file-name muster-root))

(defun muster--log-file ()
  "Return path to the current channel log file."
  (expand-file-name (format "%s/log.md" muster-channel) (muster--expand-root)))

(defun muster--board-file ()
  "Return expanded path to the board file."
  (expand-file-name muster-board-file))

(defun muster--parse-active-items ()
  "Parse the current task from `muster-board-file'.
When `muster-board-current' is set, returns only that card's info.
Otherwise returns all active (🟣) items.
Returns a list of (TITLE PATH STATUS PROGRESS) tuples."
  (let ((file (muster--board-file))
        items in-active)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match-p "^## *active" line)
              (setq in-active t))
             ((and in-active (string-match-p "^##" line))
              (setq in-active nil))
             ((and in-active (string-match "^ *\\(?:[📬👟⚙✰🕳🌀] \\)?🟣 +\\[\\([^]]+\\)\\](\\([^)]+\\))" line))
              (let* ((title (match-string 1 line))
                     (path (match-string 2 line))
                     (card-info (muster--parse-card-info path)))
                (push (list title path
                            (car card-info)
                            (cadr card-info))
                      items)))))
          (forward-line 1))))
    (let ((result (nreverse items)))
      (if muster-board-current
          (seq-filter (lambda (it) (string= (cadr it) muster-board-current)) result)
        result))))

(defun muster--parse-card-info (path)
  "Read card PATH and return (STATUS PROGRESS).
STATUS is the `status' line text.  PROGRESS is a list of non-empty
lines from the `### progress' section, if present."
  (let ((file (expand-file-name path (file-name-directory (muster--board-file))))
        status progress in-progress)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((and (null status) (string-match "^status\\s-+\\(.+\\)" line))
              (setq status (match-string 1 line)))
             ((string-match-p "^### *progress" line)
              (setq in-progress t))
             ((and in-progress (string-match-p "^#" line))
              (setq in-progress nil))
             (in-progress
              (let ((trimmed (string-trim line)))
                (unless (string-empty-p trimmed)
                  (push trimmed progress))))))
            (forward-line 1))))
    (list status (nreverse progress))))

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

;;; Board buffer

(defvar muster-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'muster-board)
    map)
  "Keymap for `muster-board-mode'.")

(define-derived-mode muster-board-mode text-mode "Muster-Board"
  "Major mode for the board pane."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

(defun muster-board--open-card (path)
  "Open board card PATH in a new frame."
  (let ((full (expand-file-name path (file-name-directory (muster--board-file)))))
    (find-file-other-frame full)))

(defun muster-board--render ()
  "Render the board content into the current buffer."
  (let ((items (muster--parse-active-items))
        (inhibit-read-only t))
    (erase-buffer)
    (insert "⟴ board\n\n")
    (if (null items)
        (insert "  no active items\n")
      (dolist (item items)
        (let* ((title (car item))
               (path (cadr item))
               (status (nth 2 item))
               (progress (nth 3 item))
               (start (point)))
          (insert (format "  %s" title))
          (let ((title-end (point)))
            (when status
              (insert (format "  — %s" status)))
            (insert "\n")
            (make-text-button start title-end
                              'action (lambda (_btn) (muster-board--open-card path))
                              'follow-link t
                              'help-echo (format "Open %s" path)))
          (when (and muster-board-show-progress progress)
            (dolist (line progress)
              (insert (format "    %s\n" line)))))))))

;;;###autoload
(defun muster-board ()
  "Open the board buffer with the top active loom cards and progress sections."
  (interactive)
  (let ((buf (get-buffer-create muster-board-buffer-name)))
    (with-current-buffer buf
      (muster-board--render)
      (muster-board-mode))
    (pop-to-buffer buf)))

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

(defun muster-log--strip-timestamps (s)
  "Remove `[YYYY-MM-DDTHH:MM:SS] ' prefixes from log lines."
  (replace-regexp-in-string "^\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\] " "" s))

(defun muster-log-refresh ()
  "Refresh the log buffer from the file, preserving scroll position.
Skip if the file content is unchanged since the last refresh.  Keeps the
tail in view for any window that was already at the end of the log."
  (when-let ((buf (get-buffer muster-log-buffer-name)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (raw (muster-log--read-file))
             (contents (muster-log--unescape raw))
             (contents (if muster-log-show-timestamps
                           contents
                         (muster-log--strip-timestamps contents))))
        (unless (string= contents (buffer-string))
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
  "Open the full deck: board + log side by side, compose across the bottom."
  (interactive)
  (delete-other-windows)
  ;; Split into top and bottom first, so bottom spans full width.
  (split-window-below)
  (let* ((top-win (selected-window))
         (bottom-win (next-window))
         (board-buf (get-buffer-create muster-board-buffer-name))
         (log-buf (get-buffer-create muster-log-buffer-name))
         (compose-buf (get-buffer-create muster-compose-buffer-name))
         (top-right (split-window top-win nil 'right)))
    ;; Top-left: board
    (set-window-buffer top-win board-buf)
    (with-current-buffer board-buf
      (muster-board-mode)
      (muster-board--render))
    ;; Top-right: log
    (set-window-buffer top-right log-buf)
    (with-current-buffer log-buf
      (muster-log-mode)
      (setq buffer-read-only t)
      (muster-log-start-watch)
      (muster-log-refresh)
      (goto-char (point-max)))
    (set-window-point top-right (with-current-buffer log-buf (point-max)))
    ;; Bottom: compose
    (select-window bottom-win)
    (set-window-buffer bottom-win compose-buf)
    (with-current-buffer compose-buf
      (unless (derived-mode-p 'muster-compose-mode)
        (muster-compose-mode)))
    (select-window top-win)))

;;;###autoload
(defun muster-deck-quit ()
  "Close muster deck buffers and stop the log watcher."
  (interactive)
  (muster-log-stop-watch)
  (dolist (buf (list muster-compose-buffer-name
                     muster-log-buffer-name
                     muster-board-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

(provide 'muster-deck)
;;; muster-deck.el ends here
