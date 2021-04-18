;;; gitmodules/fzf.el/counsel-fzf.el -*- lexical-binding: t; -*-

;;;
(comment (defun counsel--async-command-1 (cmd &optional sentinel filter name)
           "Start and return new counsel process by calling CMD.
CMD can be either a shell command as a string, or a list of the
program name to be called directly, followed by its arguments.
If the default counsel process or one with NAME already exists,
kill it and its associated buffer before starting a new one.
Give the process the functions SENTINEL and FILTER, which default
to `counsel--async-sentinel' and `counsel--async-filter',
respectively."
           (counsel-delete-process name)
           (setq name (or name " *counsel*"))
           (when (get-buffer name)
             (kill-buffer name))
           (setq counsel--async-last-command cmd)
           (let* ((process-connection-type nil)
                  (buf (get-buffer-create name))
                  (proc (if (listp cmd)
                            (apply #'start-file-process name buf cmd)
                          (start-file-process-shell-command name buf cmd)
                          )))
             (setq counsel--async-time (current-time))
             (setq counsel--async-start counsel--async-time)
             (set-process-sentinel proc (or sentinel #'counsel--async-sentinel))
             (set-process-filter proc (or filter #'counsel--async-filter))
             (when (boundp 'night/counsel--stdin)
               (progn
                 ;; (message "DBG: %s, %s" buf proc)
                 (process-send-string buf (or night/counsel--stdin ""))
                 (process-send-eof proc)
                 ))
             proc))
         )
;;;
(comment
 (defun night/helper-counsel-fzf-entries (str)
   (let ((default-directory "/")        ; TRAMP: The point is default-directory. If it is local, your command runs locally
         (entries night/counsel--fzf-entries))
     (setq ivy--old-re (ivy--regex-fuzzy str))
     (setq night/counsel--stdin (mapconcat (lambda (x) x) entries "\n"))
     (let ((night/counsel--stdin (mapconcat (lambda (x) x) entries "\n")))
       (counsel--async-command
        (list "fzf_in.dash" "-f" str)
        ;; (format (concat  "echo %s | " counsel-fzf-cmd) (shell-quote-argument (mapconcat (lambda (x) x) entries "\n")) str)
        ;; (format "echo hi %s wow | cat" str)
        )))
   nil)
 )
;;;
(if load-file-name
    (progn (setq night/fzf-cmd (list (concat (file-name-directory load-file-name) "/fzf_in2.dash")))
           (setq night/fzf-cmd-args '())))
(defun night/helper-counsel-fzf-entries (str)
  (let ((default-directory "/")        ; TRAMP: The point is default-directory. If it is local, your command runs locally
        (entries night/counsel--fzf-entries))
    (cond
     ((equal entries "MAGIC_CLIPBOARD_READ")
      ;; (setq ivy--old-re (ivy--regex-fuzzy str)) ; too slow
      (setq ivy--old-re "")
      (counsel--async-command
       (-concat night/fzf-cmd (list (getenv "CLIPBOARD_RECORD_FILE")) night/fzf-cmd-args (list "-f" str "--read0" "--print0" "--tac" "--tiebreak=index")))
      )
     (t
      (setq ivy--old-re (ivy--regex-fuzzy str))
      (let ((night/counsel--stdin (mapconcat (lambda (x) x) entries "\n")))
        (f-write-text night/counsel--stdin 'utf-8 "/tmp/nightFzf.txt")
                                        ; @bug https://emacs.stackexchange.com/questions/63507/how-to-run-commands-locally-even-when-on-tramp
                                        ; The problem is that nightFzf.txt is created locally but the command runs on the remote server.
        (counsel--async-command
         (-concat night/fzf-cmd (list "/tmp/nightFzf.txt") night/fzf-cmd-args (list "-f" str))
         )))))
  nil)
(defun night/counsel-fzf-with-entries (entries &optional action prompt)
  (interactive)
  (setq night/counsel--fzf-entries entries)
  (ivy-read (or prompt "")
            #'night/helper-counsel-fzf-entries
            :initial-input ""
            ;; :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :unwind #'counsel-delete-process
            :action (or action #'counsel-fzf-action)
            :caller 'counsel-fzf))
;;;
(if (stringp (getenv "NIGHTDIR"))
    (setq vfiles (let
                     ;; `ec $textglob | sd -s '|' '\\|'`
                     ((re "\\.\\(txt\\|md\\|org\\|m\\|cpp\\|h\\|c\\|applescript\\|as\\|osa\\|nu\\|nush\\|el\\|py\\|jl\\|scala\\|sc\\|kt\\|kotlin\\|java\\|clj\\|cljs\\|rkt\\|js\\|rs\\|zsh\\|dash\\|bash\\|sh\\|ml\\|php\\|lua\\|glsl\\|frag\\|go\\|ini\\|json\\|cson\\|toml\\|conf\\|plist\\|xml\\)$"))
                   (-concat
                    (directory-files-recursively (getenv "NIGHTDIR") re)
                    (directory-files-recursively (getenv "DOOMDIR") re)
                    (directory-files-recursively (getenv "nightNotes") re)
                    (directory-files-recursively (concat (getenv "codedir") "/nodejs") re)
                    (directory-files-recursively (concat (getenv "codedir") "/lua") re)
                    (directory-files-recursively (concat (getenv "codedir") "/python") re)
                    (directory-files-recursively (concat (getenv "codedir") "/uni") re)
                    (directory-files-recursively (concat (getenv "codedir") "/rust") re)
                    (directory-files-recursively (concat (getenv "codedir") "/golang") re))))
  (setq vfiles '()))
;; (nconc recentf-list vfiles) ; adds vfiles to the end of recentf-list
(defun night/fzf-recentf ()
  (interactive)
  (night/counsel-fzf-with-entries
   ;; recentf-list
   (-concat recentf-list vfiles)
   ;; vfiles
   (lambda (f) (progn
                 ;; (message "DBG: %s" f )
                 (find-file-existing f)))))
;;;
;; @solvedBug https://github.com/abo-abo/swiper/issues/2830 previous ivy-read dynamic collection pollutes new calls to ivy-read : use `:unwind #'counsel-delete-process`
(defun night/fzf-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence."
  (interactive)
  ;; When `counsel-M-x' returns, `last-command' would be set to
  ;; `counsel-M-x' because :action hasn't been invoked yet.
  ;; Instead, preserve the old value of `this-command'.
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (let ((externs (counsel--M-x-externs)))
    (setq night/counsel--fzf-entries (or externs obarray))
    (ivy-read (counsel--M-x-prompt)
              ;; (or externs obarray)
              #'night/helper-counsel-fzf-entries
              :predicate (if externs
                             (lambda (x)
                               (not (get (intern x) 'no-counsel-M-x)))
                           (lambda (sym)
                             (and (commandp sym)
                                  (not (get sym 'byte-obsolete-info))
                                  (not (get sym 'no-counsel-M-x)))))
              :require-match t
              :history 'counsel-M-x-history
              :action #'counsel-M-x-action
              ;; :re-builder #'ivy--regex-fuzzy
              :dynamic-collection t
              :keymap counsel-describe-map
              :initial-input initial-input
              :unwind #'counsel-delete-process
              :caller 'counsel-M-x)))

;;;
(defvar counsel-clipboard-history nil
  "History for `night/counsel-clipboard'.")
(defun night/counsel-clipboard ()
  "Interactively paste. Multiple selections are, of course, possible (see ivy-mark). Use C-o to see other options including copying the selection."
  (interactive)
  ;; let doesn't work for this
  (setq night/counsel--fzf-entries "MAGIC_CLIPBOARD_READ")
  (ivy-read "Clipboard: "
            #'night/helper-counsel-fzf-entries
            :require-match t
            ;; :history 'counsel-clipboard-history
            :action #'night/insert-from-clipboard
            :multi-action #'night/insert-multiple
            :dynamic-collection t
            :unwind #'counsel-delete-process
            ;; :caller 'counsel-register
            :caller 'night/counsel-clipboard
            ))

(after! (counsel)
  (add-to-list 'counsel-async-split-string-re-alist '(night/counsel-clipboard . "\x00"))
  (add-to-list 'ivy-re-builders-alist '(night/counsel-clipboard . ivy--regex-plus))
  )

(defun night/insert-from-clipboard (input)
  (let ((items (if (listp input)
                   input
                 (list input))))
    ;; See evil-visual-paste
    (when (evil-visual-state-p)
      ;; (z fsay hello)
      (evil-delete evil-visual-beginning
                   evil-visual-end
                   (evil-visual-type)
                   (unless evil-kill-on-visual-paste ?_)))

    (dolist (item items)
      (let ((parts (split-string item "" t)))
        (insert-for-yank (car parts)))))
  (redraw-display) ;; good for emojis
  )
(defun night/insert-multiple (items)
  (night/insert-from-clipboard
   items
   ;; (mapconcat (lambda (x) x) items "\n")
   )
  )
;; (map! :leader "zp" #'night/counsel-clipboard)
(map! :nvig "C-p" #'night/counsel-clipboard)
;; (map! :nvig "C-v" #'night/counsel-clipboard)

;;;
