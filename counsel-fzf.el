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
   (let ((entries night/counsel--fzf-entries))
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
    (setq night/fzf-cmd (concat (file-name-directory load-file-name) "/fzf_in2.dash")))
(defun night/helper-counsel-fzf-entries (str)
  (let ((entries night/counsel--fzf-entries))
    (setq ivy--old-re (ivy--regex-fuzzy str))
    (let ((night/counsel--stdin (mapconcat (lambda (x) x) entries "\n")))
      (f-write-text night/counsel--stdin 'utf-8 "/tmp/nightFzf.txt")
      (counsel--async-command
       (list night/fzf-cmd "-f" str)
       )))
  nil)
(defun night/counsel-fzf-with-entries (entries &optional  action prompt)
  (interactive)
  (setq night/counsel--fzf-entries entries)
  (ivy-read (or prompt "")
            #'night/helper-counsel-fzf-entries
            :initial-input ""
            ;; :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :action (or action #'counsel-fzf-action)
            :caller 'counsel-fzf))
;;;
(if (stringp (getenv "NIGHTDIR"))
    (setq vfiles (let
                     ;; `ec $textglob | sd -s '|' '\\|'`
                     ((re "\\.\\(txt\\|md\\|org\\|m\\|cpp\\|h\\|c\\|applescript\\|as\\|osa\\|nu\\|nush\\|el\\|py\\|jl\\|scala\\|sc\\|kt\\|kotlin\\|java\\|clj\\|cljs\\|rkt\\|js\\|rs\\|zsh\\|dash\\|bash\\|sh\\|ml\\|php\\|lua\\|glsl\\|frag\\|go\\|ini\\|json\\|cson\\|toml\\|conf\\|plist\\|xml\\)$"))
                   (-concat
                    (directory-files-recursively (getenv "NIGHTDIR") re)
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
              :caller 'counsel-M-x)))
