;;; gitmodules/fzf.el/counsel-fzf.el -*- lexical-binding: t; -*-

;;;
(defun night/helper-counsel-fzf-entries (str)
  (let ((entries night/counsel--fzf-entries))
    (setq ivy--old-re (ivy--regex-fuzzy str))
    (counsel--async-command
     (format (concat  "echo %s | " counsel-fzf-cmd) (shell-quote-argument (mapconcat (lambda (x) x) entries "\n")) str)
     ;; (format "echo hi %s wow | cat" str)
     ))
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
(defun night/fzf-recentf ()
  (interactive)
  (night/counsel-fzf-with-entries recentf-list
                                  (lambda (f) (progn
                                                ;; (message "DBG: %s" f )
                                                (find-file-existing f)))))

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
