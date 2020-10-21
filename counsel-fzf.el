;;; gitmodules/fzf.el/counsel-fzf.el -*- lexical-binding: t; -*-

(defun night/fzf-recentf ()
  (interactive)
  (night/counsel-fzf-with-entries recentf-list
                    (lambda (f) (progn
                                  ;; (message "DBG: %s" f )
                                  (find-file-existing f)))))

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
            :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :action (or action #'counsel-fzf-action)
            :caller 'counsel-fzf))
;; (counsel-fzf "" (getenv "nightNotesi") "")
