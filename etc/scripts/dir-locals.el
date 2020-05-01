;; Project specific configuration for using Emacs for work on this project.
;; Adjust the settings in this file and copy it to the project root with the
;; name ".dir-locals.el" (note the first dot).

;; In particular you'll want to update the following:
;;
;; `sql-database` to point to whatever database you want to use during
;; development.
;;
;; remove traces of `ah-project-headline`, `ah-project-license` and
;; `ah-do-fixups` since these depend on unpublished Emacs List configuration
;; files.

((nil . ((sql-database . "~/AppData/Local/ActivityLog/Alex-AL.db")
         ;; Link both GitHub and AzureBoards issues
         (bug-reference-bug-regexp . "\\(?1:AB\\)?\\#\\(?2:[0-9]+\\)\\>")
         (bug-reference-url-format
          .
          (lambda ()
            (let ((kind (match-string 1))
                  (num (match-string 2)))
              (cond
                ((equal "AB" kind)
                 (format "https://dev.azure.com/alexharsanyi0641/ActivityLog2/_workitems/edit/%s" num))
                (t
                 (format "https://github.com/alex-hhh/ActivityLog2/issues/%s" num))))))
         (indent-tabs-mode         . nil)
         (require-final-newline    . t)
         (show-trailing-whitespace . t)
         (ah-project-headline . "ActivityLog2 -- https://github.com/alex-hhh/ActivityLog2")
         (ah-project-license . gpl)))
 (yaml-mode . ((mode . goto-address)
               (mode . bug-reference)))
 (shell-script-mode . ((mode . goto-address)
                       (mode . bug-reference)))
 (log-view-mode . ((mode . goto-address)
                   (mode . bug-reference)))
 (log-edit-mode . ((mode . goto-address)
                   (mode . bug-reference)))
 (markdown-mode . ((mode . goto-address)
                   (mode . bug-reference)))
 (racket-mode . ((mode . goto-address)
                 (mode . bug-reference)
                 (ah-do-fixups . t)))
 (sql-mode . ((mode . goto-address)
              (mode . bug-reference)
              (sql-product . sqlite)
              (ah-do-fixups . t))))
