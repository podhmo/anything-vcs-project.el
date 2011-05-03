(unless (fboundp 'current-directory)
  (defun current-directory ()
    (cond (load-in-progress (file-name-directory load-file-name))
          (t default-directory)))
)

(add-to-list 'load-path (current-directory))
(require 'anything-vcs-project)

;; ;; debugging
;;(find-function 'anything-vcs-project)
;;(find-variable 'anything-vcs-project:cache-enable-p)
;;(with-shortcut "anything-vcs-project.el" (find-variable '@cache-enable-p))
