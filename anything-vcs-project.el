;;; anything-vcs-project.el --- anything interface for version controled files

;; Copyright (C) 2011  podhmo

;; Author: podhmo  <ababjam61@gmail.com>
;; Keywords: convenience, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))
(require 'with-prefix)

(with-prefix ((@ anything-vcs-project:)
              (% anything-vcs-project-util:)
              (git. anything-vcs-project-git:)
              (hg. anything-vcs-project-hg:)
              (cache. anything-vcs-project-cache:))

  ;; util macro
  (defmacro @with-lexical-bindings (syms &rest body)
    (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
      (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
  (put '@with-lexical-bindings 'lisp-indent-function 1)

  (defmacro @with-gensyms (syms &rest body)
    (let ((bindings (mapcar (lambda (x) (\` ((\, x) '(\, (gensym))))) syms)))
      (\` (let ((\,@ bindings)) (\,@ body)))))
  (put '@with-gensyms 'lisp-indent-function 1)

  (defmacro @rlet1 (var val &rest body)
    "imported from gauche"
    (\` (@let1 (\, var) (\, val) (\,@ body) (\, var))))
  (put '@rlet1 'lisp-indent-function 2)

  (defmacro @let1 (var val &rest body)
    "imported from gauche"
    (\` (let (((\, var) (\, val))) (\,@ body))))
  (put '@let1 'lisp-indent-function 2)

  (defmacro @and-let* (bindings &rest body)
    "imported from srfi-2"
    (reduce (function
             (lambda (binding r)
               (let ((head (car binding)))
                 (cond ((and (atom head) (symbolp head))
                        (\` (let ((\, binding)) (when (\, head) (\, r)))))
                       ((listp head)
                        (\` (when (\, head) (\, r))))
                       (t (error "and-let*: invalid head %s" head))))))
            bindings
            :from-end
            t
            :initial-value
            (\` (progn (\,@ body)))))
  (put '@and-let* 'lisp-indent-function 1)

  (defmacro @alambda (args &rest body)
    "Anaphoric lambda. enable to self recursion using `self' anaphorar"
    (\` (labels ((self (\, args) (\,@ body))) (function self))))
  (put '@alambda 'lisp-indent-function 1)

  (defmacro @aand (&rest args)
    "Anaphoric and. anaphorar is `it'"
    (cond ((null args)
           t)
          ((null (cdr args))
           (car args))
          (t (\` (@aif (\, (car args)) (@aand (\,@ (cdr args))))))))

  (defmacro @aif (test-form then-form &rest else-forms)
    "Anaphoric if. Temporary variable `it' is the result of test-form."
    (\` (let ((it (\, test-form))) (if it (\, then-form) (\,@ else-forms)))))
  (put '@aif 'lisp-indent-function 2)

  ;; util
  (defun %all-parent-path-candidates (head words)
    "create all candidates: path -> (\"~/foo/bar/boo\" \"~/foo/bar\" \"~/foo\" \"~\")"
    (loop for word in words
          with acc = head
          unless (string-equal "" word)
          do (setq acc (concat acc "/" word))
          and collect acc into result
          finally return (nreverse (cons head result))))
  
  (defun %check-target-is-exist-in-path (path target)
    "checking targetted file is exist in path."
    (destructuring-bind (head . words) (split-string path "/")
      (@let1 candidates (%all-parent-path-candidates head words)
        (lexical-let ((target target))
          (find target candidates
                :test (lambda (target path)
                        (file-exists-p (concat path "/" target))))))))

  (defun %in-expected-repository (expexted-dir &optional default-dir)
    "if in expexted file(directory) in repository, then return root path of one"
    (@and-let* ((default-dir (or default-dir default-directory)))
      (%check-target-is-exist-in-path (file-truename default-dir) expexted-dir)))

  (defun %commad-to-buffer (bufname root command args exclude-args &optional filter-hook)
    "notice: args is string. not list"
    (when (get-buffer bufname)
      (with-current-buffer bufname (erase-buffer)))
    (let* ((cmd (format "cd %s && %s %s %s" root command args
                        exclude-args))
           (buf (get-buffer-create bufname)))
      (and root
           (with-current-buffer buf
             (insert (shell-command-to-string cmd))
             (and filter-hook (funcall filter-hook))
             (current-buffer)))))

  (defun %path-join (dir fname)
    (let ((dir* (if (char-equal ?/ (aref dir (- (length dir) 1)))
                    (substring-no-properties dir 0 -1)
                    dir))
          (fname* (if (char-equal ?/ (aref fname 0))
                      (substring-no-properties fname 1)
                      fname)))
      (concat dir* "/" fname*)))
  
  ;; hg mercurial
  (defvar hg.exclude-args "--exclude '*.pyc' --exclude .git")

  (defun hg.generate-source (header-fmt root-dir command &optional hook)
    `((name . ,(format header-fmt root-dir))
      (init . (lambda ()
                ;; update all project-files every time (it is heavy?)
                (let* ((bufname (format " *%s*" ,header-fmt)) ;; slak-off
                       (buf (%commad-to-buffer 
                             bufname ,root-dir ,command "" hg.exclude-args ,hook)))
                  (anything-candidate-buffer buf))))
      (display-to-real . (lambda (c) (%path-join ,root-dir c)))
      (candidates-in-buffer)
      (type . file)))

  (defun hg.anything-c-sources-hg-project-for (root-dir)
    (labels ((replace-generate
              (pat)
              (@with-lexical-bindings (pat)
                (lambda ()
                  (save-excursion (goto-char (point-min))
                                  (while (re-search-forward pat nil t 1)
                                    (replace-match "")))))))
      (list (hg.generate-source "Modified files (%s)" 
                                root-dir
                                "hg status --modified"
                                (replace-generate  "^M "))
            (hg.generate-source "Untracked files (%s)"
                                root-dir
                                "hg status --unknown"
                                (replace-generate  "^\\? "))
            (hg.generate-source "All files in this project (%s)"
                                root-dir
                                "hg locate"))))
  
  ;; git
  (defvar git.exclude-args "--exclude .hg")
  
  (defun git.anything-c-sources-git-project-for (root-dir)
    (loop for (header-fmt . command-args) in
          '(("Modified files (%s)" . "--modified")
            ("Untracked files (%s)" . "--others --exclude-standard")
            ("All files in this project (%s)" . ""))
          collect
          `((name . ,(format header-fmt root-dir))
            (init . (lambda ()
                      ;; update all project-files every time (it is heavy?)
                      (let* ((bufname (format " *%s*" ,header-fmt)) ;; slak-off
                             (buf (%commad-to-buffer 
                                   bufname ,root-dir "git ls-files" ,command-args git.exclude-args)))
                        (anything-candidate-buffer buf))))
            (display-to-real . (lambda (c) (concat ,root-dir "/" c)))
            (candidates-in-buffer)
            (type . file))))

  ;; project
  (defvar @cache-enable-p t)

  (defvar cache.project-list-path "~/.emacs.d/.project.list")
    
  (defvar @anything-c-sources-project 
    '((name . "vcs project")
      (candidates-file cache.project-list-path t)
      (action . (lambda (c)
                  (run-with-timer 0.01 nil  'anything-vcs-project c t)))))

  (defun cache.add-item (item)
    (@let1 buf (find-file-noselect cache.project-list-path)
      (with-current-buffer buf
        (save-excursion 
          (goto-char (point-min))
          (unless (re-search-forward item nil t 1)
            (goto-char (point-min))
            (insert item "\n")
            (save-buffer))))))
   
  ;; wrap
  (defun @select-x-project (&optional default-dir)
    (or
     (@aif (%in-expected-repository ".hg" default-dir)
         (values 'hg.anything-c-sources-hg-project-for it "hg"))
     (@aif (%in-expected-repository ".git" default-dir)
         (values 'git.anything-c-sources-git-project-for it "git"))
    ))

  (defun anything-vcs-project (&optional default-dir project-select-is-disable-p)
    (interactive)
    (multiple-value-bind (source-generator root-dir vcs-name)
        (@select-x-project default-dir)
      (cond (root-dir
             (let* ((sources (funcall source-generator root-dir))
                    (sources* (if project-select-is-disable-p
                                  sources
                                  (cons '@anything-c-sources-project sources)))
                    (any-buffer (format "*Anything %s project in %s*" vcs-name root-dir)))       
               
               (when @cache-enable-p
                 (cache.add-item root-dir))
               (anything-other-buffer sources* any-buffer)))
            ((not project-select-is-disable-p)
             (anything-other-buffer '(@anything-c-sources-project) " *anything vcs project*")))))
  )

(provide 'anything-vcs-project)
;;; anything-vcs-project.el ends here
