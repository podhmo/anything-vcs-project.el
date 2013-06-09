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

;; util macro
(defmacro anything-vcs-project:with-lexical-bindings (syms &rest body)
  (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
    (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
(put 'anything-vcs-project:with-lexical-bindings 'lisp-indent-function 1)

(defmacro anything-vcs-project:with-gensyms (syms &rest body)
  (let ((bindings (mapcar (lambda (x) (\` ((\, x) '(\, (gensym))))) syms)))
    (\` (let ((\,@ bindings)) (\,@ body)))))
(put 'anything-vcs-project:with-gensyms 'lisp-indent-function 1)

(defmacro anything-vcs-project:rlet1 (var val &rest body)
  "imported from gauche"
  (\` (anything-vcs-project:let1 (\, var) (\, val) (\,@ body) (\, var))))
(put 'anything-vcs-project:rlet1 'lisp-indent-function 2)

(defmacro anything-vcs-project:let1 (var val &rest body)
  "imported from gauche"
  (\` (let (((\, var) (\, val))) (\,@ body))))
(put 'anything-vcs-project:let1 'lisp-indent-function 2)

(defmacro anything-vcs-project:and-let* (bindings &rest body)
  "imported from srfi-2"
  (declare (indent 1))
  (reduce #'(lambda (binding r)
              (let ((head (car binding)))
                (cond ((and (atom head) (symbolp head))
                       `(let (,binding)
                          (when ,head ,r)))
                      ((listp head)
                       `(when ,head ,r))
                      (t
                       (error "and-let*: invalid head %s" head)))))
          bindings :from-end t :initial-value `(progn ,@body)))
(put 'anything-vcs-project:and-let* 'lisp-indent-function 1)

(defmacro anything-vcs-project:alambda (args &rest body)
  "Anaphoric lambda. enable to self recursion using `self' anaphorar"
  (\` (labels ((self (\, args) (\,@ body))) (function self))))
(put 'anything-vcs-project:alambda 'lisp-indent-function 1)

(defmacro anything-vcs-project:aand (&rest args)
  "Anaphoric and. anaphorar is `it'"
  (cond ((null args)
         t)
        ((null (cdr args))
         (car args))
        (t (\` (anything-vcs-project:aif (\, (car args)) (anything-vcs-project:aand (\,@ (cdr args))))))))

(defmacro anything-vcs-project:aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (\` (let ((it (\, test-form))) (if it (\, then-form) (\,@ else-forms)))))
(put 'anything-vcs-project:aif 'lisp-indent-function 2)

;; util
(defun avp-util:all-parent-path-candidates (head words)
  "create all candidates: path -> (\"~/foo/bar/boo\" \"~/foo/bar\" \"~/foo\" \"~\")"
  (loop for word in words
        with acc = head
        unless (string-equal "" word)
        do (setq acc (concat acc "/" word))
        and collect acc into result
        finally return (nreverse (cons head result))))

(defun avp-util:check-target-is-exist-in-path (path target)
  "checking targetted file is exist in path."
  (destructuring-bind (head . words) (split-string path "/")
    (anything-vcs-project:let1 candidates (avp-util:all-parent-path-candidates head words)
      (lexical-let ((target target))
        (find target candidates
              :test (lambda (target path)
                      (file-exists-p (concat path "/" target))))))))

(defun avp-util:in-expected-repository (expexted-dir &optional default-dir)
  "if in expexted file(directory) in repository, then return root path of one"
  (let
      ((default-dir (or default-dir default-directory)))
    (and default-dir
         (avp-util:check-target-is-exist-in-path (file-truename default-dir) expexted-dir))))


(defun avp-util:commad-to-buffer (bufname root command args exclude-args &optional filter-hook)
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

(defun avp-util:path-join (dir fname)
  (let ((dir* (if (char-equal ?/ (aref dir (- (length dir) 1)))
                  (substring-no-properties dir 0 -1)
                dir))
        (fname* (if (char-equal ?/ (aref fname 0))
                    (substring-no-properties fname 1)
                  fname)))
    (concat dir* "/" fname*)))

;; hg mercurial
(defvar avp-hg:exclude-args "--exclude '*.pyc' --exclude .git")

(defun avp-hg:generate-source (header-fmt root-dir command &optional hook)
  `((name . ,(format header-fmt root-dir))
    (init . (lambda ()
              ;; update all project-files every time (it is heavy?)
              (let* ((bufname (format " *%s*" ,header-fmt)) ;; slak-off
                     (buf (avp-util:commad-to-buffer 
                           bufname ,root-dir ,command "" avp-hg:exclude-args ,hook)))
                (anything-candidate-buffer buf))))
    (display-to-real . (lambda (c) (avp-util:path-join ,root-dir c)))
    (candidates-in-buffer)
    (type . file)))

(defun avp-hg:anything-c-sources-hg-project-for (root-dir)
  (labels ((replace-generate
            (pat)
            (anything-vcs-project:with-lexical-bindings (pat)
              (lambda ()
                (save-excursion (goto-char (point-min))
                                (while (re-search-forward pat nil t 1)
                                  (replace-match "")))))))
    (list (avp-hg:generate-source "Modified files (%s)" 
                                  root-dir
                                  "hg status --modified"
                                  (replace-generate  "^M "))
          (avp-hg:generate-source "Untracked files (%s)"
                                  root-dir
                                  "hg status --unknown"
                                  (replace-generate  "^\\? "))
          (avp-hg:generate-source "All files in this project (%s)"
                                  root-dir
                                  "hg locate"))))

;; git
(defvar avp-git:exclude-args "--exclude .hg")

(defun avp-git:anything-c-sources-git-project-for (root-dir)
  (loop for (header-fmt . command-args) in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All files in this project (%s)" . ""))
        collect
        `((name . ,(format header-fmt root-dir))
          (init . (lambda ()
                    ;; update all project-files every time (it is heavy?)
                    (let* ((bufname (format " *%s*" ,header-fmt)) ;; slak-off
                           (buf (avp-util:commad-to-buffer 
                                 bufname ,root-dir "git ls-files" ,command-args avp-git:exclude-args)))
                      (anything-candidate-buffer buf))))
          (display-to-real . (lambda (c) (concat ,root-dir "/" c)))
          (candidates-in-buffer)
          (type . file))))

;; project
(defvar anything-vcs-project:enable-p t)

(defvar avp-cache:project-list-path "~/.emacs.d/.project.list")

(defvar anything-vcs-project:anything-c-sources-project 
  '((name . "vcs project")
    (candidates-file avp-cache:project-list-path t)
    (action . (lambda (c)
                (run-with-timer 0.01 nil  'anything-vcs-project c t)))))

(defun avp-cache:add-item (item)
  (anything-vcs-project:let1 buf (find-file-noselect avp-cache:project-list-path)
    (with-current-buffer buf
      (save-excursion 
        (goto-char (point-min))
        (unless (re-search-forward item nil t 1)
          (goto-char (point-min))
          (insert item "\n")
          (save-buffer))))))

;; wrap
(defun anything-vcs-project:select-x-project (&optional default-dir)
  (or
   (anything-vcs-project:aif (avp-util:in-expected-repository ".hg" default-dir)
       (values 'avp-hg:anything-c-sources-hg-project-for it "hg"))
   (anything-vcs-project:aif (avp-util:in-expected-repository ".git" default-dir)
       (values 'avp-git:anything-c-sources-git-project-for it "git"))
   ))

(defun anything-vcs-project (&optional default-dir project-select-is-disable-p)
  (interactive)
  (multiple-value-bind (source-generator root-dir vcs-name)
      (anything-vcs-project:select-x-project default-dir)
    (cond (root-dir
           (let* ((sources (funcall source-generator root-dir))
                  (sources* (if project-select-is-disable-p
                                sources
                              (cons 'anything-vcs-project:anything-c-sources-project sources)))
                  (any-buffer (format "*Anything %s project in %s*" vcs-name root-dir)))       
             
             (when anything-vcs-project:enable-p
               (avp-cache:add-item root-dir))
             (anything-other-buffer sources* any-buffer)))
          ((not project-select-is-disable-p)
           (anything-other-buffer '(anything-vcs-project:anything-c-sources-project) " *anything vcs project*")))))


(provide 'anything-vcs-project)
;;; anything-vcs-project.el ends here
