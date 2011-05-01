;; == with-prefix.el ==

;;;; how to use
;; (with-prefix 
;;     ((@ wp:)
;;      (util. with-prefix-util:))
  
;;   (defun @odd? (x) 
;;     "check a received argument is odd number or not"
;;     (if (<= x 0) nil (@even? (- x 1))))
  
;;   (defun @even? (x)
;;     (if (<= x 0) t (@odd? (- x 1))))

;;   (defun util.out (&rest args) 
;;     (message (prin1-to-string args)))

;; (util.out (@odd? 9) 
;;           (@even? 9) 
;;           (funcall '@odd? 10)
;;           (apply '@even? '(10))))

;; (setq debug-on-error t)
(eval-when-compile (require 'cl))

;; utility
(defun wp:x-to-string (x)
  (format "%s" x))


(defun wp:x-to-prefix-regexp (x)
  (let ((xs (split-string (wp:x-to-string x) "\\.")))
    (concat "^" (mapconcat 'identity xs "\\."))))

(defun wp:tree-map-safe (fn tree)
  "`wp:mapcar-safe' recursive version"
  (lexical-let ((fn fn))
    (labels ((rec (tree)
                  (wp:mapcar-safe #'(lambda (x) (if (listp x) (rec x) (funcall fn x)))
                                  tree)))
      (rec tree))))

(defun wp:mapcar-safe (fn maybe-list)
  "mapcar enable to iterate maybe-list (include dot-list)"
  (let ((r (list)) (xs maybe-list))
    (condition-case e
        (progn
          (while (not (null xs))
            (push (funcall fn (car xs)) r)
            (setq xs (cdr xs)))
          (nreverse r))
      (wrong-type-argument 
       (let ((r* (nreverse r)))
         (setcdr (last r*) (funcall fn xs))
         r*)))))


(defmacro wp:and-let*  (bindings &rest body)
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
(put 'wp:and-let* 'lisp-indent-function 1)


;; internal variables
(defvar with-prefix:buffer-prefix-relation-alist nil
  "this variable is internal variable. don't change value.")


(defun with-prefix:update-prefix-relations (bound-buf bindings)
  (let* ((bufname (buffer-name bound-buf))
        (relations (loop for (pat rep) in bindings
                         collect `(,(wp:x-to-prefix-regexp rep) . ,pat)))
        (alist (assoc bufname with-prefix:buffer-prefix-relation-alist)))
    
    (cond (alist (setf (cdr alist) (union (cdr alist) relations :test 'equal)))
          (t (add-to-list 'with-prefix:buffer-prefix-relation-alist
                          `(,bufname . ,relations))))))
  
(defmacro with-prefix (head &rest body)
  "with-prefix is pseudo-name-space(but roughly implement)"
  (declare (indent 1) (debug t))
  
  ;; binding from head
  (let ((bindings
         (mapcar (lambda (args)
                   (mapcar 'wp:x-to-string args))
                 head)))
    ;; add a relation  for describe-function
    (with-prefix:update-prefix-relations
     (current-buffer)  bindings)

    (lexical-let ((shortcut-rx-full-prefix-alist 
                   (loop for (shortcut full-prefix) in bindings
                         collect `(,(wp:x-to-prefix-regexp shortcut) . ,full-prefix))))

    (labels ((%replace-shortcut-to-full-prefix
              (elt)
              (let ((replacement* (symbol-name elt)))
                (loop for (shortcut . full-prefix) in shortcut-rx-full-prefix-alist
                      do
                      (setq replacement*
                            (replace-regexp-in-string
                             shortcut full-prefix replacement*)))
                (intern replacement*))))

             `(progn
                ;; replace
                ,@(wp:tree-map-safe
                   (lambda (elt)
                     (cond ((not (symbolp elt)) elt)
                           (t (%replace-shortcut-to-full-prefix elt))))
                   body))))))

(defmacro with-prefix1 (target replacement &rest body)
  (declare (indent 2) (debug t))
  `(with-prefix ((,target ,replacement))
                ,@body))

;; a advice for finding function location
(defadvice find-function-search-for-symbol
  (after with-prefix-force-find last (symbol type library) activate)
  (destructuring-bind (buf . pt) ad-return-value

    ;; when a function definition point is not found
    (unless pt
      (wp:and-let*
          ((pat-rep-values-maybe 
            (assoc-default (buffer-name buf)
                           with-prefix:buffer-prefix-relation-alist))
           (pat-rep-pair-maybe
            (assoc* (symbol-name symbol) pat-rep-values-maybe
                    :test (lambda (str rx) (string-match rx str)))))

        (destructuring-bind (pat . rep) pat-rep-pair-maybe
          
          ;; force finding the function definition
          (with-current-buffer buf
            (goto-char (point-min))
            (and (search-forward 
                  (replace-regexp-in-string 
                   pat rep (symbol-name symbol)) nil t)
                 (setq ad-return-value (cons buf (point))))))))))

;; ;; for debugging

;; ;; (add-to-list 'load-path default-directory)
;; ;; (find-function-search-for-symbol 'with-prefix:odd\? nil "with-prefix.el")
;; ;; (find-function-search-for-symbol 'with-prefix nil "with-prefix.el")

(provide 'with-prefix)
