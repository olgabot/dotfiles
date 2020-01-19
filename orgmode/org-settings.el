;; Word-wrap orgmode files
(add-hook 'org-mode-hook #'(lambda ()

                             ;; make the lines in the buffer wrap around the edges of the screen.

                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-indent-mode)))


;; Paste an image into orgmode

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))


;; --- Begin stolen code ----
;; From John Wiegley's dot-emacs.el file
;; https://github.com/jwiegley/dot-emacs/blob/master/dot-org.el

;; -- Custom sort tasks based on Project --
(defun org-sort-done-tasks ()
  (interactive)
  (goto-char (point-min))
  (org-sort-entries t ?F #'org-get-inactive-time #'<)
  (goto-char (point-min))
  (while (re-search-forward "
+" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "
"))
  (let (after-save-hook)
    (save-buffer))
  (org-overview))

(defalias 'sort-done-tasks 'org-sort-done-tasks)

(defun org-sort-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\* " nil t)
      (goto-char (match-beginning 0))
      (condition-case err
          (progn
            ;; (org-sort-entries t ?a)
            (org-sort-entries t ?p)
            (org-sort-entries t ?o))
        (error nil))
      (forward-line))
    (goto-char (point-min))
    (while (re-search-forward "\* PROJECT " nil t)
      (goto-char (line-beginning-position))
      (ignore-errors
        ;; (org-sort-entries t ?a)
        (org-sort-entries t ?p)
        (org-sort-entries t ?o))
      (forward-line))))

(defun org-cleanup ()
  (interactive)
  (org-archive-expired-tasks)
  (org-sort-all))

;; -- Orgmode startup --
(defun my-org-startup ()
  (org-agenda-list)
  (org-fit-agenda-window)
  (org-agenda-to-appt)
  (call-interactively #'org-resolve-clocks))

(defadvice org-refile-get-location (before clear-refile-history activate)
  "Fit the Org Agenda to its buffer."
  (setq org-refile-history nil))



;; -- TODO states --

; (defvar org-mode-completion-keys
;   '((?d . "DONE")
;     (?g . "DELEGATED")
;     (?n . "NOTE")
;     (?r . "DEFERRED")
;     (?s . "STARTED")
;     (?t . "TODO")
;     (?e . "EPIC")
;     (?o . "STORY")
;     (?w . "WAITING")
;     (?x . "CANCELED")
;     (?y . "SOMEDAY")
;     ))

(eval-and-compile
  (defvar org-todo-state-map nil)
  (define-prefix-command 'org-todo-state-map))

(dolist (ckey org-mode-completion-keys)
  (let* ((key (car ckey))
         (label (cdr ckey))
         (org-sym (intern (concat "my-org-todo-" (downcase label))))
         (org-sym-no-logging
          (intern (concat "my-org-todo-" (downcase label) "-no-logging")))
         (org-agenda-sym
          (intern (concat "my-org-agenda-todo-" (downcase label))))
         (org-agenda-sym-no-logging
          (intern (concat "my-org-agenda-todo-"
                          (downcase label) "-no-logging"))))
    (eval
     `(progn
        (defun ,org-sym ()
          (interactive)
          (org-todo ,label))
        (bind-key (concat "C-c x " (char-to-string ,key)) ',org-sym
                  org-mode-map)

        (defun ,org-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-todo ,label)))
        (bind-key (concat "C-c x " (char-to-string  ,(upcase key)))
                  ',org-sym-no-logging org-mode-map)

        (defun ,org-agenda-sym ()
          (interactive)
          (let ((org-inhibit-logging
                 (let ((style (org-entry-get
                               (get-text-property (point) 'org-marker)
                               "STYLE")))
                   (and style (stringp style)
                        (string= style "habit")))))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,key] ',org-agenda-sym)

        (defun ,org-agenda-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,(upcase key)]
          ',org-agenda-sym-no-logging)))))


;; Custom capture location
(setq org-default-notes-file (concat org-directory "~/Dropbox/self/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)


(setq org-todo-keywords
      '((sequence "TODO(t)" "RECUR(r)" "IN-PROGRESS(p@/!)" "WAITING(w@/!)" "|" "DELEGATED(e@/!)" "DEFERRED(f@/!)" "DONE(d!)" "CANCELED(c@)" "SOMEDAY(s)" )))


;; ;; Set keyword faces
;;  '(org-todo-keyword-faces
;;    (quote
;;     (("TODO" :foreground "medium blue" :weight bold)
;;      ("EPIC" :foreground "deep sky blue" :weight bold)
;;      ("STORY" :foreground "royal blue" :weight bold)
;;      ("RECUR" :foreground "cornflowerblue" :weight bold)
;;      ("APPT" :foreground "medium blue" :weight bold)
;;      ("NOTE" :foreground "brown" :weight bold)
;;      ("STARTED" :foreground "dark orange" :weight bold)
;;      ("WAITING" :foreground "red" :weight bold)
;;      ("DELEGATED" :foreground "dark violet" :weight bold)
;;      ("DEFERRED" :foreground "dark blue" :weight bold)
;;      ("SOMEDAY" :foreground "dark blue" :weight bold)
;;      ("PROJECT" :foreground "#088e8e" :weight bold))))
;;  '(org-todo-repeat-to-state "TODO")

; ;; Set keyword faces
;  '(org-todo-keyword-faces
;    (quote
;     (("TODO" :foreground "medium blue" :weight bold)
;      ("EPIC" :foreground "deep sky blue" :weight bold)
;      ("STORY" :foreground "royal blue" :weight bold)
;      ("RECUR" :foreground "cornflowerblue" :weight bold)
;      ("APPT" :foreground "medium blue" :weight bold)
;      ("NOTE" :foreground "brown" :weight bold)
;      ("STARTED" :foreground "dark orange" :weight bold)
;      ("WAITING" :foreground "red" :weight bold)
;      ("DELEGATED" :foreground "dark violet" :weight bold)
;      ("DEFERRED" :foreground "dark blue" :weight bold)
;      ("SOMEDAY" :foreground "dark blue" :weight bold)
;      ("PROJECT" :foreground "#088e8e" :weight bold))))
;  '(org-todo-repeat-to-state "TODO")
