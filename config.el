;;; config.el --- -*- lexical-binding: t -*-

(setq user-full-name "Song Pan"
      user-mail-address "pan.song@dhs.sg")

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Hasklig" :size 19 :weight 'semi-light))

(after! smartparens
  (show-smartparens-global-mode))

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(after! go-mode ; in this case the major mode and package named the same thing
  (set-ligatures! 'go-mode
    :def "func" ; function keyword
    :true "true" :false "false"
                                        ; this will replace not only definitions
                                        ; but coresponding functions aswell
    :int "int" :str "string"
    :float "float" :bool "bool"
    :for "for"
    :return "return" :yeild "yeild"))

(set-ligatures! 'MAJOR-MODE
  ;; Functional
  :lambda        "lambda keyword"
  :def           "function keyword"
  :composition   "composition"
  :map           "map/dictionary keyword"
  ;; Types
  :null          "null type"
  :true          "true keyword"
  :false         "false keyword"
  :int           "int keyword"
  :float         "float keyword"
  :str           "string keyword"
  :bool          "boolean keywork"
  :list          "list keyword"
  ;; Flow
  :not           "not operator"
  :in            "in operator"
  :not-in        "not in operator"
  :and           "and keyword"
  :or            "or keyword"
  :for           "for keyword"
  :some          "some keyword"
  :return        "return"
  :yield         "yeild"
  ;; Other
  :union         "Union keyword"
  :intersect     "Intersect keyword"
  :diff          "diff keyword"
  :tuple         "Tuple Keyword "
  :pipe          "Pipe Keyword" ;; FIXME: find a non-private char
  :dot           "Dot operator")

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(use-package! xterm-color
  :after eshell
  :config
  (add-hook! 'eshell-mode-hook (setenv "TERM" "xterm-256color"))
  (add-hook! 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(setq undo-limit 80000000)

(global-subword-mode 1)

(setq-default major-mode 'org-mode)

(after! adaptive-wrap
  (+global-word-wrap-mode +1))

(setq +ivy-buffer-preview t)

(after! ivy
  (setq ivy-extra-directories '(".")))

(after! projectile
  (setq projectile-enable-caching (not (executable-find doom-projectile-fd-binary))))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; (defadvice! always-keep-others-when-saving-perspective (&optional fname phash name keep-others &rest rest-args)
;;   :filter-args #'persp-save-to-file-by-names
;;   '(fname phash name t rest-args))

(after! files
  (setq confirm-kill-processes nil))

(after! password-cache
  (setq password-cache-expiry 3600))

(setq auth-sources '("~/.authinfo"))

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "DHS"
                    '((mu4e-sent-folder       . "/dhs/Sent Mail")
                      (mu4e-drafts-folder     . "/dhs/Drafts")
                      (mu4e-trash-folder      . "/dhs/Trash")
                      (mu4e-refile-folder     . "/dhs/All Mail")
                      (smtpmail-smtp-user     . "pan.song@dhs.sg")
                      (mu4e-compose-signature . "---\nSong Pan"))
                    t)
(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed AND maildir:/dhs/Inbox" :key 117)
        (:name "Today's messages" :query "date:today..now AND maildir:/dhs/Inbox" :key 116)))
(setq mu4e-headers-include-related nil)
(setq mu4e-update-interval 300)
(setq mu4e-use-fancy-chars t)
(setq mu4e-view-show-addresses t)
(setq mu4e-view-show-images t)

(after! message
  (setq message-cite-style message-cite-style-gmail)
  (setq message-citation-line-format "On %d %b %Y at %R, %f wrote:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  )

(after! org-capture
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/gtd/tickler.org" "Tickler")
                                 "* SCHEDULED %i%? \n %U"))))

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))
(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "APPT(a)" "WAITING(w)"
                                      "STARTED(s)" "|" "DONE(d)" "CANCELLED(c) NOTE(n)")))

  (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                             ("~/gtd/someday.org" :level . 1)
                             ("~/gtd/tickler.org" :maxlevel . 2)))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "yellow" :weight bold)
                ("APPT" :forebround "yellow" :weight bold)
                ("NOTE" :foreground "dark violet" :weight bold)
                ("STARTED" :foreground "dark orange" :weight bold)
                ("WAITING" :foreground "red" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("CANCELLED" :foreground "grey" :weight bold)))))

(defun org-agenda-skip-if-scheduled-later ()
  "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds
           (time-to-seconds
            (org-time-string-to-time
             (org-entry-get nil "SCHEDULED"))))
          (now (time-to-seconds (current-time))))
      (and scheduled-seconds
           (>= scheduled-seconds now)
           subtree-end))))

(setq org-agenda-custom-commands
      '(
        ("r" "Feel like some reading?" tags-todo "reading"
         ((org-agenda-overriding-header "Reading")
          (org-agenda-skip-function '(org-agenda-skip-if-scheduled-later))))
        ))

(defvar org-my-archive-expiry-days 7
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun org-my-archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "\\* \\(" (regexp-opt org-done-keywords) "\\) "))
          (state-regexp
           (concat "- State \"\\(" (regexp-opt org-done-keywords)
                   "\\)\"\\s-*\\[\\([^]\n]+\\)\\]")))
      (while (re-search-forward done-regexp nil t)
        (let ((end (save-excursion
                     (outline-next-heading)
                     (point)))
              begin)
          (goto-char (line-beginning-position))
          (setq begin (point))
          (if (re-search-forward state-regexp end t)
              (let* ((time-string (match-string 2))
                     (when-closed (org-parse-time-string time-string)))
                (if (>= (time-to-number-of-days
                         (time-subtract (current-time)
                                        (apply #'encode-time when-closed)))
                        org-my-archive-expiry-days)
                    (org-archive-subtree)))
            (goto-char end)))))
    (save-buffer)))

(setq safe-local-variable-values
      '((eval font-lock-add-keywords nil
              `((,(concat "("
                          (regexp-opt
                           '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                           t)
                          "\\_>")
                 1 'font-lock-variable-name-face)))
        (eval add-hook! 'after-save-hook :local #'org-babel-tangle)
        (eval add-hook 'after-save-hook
              (lambda nil
                (org-babel-tangle))
              nil t)
        (after-save-hook archive-done-tasks)
        ))

(defalias 'archive-done-tasks 'org-my-archive-done-tasks)

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(map! :leader
      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Switch workspace" "TAB" #'+workspace/switch-to)))

(after! evil
  (define-key evil-window-map "/" 'evil-window-vsplit)
  (define-key evil-window-map "-" 'evil-window-split))

(define-key evil-window-map (kbd "C--") 'evil-window-decrease-height)

(setq auto-save-default t)

(use-package! autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info t))

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t))

;; when switching out of emacs, all unsaved files will be saved
(add-hook 'focus-out-hook 'xah-save-all-unsaved)

(use-package! fish-mode)

(use-package! nix-mode)
