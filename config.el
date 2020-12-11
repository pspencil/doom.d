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

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(setq undo-limit 80000000)

(global-subword-mode 1)

(setq-default major-mode 'org-mode)

(after! adaptive-wrap
  (+global-word-wrap-mode +1))

(setq +ivy-buffer-preview t)

(after! ivy-rich
  (setq ivy-rich-path-style 'abbrev)
  (defadvice! ivy-rich-no-project-fallback (orig-fn candidate)
    :around #'ivy-rich--switch-buffer-root-and-filename
    (if-let ((result (funcall orig-fn candidate)))
        result
      (cons ""
            (expand-file-name (ivy-rich--switch-buffer-directory candidate))))))

(setq ivy-extra-directories '("."))

(after! projectile
  (setq projectile-enable-caching (not (executable-find doom-projectile-fd-binary))))

;; (defadvice! always-keep-others-when-saving-perspective (&optional fname phash name keep-others &rest rest-args)
;;   :filter-args #'persp-save-to-file-by-names
;;   '(fname phash name t rest-args))

(after! files
  (setq confirm-kill-processes nil))

(setq auth-sources '("~/.authinfo"))

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "DHS"
                    '((mu4e-sent-folder       . "/DHS/Sent Mail")
                      (mu4e-drafts-folder     . "/DHS/Drafts")
                      (mu4e-trash-folder      . "/DHS/Trash")
                      (mu4e-refile-folder     . "/DHS/All Mail")
                      (smtpmail-smtp-user     . "pan.song@dhs.sg")
                      (mu4e-compose-signature . "---\nSong Pan"))
                    t)
(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed AND maildir:/dhs/Inbox" :key 117)
        (:name "Today's messages" :query "date:today..now AND maildir:/dhs/Inbox" :key 116)
        (:name "Last 7 days" :query "date:7d..now AND maildir:/dhs/Inbox" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/* AND maildir:/dhs/Inbox" :key 112)))
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
