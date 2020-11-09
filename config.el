;;; config.el --- -*- lexical-binding: t -*-

(setq user-full-name "Song Pan"
      user-mail-address "pan.song@dhs.sg")

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Hasklig" :size 19 :weight 'semi-light))

(use-package paren
  :defer t
  :ensure nil
  :init (setq show-paren-delay 0.5)
  :config (show-paren-mode +1))

(use-package! org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(setq undo-limit 80000000)

(global-subword-mode 1)

(setq-default major-mode 'org-mode)

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(setq +ivy-buffer-preview t)

(after! ivy-rich
  (setq ivy-rich-path-style 'abbrev)
  (defadvice! ivy-rich-no-project-fallback (orig-fn candidate)
    :around #'ivy-rich--switch-buffer-root-and-filename
    (if-let ((result (funcall orig-fn candidate)))
        result
      (cons ""
            (expand-file-name (ivy-rich--switch-buffer-directory candidate))))))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; (defadvice! always-keep-others-when-saving-perspective (&optional fname phash name keep-others &rest rest-args)
;;   :filter-args #'persp-save-to-file-by-names
;;   '(fname phash name t rest-args))

(map! :leader
      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Switch to last workspace" "TAB" #'+workspace/other)))

(define-key evil-window-map "/" 'evil-window-vsplit)
(define-key evil-window-map "-" 'evil-window-split)

(define-key evil-window-map (kbd "C--") 'evil-window-decrease-height)

(setq auto-save-default t)

(use-package! autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info t))

(use-package! files
  :defer t
  :config
  (setq confirm-kill-processes nil))

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t))

;; when switching out of emacs, all unsaved files will be saved
(add-hook 'focus-out-hook 'xah-save-all-unsaved)
