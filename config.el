;;; config.el --- -*- lexical-binding: t -*-

(setq user-full-name "Song Pan"
      user-mail-address "pan.song@dhs.sg")

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light))

(use-package paren
  :defer t
  :ensure nil
  :init (setq show-paren-delay 0.5)
  :config (show-paren-mode +1))

(use-package! org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

(map! :leader
      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Switch to last workspace" "TAB" #'+workspace/other)))

(setq auto-save-default t)

(use-package! autorevert
  :defer t
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))

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
