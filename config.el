;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Song Pan"
      user-mail-address "songpan@google.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; -----------------------------------------------------------------------------
;; New packages and configs
;; -----------------------------------------------------------------------------
(use-package! keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          vterm--self-insert
          org-self-insert-command
          vterm-send-backspace
          ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; -----------------------------------------------------------------------------
;; Better defaults
;; -----------------------------------------------------------------------------
(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t)                        ; Nobody likes to loose work, I certainly don't

(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq-default major-mode 'org-mode)

;; -----------------------------------------------------------------------------
;; Ivy related stuff
;; -----------------------------------------------------------------------------
(setq +ivy-buffer-preview t)                      ; Preview buffers in ivy

;; Make ivy-rich fallback to absolute path if not in a project.
(after! ivy-rich
  (setq ivy-rich-path-style 'abbrev)
  (defadvice! ivy-rich-no-project-fallback (orig-fn candidate)
    :around #'ivy-rich--switch-buffer-root-and-filename
    (if-let ((result (funcall orig-fn candidate)))
        result
      (cons ""
            (expand-file-name (ivy-rich--switch-buffer-directory candidate))))))

;; -----------------------------------------------------------------------------
;; Workspace related stuff
;; -----------------------------------------------------------------------------
(map! :leader
      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Switch to last workspace" "TAB" #'+workspace/other)))

;; -----------------------------------------------------------------------------
;; Magit related stuff
;; -----------------------------------------------------------------------------

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------
;; Prompt for buffer after splitting windows
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

