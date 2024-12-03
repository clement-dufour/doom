;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(defvar clmnt/work (eq system-type 'windows-nt)
  "Configuration might get different on my work laptop.")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Clément Dufour")
(setq user-mail-address (if clmnt/work "clement_dufour@bce-france.fr"
                          "clmnt.dufour@gmail.com"))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(cond
 ((doom-font-exists-p "Iosevka")
  (setq doom-font (font-spec :family "Iosevka" :size 16)))
 ((doom-font-exists-p "Consolas")
  (setq doom-font (font-spec :family "Consolas" :size 12))))
(cond
 ((doom-font-exists-p "Cantarell")
  (setq doom-variable-pitch-font
        (font-spec :family "Cantarell" :size 14 :weight 'regular))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(defvar clmnt/light-theme 'doom-tomorrow-day)
(defvar clmnt/dark-theme 'doom-one)

(setq doom-theme
      (if (and (eq system-type 'windows-nt)
               (eq 1 (w32-read-registry
                      'HKCU
                      "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
                      "AppsUseLightTheme")))
          clmnt/light-theme
        clmnt/dark-theme))

(defun clmnt/change-theme (dark)
  (if dark
      (setq doom-theme clmnt/dark-theme)
    (setq doom-theme clmnt/light-theme))
  (unless (eq (car custom-enabled-themes)
              doom-theme)
    (if (custom-theme-p doom-theme)
        (enable-theme doom-theme)
      (load-theme doom-theme :noconfirm))))

(after! dbus
  (clmnt/change-theme (eq 1 (caar (dbus-ignore-errors
                                  (dbus-call-method
                                   :session
                                   "org.freedesktop.portal.Desktop"
                                   "/org/freedesktop/portal/desktop"
                                   "org.freedesktop.portal.Settings"
                                   "Read"
                                   "org.freedesktop.appearance"
                                   "color-scheme")))))

  (defun clmnt/dbus-handler (namespace key value)
    (when (and (string= namespace "org.freedesktop.appearance")
               (string= key "color-scheme"))
      (clmnt/change-theme (eq 1 (car value)))))

  ;; dbus-register-signal prevents multiple registrations
  (dbus-ignore-errors (dbus-register-signal
                       :session
                       "org.freedesktop.portal.Desktop"
                       "/org/freedesktop/portal/desktop"
                       "org.freedesktop.portal.Settings"
                       "SettingChanged"
                       #'clmnt/dbus-handler)))

;; https://github.com/doomemacs/doomemacs/issues/8119
;; (after! doom-ui
;;   (setq auto-dark-dark-theme 'doom-one
;;         auto-dark-light-theme 'doom-tomorrow-day)
;;   (auto-dark-mode +1))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (if clmnt/work
                        "~/OneDrive - RTL Group/Documents/org/"
                      "~/Documents/org/"))
(setq org-log-done 'time)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Emacs miscellaneous configuration
(setq select-enable-clipboard nil
      ;; confirm-kill-emacs nil
      visible-bell t)

;; Undo configuration, limit undo to 1 MB
(setq undo-limit (* 1024 1024)
      evil-want-fine-undo t)

(setq scroll-margin 2)

;; Frames
(setq fancy-splash-image nil
      frame-title-format "%b - Emacs")

;; The initial frame is already loaded when config.el is run, the following sexp
;; has not effect. Using the deamon solves this.
;; (add-to-list 'initial-frame-alist '(alpha-background . 95))
(add-to-list 'default-frame-alist '(alpha-background . 95))

(unless clmnt/work
 (add-hook! 'window-size-change-functions
            #'frame-hide-title-bar-when-maximized))

;; Misc hooks
;; (add-hook! 'prog-mode-hook #'tree-sitter-hl-mode)

;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
(add-hook! 'emacs-lisp-mode-hook
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))

;; Package specific configuration
(after! doom
  ;; Simpler one-liner banner
  (defun clmnt/draw-ascii-banner ()
    (insert
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       "Doom Emacs")
      'face 'doom-dashboard-loaded)
     "\n"))

  (setq +doom-dashboard-ascii-banner-fn #'clmnt/draw-ascii-banner)

  (setq +doom-dashboard-functions
        (list #'doom-dashboard-widget-banner
              #'doom-dashboard-widget-loaded)
        +doom-dashboard-name "Dashboard"))

(after! evil
  ;; Implicit /g flag on evil ex substitution
  (setq evil-ex-substitute-global t
        ;; evil-move-cursor-back nil
        evil-kill-on-visual-paste nil)

  (setq evil-vsplit-window-right t
        evil-split-window-below t)

  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (consult-buffer))

  (defadvice! center-next-search-result (&rest _)
    :after '(evil-ex-search-forward evil-ex-search-forward
             evil-ex-search-next evil-ex-search-previous)
    (evil-scroll-line-to-center nil)))

(after! writeroom-mode
  (setq +zen-text-scale 0.8)

  (add-hook! 'writeroom-mode-enable-hook
    (display-line-numbers-mode -1)
    (diff-hl-mode -1))

  (add-hook! 'writeroom-mode-disable-hook
    (display-line-numbers-mode +1)
    (diff-hl-mode +1)
    (diff-hl-update)))

(after! recentf
  (add-to-list 'recentf-exclude
               (expand-file-name ".local/etc/workspaces/autosave"
                                 doom-emacs-dir)))

;; (after! company
;;   (setq company-show-quick-access t))

(after! corfu
  (when clmnt/work
    (setq corfu-terminal-disable-on-gui nil)
    (add-hook! 'corfu-mode-hook
      (corfu-terminal-mode +1))))

(after! which-key
  (setq which-key-allow-multiple-replacements t)
  (pushnew! which-key-replacement-alist
            '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US,fr_FR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))

(after! org
  (add-hook! 'org-mode-hook
             #'+org-pretty-mode
             #'mixed-pitch-mode)
  (remove-hook! 'org-mode-hook #'flyspell-mode)
  (setq org-startup-folded 'overview
        org-ellipsis " "))

(after! ox-pandoc
  (add-to-list 'org-pandoc-options-for-docx
               (cons 'reference-doc
                     (expand-file-name "reference.docx" org-directory))))

;; Keybindings
(when clmnt/work
  (map! "M-s-<f4>" #'save-buffers-kill-terminal
        "M-<f4>" #'save-buffers-kill-terminal))

(defun clmnt/yank-link-clipboard ()
  "Copy the url at point to the system clipboard.
If on top of an Org link, will only copy the link component."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (evil-set-register ?+ (or url (user-error "No URL at point")))
    (message "Copied link to system clipboard: %s" url)))

(map! :after org
      :map org-mode-map
      :localleader :prefix "l" "Y" #'clmnt/yank-link-clipboard)

(defun clmnt/org-tab-conditional ()
  (interactive)
  (if (yas-active-snippets)
      (yas-next-field-or-maybe-expand)
    (org-cycle)))

(map! :after evil-org
      :map evil-org-mode-map
      :i "<tab>" #'clmnt/org-tab-conditional)

(map! :after evil
      :map evil-window-map
      "SPC" #'evil-window-rotate-upwards)

(map! :after better-jumper
      :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(map! :after ibuffer
      :map ibuffer-mode-map
      :n "h" #'kill-current-buffer
      :n "l" #'+ibuffer/visit-workspace-buffer)

(map! :after dired
      :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-file)

;; Modes
;; Cisco mode
(defvar cisco-font-lock-keywords
  (list
   ;; '("^[ \t]*\\(!+\\)"
   ;;   (1 font-lock-comment-delimiter-face))
   ;; '("^[ \t]*!+\\(.*\\)$"
   ;;   (1 font-lock-comment-face))
   '("^[ \t]*\\(no +\\)?\\([A-Za-z-]+\\)\\( \\|$\\)"
     (2 font-lock-keyword-face))
   '("^[ \t]*\\(no\\)\\b"
     (1 font-lock-negation-char-face))
   ;; (cons (concat "^[ \t]*\\(no +\\)?"
   ;;               (regexp-opt '("aaa"
   ;;                             "..."
   ;;                             "vtp") t)) '(2 font-lock-keyword-face))
   '("^[ \t]*interface +\\([A-Za-z-]+ *[0-9/]+\\)"
     (1 font-lock-type-face))
   '("^[ \t]*vlan +\\([[0-9]+\\)"
     (1 font-lock-type-face))
   '("^[ \t]*\\(hostname\\|description\\|name\\) +\\(.*\\)$"
     (2 font-lock-string-face))
   '("\\b\\(shutdown\\)\\b"
     (1 font-lock-warning-face))
   ;; IP adresses
   '("\\b\\(\\([0-1]?[0-9]?[0-9]\\.\\|2[0-4][0-9]\\.\\|25[0-5]\\.\\)\\{3\\}\\([0-1]?[0-9]?[0-9]\\|2[0-4][0-9]\\|25[0-5]\\)\\)\\b"
     (1 font-lock-variable-name-face))
   ;; Numbers
   '("\\b\\([0-9]+\\)\\b"
     (1 font-lock-variable-name-face))
   ;; VLAN numbers on a VLAN range
   ;; Hyphens are defined as word constituents thus not matched with \b on the
   ;; previous regex expression.
   '("\\b\\([0-9]+\\)-"
     (1 font-lock-variable-name-face))
   '("-\\([0-9]+\\)\\b"
     (1 font-lock-variable-name-face)))
  "Font lock defaults for `cisco mode'.")

(defvar cisco-imenu-expression
  '(("Interfaces" "^[ \t]*interface +\\([A-Za-z-]+ *[0-9/]+\\)" 1)
    ("VLANs" "^[ \t]*vlan +\\([[0-9]+\\)" 1)
    ("Hostnames" "^[ \t]*hostname +\\(.*\\)$" 1))
  "Matchers for `cisco mode'.")

(define-derived-mode cisco-mode
  prog-mode "Cisco"
  "Major mode for editing Cisco configuration files."
  (setq font-lock-defaults '(cisco-font-lock-keywords)
        comment-start "!"
        comment-end ""
        comment-start-skip "^[ \t]*!+[ \t]*")

  (setq imenu-case-fold-search nil
        imenu-generic-expression cisco-imenu-expression)

  (display-line-numbers-mode +1)
  ;; TODO: Should probably try to use highlight-numbers-mode here
  ;; (highlight-numbers-mode +1)

  (modify-syntax-entry ?_ "w" cisco-mode-syntax-table)
  (modify-syntax-entry ?- "w" cisco-mode-syntax-table)
  (modify-syntax-entry ?! "<" cisco-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cisco-mode-syntax-table)
  (modify-syntax-entry ?\r ">" cisco-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.cfg\\'" . cisco-mode))

;; (add-to-list 'consult-imenu-config
;;              '(cisco-mode :toplevel "Interfaces" :types
;;                ((?i "Interfaces" font-lock-type-face)
;;                 (?v "VLANs" font-lock-type-face)
;;                 (?h "Hostnames" font-lock-string-face))))

;; Vimrc mode
;; TODO: Should probably derive from conf space mode
(defvar vimrc-font-lock-keywords
  (list
   '("^[ \t]*\\(\"+\\)"
     (1 font-lock-comment-delimiter-face))
   '("^[ \t]*\"+\\(.*\\)$"
     (1 font-lock-comment-face prepend))
   '("^[ \t]*\\([A-Za-z]+\\)\\( \\|$\\)"
     (1 font-lock-keyword-face))
   '("^[ \t]*\\(let\\|set\\) +\\([A-Za-z_]*\\)\\>"
     (2 font-lock-variable-name-face))
   '("<\\([A-Za-z]+\\)"
     (1 font-lock-builtin-face)))
  "Font lock defaults for `vimrc mode'.")

(define-derived-mode vimrc-mode
  prog-mode "vimrc"
  "Major mode for editing vim configuration files."
  (setq font-lock-defaults '(vimrc-font-lock-keywords t)
        comment-start "\"\""
        comment-end ""
        comment-start-skip "^[ \t]*\"+[ \t]*")

  (display-line-numbers-mode +1))

(add-to-list 'auto-mode-alist '("init\\.vim\\'" . vimrc-mode))
