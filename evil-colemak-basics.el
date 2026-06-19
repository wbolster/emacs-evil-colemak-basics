;;; evil-colemak-basics.el --- Basic Colemak key bindings for evil-mode  -*- lexical-binding: t; -*-

;; author: wouter bolsterlee <wouter@bolsterl.ee>
;; Version: 3.0.0
;; Package-Requires: ((emacs "28.1") (evil "1.15.0") (evil-snipe "2.1.3"))
;; Keywords: convenience emulations colemak evil
;; URL: https://github.com/wbolster/evil-colemak-basics
;;
;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This package provides basic key rebindings for evil-mode with the
;; Colemak keyboard layout.  See the README for more information.
;;
;; To enable globally, use:
;;
;;   (global-evil-colemak-basics-mode)
;;
;; To enable for just a single buffer, use:
;;
;;   (evil-colemak-basics-mode)

;;; Code:

(require 'evil)
(require 'evil-snipe)

(defgroup evil-colemak-basics nil
  "Basic key rebindings for evil-mode with the Colemak keyboard layout."
  :prefix "evil-colemak-basics-"
  :group 'evil)

(defcustom evil-colemak-basics-layout-mod nil
  "Which Colemak layout mod to use.

Colemak Mod-DH, also known as Colemak Mod-DHm, has m where the h
key is on qwerty. This means we need to swap the h and m
bindings. No other changes are necessary."
  :group 'evil-colemak-basics
  :type '(choice (const :tag "default" nil)
                 (const :tag "mod-dh" mod-dh)))

(defcustom evil-colemak-basics-rotate-t-f-j t
  "Whether to keep find-char and end of word jumps at their qwerty position.

When non-nil, this will rotate the t, f, and j keys, so that f
jumps to the end of the word (qwerty e, same position), t jumps to a
char (qwerty f, same position), and j jumps until a char (qwerty t,
different position)."
  :group 'evil-colemak-basics
  :type 'boolean)

(defcustom evil-colemak-basics-char-jump-commands nil
  "The set of commands to use for jumping to characters.

By default, the built-in evil commands evil-find-char (and
variations) are used; when set to the symbol \\='evil-snipe, this
behaves like evil-snipe-override-mode, but adapted to the right
keys.

This setting is only used when the character jump commands are
rotated; see evil-colemak-basics-rotate-t-f-j."
  :group 'evil-colemak-basics
  :type '(choice (const :tag "default" nil)
                 (const :tag "evil-snipe" evil-snipe)))

(declare-function evil-colemak-basics-snipe-t "evil-colemak-basics")
(declare-function evil-colemak-basics-snipe-T "evil-colemak-basics")
(declare-function evil-colemak-basics-snipe-j "evil-colemak-basics")
(declare-function evil-colemak-basics-snipe-J "evil-colemak-basics")

(defun evil-colemak-basics--clear-keys ()
  "Clear existing key bindings for the minor mode."
  (dolist (state-entry evil-minor-mode-keymaps-alist)
    (setcdr state-entry
            (assq-delete-all 'evil-colemak-basics-mode (cdr state-entry)))))

(defun evil-colemak-basics--define-keys ()
  "Define key bindings based on the current configuration."
  (evil-colemak-basics--clear-keys)
  (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
    "n" 'evil-next-line
    "gn" 'evil-next-visual-line
    "e" 'evil-previous-line
    "E" 'evil-lookup
    "ge" 'evil-previous-visual-line
    "i" 'evil-forward-char
    "I" 'evil-window-bottom
    "zi" 'evil-scroll-column-right
    "zI" 'evil-scroll-right
    "j" 'evil-forward-word-end
    "J" 'evil-forward-WORD-end
    "gj" 'evil-backward-word-end
    "gJ" 'evil-backward-WORD-end
    "k" (if (eq evil-search-module 'evil-search) 'evil-ex-search-next 'evil-search-next)
    "K" (if (eq evil-search-module 'evil-search) 'evil-ex-search-previous 'evil-search-previous)
    "gk" 'evil-next-match
    "gK" 'evil-previous-match)
  (evil-define-minor-mode-key '(normal visual) 'evil-colemak-basics-mode
    "N" 'evil-join
    "gN" 'evil-join-whitespace
    "gl" 'evil-downcase
    "gL" 'evil-upcase)
  (evil-define-minor-mode-key 'normal 'evil-colemak-basics-mode
    "l" 'evil-undo
    "u" 'evil-insert
    "U" 'evil-insert-line
    "gu" 'evil-insert-resume
    "gU" 'evil-insert-0-line)
  (evil-define-minor-mode-key 'visual 'evil-colemak-basics-mode
    "l" 'evil-downcase
    "L" 'evil-upcase
    "U" 'evil-insert)
  (evil-define-minor-mode-key '(visual operator) 'evil-colemak-basics-mode
    "u" evil-inner-text-objects-map)
  (evil-define-minor-mode-key 'operator 'evil-colemak-basics-mode
    "i" 'evil-forward-char)
  (when evil-colemak-basics-rotate-t-f-j
    (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
      "f" 'evil-forward-word-end
      "F" 'evil-forward-WORD-end
      "gf" 'evil-backward-word-end
      "gF" 'evil-backward-WORD-end)
    (evil-define-minor-mode-key 'normal 'evil-colemak-basics-mode
      "gt" 'find-file-at-point
      "gT" 'evil-find-file-at-point-with-line)
    (evil-define-minor-mode-key 'visual 'evil-colemak-basics-mode
      "gt" 'evil-find-file-at-point-visual)
    (when (featurep 'tab-bar)  ; Evil also checks this; see evil-maps.el
      (evil-define-minor-mode-key 'normal 'evil-colemak-basics-mode
        "gj" 'tab-bar-switch-to-next-tab
        "gJ" 'tab-bar-switch-to-prev-tab))
    (cond
     ((eq evil-colemak-basics-char-jump-commands nil)
      (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
        "t" 'evil-find-char
        "T" 'evil-find-char-backward
        "j" 'evil-find-char-to
        "J" 'evil-find-char-to-backward))
     ((eq evil-colemak-basics-char-jump-commands 'evil-snipe)
      ;; XXX https://github.com/hlissner/evil-snipe/issues/46
      (evil-snipe-def 1 'inclusive "t" "T"
                      :forward-fn evil-colemak-basics-snipe-t
                      :backward-fn evil-colemak-basics-snipe-T)
      (evil-snipe-def 1 'exclusive "j" "J"
                      :forward-fn evil-colemak-basics-snipe-j
                      :backward-fn evil-colemak-basics-snipe-J)
      (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
        "t" 'evil-colemak-basics-snipe-t
        "T" 'evil-colemak-basics-snipe-T
        "j" 'evil-colemak-basics-snipe-j
        "J" 'evil-colemak-basics-snipe-J))
     (t (user-error "Invalid evil-colemak-basics-char-jump-commands configuration"))))
  (when (eq evil-colemak-basics-layout-mod 'mod-dh)
    (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
      "m" 'evil-backward-char)
    (evil-define-minor-mode-key '(normal visual) 'evil-colemak-basics-mode
      "h" 'evil-set-marker))
  (when evil-respect-visual-line-mode
    (evil-define-minor-mode-key '(motion normal visual) 'evil-colemak-basics-mode
      "n" 'evil-next-visual-line
      "gn" 'evil-next-line
      "e" 'evil-previous-visual-line
      "ge" 'evil-previous-line
      "0" 'evil-beginning-of-visual-line
      "g0" 'evil-beginning-of-line
      "$" 'evil-end-of-visual-line
      "g$" 'evil-end-of-line
      "V" 'evil-visual-screen-line)))

(evil-colemak-basics--define-keys)

;;;###autoload
(define-minor-mode evil-colemak-basics-mode
  "Minor mode with evil-mode enhancements for the Colemak keyboard layout."
  :lighter " hnei")

;;;###autoload
(define-globalized-minor-mode global-evil-colemak-basics-mode
  evil-colemak-basics-mode
  (lambda () (evil-colemak-basics-mode t))
  :predicate '(t) ;; note: not plain ‘t’ b/c that breaks add-to-list
  "Global minor mode with evil-mode enhancements for the Colemak keyboard layout.")

(provide 'evil-colemak-basics)

;;; evil-colemak-basics.el ends here
