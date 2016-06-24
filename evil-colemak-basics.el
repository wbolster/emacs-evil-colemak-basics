;;; evil-colemak-basics.el --- Basic Colemak key bindings for evil-mode.

;; Author: Wouter Bolsterlee <wouter@bolsterl.ee>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24") (evil "0"))
;; Keywords: colemak evil
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

;;;###autoload
(define-minor-mode evil-colemak-basics-mode
  "Minor mode with evil-mode enhancements for the Colemak keyboard layout."
  :keymap (make-sparse-keymap)
  :lighter " hnei")

;;;###autoload
(define-globalized-minor-mode global-evil-colemak-basics-mode
  evil-colemak-basics-mode
  (lambda () (evil-colemak-basics-mode t))
  "Global minor mode with evil-mode enhancements for the Colemak keyboard layout.")

(evil-define-minor-mode-key
  'motion 'evil-colemak-basics-mode
  "n" 'evil-next-line
  "e" 'evil-previous-line
  "E" 'evil-lookup
  "i" 'evil-forward-char
  "j" 'evil-forward-word-end
  "J" 'evil-forward-WORD-end
  "k" 'evil-search-next
  "K" 'evil-search-previous)

(evil-define-minor-mode-key
  'normal 'evil-colemak-basics-mode
  "i" 'evil-forward-char
  "j" 'evil-forward-word-end
  "J" 'evil-forward-WORD-end
  "k" 'evil-search-next
  "K" 'evil-search-previous
  "l" 'undo-tree-undo
  "N" 'evil-join
  "u" 'evil-insert
  "U" 'evil-insert-line)

(evil-define-minor-mode-key
  'visual 'evil-colemak-basics-mode
  "i" 'evil-forward-char
  "j" 'evil-forward-word-end
  "J" 'evil-forward-WORD-end
  "K" 'evil-search-previous
  "l" 'undo-tree-undo
  "N" 'evil-join
  "u" evil-inner-text-objects-map
  "U" 'evil-insert)

(evil-define-minor-mode-key
  'operator 'evil-colemak-basics-mode
  "i" 'evil-forward-char
  "u" evil-inner-text-objects-map)

(provide 'evil-colemak-basics)

;;; evil-colemak-basics.el ends here
