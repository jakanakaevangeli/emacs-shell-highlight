;;; shell-highlight-undef.el --- Highlight non-existent shell commands -*- lexical-binding: t; -*-

;; Filename: shell-highlight-undef.el
;; Description: Highlight non-existent shell commands
;; Author: jakanakaevangeli <jakanakaevangeli@chiru.no>
;; Created: 2021-07-16
;; Version: 1.0
;; URL: https://github.com/jakanakaevangeli/emacs-shell-highlight

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlight non-existent shell commands. This mode is meant to be used in M-x
;; shell buffers. If you use shell aliases, add them to
;; `shell-highlight-undef-aliases'.
;;
;; Installation:
;;
;; Add the following to your Emacs init file:
;;
;;  (add-to-list 'load-path "/path/to/emacs-shell-highlight")
;;  (require 'shell-highlight-undef)
;;  (add-hook 'shell-mode-hook #'shell-highlight-undef-mode 5)

;;; Code:

(require 'sh-script)

(defvar-local shell-highlight-undef-keywords nil)

(defface shell-highlight-undef-defined-face
  '((t :inherit 'font-lock-function-name-face))
  "Face used for existent commands."
  :group 'faces)
(defface shell-highlight-undef-undefined-face
  '((t :inherit 'font-lock-warning-face))
  "Face used for non-existent commands."
  :group 'faces)
(defface shell-highlight-undef-alias-face
  '((t :inherit 'font-lock-variable-name-face))
  "Face used command aliases."
  :group 'faces)

(defcustom shell-highlight-undef-aliases nil
  "List commands to highlight as a command alias."
  :group 'faces
  :type '(repeat string))

(defcustom shell-highlight-undef-search-remote nil
  "If t, allow searching on remote hosts for executable files.
The remote host is chosen as indicated by `default-directory'."
  :group 'faces
  :type 'boolean)

(defvar shell-highlight-undef--face 'font-lock-function-name-face)
(defvar shell-highlight-undef-keywords
  `((,#'shell-highlight-undef-matcher  6 shell-highlight-undef--face)))
(defvar-local shell-highlight-undef-regexp
  (or (bound-and-true-p regexp-unmatchable) "\\`a\\`"))

(defun shell-highlight-undef-matcher (end)
  "Matcher used to highlight commands up to END."
  (when (re-search-forward shell-highlight-undef-regexp end t)
    (let ((cmd (match-string 6)))
      (setq shell-highlight-undef--face
            (save-match-data
              (cond
               ((member cmd shell-highlight-undef-aliases)
                'shell-highlight-undef-alias-face)
               ((executable-find cmd shell-highlight-undef-search-remote)
                'shell-highlight-undef-defined-face)
               (t 'shell-highlight-undef-undefined-face)))))
    t))

;;;###autoload
(define-minor-mode shell-highlight-undef-mode
  "Highlight undefined shell commands and aliases."
  :init-value nil
  (cond
   (shell-highlight-undef-mode
    (setq shell-highlight-undef-regexp
          ;; Taken from `sh-font-lock-keywords-1'
          (concat "\\([;(){}`|&]\\|^\\)[ \t]*\\(\\("
                  (regexp-opt (sh-feature sh-leading-keywords) t)
                  "[ \t]+\\)?"
                  (regexp-opt (append (sh-feature sh-leading-keywords)
                                      (sh-feature sh-other-keywords))
                              t)
                  "[ \t]+\\)?\\<\\(.+?\\)\\>"))
    (font-lock-add-keywords nil shell-highlight-undef-keywords t)
    (font-lock-flush))
   (shell-highlight-undef-keywords
    (font-lock-remove-keywords nil shell-highlight-undef-keywords)
    (font-lock-flush))))
