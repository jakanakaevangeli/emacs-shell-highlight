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

;; Highlight non-existent shell commands.  This mode is meant to be used in
;; M-x shell buffers.  If you use shell aliases, functions or builtins, unknown
;; to sh-script, add them to `shell-highlight-undef-aliases'.
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
  `((,#'shell-highlight-undef-matcher 6 shell-highlight-undef--face)))
(defvar-local shell-highlight-undef-regexp
  (or (bound-and-true-p regexp-unmatchable) "\\`a\\`"))

(defun shell-highlight-undef-matcher (end)
  "Matcher used to highlight commands up to END."
  (when (re-search-forward shell-highlight-undef-regexp end t)
    (let ((cmd (match-string 6))
          (beg (match-beginning 6)))
      (setq shell-highlight-undef--face
            (save-match-data
              (cond
               ;; Don't highlight command output.  Only useful if
               ;; `shell-highlight-mode' is disabled.
               ((text-property-any beg (point) 'field 'output)
                nil)
               ((member cmd shell-highlight-undef-aliases)
                'shell-highlight-undef-alias-face)
               ;; Check if it contain a directory separator
               ((file-name-directory cmd)
                (if (or (file-executable-p cmd)
                        (file-directory-p cmd))
                    'shell-highlight-undef-defined-face
                  'shell-highlight-undef-undefined-face))
               ((executable-find cmd shell-highlight-undef-search-remote)
                'shell-highlight-undef-defined-face)
               (t 'shell-highlight-undef-undefined-face)))))
    t))

(defvar shell-highlight-fl-keywords)
(defvar-local shell-highlight-undef--added-to nil
  "Which variable were the undef keywords added to.
If 'shell-highlight, they were added to
`shell-highlight-fl-keywords'.  If 'font-lock, they were added to
`font-lock-keywords'.")

;;;###autoload
(define-minor-mode shell-highlight-undef-mode
  "Highlight undefined shell commands and aliases."
  :init-value nil
  (if shell-highlight-undef-mode
      (progn
        (setq shell-highlight-undef-regexp
              ;; Adapted from `sh-font-lock-keywords-1'
              (concat
               "\\("
               "[;(){}`|&]"
               (if (bound-and-true-p shell-highlight-mode)
                   ;; `shell-highlight' already puts point-min on end of prompt
                   ""
                 (concat "\\|" comint-prompt-regexp))
               "\\|^"
               "\\)"
               "[ \t]*\\(\\("
               (regexp-opt (sh-feature sh-leading-keywords) t)
               "[ \t]+\\)?"
               (regexp-opt (append (sh-feature sh-leading-keywords)
                                   (sh-feature sh-other-keywords))
                           t)
               "[ \t]+\\)?\\_<\\(\\(?:\\s_\\|\\sw\\|/\\)+\\)\\_>"))
        (if (bound-and-true-p shell-highlight-mode)
            (let ((font-lock-keywords shell-highlight-fl-keywords))
              (font-lock-add-keywords nil shell-highlight-undef-keywords t)
              (setq shell-highlight-fl-keywords font-lock-keywords)
              (setq shell-highlight-undef--added-to 'shell-highlight))
          (font-lock-add-keywords nil shell-highlight-undef-keywords t)
          (setq shell-highlight-undef--added-to 'font-lock)))

    (pcase (prog1 shell-highlight-undef--added-to
             (setq shell-highlight-undef--added-to nil))
      ('shell-highlight
       (let ((font-lock-keywords shell-highlight-fl-keywords))
         (font-lock-remove-keywords nil shell-highlight-undef-keywords)
         (setq shell-highlight-fl-keywords font-lock-keywords)))
      ('font-lock
       (font-lock-remove-keywords nil shell-highlight-undef-keywords))))

  (font-lock-flush))

(add-hook 'shell-highlight-mode-hook #'shell-highlight-undef-reset-mode)

(defun shell-highlight-undef-reset-mode ()
  "If `shell-highlight-undef-mode' is on, turn it off and on."
  (when shell-highlight-undef-mode
    (shell-highlight-undef-mode -1)
    (shell-highlight-undef-mode 1)))

(provide 'shell-highlight-undef)
;;; shell-highlight-undef.el ends here
