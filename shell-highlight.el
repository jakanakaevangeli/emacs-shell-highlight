;;; shell-highlight.el --- Syntax highlighting in M-x shell buffers -*- lexical-binding: t; -*-

;; Filename: shell-highlight.el
;; Description: Syntax highlighting in M-x shell buffers
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

;; Syntax highlighting in M-x shell buffers, derived from `sh-script-mode'.
;;
;; Installation:
;;
;; Add the following to your Emacs init file:
;;
;;  (add-to-list 'load-path "/path/to/emacs-shell-highlight")
;;  (require 'shell-highlight)
;;  (add-hook 'shell-mode-hook #'shell-highlight-mode)

;;; Code:

(require 'font-lock)
(require 'sh-script)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defvar comint-highlight-input)

(defvar-local shell-highlight-fl-keywords nil
  "A font lock variable, used by `shell-highlight'.
Used as `font-lock-keywords', to highlight input regions.")
(defvar-local shell-highlight-fl-keywords-only nil
  "A font lock variable, used by `shell-highlight'.
Used as `font-lock-keywords-only', to highlight input regions.")
(defvar-local shell-highlight-fl-keywords-case-fold-search nil
  "A font lock variable, used by `shell-highlight'.
Used as `font-lock-keywords-case-fold-search', to highlight input
regions.")
(defvar-local shell-highlight-fl-syntax-table nil
  "A font lock variable, used by `shell-highlight'.
Used as `font-lock-syntax-table', to highlight input regions.")
(defvar-local shell-highlight-fl-syntactic-face-function nil
  "A font lock variable, used by `shell-highlight'.
Used as `font-lock-syntactic-face-function', to highlight input
regions.")

(defvar-local shell-highlight-fl-syntax-propertize-function nil
  "Used by `shell-highlight' to propertize input regions.")

(defvar-local shell-highlight-fl-orig-dont-widen nil
  "Original value of `font-lock-dont-widen'.")

(defvar-local shell-highlight--remove-extend-functions nil)

(defun shell-highlight--intersect-regions (fun-output fun-input beg end)
  "Run functions for suitable regions.
From the region specified by BEG and END, extract smaller regions
that cover either process output (its 'filed property is 'output)
or input (all remaining text).  Interchangeably call FUN-OUTPUT
on each output region, and FUN-INPUT on each input region.
Restrict to this region beforehand.

FUN-OUTPUT and FUN-INPUT are passed two arguments, the beginning
and end of their respective extracted regions.  You can also pass
nil as either function to skip its respective regions.

Return a cons cell of return values of the first and last
function called, or nil, if no function was called (if BEG = END)."
  (let ((beg1 beg) (end1 beg)
        (return-beg nil) (return-end nil)
        (is-output (eq (get-text-property beg 'filed) 'output)))
    (while
        (and (< beg1 end)
             (setq end1 (or (if is-output
                                (text-property-not-all beg1 end 'field 'output)
                              (text-property-any beg1 end 'field 'output))
                            end)))
      (when-let* ((fun (if is-output fun-output fun-input)))
        (save-restriction
          (let ((beg2 beg1)
                (end2 end1))
            (when (= beg2 beg)
              (setq beg2 (field-beginning beg2)))
            (when (= end2 end)
              (setq end2 (field-end end2)))
            ;; Narrow to the whole field surrounding the region
            (narrow-to-region beg2 end2))
          (setq return-end (list (funcall fun beg1 end1))))
        (unless return-beg
          (setq return-beg return-end)))
      (setq beg1 end1)
      (setq is-output (not is-output)))
    (when return-beg
      (cons (car return-beg) (car return-end)))))

(defun shell-highlight-fontify-region (fun beg end verbose)
  "Around advice for `font-lock-fontify-region-function'.
Fontify region between BEG and END for shell-highlight.  First,
highlight it using FUN.  Then highlight only the input text in
the region using `sh-script-mode' highlighting keywords.
VERBOSE is passed to the fontify-region functions."
  (let ((ret-beg nil)
        (ret-end nil))
    (pcase (funcall fun beg end verbose)
      (`(jit-lock-bounds ,beg1 . ,end1)
       (setq ret-beg beg1 ret-end end1)))
    (pcase
        (shell-highlight--intersect-regions
         nil
         (lambda (beg end)
           (let ((font-lock-keywords
                  shell-highlight-fl-keywords)
                 (font-lock-keywords-only
                  shell-highlight-fl-keywords-only)
                 (font-lock-keywords-case-fold-search
                  shell-highlight-fl-keywords-case-fold-search)
                 (font-lock-syntax-table
                  shell-highlight-fl-syntax-table)
                 (font-lock-syntactic-face-function
                  shell-highlight-fl-syntactic-face-function))
             (prog1 (funcall fun beg end verbose)
               ;; The default fontify function may modify the keywords variable
               (setq shell-highlight-fl-keywords
                     font-lock-keywords))))
         beg end)
      ((and (guard ret-beg)
            `((jit-lock-bounds ,beg1 . ,_) . (jit-lock-bounds ,_ . ,end1)))
       `(jit-lock-bounds ,(min beg1 beg ret-beg) .
                         ,(max end1 end ret-end))))))

(defun shell-highlight-syntax-propertize (beg end)
  "After advice for `syntax-propertize-function'.
Then propertize input input text in the region specified by BEG
and END using `sh-script-mode' propertize function."
  (if-let* ((fun shell-highlight-fl-syntax-propertize-function))
      (shell-highlight--intersect-regions
       nil
       (lambda (beg end)
         (let ((font-lock-keywords
                shell-highlight-fl-keywords)
               (font-lock-keywords-only
                shell-highlight-fl-keywords-only)
               (font-lock-keywords-case-fold-search
                shell-highlight-fl-keywords-case-fold-search)
               (font-lock-syntax-table
                shell-highlight-fl-syntax-table)
               (font-lock-syntactic-face-function
                shell-highlight-fl-syntactic-face-function))
           (funcall fun beg end)))
       beg end)))

;;;###autoload
(define-minor-mode shell-highlight-mode
  "Highlight input text in shell-mode.
Also, disable highlighting the whole input text after RET."
  :group 'shell
  (if shell-highlight-mode
      (progn
        (font-lock-set-defaults)
        ;; Trick to turn on jit-lock with `jit-lock-contextually' set to t
        (font-lock-mode -1)
        (let ((font-lock-keywords-only nil))
          (font-lock-mode 1))

        (make-local-variable 'font-lock-keywords-case-fold-search)
        (make-local-variable 'font-lock-syntax-table)
        (make-local-variable 'font-lock-syntactic-face-function)
        (make-local-variable 'font-lock-fontify-region-function)
        (make-local-variable 'syntax-propertize-function)
        (make-local-variable 'font-lock-keywords)
        (let ((font-lock-set-defaults nil)
              (font-lock-defaults
               ;; Taken from from `sh-mode'
               `((sh-font-lock-keywords
                  sh-font-lock-keywords-1 sh-font-lock-keywords-2)
                 nil nil
                 ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
                 (font-lock-syntactic-face-function
                  . ,#'sh-font-lock-syntactic-face-function)))
              (font-lock-keywords
               font-lock-keywords)
              (font-lock-keywords-only
               font-lock-keywords-only)
              (font-lock-keywords-case-fold-search
               font-lock-keywords-case-fold-search)
              (font-lock-syntax-table
               font-lock-syntax-table)
              (font-lock-syntactic-face-function
               font-lock-syntactic-face-function))
          (font-lock-set-defaults)
          ;; Set up our font-lock variables
          (setq shell-highlight-fl-keywords
                font-lock-keywords)
          (setq shell-highlight-fl-keywords-only
                font-lock-keywords-only)
          (setq shell-highlight-fl-keywords-case-fold-search
                font-lock-keywords-case-fold-search)
          (setq shell-highlight-fl-syntax-table
                font-lock-syntax-table)
          (setq shell-highlight-fl-syntactic-face-function
                font-lock-syntactic-face-function)
          (setq-local shell-highlight-fl-syntax-propertize-function
                      #'sh-syntax-propertize-function))

        ;; Set up our fontify and propertize functions
        (unless syntax-propertize-function
          (setq-local syntax-propertize-function #'ignore))
        (add-function :after (local 'syntax-propertize-function)
                      #'shell-highlight-syntax-propertize)
        (add-function :around (local 'font-lock-fontify-region-function)
                      #'shell-highlight-fontify-region)

        ;; Misc
        (setq shell-highlight-fl-orig-dont-widen
              font-lock-dont-widen)
        (setq font-lock-dont-widen t)
        (setq-local comint-highlight-input nil)

        (unless (memq #'syntax-propertize-multiline
                      syntax-propertize-extend-region-functions)
          (setq shell-highlight--remove-extend-functions t)
          (add-hook 'syntax-propertize-extend-region-functions
                    #'syntax-propertize-multiline 'append 'local)))

    (remove-function (local 'font-lock-fontify-region-function)
                     #'shell-highlight-fontify-region)
    (remove-function (local 'syntax-propertize-function)
                     #'shell-highlight-syntax-propertize)

    (setq font-lock-dont-widen
          shell-highlight-fl-orig-dont-widen)
    (kill-local-variable 'comint-highlight-input)
    (when shell-highlight--remove-extend-functions
      (remove-hook 'syntax-propertize-extend-region-functions
                   #'syntax-propertize-multiline 'local)))

  (font-lock-flush))

;; Disable comint highlighting input after RET

(defun shell-highlight-comint-send-input-adv (fun &rest args)
  "Advice for `comint-send-input'.
Calls FUN with ARGS, preventing it from highlighting prompt if
`comint-highlight-input' is nil.  This is not needed for Emacs
versions 28 or higher."
  (if comint-highlight-input
      (apply fun args)
    (cl-letf* ((add-text-properties (symbol-function 'add-text-properties))
               ((symbol-function 'add-text-properties)
                (lambda (start end props &rest rest)
                  (if (eq (plist-get props 'font-lock-face)
                          'comint-highlight-input)
                      (ignore)
                    (apply add-text-properties start end props rest)))))
      (apply fun args))))

(with-eval-after-load 'comint
  ;; Only install advice for Emacs versions < 28
  (unless (boundp 'comint-highlight-input)
    (defvar comint-highlight-input t)
    (advice-add #'comint-send-input :around
                #'shell-highlight-comint-send-input-adv)))

(provide 'shell-highlight)
;;; shell-highlight.el ends here
