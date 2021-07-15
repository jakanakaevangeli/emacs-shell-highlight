;; -*- lexical-binding: t; -*-

(defvar comint-highlight-input)

(defun shell-highlight-fontify-region-advice (fun beg end &optional verbose)
  "Around advice for `font-lock-fontify-region-function'.
Take an intersection of text, input by user at prompt (its 'filed
property isn't 'output), and the region specified by BEG and END.
Call FUN on each continuous region of this intersection. Restrict
to this region beforehand. VERBOSE is passed to FUN."
  (let ((beg1 beg)
        (end1 nil))
    (while (setq beg1 (text-property-not-all beg1 end 'field 'output))
      (setq end1 (or (text-property-any beg1 end 'field 'output) end))
      (save-restriction
        (let ((beg2 beg1)
              (end2 end1))
          (when (= beg2 beg)
            (setq beg2 (constrain-to-field (point-min) beg2)))
          (when (= end2 end)
            (setq end2 (constrain-to-field (point-max) end2)))
          ;; Narrow to the whole input field surrounding the region
          (narrow-to-region beg2 end2))
        (funcall fun beg1 end1 verbose))
      (setq beg1 end1))))

(add-hook 'shell-mode-hook #'shell-highlight-setup-shell-mode)

(defun shell-highlight-setup-shell-mode ()
  "Set up font lock to highlight input text.
Also disable highlighting the whole input text after RET."
  ;; Adapted from `sh-mode'
  (setq font-lock-defaults
        `((sh-font-lock-keywords
           sh-font-lock-keywords-1 sh-font-lock-keywords-2)
          nil nil
          ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
          (font-lock-syntactic-face-function
           . ,#'sh-font-lock-syntactic-face-function)
          (font-lock-fontify-region-function
           . ,(apply-partially #'shell-highlight-fontify-region-advice
                                 font-lock-fontify-region-function))
          (font-lock-dont-widen . t)))
  (setq-local comint-highlight-input nil))

;; Disable comint highlighting input after RET

(defun shell-highlight-comint-send-input-adv (fun &rest args)
  "Advice for `comint-send-input'.
Calls FUN with ARGS, preventing it from highlighting prompt if
`comint-highlight-input' is nil. This is not needed for emacs
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
  ;; Only install advice for emacs versions < 28
  (unless (boundp 'comint-highlight-input)
    (defvar comint-highlight-input t)
    (advice-add 'comint-send-input :around
                 #'shell-highlight-comint-send-input-adv)))
