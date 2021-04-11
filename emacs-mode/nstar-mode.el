;;; nstar-mode.el --- Major mode for N*
;; SPDX-License-Identifier: BSD3 License

;;; Commentary:

;; A major mode providing some syntax highlighting for the typed assembly language N*.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dependency

;;; Code:

(defgroup nstar nil
  "Major mode for editing N* code."
  :link '(custom-group-link :tag "Font lock faces group" font-lock-faces)
  :group 'languages)

(defconst nstar-font-lock-keywords
  '(
    ("^\\([ \t]*\\)\\(\\(\\sw\\|\\s_\\)+\\):"
     2 font-lock-function-name-face)
    ;; function definition
    ("\\b\\(forall\\|unsafe\\|section\\|include\\)\\b"
     1 font-lock-keyword-face)
    ;; keywords
    ("%r[0-5]" .
     font-lock-variable-name-face)
    ;; registers
    ("\\b\\(mv\\|st\\|ld\\|salloc\\|sld\\|sst\\|call\\|ret\\|jmp\\|jz\\|dec\\|inc\\|sfree\\|mul\\|nop\\|sref\\)\\b"
     1 font-lock-builtin-face)
    ;; instructions
    ("\\b\\(T\\(a\\|c\\|s\\|[0-9]+\\)\\|s\\(8\\|16\\|32\\|64\\)\\|u\\(8\\|16\\|32\\|64\\)\\)\\b"
     1 font-lock-type-face)
    ;; builtin types
    )
  "Additional expressions to highlight in N* mode.")

(defvar nstar-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?#  "< b" st)
    (modify-syntax-entry ?_  "w" st)
    (modify-syntax-entry ?\" "|" st)
    (modify-syntax-entry ?'  "|" st)
    st)
  "Syntax table used while in N* mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nst\\'" . nstar-mode))
;;;###autoload
(modify-coding-system-alist 'file "\\.nst\\'" 'utf-8)
;;;###autoload
(define-derived-mode nstar-mode prog-mode "N*"
  "Major mode for editing simple N* source files.

Only provides syntax highlighting."

  (setq-local font-lock-defaults '(nstar-font-lock-keywords))
  (set-syntax-table (make-syntax-table nstar-mode-syntax-table))

  (setq-local comment-start "#")
  (setq-local comment-add 1)
  (setq-local comment-end "")

  (setq-local comment-start-skip "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\|\\*+/\\)")

  (setq-local comment-multi-line 1)
  )


(provide 'nstar-mode)

;;; nstar-mode.el ends here
