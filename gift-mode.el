;;; gift-mode.el --- major mode for editing GIFT format quizzes

;; Copyright (C) 2017 Christophe Rhodes <csr21@cantab.net>

(defconst gift-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; maybe these should be in the font-lock-syntax-table instead?
    (modify-syntax-entry ?~ "_   " table)
    (modify-syntax-entry ?\\ "_   " table)
    table))

(defface gift-keyword '((t (:inherit font-lock-keyword-face)))
  "keywords for GIFT quizzes")

(defface gift-category '((t (:inherit outline-1)))
  "category for GIFT quizzes")

(defface gift-question-name '((t (:inherit outline-2)))
  "question name in GIFT quizzes")

(defface gift-latex-math '((t (:inherit font-latex-math-face)))
  "math notation in GIFT quizzes")

(defface gift-wrong '((t (:foreground "red")))
  "wrong answers in multiple-choice questions in GIFT quizzes")

(defface gift-right '((t (:foreground "green")))
  "right answers in multiple-choice questions in GIFT quizzes")

(defvar gift-font-lock-keywords
  '(("\\_<=\\(\\([^\\~=}]\\|\\\\[}~=]\\)*\\)" (1 'gift-right))
    ("\\_<~\\(\\([^\\~=}]\\|\\\\[}~=]\\)*\\)" (1 'gift-wrong))
    ("\\$\\$.*[^\\]\\$\\$" . 'gift-latex-math) ; doesn't handle \$$$O(n)$$ correctly
    ("\\(\\$CATEGORY\\):\s-*\\(\\$course\\$/?\\|\\)\\(.*?\\)\\(//\\|$\\)" (1 'gift-keyword) (2 'gift-keyword) (3 'gift-category))
    ("::\\([^:]\\|\\\\:\\)+::" . 'outline-2)))

(define-derived-mode gift-mode text-mode "GIFT"
  "Major mode for editing GIFT format quizzes.
\\{gift-mode-map}"
  :syntax-table gift-mode-syntax-table
  (setq font-lock-defaults (list '(gift-font-lock-keywords) nil))
  (set (make-local-variable 'outline-regexp) "\\(\\$CATEGORY\\|::\\)")
  (set (make-local-variable 'outline-heading-end-regexp)
       "\\(\\$CATEGORY.*\n\\|::[^:]+::\\(.\\|\n\\)\\)")
  (set (make-local-variable 'outline-heading-alist)
       '(("$CATEGORY" . 1) ("::" . 2))))
