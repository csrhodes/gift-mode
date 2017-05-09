;;; gift-mode.el --- major mode for editing GIFT format quizzes

;; Copyright (C) 2017 Christophe Rhodes <csr21@cantab.net>

;; Author: Christophe Rhodes <christophe@rhodes.io>
;; URL: https://github.com/csrhodes/gift-mode
;; Package-Version: 20170509.01

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

(defface gift-wrong '((t (:inherit org-todo)))
  "wrong answers in multiple-choice questions in GIFT quizzes")

(defface gift-right '((t (:inherit org-done)))
  "right answers in multiple-choice questions in GIFT quizzes")

(defface gift-wrong-credit '((t (:inherit org-scheduled-previously)))
  "credit for wrong answer in GIFT quizzes")

(defface gift-right-credit '((t (:inherit org-scheduled-today)))
  "credit for right answer in GIFT quizzes")

(defvar gift-font-lock-keywords
  '(("\\$\\$\\([^$\n]\\|\\\\$\\)*[^\\]\\$\\$" . 'gift-latex-math) ; doesn't handle \$$$O(n)$$ correctly; think about font-lock-multiline
    ("\\_<=\\(\\([^\\~=\n}]\\|\\\\[}~=]\\)*\\)" (1 'gift-right))
    ("\\_<~\\(%\\([0-9.]+\\)%\\)\\(\\([^\\~=%}\n]\\|\\\\[A-Za-z0-9}~=%]\\)*\\)" (2 'gift-right-credit) (3 'gift-right))
    ("\\_<~\\(%\\(-[0-9.]+\\)%\\)\\(\\([^\\~=%}\n]\\|\\\\[A-Za-z0-9}~=%]\\)*\\)" (2 'gift-wrong-credit) (3 'gift-wrong))
    ("\\_<~\\(\\([^\\~=%}\n]\\|\\\\[}~=%]\\)*\\)" (1 'gift-wrong))
    ("\\(\\$CATEGORY\\):\s-*\\(\\$course\\$/?\\|\\)\\(.*?\\)\\(//\\|$\\)" (1 'gift-keyword) (2 'gift-keyword) (3 'gift-category))
    ("::\\([^:]\\|\\\\:\\)+::" . 'outline-2)))

;;;###autoload
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gift\\'" . gift-mode))

(provide 'gift-mode)
;;; gift-mode.el ends here
