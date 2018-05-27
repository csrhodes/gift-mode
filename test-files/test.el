

(message "%s" (let*
                  ((str "This is the question \\{ with escaped character{ content }")
                   (pos (string-match "\\({.*}\\)" str)))
                (match-string 1 str)))

(message "%s" (let*
                  ((str "This is the question \\{ with escaped character{ content }")
                   (pos (string-match "\\(\\\\{.....\\)" str)))
                (match-string 1 str)))




(message "%s" (let*
                  ((str "This is the question \\{ with escaped character{ content }")
                   (pos (string-match "\\(?:\\\\{\\|.\\)*\\({.*}\\)" str)))
                (match-string 1 str)))
