;;; tabular-test.el --- tests for tabular.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'tabular)

(defmacro tabular-test-with-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer initialized with CONTENT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body
     (buffer-string)))

(ert-deftest tabular-test-parse-command-pattern ()
  (should (equal (tabular--parse-command-pattern "/,/r1c1l0")
                 '("," "r1c1l0")))
  (should (equal (tabular--parse-command-pattern "/foo\\/bar/l0")
                 '("foo\\/bar" "l0")))
  (should-not (tabular--parse-command-pattern "assignment")))

(ert-deftest tabular-test-parse-format ()
  (should (equal (tabular--parse-format "r1c1l0")
                 '(("r" 1) ("c" 1) ("l" 0))))
  (should-error (tabular--parse-format "l-1")))

(ert-deftest tabular-test-split-delim ()
  (should (equal (tabular--split-delim "a<<b<<c" "<<\\|>>")
                 '("a" "<<" "b" "<<" "c")))
  (should (equal (tabular--split-delim "abc,def,ghi" "^[^,]*\\zs,")
                 '("abc" "," "def,ghi")))
  (should (equal (tabular--split-delim "ab    c" "  ")
                 '("ab" "  " "" "  " "c"))))

(ert-deftest tabular-test-tabularize-lines-default-format ()
  (should
   (equal
    (tabular--tabularize-lines
     '("Some short phrase,some other phrase"
       "A much longer phrase here,and another long phrase")
     ",")
    '("Some short phrase         , some other phrase"
      "A much longer phrase here , and another long phrase"))))

(ert-deftest tabular-test-tabularize-lines-custom-format ()
  (should
   (equal
    (tabular--tabularize-lines
     '("abc,def,ghi"
       "a,b"
       "a,b,c")
     ","
     "r1c1l0")
    '("abc , def, ghi"
      "  a , b"
      "  a , b  ,  c"))))

(ert-deftest tabular-test-gtabularize-lines ()
  (should
   (equal
    (tabular--tabularize-lines
     '("a=1"
       "no match"
       "bbb=2")
     "="
     nil
     t)
    '("a   = 1"
      "no match"
      "bbb = 2"))))

(ert-deftest tabular-test-tabularize-expands-single-line-range ()
  (should
   (equal
    (tabular-test-with-buffer
        "a=1\nbbb=2\nccc=3\n\nx\n"
      (forward-line 1)
      (tabularize (point) (point) "/=/"))
    "a   = 1\nbbb = 2\nccc = 3\n\nx\n")))

(ert-deftest tabular-test-gtabularize-keeps-nonmatching-lines ()
  (should
   (equal
    (tabular-test-with-buffer
        "a=1\nskip me\nbbb=2\n"
      (gtabularize (point-min) (point-max) "/=/"))
    "a   = 1\nskip me\nbbb = 2\n")))

(ert-deftest tabular-test-first-comma-pattern ()
  (should
   (equal
    (tabular-test-with-buffer
        "abc,def,ghi\na,b\na,b,c\n"
      (tabularize (point-min) (point-max) "/^[^,]*\\zs,/r0c0l0"))
    "abc,def,ghi\n  a,b\n  a,b,c\n")))

(ert-deftest tabular-test-named-command-multiple-spaces ()
  (should
   (equal
    (tabular-test-with-buffer
        "a    b\nlonger  c\n"
      (tabularize (point-min) (point-max) "multiple_spaces"))
    "a       b\nlonger  c\n")))

(ert-deftest tabular-test-last-command-reuse ()
  (should
   (equal
    (tabular-test-with-buffer
        "a=1\nbbb=2\n"
      (tabularize (point-min) (point-max) "/=/")
      (setq tabular--last-command "/,/")
      (should (tabularize-has-pattern-p)))
    "a   = 1\nbbb = 2\n")))

(provide 'tabular-test)
;;; tabular-test.el ends here
