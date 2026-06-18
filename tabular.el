;;; tabular.el --- align columnar data by regexp -*- lexical-binding: t; -*-

;; Author: Andrew Peck
;; Maintainer: Andrew Peck
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (dash "2.19") (s "1.12.0"))
;; Keywords: convenience, text
;; URL: https://github.com/andrewpeck/tabular.el

;; Copyright (c) 2012, Matthew J. Wozniski
;; Copyright (c) 2022-2026, Andrew Peck

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Emacs port of Tabular.vim.
;;
;; The core behavior is preserved:
;; - align on a delimiter regexp
;; - support repeated delimiters on a line
;; - support format strings like `r1c1l0'
;; - support automatic single-line range expansion
;; - support named commands and simple pipelines
;;
;; Built-in Emacs helpers are used where they fit naturally (`string-trim',
;; `string-width', buffer range primitives).  The delimiter engine remains
;; custom because `align-regexp' cannot express the repeated-delimiter behavior
;; that makes Tabular useful.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 's)

(defgroup tabular nil
  "Align columnar data by regexp."
  :group 'editing
  :prefix "tabular-")

(defcustom tabular-default-format "l1"
  "Default alignment format.

Each element is `l', `r', or `c' followed by a non-negative integer.
The letter controls alignment and the number controls padding inserted
after that field."
  :type 'string)

(cl-defstruct tabular-command
  pattern
  filters)

(defvar tabular--commands (make-hash-table :test #'equal))
(defvar-local tabular--local-commands (make-hash-table :test #'equal))
(defvar tabular--last-command nil)
(defvar tabular--gtabularize nil)

(defconst tabular--format-element-regexp "\\([lrc]\\)\\([0-9]+\\)")

(defun tabular--left-align (string width)
  "Left align STRING to WIDTH."
  (concat string (make-string (max 0 (- width (string-width string))) ? )))

(defun tabular--right-align (string width)
  "Right align STRING to WIDTH."
  (concat (make-string (max 0 (- width (string-width string))) ? ) string))

(defun tabular--center-align (string width)
  "Center align STRING to WIDTH."
  (let* ((spaces (max 0 (- width (string-width string))))
         (right (/ spaces 2))
         (left (- spaces right)))
    (concat (make-string left ? ) string (make-string right ? ))))

(defun tabular--validate-format (format)
  "Return non-nil when FORMAT is valid."
  (and (stringp format)
       (string-match-p
        (concat "\\`\\(?:" tabular--format-element-regexp "\\)+\\'")
        format)))

(defun tabular--parse-format (format)
  "Parse FORMAT into a list of (ALIGN PAD) entries."
  (unless (tabular--validate-format format)
    (error "Invalid tabular format: %S" format))
  (save-match-data
    (cl-loop with start = 0
             while (string-match tabular--format-element-regexp format start)
             collect (list (match-string 1 format)
                           (string-to-number (match-string 2 format)))
             do (setq start (match-end 0)))))

(defun tabular--parse-command-pattern (string)
  "Parse /pattern/format from STRING.

Return (PATTERN FORMAT) or nil when STRING is not of that form.
Escaped slashes inside the pattern are preserved."
  (when (and (stringp string)
             (string-match
              (rx (seq string-start
                       "/"
                       (group (zero-or-more (or (seq "\\" not-newline)
                                                (not (any "/")))))
                       "/"
                       (group (zero-or-more not-newline))
                       string-end))
              string))
    (let ((pattern (match-string 1 string))
          (format (match-string 2 string)))
      (unless (or (string-empty-p format)
                  (tabular--validate-format format))
        (error "Invalid tabular format: %S" format))
      (list pattern (unless (string-empty-p format) format)))))

(defun tabular--command-names ()
  "Return all registered command names."
  (-union (hash-table-keys tabular--local-commands)
          (hash-table-keys tabular--commands)))

(defun tabular--lookup-command (name)
  "Return registered command NAME."
  (or (gethash name tabular--local-commands)
      (gethash name tabular--commands)))

(defun tabular--put-command (name command local overwrite)
  "Store COMMAND under NAME.
If LOCAL is non-nil, use the buffer-local command table."
  (let ((table (if local tabular--local-commands tabular--commands)))
    (when (and (not overwrite) (gethash name table))
      (error "Tabular command %S already exists" name))
    (puthash name command table)))

(defun tabular--longest-common-indent (strings)
  "Return the longest common leading indent shared by all STRINGS."
  (if (null strings)
      ""
    (cl-loop for idx from 0
             for chars = (--map (and (< idx (length it)) (aref it idx)) strings)
             while (and (cl-every #'identity chars)
                        (or (cl-every (lambda (c) (eq c ? )) chars)
                            (cl-every (lambda (c) (eq c ?\t)) chars)))
             finally return (substring (car strings) 0 idx))))

(defun tabular--compile-delimiter (pattern)
  "Compile PATTERN into a delimiter spec plist.

Handles the Vim-style prefix-capture syntax: text before `\\zs' is kept
as context while only the text after it counts as the delimiter."
  (let ((zs-pos (string-match "\\\\zs" pattern)))
    (if zs-pos
        (list :regexp
              (concat "\\(?:" (substring pattern 0 zs-pos) "\\)"
                      "\\(" (substring pattern (+ zs-pos 3)) "\\)")
              :group 1)
      (list :regexp pattern :group 0))))

(defun tabular--next-match (compiled string start)
  "Return next match for COMPILED delimiter in STRING starting at START.
Return (DELIM-START DELIM-END TEXT) or nil when no match is found."
  (let ((regexp (plist-get compiled :regexp))
        (group (plist-get compiled :group)))
    (when (and (<= start (length string))
               (string-match regexp string start))
      (let ((delim-start (match-beginning group))
            (delim-end (match-end group)))
        (unless (or (null delim-start) (null delim-end))
          (list delim-start delim-end
                (substring string delim-start delim-end)))))))

(defun tabular--split-delim (string pattern)
  "Split STRING on PATTERN, keeping delimiters."
  (let ((matcher (tabular--compile-delimiter pattern))
        (beg 0)
        (searchoff 0)
        (len (length string))
        result
        match)
    (while (setq match (tabular--next-match matcher string (+ beg searchoff)))
      (pcase-let ((`(,delim-start ,delim-end ,text) match))
        (unless (= delim-start len)
          (if (and (= delim-start beg) (= delim-end beg))
              (setq searchoff (1+ searchoff))
            (push (substring string beg delim-start) result)
            (push text result)
            (setq beg delim-end)
            (setq searchoff 0)))))
    (nreverse (cons (substring string beg) result))))

(defun tabular--strip-line (line gtabularize)
  "Trim non-delimiter fields in LINE.
When GTABULARIZE is non-nil, single-field lines are left untouched."
  (if (and gtabularize (= (length line) 1))
      line
    (--map-indexed (if (cl-evenp it-index) (string-trim it) it) line)))

(defun tabular--max-widths (lines gtabularize)
  "Return per-field maximum widths for LINES.
When GTABULARIZE is non-nil, single-field lines are excluded."
  (let* ((relevant (if gtabularize (--remove (= (length it) 1) lines) lines))
         (n-cols (apply #'max 0 (-map #'length relevant))))
    (cl-loop for i from 0 below n-cols
             collect (apply #'max 0 (-map #'string-width (--keep (nth i it) relevant))))))

(defun tabular--format-field (field width spec)
  "Format FIELD to WIDTH according to SPEC."
  (pcase-let ((`(,align ,pad) spec))
    (concat
     (pcase align
       ("l" (tabular--left-align field width))
       ("r" (tabular--right-align field width))
       ("c" (tabular--center-align field width))
       (_ (error "Unknown alignment directive: %S" align)))
     (make-string pad ? ))))

(defun tabular--tabularize-lines (lines pattern &optional format gtabularize)
  "Align LINES using PATTERN and FORMAT.

When GTABULARIZE is non-nil, lines not matching PATTERN are returned unchanged
and do not affect the width calculation."
  (let* ((format-spec (tabular--parse-format (or format tabular-default-format)))
         (split-lines (--map (tabular--split-delim it pattern) lines))
         (first-fields (->> split-lines
                            (--remove (and gtabularize (= (length it) 1)))
                            (-map #'car)))
         (common-indent (tabular--longest-common-indent first-fields))
         (stripped-lines (--map (tabular--strip-line it gtabularize) split-lines))
         (maxes (tabular--max-widths stripped-lines gtabularize)))
    (-zip-with
     (lambda (original line)
       (if (and gtabularize (= (length line) 1))
           original
         (string-trim-right
          (concat common-indent
                  (string-join
                   (--map-indexed
                    (tabular--format-field
                     it
                     (or (nth it-index maxes) 0)
                     (nth (mod it-index (length format-spec)) format-spec))
                    line))))))
     lines
     stripped-lines)))

(defun tabular--line-range-lines (beg end)
  "Return lines between BEG and END inclusive."
  (save-excursion
    (goto-char beg)
    (let* ((start (line-beginning-position))
           (finish (progn
                     (goto-char end)
                     (line-end-position))))
      (split-string
       (buffer-substring-no-properties start finish)
       "\n" nil))))

(defun tabular--replace-line-range (beg end lines)
  "Replace lines between BEG and END inclusive with LINES."
  (save-excursion
    (goto-char beg)
    (let ((start (line-beginning-position))
          (finish (progn
                    (goto-char end)
                    (line-end-position))))
      (delete-region start finish)
      (insert (string-join lines "\n")))))

(defun tabular--expand-range (beg end includepat gtabularize)
  "Expand single-line region from BEG to END using INCLUDEPAT.

When GTABULARIZE is non-nil, range expansion is disabled."
  (if (or gtabularize
          (null includepat)
          (/= (save-excursion (goto-char beg) (line-beginning-position))
              (save-excursion (goto-char end) (line-beginning-position))))
      (cons beg end)
    (save-excursion
      (goto-char beg)
      (let ((regexp (plist-get (tabular--compile-delimiter includepat) :regexp)))
        (cl-flet ((line-matches-p ()
                    (string-match-p regexp (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position)))))
          (if (not (line-matches-p))
              (cons beg end)
            (let ((top (line-beginning-position))
                  (bot (line-end-position)))
              (while (and (> (line-beginning-position) (point-min))
                          (progn (forward-line -1) (line-matches-p)))
                (setq top (line-beginning-position)))
              (goto-char beg)
              (while (and (= (forward-line 1) 0) (line-matches-p))
                (setq bot (line-end-position)))
              (cons top bot))))))))

(defun tabular--apply-filters (lines filters)
  "Apply FILTERS to LINES."
  (-reduce-from (lambda (acc f) (funcall f acc)) lines filters))

(defun tabular--resolve-command (command)
  "Resolve COMMAND into (PATTERN FILTERS)."
  (let ((parsed (tabular--parse-command-pattern command)))
    (if parsed
        (pcase-let ((`(,pattern ,format) parsed))
          (list pattern
                (list (lambda (lines)
                        (tabular--tabularize-lines lines pattern format tabular--gtabularize)))))
      (let ((entry (tabular--lookup-command command)))
        (unless entry
          (error "Unknown tabular command: %S" command))
        (list (tabular-command-pattern entry)
              (tabular-command-filters entry))))))

(defun tabular--operate (beg end command gtabularize)
  "Apply COMMAND between BEG and END.
When GTABULARIZE is non-nil, non-matching lines are left untouched."
  (pcase-let* ((`(,pattern ,filters) (tabular--resolve-command command))
               (`(,range-beg . ,range-end) (tabular--expand-range beg end pattern gtabularize))
               (lines (tabular--line-range-lines range-beg range-end))
               (output (let ((tabular--gtabularize gtabularize))
                         (tabular--apply-filters lines filters))))
    (tabular--replace-line-range range-beg range-end output)))

(defun tabular--pipeline-tabularize (pattern &optional format)
  "Return a pipeline filter for PATTERN and FORMAT."
  (lambda (lines)
    (tabular--tabularize-lines lines pattern format tabular--gtabularize)))

;;;###autoload
(defun tabular-add-pattern (name pattern &optional format local overwrite)
  "Register NAME as a tabular command for PATTERN and FORMAT.
If LOCAL is non-nil, use the buffer-local table; OVERWRITE allows replacement."
  (tabular--put-command
   name
   (make-tabular-command
    :pattern pattern
    :filters (list (tabular--pipeline-tabularize pattern format)))
   local overwrite))

;;;###autoload
(defun tabular-add-pipeline (name pattern filters &optional local overwrite)
  "Register NAME as a tabular command for PATTERN and FILTERS.
If LOCAL is non-nil, use the buffer-local table; OVERWRITE allows replacement."
  (unless (cl-every #'functionp filters)
    (error "All tabular pipeline elements must be functions"))
  (tabular--put-command
   name
   (make-tabular-command :pattern pattern :filters filters)
   local overwrite))

(defun tabular--region-or-line ()
  "Return active region bounds or current line bounds."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

(defun tabular--read-command-args (prompt-prefix bounds-fn)
  "Read a tabular command interactively.
PROMPT-PREFIX is the label shown; BOUNDS-FN returns the default (BEG . END)."
  (let* ((bounds (funcall bounds-fn))
         (input (read-string
                 (if tabular--last-command
                     (format "%s (default %s): " prompt-prefix tabular--last-command)
                   (format "%s: " prompt-prefix)))))
    (list (car bounds)
          (cdr bounds)
          (if (string-empty-p input)
              (or tabular--last-command
                  (user-error "No previous tabular command"))
            input))))

;;;###autoload
(defun tabularize (beg end command)
  "Align region from BEG to END using COMMAND.

COMMAND may be `/pattern/format' or the name of a registered command.
When called interactively with an empty prompt, reuse the previous command."
  (interactive (tabular--read-command-args "Tabularize" #'tabular--region-or-line))
  (setq tabular--last-command command)
  (tabular--operate beg end command nil))

;;;###autoload
(defun gtabularize (beg end command)
  "Align region from BEG to END using COMMAND.
Non-matching lines are left unchanged."
  (interactive (tabular--read-command-args
                "GTabularize"
                (lambda ()
                  (if (use-region-p)
                      (cons (region-beginning) (region-end))
                    (cons (point-min) (point-max))))))
  (setq tabular--last-command command)
  (tabular--operate beg end command t))

(defun tabularize-has-pattern-p ()
  "Return non-nil when a previous interactive command exists."
  (and tabular--last-command t))

(defun tabular--regexp-replace-filter (regexp replacement)
  "Return a filter replacing REGEXP with REPLACEMENT in each line."
  (lambda (lines)
    (--map (s-replace-regexp regexp replacement it) lines)))

(defun tabular--split-c-declarations (lines)
  "Split multi-declaration C statements in LINES."
  (-mapcat
   (lambda (line)
     (let* ((parts (split-string line "\\s-*[,;]\\s-*"))
            (type (s-replace-regexp
                   "\\(?:\\(?:[&*]\\s-*\\)*\\)?[[:word:]_]+\\'" "" (car parts))))
       (cons (concat (car parts) ";")
             (--map (concat type it ";") (cdr parts)))))
   lines))

;;;###autoload
(defun tabular-install-default-commands ()
  "Install the built-in Tabular command set."
  (interactive)
  (clrhash tabular--commands)
  (tabular-add-pattern "assignment"
                       "[|&+*/%<>=!~-]\\(?!=\\)[|&+*/%<>=!~-]*="
                       "l1r1" nil t)
  (tabular-add-pattern "two_spaces" "  " "l0" nil t)
  (tabular-add-pipeline "multiple_spaces" "  "
                        (list (tabular--regexp-replace-filter "   *" "  ")
                              (tabular--pipeline-tabularize "  " "l0"))
                        nil t)
  (tabular-add-pipeline "spaces" " "
                        (list (tabular--regexp-replace-filter "  *" " ")
                              (tabular--pipeline-tabularize " " "l0"))
                        nil t)
  (tabular-add-pipeline "argument_list" "(.*)"
                        (list (tabular--regexp-replace-filter "\\s-*\\([(,)]\\)\\s-*" "\\1")
                              (tabular--pipeline-tabularize "[(,)]" "l0")
                              (tabular--regexp-replace-filter "\\(\\s-*\\)," ",\\1 ")
                              (tabular--regexp-replace-filter "\\s-*)" ")"))
                        nil t)
  (tabular-add-pipeline "split_declarations" ",.*;"
                        (list #'tabular--split-c-declarations)
                        nil t)
  (tabular-add-pattern "ternary_operator" "^.*\\(?:\\?\\|:\\)" "l1" nil t)
  (tabular-add-pattern "cpp_io" "<<\\|>>" "l1" nil t)
  (tabular-add-pattern "pascal_assign" ":=" "l1" nil t)
  (tabular-add-pattern "trailing_c_comments" "/\\*\\|\\*/\\|//" "l1" nil t))

(tabular-install-default-commands)

(provide 'tabular)
;;; tabular.el ends here
;; LocalWords: plist zs
