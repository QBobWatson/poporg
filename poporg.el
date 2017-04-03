;;; poporg.el --- Pop a comment or string to an empty buffer for text editing

;; Copyright © 2014 Joseph Rabinoff.
;; Copyright © 2013 Ubity inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;;      Joseph Rabinoff <rabinoff@post.harvard.edu>
;; Maintainer: Joseph Rabinoff <rabinoff@post.harvard.edu>
;; Keywords: outlines, tools
;; URL: https://github.com/QBobWatson/poporg

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; poporg is a small Emacs Lisp project to help editing program strings and
;; comments using Org mode (or any other major mode).  This can be useful as it
;; is often more convenient to edit large pieces of text, like Emacs Lisp or
;; Python docstrings, in an org-mode buffer instead of in a comment or a string.

;; See the README.org file located at https://github.com/QBobWatson/poporg for
;; detailed usage information.

;;; Code:

(eval-when-compile
  (require 'cl))

;; * Customs
;; ** Group

(defgroup poporg nil
  "Edit strings and comments in text buffers."
  :prefix "poporg-"
  :group 'lisp)

;; ** defcustom's

(defcustom poporg-adjust-fill-column t
  "Whether to adjust the fill column in the edit buffer.

If non-nil, in the edit buffer decrement `fill-column' by the prefix length."
  :group 'poporg
  :type 'boolean)

(defcustom poporg-delete-trailing-whitespace t
  "Whether to delete trailing whitespace from the prefix.

If t, when inserting a blank line from the edit buffer back into the source
buffer, remove trailing whitespace from the prefix.  This is very useful when
editing docstrings in python, for instance.  If equal to the symbol 'all, don't
insert the prefix at all for blank lines."
  :group 'poporg
  :type '(choice
          (const :tag "Do not delete trailing whitespace" nil)
          (const :tag "Delete trailing whitespace" t)
          (const :tag "Delete the entire prefix" all)))

(defcustom poporg-buffer-name "*poporg: %s*"
  "Template for poporg buffer names.

The tag %s is replaced by the original buffer name."
  :group 'poporg
  :type 'string)

(defcustom poporg-comment-skip-regexp "[[:space:]*]*"
  "Ignore these additional characters at the beginning of a commented line.

Characters not matched by this regexp will not be included in the common prefix
for comments.  This is matched after `comment-start'.  By default this matches
whitespace and the * character; the latter is useful in C-style comments.  This
should not match newlines."
  :group 'poporg
  :type 'regexp)

(defcustom poporg-edit-hook '(org-mode)
  "List of hooks to run once a new editing buffer has been filled.

In the absence of any hooks here, the poporg editing buffer is in
`fundamental-mode', so you should probably use this hook to set the major mode.
By default this hook enables `org-mode'."
  :group 'poporg
  :type 'hook)

(defcustom poporg-edit-exit-hook nil
  "List of hooks to run prior to moving back an editing buffer."
  :group 'poporg
  :type 'hook)

(defvar poporg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'poporg-edit-exit)
    (define-key map (kbd "C-c C-c") 'poporg-update)
    (define-key map (kbd "C-c C-s") 'poporg-update-and-save)
    map)
  "Keys used in `poporg-mode' buffers.")

;; ** Face

(defface poporg-edited-face
  '((((class color) (background light))
     (:foreground "gray"))
    (((class color) (background dark))
     (:foreground "gray")))
  "Face for a region while it is being edited."
  :group 'poporg)

;; * Internal variables

(defvar poporg-data nil
  "List of (BUFFER OVERLAY PREFIX TYPE) lists.

For each edit BUFFER, there is an OVERLAY graying out the edited block comment
or string in the original buffer, and a PREFIX that was removed from all lines
in the edit buffer and which is going to be prepended to these lines before
returning them the original buffer.  TYPE is either 'string, 'comment, or
'region.")

(defvar poporg-orig-point nil
  "Keeps track of the value of point in the calling buffer.
Dynamically bound variable.")

(defvar poporg-new-point nil
  "Keeps track of the value of point in the new buffer.
Dynamically bound variable.")

(defvar poporg-pre-window-configuration nil
  "Variable to store the original window configuration.")

;; * Functions

;; ** utility

(defun poporg-chomp (str)
  "Chomp leading and trailing whitespace from STR."
  (while (string-match
          "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
          str)
    (setq str (replace-match "" t t str)))
  str)

(defun poporg-chomp-end (str)
  "Chomp trailing whitespace from STR."
  (while (string-match "\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun poporg-check-already-edited (beg end)
  "Check if there is an already edited region overlapping BEG to END.
If yes, pop the editing buffer for the first one and return t."
  (let ((overlays (overlays-in beg end)))
    (catch 'found
      (while overlays
        (let ((entry (overlay-get (pop overlays) 'poporg-overlay)))
          (when entry
            (pop-to-buffer (car entry))
            (throw 'found entry))))
      nil)))

(defun poporg-make-buffer ()
  "Make a poporg buffer."
  (generate-new-buffer (format poporg-buffer-name (buffer-name))))

(defun poporg-fc (arg)
  "Like `forward-char' on ARG but won't throw an error."
  (condition-case nil (forward-char arg) (error nil)))

(defun poporg-orig-buffer ()
  "If this is an edit buffer, find the originating buffer."
  (let* ((entry (assq (current-buffer) poporg-data))
         (overlay (cadr entry)))
    (when overlay (overlay-buffer overlay))))

;; *** skip past comments

(defun poporg-skip-past-comment-start ()
  "Skip whitespace, `comment-start', and comment syntax chars."
  (skip-syntax-forward " ")
  (let ((com-start (if comment-start (poporg-chomp comment-start) "")))
    (when (looking-at (regexp-quote com-start))
      (goto-char (match-end 0))))
  (skip-syntax-forward "<"))

(defun poporg-skip-past-comment-end ()
  "Skip whitespace and `comment-end'."
  (skip-syntax-forward " ")
  (let ((com-end (if comment-end (poporg-chomp comment-end) "")))
    (when (looking-at (regexp-quote com-end))
      (goto-char (match-end 0)))))

;; *** check whitespace

(defun poporg-whitespace-before-p (pos)
  "Return t if there is only whitespace before POS on its line."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (skip-syntax-forward " ")
    (equal pos (if (markerp pos) (point-marker) (point)))))

(defun poporg-whitespace-after-p (pos)
  "Return t if there is only whitespace after POS on its line."
  (save-excursion
    (goto-char pos)
    (skip-syntax-forward " ")
    (eolp)))

;; ** find and insert

;; *** insert into other buffer

(defun poporg-insert-substring (buf start end)
  "Call `insert-buffer-substring-no-properties' on BUF START END.

Keep track of where the point is using `poporg-orig-point'
and `poporg-new-point'."
  (let ((starting (point)))
    (insert-buffer-substring-no-properties buf start end)
    (cond
     ((>= poporg-orig-point end)
      (setq poporg-new-point (point)))
     ((>= poporg-orig-point start)
      (setq poporg-new-point (+ starting (- poporg-orig-point start)))))))

(defun poporg-insert-without-prefix (buf prefix start end)
  "Insert lines into BUF after removing PREFIX.

Start at START in current buffer and end at END.  On lines that do not start
with prefix, or contain only whitespace after the prefix, just insert a
newline.  Respects the value of `poporg-delete-trailing-whitespace'."
  (let ((prefix-re (regexp-quote prefix))
        (cur-buf (current-buffer)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (if (looking-at prefix-re)
            (progn
              (goto-char (match-end 0))
              (if (and poporg-delete-trailing-whitespace
                       (poporg-whitespace-after-p (point)))
                  (with-current-buffer buf (insert "\n")) ; uninteresting
                ;; interesting
                (let ((s (point))
                      (e (save-excursion (forward-line 1) (point))))
                  (with-current-buffer buf
                    (poporg-insert-substring cur-buf s e)))))
          ;; uninteresting
          (with-current-buffer buf (insert "\n")))
        (forward-line 1)))))

(defun poporg-insert-with-prefix (buf start end prefix &optional no-first)
  "Use the contents of BUF to replace the region from START to END.

Prepend PREFIX onto each line.  If NO-FIRST is non-nil, do not prepend PREFIX
onto the first line.  Delete trailing whitespace from blank lines if
`poporg-delete-trailing-whitespace' is set."
  (delete-region start end)
  (goto-char start)
  (let ((cur-buf (current-buffer))
        (prefix-no-ws (poporg-chomp-end prefix)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((s (point))
                 (char-at (char-after s))
                 e)
            (forward-line 1)
            (setq e (point))
            (with-current-buffer cur-buf
              (if no-first
                  (setq no-first nil)
                (if (and poporg-delete-trailing-whitespace
                         (or (null char-at) (= char-at ?\n)))
                    ;; strip whitespace from prefix for blank lines
                    (unless (eq poporg-delete-trailing-whitespace 'all)
                      (insert prefix-no-ws))
                  (insert prefix)))
              (poporg-insert-substring buf s e))))))))

;; *** find string or comment

(defun poporg-find-string-or-comment ()
  "Return the start and end positions of the nearest string or comment.

If the point is in a string or comment, this returns the extents of the current
string or comment.  If the point is immediately before (resp.  after) a string
or comment, returns the extents of the following (resp.  preceding) string or
comment.  This function uses the current buffer's syntax tables for its
searches.

If a string or comment was found, return a list

   (TYPE START END)

where TYPE is either 'string or 'comment and START and END are markers.  The
enclosed region includes the delimiters.

If a comment was found, the region between START and END is a number of complete
lines (including trailing newlines) containing only comments.  This means that
comments not on their own line are ignored.  There may also be blank lines in
this region.

If a string was found, the region from START to END bounds the string with its
delimiters.  There will only be whitespace before the start of the string.  This
means that a string with non-whitespace before it is ignored.

If no string or comment was found satisfying the above criteria, return nil."
  (save-excursion
    (let ((ppss (syntax-ppss))
          (search-start (point)))
      (unless (nth 8 ppss)
        ;; We're not in a string or comment.  Skip past whitespace and search
        ;; one character at a time until we are.  Sometimes stupidest algorithm
        ;; is the most reliable.  First search forward.
        (skip-syntax-forward " >")
        (catch 'foundit
          (dotimes (i 10)
            (setq ppss (syntax-ppss))
            (when (nth 8 ppss)
              (throw 'foundit nil))
            (poporg-fc 1))
          ;; now search backward
          (goto-char search-start)
          (skip-syntax-backward " >")
          (dotimes (i 10)
            (setq ppss (syntax-ppss))
            (when (nth 8 ppss)
              (throw 'foundit nil))
            (poporg-fc -1))))
      ;; done searching
      (let ((in-string  (nth 3 ppss))
            (in-comment (nth 4 ppss))
            (start-pos  (nth 8 ppss))
            start end)
        (when start-pos
          ;; in string or comment
          (if in-string
              (progn
                (setq start (set-marker (make-marker) start-pos))
                ;; find end of string
                (parse-partial-sexp (point) (buffer-size)
                                    nil nil ppss 'syntax-table)
                (setq end (point-marker))
                (when (poporg-whitespace-before-p start)
                  (list 'string start end)))
            (when in-comment ; should be true at this point
              (goto-char start-pos)
              ;; skip backward over comments and whitespace
              (forward-comment (- (buffer-size)))
              ;; skip forward to beginning of first comment
              (skip-syntax-forward " >")
              (if (not (poporg-whitespace-before-p (point)))
                  (forward-line 1) ; it's not on its own line
                (forward-line 0))
              ;; beginning of line of first comment
              (setq start (point-marker))
              ;; skip forward over comments and whitespace
              (forward-comment (buffer-size))
              ;; skip back to end of last comment
              (skip-syntax-backward " >")
              (save-excursion (forward-line 1)
                              (setq end (point-marker)))
              (when (and (> end start)
                         (poporg-whitespace-after-p (point)))
                (list 'comment start end)))))))))

;; *** insert from comment

(defun poporg-get-comment-lines (buf start end)
  "Parse a comment and insert it, with common prefix removed, into BUF.

START and END are positions as returned by `poporg-find-string-or-comment'.

At the beginning of every line, ignore whitespace, `comment-end',
`comment-start', comment syntax characters, and `poporg-comment-skip-regexp', in
that order.  This is what is used to calculate the common prefix.  If there is
anything left, that line is considered interesting.  This skips over
uninteresting lines in the beginning and end.  For instance, in the C-style
comment:

 /*
  * Only this line will be extracted, not the lines above and below.
  */

The prefix will be \"  \" or \"  * \", depending on whether
`poporg-comment-skip-regexp' matches the star character.  If there are no
interesting lines, extract the second comment line, if there is one; otherwise
use the unique comment line.

Return a list (START END PREFIX), where START is the beginning of the first
interesting line, END is the end of the last interesting line (including the
newline), and PREFIX is the common prefix of all interesting lines.  START and
END are markers."
  (let (start2 end2 line-start line-end prefix)
    (save-excursion
      (goto-char start)
      (forward-line 0)
      ;; make a list of interesting lines
      (while (< (point) end)
        (setq line-start (point))
        (poporg-skip-past-comment-end)
        (poporg-skip-past-comment-start)
        (when (looking-at poporg-comment-skip-regexp)
          (goto-char (match-end 0)))
        (when (not (eolp))
          ;; this is an interesting line
          (setq line-end (save-excursion (forward-line 1) (point)))
          ;; update prefix
          (let ((beg (buffer-substring-no-properties line-start (point))))
            (if prefix
                (setq prefix (or (fill-common-string-prefix beg prefix) ""))
              (setq prefix beg)))
          (unless start2 (setq start2 line-start))
          (setq end2 line-end))
        (forward-line 1)))
    (if prefix
        ;; insert interesting lines into buf
        (poporg-insert-without-prefix buf prefix start2 end2)
      ;; Make a blank buffer; insert over second comment line, or first if there
      ;; is none.  This way one can compose blank comments.
      (save-excursion
        (goto-char start)
        (forward-line 0)
        (setq start2 (point))
        (forward-line 1)
        (if (< (point) end)
            ;; use the second line
            (setq start2 (point))
          (goto-char start2))
        (skip-syntax-forward "^>")
        (setq prefix (buffer-substring-no-properties
                      start2 (point)))
        (forward-line 1)
        (setq end2 (point))
        (with-current-buffer buf (insert "\n"))))
    (list (set-marker (make-marker) start2)
          (set-marker (make-marker) end2)
          prefix)))

(defun poporg-insert-comment-lines (buf start end prefix overlay)
  "Insert the contents of BUF as comments in the current buffer.

Replace the region from START to END and prepend PREFIX onto each line.  Append
a trailing newline if necessary.  Uses `poporg-insert-with-prefix' to do the
work.  Move OVERLAY to the newly-inserted region."
  (poporg-insert-with-prefix buf start end prefix)
  ;; For our purposes, comments always comprise entire lines, so insert a
  ;; trailing newline if necessary.
  (when (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            (and (char-before) (not (= (char-before) ?\n)))))
    (insert "\n"))
  (move-overlay overlay start (point)))

;; *** insert from string

(defun poporg-get-string-lines (buf start end)
  "Parse a string and insert it, with common indentation removed, into BUF.

START and END are positions as returned by `poporg-find-string-or-comment'.

This function does not insert the start and end string delimiters.  Lines that
are not composed entirely of whitespace count toward determining the
indentation.  The indentation of the first line is the indentation before the
opening string delimiter.

This function refuses to edit empty strings, since there is no reliable way to
decide which are the starting and ending delimiters if there is nothing between
them.

Return (START END PREFIX) as in `poporg-get-comment-lines'.  The returned values
of START and END agree with the passed arguments.  (They are included so that
this function has the same usage as `poporg-get-comment-lines')."
  (let* ((beg-last-line (save-excursion
                          (goto-char end) (forward-line 0) (point)))
         (end-last-line (save-excursion
                          (goto-char end) (skip-syntax-backward "\"|") (point)))
         (one-line-p (<= beg-last-line start))
         (cur-buf (current-buffer))
         prefix line-start start2)
    (when (<= end-last-line start)
      (user-error "Refusing to edit empty string"))
    (save-excursion
      (goto-char start)
      ;; starting prefix is whitespace before opening delimiter
      (setq prefix (buffer-substring-no-properties
                    (save-excursion (forward-line 0) (point)) start))
      (forward-line 1)
      ;; loop over lines with no delimiters
      (while (< (point) beg-last-line)
        (setq line-start (point))
        (skip-syntax-forward " ")
        (unless (eolp)
          (setq prefix (or (fill-common-string-prefix
                            (buffer-substring-no-properties
                             line-start (point))
                            prefix)
                           "")))
        (forward-line 1))
      (unless one-line-p
        ;; handle last line
        (setq line-start (point))
        (skip-syntax-forward " ")
        (setq prefix (or (fill-common-string-prefix
                          (buffer-substring-no-properties
                           line-start (point))
                          prefix) ""))))
    ;; insert into buf
    (save-excursion
      (goto-char start)
      (skip-syntax-forward "\"|")
      (setq start2 (point))
      (if one-line-p
          (with-current-buffer buf
            (poporg-insert-substring cur-buf start2 end-last-line))
        (forward-line 1)
        (let ((end2 (point)))
          (with-current-buffer buf
            (poporg-insert-substring cur-buf start2 end2)))
        (poporg-insert-without-prefix buf prefix (point) beg-last-line)
        (goto-char beg-last-line)
        ;; the last line by definition starts with prefix
        (forward-char (length prefix))
        (setq start2 (point))
        (with-current-buffer buf
          (poporg-insert-substring cur-buf start2 end-last-line))))
    (list (set-marker (make-marker) start)
          (set-marker (make-marker) end)
          prefix)))

(defun poporg-insert-string-lines (buf start end prefix overlay)
  "Insert the contents of BUF into a string in the current buffer.

Replace the string between START and END and prepend PREFIX onto each interior
line.  Skip delimiters on both sides.  Uses `poporg-insert-with-prefix' to do
the work.  Move OVERLAY to the newly-inserted region."
  (let ((start-mark (set-marker (make-marker) start))
        (end-mark (set-marker (make-marker) end)))
    (save-excursion
      (goto-char start)
      (skip-syntax-forward "\"|")
      (setq start (point)))
    (save-excursion
      (goto-char end)
      (skip-syntax-backward "\"|")
      (setq end (point)))
    (poporg-insert-with-prefix buf start end prefix 'no-first-line)
    ;; if the buffer is terminated by a newline, need to prepend the prefix before
    ;; the closing delimiter
    (when (with-current-buffer buf
            (save-excursion
              (goto-char (point-max))
              (= (char-before) ?\n)))
      (insert prefix))
    (move-overlay overlay
                  (marker-position start-mark)
                  (marker-position end-mark))))

;; *** insert from region

(defun poporg-get-region-lines (buf start end)
  "Insert lines into BUF between START and END with common prefix removed.

This narrows the buffer before doing any parsing.  The common prefix is
calculated naively, as the literal common prefixes of all lines in the region
\(after narrowing).

Return (START END PREFIX) as in `poporg-get-comment-lines'.  The returned START
and END are the same as the passed arguments."
  (save-restriction
    (narrow-to-region start end)
    (save-excursion
      (goto-char (point-min))
      (let (line-start prefix)
        (while (< (point) (point-max))
          (setq line-start (point))
          (skip-syntax-forward " ")
          (unless (eolp)
            ;; use the whole line to determine prefix
            (let ((line (buffer-substring-no-properties
                         line-start
                         (save-excursion (skip-chars-forward "^\n")
                                         (point)))))
              (if prefix
                  (setq prefix (or (fill-common-string-prefix line prefix)
                                   ""))
                (setq prefix line))))
          (forward-line 1))
        (unless prefix (setq prefix ""))
        (poporg-insert-without-prefix buf prefix (point-min) (point-max))
        (list (set-marker (make-marker) start)
              (set-marker (make-marker) end)
              prefix)))))

(defun poporg-insert-region-lines (buf start end prefix overlay)
  "Insert the contents of BUF into the current buffer.

Replace the region between START and END and prepend PREFIX onto each line.
This simply runs `poporg-insert-with-prefix'.  Move OVERLAY to the
newly-inserted region."
  ;; don't have to do anything special
  (poporg-insert-with-prefix buf start end prefix)
  (move-overlay overlay start (point)))

;; ** make text mode buffer

(defun poporg-edit-thing (start end type)
  "Edit the region from START to END in an empty buffer.

Use the function `poporg-get-TYPE-lines' associated to TYPE to extract the
region.  Install the protection overlay on the extracted region.  If there is an
active editing overlay overlapping the region from START to END, pop to its edit
buffer instead."
  (unless (poporg-check-already-edited start end)
    (let* ((edit-buffer (poporg-make-buffer))
           (f-c fill-column)
           (poporg-orig-point (point))
           (poporg-new-point 1)
           (inserter (intern (concat "poporg-get-" (symbol-name type) "-lines")))
           (reg    (funcall inserter edit-buffer start end))
           (start  (nth 0 reg))
           (end    (nth 1 reg))
           (prefix (nth 2 reg))
           (overlay (make-overlay start end)))
      (setq poporg-pre-window-configuration (current-window-configuration))
      ;; Dim and protect the original text.
      (overlay-put overlay 'face 'poporg-edited-face)
      (overlay-put overlay 'intangible t)
      (overlay-put overlay 'read-only t)
      ;; Initialize a popup edit buffer.
      (pop-to-buffer edit-buffer)
      (goto-char poporg-new-point)
      ;; Don't allow undoing the initial buffer insertions.
      (buffer-disable-undo)
      (buffer-enable-undo)
      ;; Save buffer contents to a temporary file so the undo command knows
      ;; whether the contents have modified or not.  This could potentially have
      ;; other uses later on.
      (let ((buf-name (buffer-name)))
        (set-visited-file-name (make-temp-file "poporg-"))
        (rename-buffer buf-name t))
      (let ((require-final-newline nil)) (save-buffer))
      ;; This is mainly to hide the `save-buffer' message
      (message
       (substitute-command-keys
        "poporg: type \\<poporg-mode-map>\\[poporg-edit-exit] when done"))
      ;;(set-buffer-modified-p nil)
      ;; Save data and possibly activate hooks.
      (unless poporg-data
        (push 'poporg-kill-buffer-query kill-buffer-query-functions)
        (add-hook 'kill-buffer-hook 'poporg-kill-buffer-routine))
      (push (list edit-buffer overlay prefix type) poporg-data)
      (overlay-put overlay 'poporg-overlay (car poporg-data))
      ;; All set up for editing.
      (with-demoted-errors "Edit hook error: %S" (run-hooks 'poporg-edit-hook))
      (poporg-mode +1)
      ;; Adjust fill column after running the hooks and setting the mode since
      ;; org-mode sets the fill column.
      (when poporg-adjust-fill-column
        (setq fill-column (max 0 (- f-c (length prefix))))))))

;; ** buffer kill hook functions

(defun poporg-kill-buffer-query ()
  "Warn when killing an edit buffer or a source buffer with active edit buffers."
  (let ((entry (assq (current-buffer) poporg-data)))
    (if entry
        (or (not (buffer-modified-p))
            (yes-or-no-p "Really abandon this edit? "))
      (let ((data poporg-data)
            (value t))
        (while data
          (let ((buffer (overlay-buffer (cadar data))))
            (if (not (eq buffer (current-buffer)))
                (setq data (cdr data))
              (pop-to-buffer (caar data))
              (message "First, either complete or kill this edit.")
              (setq data nil
                    value nil))))
        value))))

(defun poporg-kill-buffer-routine ()
  "Cleanup an edit buffer whenever killed."
  ;; Delete the temporary file
  (let ((entry (assq (current-buffer) poporg-data)))
    (when entry
      (let* ((overlay (cadr entry))
             (buffer (overlay-buffer overlay)))
        (when buffer
          (ignore-errors (set-buffer-modified-p nil)
                         (delete-file (buffer-file-name)))
          (delete-overlay overlay)
          (setq poporg-data (delq entry poporg-data))
          (unless poporg-data
            (setq kill-buffer-query-functions
                  (delq 'poporg-kill-buffer-query kill-buffer-query-functions))
            (remove-hook 'kill-buffer-hook 'poporg-kill-buffer-routine))
          ;; switch back if we're killing the buffer in the selected window
          (when (equal (current-buffer) (window-buffer))
            (unless (one-window-p) (delete-window))
            (switch-to-buffer buffer)))))))

;; * Commands

;;;###autoload
(defun poporg-dwim ()
  "Single overall command for poporg (a single keybinding may do it all).

If the current buffer is an edit buffer, run `poporg-edit-exit'.

If the region is active, edit it in an empty buffer.  Otherwise, find a nearby
string or comment using `poporg-find-string-or-comment' and edit that in an
empty buffer.  If there is an active edit nearby, pop to its other buffer and
edit that instead."
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (cond
     ((assq (current-buffer) poporg-data) (poporg-edit-exit))
     ((use-region-p)
      (poporg-edit-thing (region-beginning) (region-end) 'region))
     (t
      (let ((reg (poporg-find-string-or-comment)))
        (cond
         ((eq (car reg) 'string)
          (poporg-edit-thing (nth 1 reg) (nth 2 reg) 'string))
         ((eq (car reg) 'comment)
          (poporg-edit-thing (nth 1 reg) (nth 2 reg) 'comment))
         (t
          (user-error "Nothing to edit!"))))))))

;;;###autoload
(defun poporg-update (with-save)
  "Update the contents of the original buffer.

If prefix argument WITH-SAVE is non-nil, save the original buffer too.

Also update the overlay."
  (interactive "P")
  (let* ((edit-buffer (current-buffer))
         (entry (assq edit-buffer poporg-data))
         (overlay (cadr entry))
         (buffer (when overlay (overlay-buffer overlay)))
         (prefix (caddr entry))
         (type (nth 3 entry))
         (poporg-orig-point (point))
         (inserter (intern (concat "poporg-insert-"
                                   (symbol-name type) "-lines"))))
    (unless buffer
      (error "Not an edit buffer or original buffer vanished"))
    (when (buffer-modified-p)
      ;; Move everything back in place.
      ;; Allow the inserter to edit the region.
      (overlay-put overlay 'intangible nil)
      (overlay-put overlay 'read-only nil)
      (let* ((start (overlay-start overlay))
             (end (overlay-end overlay)))
        (with-current-buffer buffer
          ;; This updates the overlay
          (funcall inserter edit-buffer start end prefix overlay))
        ;; This is only used to mark the buffer as saved at this tamestamp, so
        ;; undo knows at what stage the buffer is unmodified
        (let ((require-final-newline nil)) (save-buffer))
        ;; This is manily to hide the `save-buffer' message
        (message "poporg: original buffer updated"))
      (overlay-put overlay 'intangible t)
      (overlay-put overlay 'read-only t))
    (with-current-buffer buffer (undo-boundary))
    (when with-save (with-current-buffer buffer (save-buffer)))))

;;;###autoload
(defun poporg-update-and-save ()
  "Update and save the original buffer; update the region."
  (interactive)
  (poporg-update t))

;;;###autoload
(defun poporg-edit-exit ()
  "Exit the edit buffer, replacing the original region."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (entry (assq edit-buffer poporg-data))
         (overlay (cadr entry))
         (buffer (when overlay (overlay-buffer overlay)))
         poporg-new-point)
    (unless buffer
      (error "Not an edit buffer or original buffer vanished"))
    (poporg-update nil)
    (with-demoted-errors "Edit hook error: %S"
      (run-hooks 'poporg-edit-exit-hook))
    ;; Killing the buffer triggers a cleanup through the kill hook.
    (kill-buffer edit-buffer)
    (set-window-configuration poporg-pre-window-configuration)
    (with-current-buffer buffer
      (let ((inhibit-point-motion-hooks t))
        (when poporg-new-point ; unset if unmodified or aborted
          (goto-char poporg-new-point))))))

;; ** mode

(define-minor-mode poporg-mode
  "Install keybindings for a poporg edit buffer."
  nil " pop" poporg-mode-map)


(provide 'poporg)

;; Local Variables:
;;   coding: utf-8
;; End:

;;; poporg.el ends here
