;;; origami-parsers.el --- Collection of parsers  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: parsers
;; URL: https://github.com/gregsexton/

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;;; Code:
(require 'origami)
(require 'dash)
(require 's)
(require 'cl-lib)

(defun origami-get-positions (content regex)
  "Returns a list of positions where REGEX matches in CONTENT. A
position is a cons cell of the character and the numerical
position from first group or entire part of REGEXP."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward regex nil t)
        (let ((string (or (match-string 1)
                          (match-string 0)))
              (point (or (match-beginning 1)
                         (match-beginning 0))))
          (setq acc (cons (cons string point)
                          acc))))
      (reverse acc))))

(defun origami-has-any-face? (s faces)
  (let ((face (get-text-property 0 'face s)))
    (-any? (lambda (f) (memq f faces))
           (if (listp face) face (list face)))))

;;;###autoload
(defun origami-indent-parser (content)
  (cl-labels ((lines (string) (origami-get-positions string ".*?\r?\n"))
              (annotate-levels (lines)
                               (-map (lambda (line)
                                       ;; TODO: support tabs
                                       (let ((indent (length (car (s-match "^ *" (car line)))))
                                             (beg (cdr line))
                                             (end (+ (cdr line) (length (car line)) -1)))
                                         (if (s-blank? (s-trim (car line)))
                                             'newline ;sentinel representing line break
                                           (vector indent beg end (- end beg)))))
                                     lines))
              (indent (line) (if (eq line 'newline) -1 (aref line 0)))
              (beg (line) (aref line 1))
              (end (line) (aref line 2))
              (fold-beg (line) (aref line 3))
              (collapse-same-level (lines)
                                   (->>
                                    (cdr lines)
                                    (-reduce-from (lambda (acc line)
                                                    (cond ((and (eq line 'newline) (eq (car acc) 'newline)) acc)
                                                          ((= (indent line) (indent (car acc)))
                                                           (cons (vector (indent (car acc))
                                                                         (beg (car acc))
                                                                         (end line)
                                                                         (fold-beg (car acc)))
                                                                 (cdr acc)))
                                                          (t (cons line acc))))
                                                  (list (car lines)))
                                    (remove 'newline)
                                    reverse))
              (create-tree (levels)
                           (if (null levels)
                               levels
                             (let ((curr-indent (indent (car levels))))
                               (->> levels
                                    (-partition-by (lambda (l) (= (indent l) curr-indent)))
                                    (-partition-all 2)
                                    (-mapcat (lambda (x)
                                        ;takes care of multiple identical levels, introduced when there are newlines
                                               (-concat
                                                (-map 'list (butlast (car x)))
                                                (list (cons (-last-item (car x)) (create-tree (cadr x)))))))))))
              (build-nodes (tree)
                           (if (null tree) (cons 0 nil)
                             ;; complexity here is due to having to find the end of the children so that the
                             ;; parent encompasses them
                             (-reduce-r-from (lambda (nodes acc)
                                               (cl-destructuring-bind (children-end . children) (build-nodes (cdr nodes))
                                                 (let ((this-end (max children-end (end (car nodes)))))
                                                   (cons (max this-end (car acc))
                                                         (cons (origami-new-branch-node
                                                                (beg (car nodes))
                                                                this-end
                                                                (fold-beg (car nodes))
                                                                this-end
                                                                children)
                                                               (cdr acc))))))
                                             '(0 . nil)
                                             tree))))
    (-> content
        lines
        annotate-levels
        collapse-same-level
        create-tree
        build-nodes
        cdr)))

(defun origami-build-pair-tree (open-regexp-or-pred close-regexp-or-pred positions)
  (cl-labels ((build (positions)
                     ;; this is so horrible, but fast
                     (let (acc beg open (should-continue t))
                       (while (and should-continue positions)
                         (let ((string (caar positions))
                               (point (cdar positions)))
                           (cond ((if (functionp open-regexp-or-pred)
                                      (funcall open-regexp-or-pred string)
                                    (string-match open-regexp-or-pred string))
                                  (if beg ;go down a level
                                      (let* ((res (build positions))
                                             (new-pos (car res))
                                             (children (cdr res))
                                             (fold-end (cdar new-pos))
                                             (close (caar new-pos))
                                             (end (+ fold-end (length close)))
                                             (fold-beg (+ beg (length open))))
                                        (setq positions (cdr new-pos))
                                        (when (< fold-beg fold-end)
                                          (setq acc (cons (origami-new-branch-node beg end fold-beg fold-end children) acc)))
                                        (setq beg nil)
                                        (setq open nil))
                                    ;; begin a new pair
                                    (setq beg point)
                                    (setq open string)
                                    (setq positions (cdr positions))))
                                 ((if (functionp close-regexp-or-pred)
                                      (funcall close-regexp-or-pred string)
                                    (string-match close-regexp-or-pred string))
                                  (if beg ;close with no children
                                      (let* ((fold-beg (+ beg (length open)))
                                             (fold-end point)
                                             (end (+ fold-end (length string))))
                                        (when (< fold-beg fold-end)
                                          (setq acc (cons (origami-new-branch-node beg end fold-beg fold-end nil) acc)))
                                        (setq positions (cdr positions))
                                        (setq beg nil)
                                        (setq open nil))
                                    (setq should-continue nil))))))
                       (cons positions (reverse acc)))))
    (cdr (build positions))))

;;; TODO: tag these nodes? have ability to manipulate nodes that are
;;; tagged? in a scoped fashion?
(defun origami-javadoc-parser (content)
  (let ((positions (->> (origami-get-positions content "/\\*\\*\\|\\*/")
                        (-filter (lambda (position)
                                   (origami-has-any-face? (car position) '(font-lock-doc-face)))))))
    (origami-build-pair-tree (rx "/**") (rx "*/") positions)))

;;;###autoload
(defun origami-c-style-parser (content)
  (let ((positions (->> (origami-get-positions content "[{}]")
                        (cl-remove-if (lambda (position)
                                        (origami-has-any-face? (car position)
                                                               '(font-lock-doc-face font-lock-comment-face font-lock-string-face))))
                        (-map (lambda (position)
                                (goto-char (cdr position))
                                (if (re-search-backward (rx (or bol ";")) nil t)
                                    (cons (concat (buffer-substring-no-properties (match-end 0) (cdr position))
                                                  (car position))
                                          (match-end 0))
                                  position))))))
    (origami-build-pair-tree (rx "{" eos) "}" positions)))

(defun origami-c-macro-parser (content)
  (let ((positions (origami-get-positions content "#if\\|#endif")))
    (origami-build-pair-tree "#if" "#endif" positions)))

;;;###autoload
(defun origami-c-parser (content)
  (origami-node-children
   (origami-node-shallow-merge
    (origami-new-root-node (origami-c-style-parser content))
    (origami-new-root-node (origami-c-macro-parser content)))))

;;;###autoload
(defun origami-java-parser (content)
  (origami-node-children
   (origami-node-shallow-merge
    (origami-new-root-node (origami-c-style-parser content))
    (origami-new-root-node (origami-javadoc-parser content)))))

(defun origami-python-subparser (beg end)
  "find all fold block between beg and end."
  (goto-char beg)
  (let (acc)
	;; iterate all same level children.
	(while (and (beginning-of-defun -1) (<= (point) end)) ;; have children between beg and end?
	  (let* ((new-beg (if (looking-back (rx (+ (any " \t"))) nil t)
                          (match-beginning 0)
                        (point)))
		     (new-fold-beg (progn (search-forward-regexp ":" nil t) (point)))
		     (new-end (progn (end-of-defun) (point))))
	    (setq acc (cons (origami-new-branch-node new-beg new-end new-fold-beg new-end
				                                 (origami-python-subparser new-beg new-end))
			            acc))
	    (goto-char new-end)))
	acc))

;;;###autoload
(defun origami-python-parser (content)
  (with-temp-buffer
    (insert content)
    (python-mode)
    (origami-python-subparser (point-min) (point-max))))

(defun origami-ruby-block-parser (content)
  (let* ((positions '())
         (end-position-re (rx bos "end" eos))
         (open-position? (lambda (s)
                           (not (string-match end-position-re s))))
         (end-position? (lambda (s)
                          (string-match end-position-re s))))
    (with-temp-buffer
      (insert content)
      (let* ((ignore-faces '(font-lock-doc-face font-lock-comment-face font-lock-string-face))
             (ignore-position? (lambda (position)
                                 (origami-has-any-face? (car position) ignore-faces)))
             (open-re-maker (lambda (&rest words)
                              (rx-to-string `(and (or bol ";") (* (any " \t")) (group (or ,@words)) (or eol space "\\")))))
             (fix-open-prefix (lambda (position &rest end-words)
                                (let ((pt (progn
                                            (goto-char (cdr position))
                                            (if (looking-back (rx (+ (any " \t"))) nil t)
                                                (match-beginning 0)
                                              (point)))))
                                  (re-search-forward (rx-to-string `(or eol ,@end-words)) nil t)
                                  (cons (buffer-substring-no-properties pt (point)) pt))))
             (end-positions (->> (origami-get-positions content (rx (or bol space) (group "end") (or eol space)))
                                 (cl-remove-if ignore-position?)))
             (do-positions (->> (origami-get-positions content (rx (or bol space) (group "do") (or eol space)))
                                (cl-remove-if ignore-position?)
                                (-map (lambda (position)
                                        (goto-char (cdr position))
                                        (if (re-search-backward (rx (or bol ";")) nil t)
                                            (cons (concat (buffer-substring-no-properties (match-end 0) (cdr position))
                                                          (car position))
                                                  (match-end 0))
                                          position)))))
             (loop-positions (->> (origami-get-positions content (funcall open-re-maker "for" "while" "until"))
                                  (cl-remove-if ignore-position?)
                                  (-map (lambda (position) (funcall fix-open-prefix position ";" "do")))))
             (loop-positions (cl-remove-if
                              (lambda (position)
                                ;; A loop syntax accepts expression with `do' like the following code.
                                ;;
                                ;; while (
                                ;;   i += 1
                                ;; ) < limit do
                                ;;   something
                                ;; end
                                ;;
                                ;; Therefore, reject if next `do' has found before `end' or nested loop syntax start.
                                (let ((do-position (or (-first (lambda (x) (> (cdr x) (cdr position))) do-positions) (point-max)))
                                      (end-position (or (-first (lambda (x) (> (cdr x) (cdr position))) end-positions) (point-max)))
                                      (loop-position (or (-first (lambda (x) (> (cdr x) (cdr position))) loop-positions) (point-max))))
                                  (and (< do-position end-position)
                                       (< do-position loop-position))))
                              loop-positions))
             (if-positions (->> (origami-get-positions content (funcall open-re-maker "if" "unless"))
                                (cl-remove-if ignore-position?)
                                (-map (lambda (position) (funcall fix-open-prefix position ";" "then")))))
             (other-positions (->> (origami-get-positions content (funcall open-re-maker "class" "module" "def" "begin" "case"))
                                   (cl-remove-if ignore-position?)
                                   (-map (lambda (position) (funcall fix-open-prefix position ";"))))))
        (setq positions (--sort (< (cdr it) (cdr other))
                                (append end-positions do-positions loop-positions if-positions other-positions)))))
    (origami-build-pair-tree open-position? end-position? positions)))

(defun origami-ruby-paren-parser (content)
  (let* ((ignore-faces '(font-lock-doc-face font-lock-comment-face font-lock-string-face))
         (ignore-position? (lambda (position)
                             (origami-has-any-face? (car position) ignore-faces)))
         (positions (->> (origami-get-positions content (rx (any "{}[]")))
                         (cl-remove-if ignore-position?)
                         (-map (lambda (position)
                                 (goto-char (cdr position))
                                 (if (and (string-equal (car position) "{")
                                          (re-search-backward (rx (or bol ";")) nil t))
                                     (cons (concat (buffer-substring-no-properties (match-end 0) (cdr position))
                                                   (car position))
                                           (match-end 0))
                                   position))))))
    (origami-build-pair-tree (rx (any "{[") eos) (rx (any "}]")) positions)))

;;;###autoload
(defun origami-ruby-parser (content)
  (origami-node-children
   (origami-node-shallow-merge
    (origami-new-root-node (origami-ruby-block-parser content))
    (origami-new-root-node (origami-ruby-paren-parser content)))))

(defun origami-lisp-parser (content regex)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (beginning-of-defun -1)
    (let (beg end fold-beg fold-end acc)
      (while (< (point) (point-max))
        (setq beg (point))
        (search-forward-regexp regex nil t)
        (setq fold-beg (point))
        (end-of-defun)
        (backward-char)      ;move point to one after the last paren
        (setq fold-end (1- (point))) ;don't include the last paren in the fold
        (setq end (point))
        (when (> fold-beg beg)
          (setq acc (cons (origami-new-branch-node beg end fold-beg fold-end nil) acc)))
        (beginning-of-defun -1))
      (reverse acc))))

;;;###autoload
(defun origami-elisp-parser (content)
  (origami-lisp-parser content "(def\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

;;;###autoload
(defun origami-clj-parser (content)
  (origami-lisp-parser content "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(defmacro origami-define-markers-parser (name start-marker end-marker)
  "Create a parser for simple start and end markers."
  (let ((regex (rx-to-string `(or ,start-marker ,end-marker)))
        (name (intern (format "origami-%s-markers-parser" name))))
    `(defun ,name (content)
       (let ((positions (origami-get-positions content ,regex)))
         (origami-build-pair-tree ,start-marker ,end-marker positions)))))

;;;###autoload
(defun origami-vim-like-markers-parser (_content))
(with-no-warnings
  (origami-define-markers-parser "vim-like" "{{{" "}}}"))

(provide 'origami-parsers)
;;; origami-parsers.el ends here
