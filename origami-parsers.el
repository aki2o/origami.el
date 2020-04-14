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
              (offset (line) (aref line 3))
              (collapse-same-level (lines)
                                   (->>
                                    (cdr lines)
                                    (-reduce-from (lambda (acc line)
                                                    (cond ((and (eq line 'newline) (eq (car acc) 'newline)) acc)
                                                          ((= (indent line) (indent (car acc)))
                                                           (cons (vector (indent (car acc))
                                                                         (beg (car acc))
                                                                         (end line)
                                                                         (offset (car acc)))
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
                                                         (cons (origami-create-fold-node
                                                                (beg (car nodes))
                                                                this-end
                                                                (offset (car nodes))
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

(defun origami-build-pair-tree (open-regexp close-regexp positions)
  (cl-labels ((build (positions)
                     ;; this is so horrible, but fast
                     (let (acc beg (should-continue t))
                       (while (and should-continue positions)
                         (cond ((string-match open-regexp (caar positions))
                                (if beg ;go down a level
                                    (let* ((res (build positions))
                                           (new-pos (car res))
                                           (children (cdr res)))
                                      (setq positions (cdr new-pos))
                                      (setq acc (cons (origami-create-fold-node beg (cdar new-pos) (length (caar positions)) children)
                                                      acc))
                                      (setq beg nil))
                                  ;; begin a new pair
                                  (setq beg (cdar positions))
                                  (setq positions (cdr positions))))
                               ((string-match close-regexp (caar positions))
                                (if beg
                                    (progn ;close with no children
                                      (setq acc (cons (origami-create-fold-node beg (cdar positions) (length (caar positions)) nil)
                                                      acc))
                                      (setq positions (cdr positions))
                                      (setq beg nil))
                                  (setq should-continue nil)))))
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
                                                               '(font-lock-doc-face font-lock-comment-face font-lock-string-face)))))))
    (origami-build-pair-tree "{" "}" positions)))

(defun origami-c-macro-parser (content)
  (let ((positions (origami-get-positions content "#if\\|#endif")))
    (origami-build-pair-tree "#if" "#endif" positions)))

;;;###autoload
(defun origami-c-parser (content)
  (origami-fold-children
   (origami-fold-shallow-merge
    (origami-fold-root-node (origami-c-style-parser content))
    (origami-fold-root-node (origami-c-macro-parser content)))))

;;;###autoload
(defun origami-java-parser (content)
  (origami-fold-children
   (origami-fold-shallow-merge
    (origami-fold-root-node (origami-c-style-parser content))
    (origami-fold-root-node (origami-javadoc-parser content)))))

(defun origami-python-subparser (beg end)
  "find all fold block between beg and end."
  (goto-char beg)
  (let (acc)
	;; iterate all same level children.
	(while (and (beginning-of-defun -1) (<= (point) end)) ;; have children between beg and end?
	  (let* ((new-beg (point))
		     (new-offset (progn (search-forward-regexp ":" nil t) (- (point) new-beg)))
		     (new-end (progn (end-of-defun) (point))))
	    (setq acc (cons (origami-create-fold-node new-beg new-end new-offset
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

(defun origami-lisp-parser (content regex)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (beginning-of-defun -1)
    (let (beg end offset acc)
      (while (< (point) (point-max))
        (setq beg (point))
        (search-forward-regexp regex nil t)
        (setq offset (- (point) beg))
        (end-of-defun)
        (backward-char)      ;move point to one after the last paren
        (setq end (1- (point))) ;don't include the last paren in the fold
        (when (> offset 0)
          (setq acc (cons (origami-create-fold-node beg end offset nil) acc)))
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
