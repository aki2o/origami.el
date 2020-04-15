;;; origami.el --- Flexible text folding  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: folding
;; URL: https://github.com/gregsexton/origami.el
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (emacs "24") (cl-lib "0.5"))

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

(require 'dash)
(require 's)
(require 'cl-lib)

;;; fold display mode and faces

(defgroup origami nil
  "Flexible text folding"
  :prefix "origami-"
  :group 'convenience)

(defcustom origami-parser-alist
  '((java-mode             . origami-java-parser)
    (c-mode                . origami-c-parser)
    (c++-mode              . origami-c-style-parser)
    (perl-mode             . origami-c-style-parser)
    (cperl-mode            . origami-c-style-parser)
    (js-mode               . origami-c-style-parser)
    (js2-mode              . origami-c-style-parser)
    (js3-mode              . origami-c-style-parser)
    (go-mode               . origami-c-style-parser)
    (php-mode              . origami-c-style-parser)
    (python-mode           . origami-python-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (clojure-mode          . origami-clj-parser)
    (triple-braces         . origami-vim-like-markers-parser))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

(defcustom origami-fold-replacement "..."
  ;; TODO: this should also be specifiable as a function: folded text -> string
  "Show this string instead of the folded text."
  :type 'string
  :group 'origami)

(defcustom origami-show-fold-header nil
  "Highlight the line the fold start on."
  :type 'boolean
  :group 'origami)

(defface origami-fold-header-face
  `((t (:box (:line-width 1 :color ,(face-attribute 'highlight :background))
             :background ,(face-attribute 'highlight :background))))
  "Face used to display fold headers.")

(defface origami-fold-fringe-face
  '((t ()))
  "Face used to display fringe contents.")

(defface origami-fold-replacement-face
  '((t :inherit 'font-lock-comment-face))
  "Face used to display the fold replacement text.")

;;; overlay manipulation

(defun origami-header-overlay-range (fold-overlay)
  "Given a `fold-overlay', return the range that the corresponding
header overlay should cover. Result is a cons cell of (begin . end)."
  (with-current-buffer (overlay-buffer fold-overlay)
    (let ((fold-begin
           (save-excursion
             (goto-char (overlay-start fold-overlay))
             (line-beginning-position)))
          (fold-end
           ;; Find the end of the folded region -- include the following
           ;; newline if possible. The header will span the entire fold.
           (save-excursion
             (goto-char (overlay-end fold-overlay))
             (when (looking-at ".")
               (forward-char 1)
               (when (looking-at "\n")
                 (forward-char 1)))
             (point))))
      (cons fold-begin fold-end))))

(defun origami-header-overlay-reset-position (header-overlay)
  (-when-let (fold-ov (overlay-get header-overlay 'fold-overlay))
    (let ((range (origami-header-overlay-range fold-ov)))
      (move-overlay header-overlay (car range) (cdr range)))))

(defun origami-header-modify-hook (header-overlay after-p _b _e &optional _l)
  (if after-p (origami-header-overlay-reset-position header-overlay)))

(defun origami-create-overlay (beg end offset buffer)
  (when (> (- end beg) 0)
    (let ((ov (make-overlay (+ beg offset) end buffer)))
      (overlay-put ov 'creator 'origami)
      (overlay-put ov 'isearch-open-invisible 'origami-isearch-show)
      (overlay-put ov 'isearch-open-invisible-temporary
                   (lambda (ov hide-p) (if hide-p (origami-hide-overlay ov)
                                         (origami-show-overlay ov))))
      ;; We create a header overlay even when disabled; this could be avoided,
      ;; especially if we called origami-reset for each buffer if customizations
      ;; changed.
      (let* ((range (origami-header-overlay-range ov))
             (header-ov (make-overlay (car range) (cdr range) buffer
                                      nil))) ;; no front advance
        (overlay-put header-ov 'creator 'origami)
        (overlay-put header-ov 'fold-overlay ov)
        (overlay-put header-ov 'modification-hooks '(origami-header-modify-hook))
        (overlay-put ov 'header-ov header-ov))
      ov)))

(defun origami-hide-overlay (ov)
  (overlay-put ov 'invisible 'origami)
  (overlay-put ov 'display origami-fold-replacement)
  (overlay-put ov 'face 'origami-fold-replacement-face)
  (if origami-show-fold-header
      (origami-activate-header (overlay-get ov 'header-ov))))

(defun origami-show-overlay (ov)
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'display nil)
  (overlay-put ov 'face nil)
  (origami-deactivate-header (overlay-get ov 'header-ov)))

(defun origami-hide-node-overlay (node)
  (-when-let (ov (origami-node-data node))
    (origami-hide-overlay ov)))

(defun origami-show-node-overlay (node)
  (-when-let (ov (origami-node-data node))
    (origami-show-overlay ov)))

(defun origami-activate-header (ov)
  ;; Reposition the header overlay. Since it extends before the folded area, it
  ;; may no longer cover the appropriate locations.
  (origami-header-overlay-reset-position ov)
  (overlay-put ov 'origami-header-active t)
  (overlay-put ov 'face 'origami-fold-header-face)
  (overlay-put ov 'before-string
               (propertize
                "…"
                'display
                '(left-fringe empty-line origami-fold-fringe-face))))

(defun origami-deactivate-header (ov)
  (overlay-put ov 'origami-header-active nil)
  (overlay-put ov 'face nil)
  (overlay-put ov 'before-string nil)
  (overlay-put ov 'after-string nil))

(defun origami-isearch-show (_ov)
  (origami-show-node (current-buffer) (point)))

(defun origami-hide-overlay-from-fold-tree-fn (node)
  (origami-node-postorder-each node 'origami-hide-node-overlay))

(defun origami-show-overlay-from-fold-tree-fn (node)
  (origami-node-postorder-each node 'origami-show-node-overlay))

(defun origami-change-overlay-from-fold-node-fn (old new)
  (if (origami-node-open? new)
      (origami-show-node-overlay old)
    (origami-hide-node-overlay new)))

(defun origami-remove-all-overlays (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'creator 'origami)))

;;; fold structure

(defun origami-new-node (beg end offset open &optional children data)
  (let ((sorted-children (-sort (lambda (a b)
                                  (or (< (origami-node-beg a) (origami-node-beg b))
                                      (and (= (origami-node-beg a) (origami-node-beg b))
                                           (< (origami-node-end a) (origami-node-end b)))))
                                (remove nil children))))
    ;; ensure invariant: no children overlap
    (when (-some? (lambda (pair)
                    (let ((a (car pair))
                          (b (cadr pair)))
                      (when b ;for the odd numbered case - there may be a single item
                        ;; the < function doesn't support varargs
                        (or (>= (origami-node-beg a) (origami-node-end a))
                            (>= (origami-node-end a) (origami-node-beg b))
                            (>= (origami-node-beg b) (origami-node-end b))))))
                  (-partition-all-in-steps 2 1 sorted-children))
      (error "Tried to construct a node where the children overlap or are not distinct regions: %s"
             sorted-children))
    ;; ensure invariant: parent encompases children
    (let ((beg-children (origami-node-beg (car sorted-children)))
          (end-children (origami-node-end (-last-item sorted-children))))
      (if (and beg-children (or (> beg beg-children) (< end end-children)))
          (error "Node does not overlap children in range. beg=%s end=%s beg-children=%s end-children=%s"
                 beg end beg-children end-children)
        (if (> (+ beg offset) end)
            (error "Offset is not within the range of the node: beg=%s end=%s offset=%s" beg end offset)
          (vector beg end offset open sorted-children data))))))

(defun origami-new-root-node (&optional children)
  "Create a root container node."
  (origami-new-node 1 most-positive-fixnum 0 t children 'root))

(defun origami-new-branch-node (beg end offset children)
  (let* ((buffer (current-buffer))
         (cached-tree (origami-get-cached-tree buffer))
         (previous-fold (-last-item (origami-node-find-path-with-range cached-tree beg end))))
    (origami-new-node beg end offset
                      (if previous-fold (origami-node-open? previous-fold) t)
                      children
                      (or (-> (origami-node-find-path-with-range
                               (origami-get-cached-tree buffer) beg end)
                              -last-item
                              origami-node-data)
                          (origami-create-overlay beg end offset buffer)))))

(defun origami-node-is-root? (node) (eq (origami-node-data node) 'root))

(defun origami-node-beg (node)
  (when node
    (if (origami-node-is-root? node)
        (aref node 0)
      (- (overlay-start (origami-node-data node)) (origami-node-offset node)))))

(defun origami-node-end (node)
  (when node
    (if (origami-node-is-root? node)
        (aref node 1)
      (overlay-end (origami-node-data node)))))

(defun origami-node-offset (node) (when node (aref node 2)))

(defun origami-node-open? (node) (when node (aref node 3)))

(defun origami-node-open-set (node value)
  (when node
    (if (origami-node-is-root? node)
        node
      (origami-new-node (origami-node-beg node)
                        (origami-node-end node)
                        (origami-node-offset node)
                        value
                        (origami-node-children node)
                        (origami-node-data node)))))

(defun origami-node-children (node) (when node (aref node 4)))

(defun origami-node-children-set (node children)
  (when node
    (origami-new-node (origami-node-beg node)
                      (origami-node-end node)
                      (origami-node-offset node)
                      (origami-node-open? node)
                      children
                      (origami-node-data node))))

(defun origami-node-data (node) (when node (aref node 5)))

;;; fold structure utils

(defun origami-node-range-equal (a b)
  (and (equal (origami-node-beg a) (origami-node-beg b))
       (equal (origami-node-end a) (origami-node-end b))))

(defun origami-node-state-equal (a b)
  (equal (origami-node-open? a) (origami-node-open? b)))

(defun origami-node-add-child (node new)
  (origami-node-children-set node
                             (cons new (origami-node-children node))))

(defun origami-node-replace-child (node old new)
  (origami-node-children-set node
                             (cons new (remove old (origami-node-children node)))))

(defun origami-node-assoc (path f)
  "Rewrite the tree, replacing the node referenced by PATH with
F applied to the leaf."
  (cdr
   (-reduce-r-from (lambda (node acc)
                     (cl-destructuring-bind (old-node . new-node) acc
                       (cons node (origami-node-replace-child node old-node new-node))))
                   (let ((leaf (-last-item path))) (cons leaf (funcall f leaf)))
                   (butlast path))))

(defun origami-node-diff (old new on-add on-remove on-change)
  (cl-labels ((diff-children (old-children new-children)
                             (let ((old (car old-children))
                                   (new (car new-children)))
                               (cond ((null old) (-each new-children on-add))
                                     ((null new) (-each old-children on-remove))
                                     ((and (null old) (null new)) nil)
                                     ((origami-node-range-equal old new)
                                      (origami-node-diff old new on-add on-remove on-change)
                                      (diff-children (cdr old-children) (cdr new-children)))
                                     ((<= (origami-node-beg old) (origami-node-beg new))
                                      (funcall on-remove old)
                                      (diff-children (cdr old-children) new-children))
                                     (t (funcall on-add new)
                                        (diff-children old-children (cdr new-children)))))))
    (unless (origami-node-range-equal old new)
      (error "Precondition invalid: old must have the same range as new."))
    (unless (origami-node-state-equal old new)
      (funcall on-change old new))
    (diff-children (origami-node-children old)
                   (origami-node-children new))))

(defun origami-node-postorder-each (node f)
  (-each (origami-node-children node) f)
  (funcall f node))

(defun origami-node-map (f tree)
  "Map F over the tree. Replacing each node with the result of (f
node). The children cannot be manipulated using f as the map will
replace them. This cannot change the structure of the tree, just
the state of each node."
  (origami-node-children-set
   (funcall f tree)
   (-map (lambda (node) (origami-node-map f node))
         (origami-node-children tree))))

(defun origami-node-path-map (f path)
  "Map F over the nodes in path. As with `origami-node-map',
children cannot be manipulated."
  (cond ((null path) nil)
        ((cdr path) (funcall f (origami-node-replace-child (car path)
                                                           (cadr path)
                                                           (origami-node-path-map f (cdr path)))))
        (t (funcall f (car path)))))

(defun origami-node-find-deepest (tree pred)
  (when tree
    (when (funcall pred tree)
      (-if-let (child (-first pred (origami-node-children tree)))
          (cons tree (origami-node-find-deepest child pred))
        (list tree)))))

(defun origami-node-find-path-containing-range (tree beg end)
  (origami-node-find-deepest tree
                             (lambda (node)
                               (and (>= beg (origami-node-beg node))
                                    (<= end (origami-node-end node))))))

(defun origami-node-find-path-with-range (tree beg end)
  "Return the path to the most specific (deepest) node that has
exactly the range BEG-END, or null."
  (-when-let (path (origami-node-find-path-containing-range tree beg end))
    (let ((last (-last-item path)))
      (when (and (= beg (origami-node-beg last))
                 (= end (origami-node-end last)))
        path))))

(defun origami-node-find-path-containing (tree point)
  "Return the path to the most specific (deepest) node that
contains point, or null."
  (origami-node-find-deepest tree
                             (lambda (node)
                               (and (<= (origami-node-beg node) point)
                                    (>= (origami-node-end node) point)))))

(defun origami-node-preorder-reduce (tree f initial-state)
  "Reduce the tree by doing a preorder traversal. F is applied
with the current state and the current node at each iteration."
  (-reduce-from (lambda (state node) (origami-node-preorder-reduce node f state))
                (funcall f initial-state tree)
                (origami-node-children tree)))

(defun origami-node-postorder-reduce (tree f initial-state)
  "Reduce the tree by doing a postorder traversal. F is applied
with the current state and the current node at each iteration."
  (funcall f (-reduce-from (lambda (state node) (origami-node-postorder-reduce node f state))
                           initial-state
                           (origami-node-children tree))
           tree))

(defun origami-node-recursively-closed? (node)
  (origami-node-postorder-reduce node (lambda (acc node)
                                        (and acc (not (origami-node-open? node)))) t))

(defun origami-node-recursively-open? (node)
  (origami-node-postorder-reduce node (lambda (acc node)
                                        (and acc (origami-node-open? node))) t))

(defun origami-node-shallow-merge (tree1 tree2)
  "Shallow merge the children of TREE2 in to TREE1."
  (-reduce-from (lambda (tree node)
                  (origami-node-assoc (origami-node-find-path-containing-range tree
                                                                               (origami-node-beg node)
                                                                               (origami-node-end node))
                                      (lambda (leaf)
                                        (origami-node-add-child leaf node))))
                tree1 (origami-node-children tree2)))

(defun origami-node-parent (path)
  (-last-item (-butlast path)))

(defun origami-node-prev-sibling (siblings node)
  (->> siblings
       (-partition-in-steps 2 1)
       (-drop-while (lambda (pair) (not (equal (cadr pair) node))))
       caar))

(defun origami-node-next-sibling (siblings node)
  (->> siblings
       (-drop-while (lambda (n) (not (equal n node))))
       cadr))

;;; linear history structure

(defun origami-h-new (present)
  "Create a new history structure."
  (vector nil present nil))

(defun origami-h-push (h new)
  "Create a new history structure with new as the present value."
  (when new
    (let ((past (aref h 0))
          (present (aref h 1)))
      (vector (cons present (-take 19 past)) new nil))))

(defun origami-h-undo (h)
  (let ((past (aref h 0))
        (present (aref h 1))
        (future (aref h 2)))
    (if (null past) h
      (vector (cdr past) (car past) (cons present future)))))

(defun origami-h-redo (h)
  (let ((past (aref h 0))
        (present (aref h 1))
        (future (aref h 2)))
    (if (null future) h
      (vector (cons present past) (car future) (cdr future)))))

(defun origami-h-present (h)
  (when h (aref h 1)))

;;; minor mode

(defvar origami-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `origami-mode'.")

(defcustom origami-mode-hook nil
  "Hook called when origami minor mode is activated or deactivated."
  :type 'hook
  :group 'origami)

(defun origami-find-occurrence-show-node ()
  (call-interactively 'origami-show-node))

;;;###autoload
(define-minor-mode origami-mode
  "Minor mode to selectively hide/show text in the current buffer.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Lastly, the normal hook `origami-mode-hook' is run using
`run-hooks'.

Key bindings:
\\{origami-mode-map}"
  :group 'origami
  :lighter nil
  :keymap origami-mode-map
  :init-value nil
  (if origami-mode
      (progn
        (add-hook 'occur-mode-find-occurrence-hook
                  'origami-find-occurrence-show-node nil t)
        (setq next-error-move-function (lambda (ignored pos)
                                         (goto-char pos)
                                         (call-interactively 'origami-show-node)))
        (add-hook 'clone-indirect-buffer-hook
                  (lambda () (origami-reset (current-buffer)))))
    (remove-hook 'occur-mode-find-occurrence-hook
                 'origami-find-occurrence-show-node t)
    (setq next-error-move-function nil))
  (origami-reset (current-buffer)))

;;;###autoload
(define-global-minor-mode global-origami-mode origami-mode
  (lambda () (origami-mode 1)))

;;; interactive utils

(defvar origami-history nil)
(defvar origami-tree-tick nil)

(defun origami-setup-local-vars (buffer)
  (with-current-buffer buffer
    (set (make-local-variable 'origami-history)
         (origami-h-new (origami-new-root-node)))
    (set (make-local-variable 'origami-tree-tick) 0)))

(defun origami-get-cached-tree (buffer)
  (or (local-variable-p 'origami-history buffer)
      (error "Necessary local variables were not available"))
  (origami-h-present (buffer-local-value 'origami-history buffer)))

(defun origami-store-cached-tree (buffer tree)
  (or (and (local-variable-p 'origami-history buffer)
           (local-variable-p 'origami-tree-tick buffer))
      (error "Necessary local variables were not available"))
  (with-current-buffer buffer
    (setq origami-tree-tick (buffer-modified-tick))
    (setq origami-history (origami-h-push origami-history tree)))
  tree)

(defun origami-update-history (buffer f)
  (or (local-variable-p 'origami-history buffer)
      (error "Necessary local variables were not available"))
  (with-current-buffer buffer
    (setq origami-history (funcall f origami-history))))

(defun origami-rebuild-tree? (buffer)
  "Determines if the tree needs to be rebuilt for BUFFER since it
was last built."
  (not (= (buffer-local-value 'origami-tree-tick buffer)
          (buffer-modified-tick buffer))))

(defun origami-build-tree (buffer parser)
  (when parser
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
        (-> parser
            (funcall contents)
            origami-new-root-node)))))

(defvar origami-fold-style nil)
(make-variable-buffer-local 'origami-fold-style)

(defun origami-get-parser (buffer)
  (let ((parser-key (or (buffer-local-value 'origami-fold-style buffer)
                        (buffer-local-value 'major-mode buffer))))
    (or (assoc-default parser-key origami-parser-alist)
        'origami-indent-parser)))

(defun origami-get-fold-tree (buffer)
  "Facade. Build the tree if it hasn't already been built
otherwise fetch cached tree."
  (when origami-mode
    (if (origami-rebuild-tree? buffer)
        (origami-build-tree buffer (origami-get-parser buffer))
      (origami-get-cached-tree buffer))))

(defun origami-apply-new-tree (old-tree new-tree)
  (when new-tree
    (origami-node-diff old-tree new-tree
                       'origami-hide-overlay-from-fold-tree-fn
                       'origami-show-overlay-from-fold-tree-fn
                       'origami-change-overlay-from-fold-node-fn)))

(defun origami-search-forward-for-path (buffer point)
  (let (end)
    (with-current-buffer buffer
      (save-excursion
        (goto-char point)
        (setq end (line-end-position))))
    (-when-let (tree (origami-get-fold-tree buffer))
      (-when-let (path (origami-node-find-path-containing tree point))
        (let ((forward-node (-first (lambda (node)
                                      (and (>= (origami-node-beg node) point)
                                           (<= (origami-node-beg node) end)))
                                    (origami-node-children (-last-item path)))))
          (if forward-node (append path (list forward-node)) path))))))

;;; commands

(defun origami-open-node (buffer point)
  "Open the fold node at POINT in BUFFER. The fold node opened
will be the deepest nested at POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree tree (origami-store-cached-tree
                                    buffer
                                    (origami-node-assoc path (lambda (node)
                                                               (origami-node-open-set node t))))))))

(defun origami-open-node-recursively (buffer point)
  "Open the fold node and all of its children at POINT in BUFFER.
The fold node opened will be the deepest nested at POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree
       tree (origami-store-cached-tree
             buffer
             (origami-node-assoc path
                                 (lambda (node)
                                   (origami-node-map (lambda (node)
                                                       (origami-node-open-set node t))
                                                     node))))))))

(defun origami-show-node (buffer point)
  "Like `origami-open-node' but also opens parent fold nodes
recursively so as to ensure the position where POINT is is
visible."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree tree (origami-store-cached-tree
                                    buffer
                                    (origami-node-path-map
                                     (lambda (node)
                                       (origami-node-open-set node t))
                                     path))))))

(defun origami-close-node (buffer point)
  "Close the fold node at POINT in BUFFER. The fold node closed
will be the deepest nested at POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree tree (origami-store-cached-tree
                                    buffer
                                    (origami-node-assoc
                                     path (lambda (node)
                                            (origami-node-open-set node nil))))))))

(defun origami-close-node-recursively (buffer point)
  "Close the fold node and all of its children at POINT in BUFFER.
The fold node closed will be the deepest nested at POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree
       tree (origami-store-cached-tree
             buffer
             (origami-node-assoc path
                                 (lambda (node)
                                   (origami-node-map (lambda (node)
                                                       (origami-node-open-set node nil))
                                                     node))))))))

(defun origami-toggle-node (buffer point)
  "Toggle the fold node at POINT in BUFFER open or closed. The
fold node opened or closed will be the deepest nested at POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (origami-apply-new-tree tree (origami-store-cached-tree
                                    buffer
                                    (origami-node-assoc
                                     path (lambda (node)
                                            (origami-node-open-set
                                             node (not (origami-node-open?
                                                        (-last-item path)))))))))))

(defun origami-forward-toggle-node (buffer point)
  "Like `origami-toggle-node' but search forward in BUFFER for a
fold node. If a fold node is found after POINT and before the
next line break, this will be toggled. Otherwise, behave exactly
as `origami-toggle-node'."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-search-forward-for-path buffer point))
      (origami-apply-new-tree tree (origami-store-cached-tree
                                    buffer
                                    (origami-node-assoc
                                     path (lambda (node)
                                            (origami-node-open-set
                                             node (not (origami-node-open?
                                                        (-last-item path)))))))))))

(defun origami-recursively-toggle-node (buffer point)
  "Cycle a fold node between recursively closed, open and
recursively open depending on its current state. The fold node
acted upon is searched for forward in BUFFER from POINT. If a
fold node is found after POINT and before the next line break,
this will be toggled otherwise the fold node nested deepest at
POINT will be acted upon.

This command will only work if bound to a key. For those familiar
with org-mode heading opening and collapsing, this will feel
familiar. It's easiest to grasp this just by giving it a go."
  (interactive (list (current-buffer) (point)))
  (-when-let (path (origami-search-forward-for-path buffer point))
    (let ((node (-last-item path)))
      (if (eq last-command 'origami-recursively-toggle-node)
          (cond ((origami-node-recursively-open? node)
                 (origami-close-node-recursively buffer (origami-node-beg node)))
                ((origami-node-recursively-closed? node)
                 (origami-toggle-node buffer (origami-node-beg node)))
                (t (origami-open-node-recursively buffer (origami-node-beg node))))
        (origami-forward-toggle-node buffer point)))))

(defun origami-open-all-nodes (buffer)
  "Recursively open every fold node in BUFFER."
  (interactive (list (current-buffer)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (origami-apply-new-tree tree (origami-store-cached-tree
                                  buffer
                                  (origami-node-map
                                   (lambda (node)
                                     (origami-node-open-set node t))
                                   tree)))))

(defun origami-close-all-nodes (buffer)
  "Recursively close every fold node in BUFFER."
  (interactive (list (current-buffer)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (origami-apply-new-tree tree (origami-store-cached-tree
                                  buffer
                                  (origami-node-map
                                   (lambda (node)
                                     (origami-node-open-set node nil))
                                   tree)))))

(defun origami-toggle-all-nodes (buffer)
  "Toggle all fold nodes in the buffer recursively open or
recursively closed."
  (interactive (list (current-buffer)))
  (-when-let (tree (origami-get-fold-tree buffer))
    ;; use the first child as root is always open
    (if (-> tree origami-node-children car origami-node-open?)
        (origami-close-all-nodes buffer)
      (origami-open-all-nodes buffer))))

(defun origami-show-only-node (buffer point)
  "Close all fold nodes in BUFFER except for those necessary to
make POINT visible. Very useful for quickly collapsing everything
in the buffer other than what you are looking at."
  (interactive (list (current-buffer) (point)))
  (origami-close-all-nodes buffer)
  (origami-show-node buffer point))

(defun origami-previous-fold (buffer point)
  "Move point to the beginning of the fold before POINT. If POINT
is in a fold, move to the beginning of the fold that POINT is
in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (push-mark)
    (-> tree
        (origami-node-preorder-reduce (lambda (state n)
                                        (cons (origami-node-beg n) state)) nil)
        (->> (-reduce (lambda (state pos)
                        (if (< state point) state pos))))
        goto-char)))

(defun origami-next-fold (buffer point)
  "Move point to the end of the fold after POINT. If POINT is in
a fold, move to the end of the fold that POINT is in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (push-mark)
    (-> tree
        (origami-node-postorder-reduce (lambda (state n)
                                         (cons (origami-node-end n) state)) nil)
        (->> (-last (lambda (pos) (> pos point))))
        goto-char)))

(defun origami-forward-fold (buffer point)
  "Move point to the beginning of the first fold in the BUFFER
after POINT."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (push-mark)
    (-> tree
        (origami-node-preorder-reduce (lambda (state n)
                                        (cons (origami-node-beg n) state)) nil)
        (->> (-last (lambda (pos) (> pos point))))
        goto-char)))

(defun origami-forward-fold-same-level (buffer point)
  "Move point to the beginning of the next fold in the buffer
that is a sibling of the fold the point is currently in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (push-mark)
      (-when-let (c (-> (origami-node-next-sibling (origami-node-children
                                                    (origami-node-parent path))
                                                   (-last-item path))
                        origami-node-beg))
        (goto-char c)))))

(defun origami-backward-fold-same-level (buffer point)
  "Move point to the beginning of the previous fold in the buffer
that is a sibling of the fold the point is currently in."
  (interactive (list (current-buffer) (point)))
  (-when-let (tree (origami-get-fold-tree buffer))
    (-when-let (path (origami-node-find-path-containing tree point))
      (push-mark)
      (-when-let (c (-> (origami-node-prev-sibling (origami-node-children
                                                    (origami-node-parent path))
                                                   (-last-item path))
                        origami-node-beg))
        (goto-char c)))))

(defun origami-undo (buffer)
  "Undo the last folding operation applied to BUFFER. Undo
history is linear. If you undo some fold operations and then
perform a new fold operation you will lose the history of
operations undone."
  (interactive (list (current-buffer)))
  (let ((current-tree (origami-get-cached-tree buffer)))
    (origami-update-history buffer (lambda (h) (origami-h-undo h)))
    (let ((old-tree (origami-get-cached-tree buffer)))
      (origami-apply-new-tree current-tree old-tree))))

(defun origami-redo (buffer)
  "Redo the last folding operation applied to BUFFER. You can
only redo undone operations while a new folding operation hasn't
been performed to BUFFER."
  (interactive (list (current-buffer)))
  (let ((current-tree (origami-get-cached-tree buffer)))
    (origami-update-history buffer (lambda (h) (origami-h-redo h)))
    (let ((new-tree (origami-get-cached-tree buffer)))
      (origami-apply-new-tree current-tree new-tree))))

(defun origami-reset (buffer)
  "Remove all folds from BUFFER and reset all origami state
associated with this buffer. Useful during development or if you
uncover any bugs."
  (interactive (list (current-buffer)))
  (origami-setup-local-vars buffer)
  (origami-remove-all-overlays buffer))

(provide 'origami)
;;; origami.el ends here
