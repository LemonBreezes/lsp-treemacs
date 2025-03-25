;;; lsp-treemacs-generic.el --- treemacs wrapper     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'lsp-mode)

(require 'treemacs)
(require 'treemacs-treelib)
(require 'lsp-treemacs-themes)

(defvar-local lsp-treemacs-tree nil)
(defvar-local lsp-treemacs--right-click-actions nil)
(defvar-local lsp-treemacs-generic-filter nil)

(declare-function lsp-treemacs--set-mode-line-format "lsp-treemacs.el")

(defmacro lsp-treemacs-wcb-unless-killed (buffer &rest body)
  "`with-current-buffer' unless buffer killed."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (get-buffer ,buffer))
     (with-current-buffer ,buffer
       ,@body)))

(defun lsp-treemacs-generic-refresh (&optional _cache)
  (condition-case err
      (let ((inhibit-read-only t))
        ;; Verify we have valid tree data
        (when lsp-treemacs-tree
          (message "LSP-Treemacs: Refreshing with %s items in tree" (length lsp-treemacs-tree)))
        
        ;; First clear any variables that might reference the DOM
        ;; Only copy if tree exists and is valid
        (when (and lsp-treemacs-tree (listp lsp-treemacs-tree))
          (setq-local lsp-treemacs-tree (copy-tree lsp-treemacs-tree t)))
        
        ;; Reset the DOM completely to avoid corruption
        (setq-local treemacs-dom (make-hash-table :test #'equal))
        (treemacs--reset-dom)
        
        ;; Then update from a clean state
        (save-excursion
          (goto-char (point-min))
          (erase-buffer)
          (treemacs-initialize lsp-treemacs-generic-root
            :with-expand-depth 0)
          
          ;; Force DOM synchronization for the root node
          (when (and lsp-treemacs-tree (listp lsp-treemacs-tree) (not (null lsp-treemacs-tree)))
            (goto-char (point-min))
            (when-let ((root-btn (or (treemacs-button-get (point) 'button)
                                     (treemacs-button-get (point-min) 'button))))
              (let ((key 'lsp-treemacs-generic-root))
                (treemacs-on-expand key (copy-marker (point-min) t))
                (message "LSP-Treemacs: Synchronized DOM for root node in refresh"))))))
    (error
     (message "Error refreshing lsp-treemacs: %s" (error-message-string err)))))

(defun lsp-treemacs-generic-update (tree)
  (setq lsp-treemacs-tree tree)
  (lsp-treemacs-generic-refresh))

(defun lsp-treemacs-generic-right-click (event)
  (interactive "e")
  (let* ((ec (event-start event))
         (p1 (posn-point ec))
         (w1 (posn-window ec)))
    (select-window w1)
    (goto-char p1)
    (hl-line-highlight)
    (run-with-idle-timer
     0.001 nil
     (lambda ()
       (-when-let* ((actions (if-let (node (treemacs-node-at-point))
                                 (lsp-resolve-value (plist-get (button-get node :item) :actions))
                               lsp-treemacs--right-click-actions))
                    (menu (easy-menu-create-menu nil actions))
                    (choice (x-popup-menu event menu)))
         (when choice (call-interactively (lookup-key menu (apply 'vector choice))))
         (hl-line-highlight))))))

(defvar lsp-treemacs-generic-map
  (-doto (make-sparse-keymap)
    (define-key [mouse-1]  #'treemacs-TAB-action)
    (define-key [double-mouse-1] #'treemacs-RET-action)
    (define-key [mouse-3]  #'lsp-treemacs-generic-right-click))
  "Keymap for `lsp-treemacs-generic-mode'")

(define-minor-mode lsp-treemacs-generic-mode "Treemacs generic mode."
  :keymap lsp-treemacs-generic-map)

(defun lsp-treemacs--generic-icon (item expanded?)
  "Get the symbol for the the kind."
  (concat
   (if (or (plist-get item :children)
           (plist-get item :children-async))
       (if expanded?  "▾ " "▸ ")
     "  ")
   (or (plist-get item :icon-literal)
       (-if-let (icon (plist-get item :icon))
           (treemacs-get-icon-value
            icon
            nil
            lsp-treemacs-theme)
         "   "))))

(defun lsp-treemacs-filter-if-needed (result)
  (if lsp-treemacs-generic-filter
      (funcall lsp-treemacs-generic-filter result)
    result))

(defun lsp-treemacs-perform-ret-action (&rest _)
  (interactive)
  (if-let (action (-> (treemacs-node-at-point)
                      (button-get :item)
                      (plist-get :ret-action)))
      (funcall-interactively action)
    (treemacs-pulse-on-failure "No ret action defined.")))

(treemacs-define-expandable-node-type lsp-treemacs-generic-node
  :closed-icon (lsp-treemacs--generic-icon item nil)
  :open-icon (lsp-treemacs--generic-icon item t)
  :label (plist-get item :label)
  :key (plist-get item :key)
  :ret-action #'lsp-treemacs-perform-ret-action
  :children (-let (((item &as &plist :children :children-async) item))
              (cond
               ((functionp children) (->> (funcall children item)
                                          (lsp-treemacs-filter-if-needed)
                                          (funcall callback)))
               (children-async
                (-let [buffer (current-buffer)]
                  (funcall children-async
                           item
                           (lambda (result)
                             (lsp-treemacs-wcb-unless-killed buffer
                               (funcall callback (lsp-treemacs-filter-if-needed result)))))))
               (t (funcall callback (lsp-treemacs-filter-if-needed children)))))
  :child-type 'lsp-treemacs-generic-node
  :async? t)

;; The children parameter should evaluate to our buffer-local tree
;; or an empty list as fallback to avoid errors
(treemacs-define-variadic-entry-node-type lsp-treemacs-generic-root
  :key 'lsp-treemacs-generic-root
  :children (progn
              (when (and (boundp 'lsp-treemacs-tree) lsp-treemacs-tree)
                (message "LSP-Treemacs: Root children accessible: %s items" 
                         (length lsp-treemacs-tree)))
              (or (when (and (boundp 'lsp-treemacs-tree) 
                             lsp-treemacs-tree
                             (listp lsp-treemacs-tree)) 
                    (copy-sequence lsp-treemacs-tree)) 
                  '()))
  :child-type 'lsp-treemacs-generic-node)

(defun lsp-treemacs-render (tree title expand-depth
                                 &optional buffer-name right-click-actions _clear-cache?)
  "Render TREE in a treemacs buffer with TITLE.
EXPAND-DEPTH controls auto-expansion, BUFFER-NAME sets the buffer name,
RIGHT-CLICK-ACTIONS provides context menu items."
  (let ((buffer (get-buffer-create (or buffer-name "*LSP Lookup*"))))
    (with-current-buffer buffer
      ;; Clear any previous state
      (let ((inhibit-read-only t))
        ;; Ensure we have a clean state by clearing buffer-local variables
        ;; that might hold references to DOM nodes
        (setq-local lsp-treemacs-tree nil)
        
        ;; Fully reset the DOM with a fresh hash table
        (setq-local treemacs-dom (make-hash-table :test #'equal))
        (treemacs--reset-dom)
        
        ;; Safety check for tree data - ensure it's a proper list
        (unless (listp tree)
          (setq tree nil)
          (message "Warning: Invalid tree data passed to lsp-treemacs-render"))
        
        ;; Create a deep copy of the tree to avoid reference issues
        (when tree
          (setq tree (copy-tree tree t)))
          
        ;; Set tree data before initialize to ensure it's available
        (setq-local lsp-treemacs-tree tree)
        
        ;; Clear buffer completely
        (erase-buffer))
      
      ;; Initialize with clean slate
      (treemacs-initialize lsp-treemacs-generic-root
        :with-expand-depth (or expand-depth 0)
        :and-do (progn
                  (lsp-treemacs--set-mode-line-format buffer title)
                  (setq-local face-remapping-alist '((button . default)))
                  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
                  (setq-local lsp-treemacs--right-click-actions right-click-actions)
                  (setq-local window-size-fixed nil)
                  (setq-local treemacs--width-is-locked nil)
                  (setq-local treemacs-space-between-root-nodes nil)
                  
                  ;; Force DOM synchronization for the root node - this is crucial
                  ;; to ensure the DOM gets properly populated
                  (when-let ((root-btn (treemacs-button-get (point-min) 'button)))
                    (let ((key 'lsp-treemacs-generic-root))
                      (when tree
                        (treemacs-on-expand key (copy-marker (point-min) t))
                        (message "LSP-Treemacs: Synchronized DOM for root node"))))
                  
                  (when treemacs-text-scale
                    (text-scale-increase treemacs-text-scale))
                  (lsp-treemacs-generic-mode t)))
      (current-buffer))))

(provide 'lsp-treemacs-generic)
;;; lsp-treemacs-generic.el ends here
