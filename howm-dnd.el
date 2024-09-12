;;; howm-dnd.el --- Drag-and-drop support
(defun howm-drag-and-drop (uri action)
  "Drag-and-drop files into a Howm buffer to link them."
  (let* ((target (dnd-get-local-file-name uri t)))
    (if target
        (progn
          (insert ">>> ~/")
          (insert (file-relative-name target "~")))
      (message "Failed to drag-and-drop a link to file!"))
    action))

(defun howm-drag-and-drop-setup ()
  "Setup the drag-and-drop handler for local files."
  (add-to-list 'dnd-protocol-alist '("^file:" . howm-drag-and-drop)))

(add-hook 'howm-mode-hook 'howm-drag-and-drop-setup)

(provide 'howm-dnd)
;;; howm-dnd.el ends here
