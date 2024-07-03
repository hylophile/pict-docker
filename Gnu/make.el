;; My customisation of compilation mode.

(require 'compile)

(setq compilation-window-height (min 12 (/ (frame-height) 3)))

(setq compilation-error-regexp-alist
 '(("^File \"\\([^\"\n]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)"
    1 2 3)
   ("^\\([^ :\n]+\\):\\([0-9]+\\)\\.\\([0-9]+\\):" 1 2 3)
   ("^\\([^ :\n]+\\):\\([0-9]+\\):" 1 2)))

(defvar gmake-directory "~/"
  "*Last directory used to do a compilation; default for next compilation.")

(defun gmake-again ()
  "Execute last compilation again."
  (interactive)
  (save-some-buffers t)
  (let ((default-directory gmake-directory))
    (compile-internal compile-command "No more errors"))
)

(defun gmake-compile (command directory)
  "Customised interface to \(\\[compile]\) which prompts for a directory."
  (interactive "sCompile command: \nDIn directory: ")
  (setq compile-command command)
  (setq gmake-directory directory)
  (save-some-buffers t)
  (let ((default-directory directory))
    (compile-internal compile-command "No more errors"))
)

(provide 'gmake)
