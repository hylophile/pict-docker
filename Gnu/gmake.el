;; Originally distributed with compile2 version 2.05 beta
;; Copyright Nick Duffek, 1993, Simplified by David N. Turner, 1994.
;; Small improvements by Benjamin Pierce and David Turner, 1994-1996.
;;
;; This file is not part of GNU Emacs.  However, the following applies as if
;; it were:
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY.  No author or distributor accepts responsibility to anyone
;; for the consequences of using it or for whether it serves any particular
;; purpose or works at all, unless he says so in writing.  Refer to the GNU
;; Emacs General Public License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute GNU Emacs,
;; but only under the conditions described in the GNU Emacs General Public
;; License.  A copy of this license is supposed to have been given to you
;; along with GNU Emacs so you can know your rights and responsibilities.  It
;; should be in a file named COPYING.  Among other things, the copyright
;; notice and this notice must be preserved on all copies.
;;

;; Idea: Implement previous-error by doing (forward-line -1)

(defvar gmake-save-buffers t
  "t unconditionally saves all modified buffers before \\[gmake] \(gmake\)
or \\[gmake-again] \(gmake-again\), non-t and non-nil asks before saving
each buffer, nil saves no buffers.")

(defvar gmake-pos-x 0)
(defvar gmake-pos-y 0)
(defvar gmake-width 100)
(defvar gmake-height 10)

(defun gmake-request-directory ()
  "Prompt for and read a directory name.
This function bypassing read-file-name's peculiar behavior of returning
buffer-file-name if the users' response is default-directory."
  (let ((dir (expand-file-name
	      (let ((buffer-file-name nil))
		(read-file-name "In directory: "
				default-directory nil t)))))
    (or (eq system-type 'vax-vms)
	(setq dir (file-name-as-directory dir)))
    (or (file-directory-p dir)
	(error "%s is not a directory" dir))
    dir))

(defvar gmake-font "5x8")

(defun gmake-pop-up-buffer (buffer)
  "Display but don't select BUFFER in a window.
Returns the window displaying the buffer."
  (if (not window-system)
       (display-buffer buffer)
      (let ((w (get-buffer-window buffer t)))
        (cond
         (w
          (raise-frame (window-frame w))
          w)
         (t
          (let* ((default-frame-alist 
                   (list
                    '(minibuffer . nil) '(menu-bar-lines -1)
                    (cons 'font gmake-font) 
                    (cons 'width gmake-width) (cons 'height gmake-height)))
                 (pop-up-windows t)
                 (pop-up-frames t)
                 (w (display-buffer buffer))
                 (f (window-frame w)))
            (set-frame-position f gmake-pos-x gmake-pos-y)
            w))))))

(defvar gmake-command "gmake"
  "*Last gmake compilation command. Default for next compilation.")

(defvar gmake-directory ""
  "*Last gmake compilation directory. Default for next compilation.")

(defvar gmake-last-error nil
  "Marks the last error we parsed. Is nil if we haven't parsed anything yet.")

(defun gmake-again ()
  "Execute last gmake compilation again."
  (interactive)
  (if (string= gmake-directory "")
      (setq gmake-directory (file-name-directory (buffer-file-name))))
  (gmake-compile gmake-command gmake-directory)
)

(defun gmake-compile (command directory)
  "Execute COMMAND asynchronously in DIRECTORY, collecting output in a buffer.
While or after COMMAND executes, gmake-next-error \(\\[gmake-next-error]\)
finds the text to which errors refer."
  (interactive
   (list
    (read-string "Compile command: " gmake-command) (gmake-request-directory)))
  (if (string= command "") (error "No command given"))
  (setq gmake-command command)
  (setq gmake-directory directory)
  (let* ((start-buffer (current-buffer))
	 (compile-buffer (get-buffer-create "*compilation*"))
	 (compile-process (get-buffer-process compile-buffer))
	 (compile-window (gmake-pop-up-buffer compile-buffer)))

    (and compile-process
	 (or (eq (process-status compile-process) 'stop)
	     (eq (process-status compile-process) 'run))
	 (delete-process compile-process))
	       
    (and gmake-save-buffers
	 (save-some-buffers (eq gmake-save-buffers t)))
    
    (unwind-protect
	(progn
	  (set-buffer compile-buffer)
	  (setq default-directory directory)
	  (erase-buffer)
	  (insert "cd " directory "\n" command "\n")
	  (setq gmake-last-error (point-min))
	  (set-marker
	   (process-mark
	    (start-process command compile-buffer
			   shell-file-name "-c" command))
	   (point))
	  (set-window-point compile-window (point-max))))
    ))

(defvar gmake-error-regexps
  '(;;
    ;; File "FILE", line LINE, characters COLUMN-...
    ;;
    ("File \"\\([^\"\n]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)"
     1 2 3)
    ;;
    ;; ...line LINE of "FILE" (yacc)
    ;;
    (".*line \\([0-9]+\\) of \"\\([^\"\n]+\\)\"[:,]" 2 1 nil)
    ;;
    ;; ..."FILE" ?,? (?line LINE)?
    ;;
    ("[^\"\n]*\"\\([^\"\n]+\\)\" ?,?[ \t]+(?line[ \t]+\\([0-9]+\\))?" 1 2 nil)
    ;;
    ;; ... in file FILE at line LINE (perl)
    ;;
    (".* in file \\([^ ]+\\) at line \\([0-9]+\\)," 1 2 nil)
    ;;
    ;; FILE:... at line LINE: (/bin/sh)
    ;;
    ("\\([^:\n]+\\):.* at line \\([0-9]+\\):" 1 2 nil)
    ;;
    ;;
    ;; FILE: LINE: (/bin/sh)
    ;;
    ("\\([^:\n]+\\): \\([0-9]+\\):" 1 2 nil)
    ;;
    ;; ... at FILE line LINE (perl)
    ;;
    (".* at \\([^ \n]+\\) line \\([0-9]+\\)" 1 2 nil)
    ;;
    ;; ... FILE, line LINE:
    ;;
    ("\\(.* \\)?\\([^ \n,]+\\), line \\([0-9]+\\): " 2 3 nil)
    ;;
    ;; FILE:LINE.COLUMN: (Pict)
    ;;
    ("[ \t]*\\([^ :\n]+\\):\\([0-9]+\\)\\.\\([0-9]+\\):" 1 2 3)
    ;;
    ;; FILE:LINE ...: (GNU software)
    ;;
    ("\\([^ :\n]+\\):\\([0-9]+\\)\\( ([^ \n\)]+)\\)?:" 1 2 nil)
    ;;
    ;; rule 8 -- FILE(LINE)
    ;;
    ;; 'seekdir   llib-lc(345) :: uuq.c?(73)' (sysV lint -- kamat@uceng.uc.edu)
    ;; 'rcmd         cico.c?(243)' (sysV)
    ;; 'foo.c(8): warning: w may be used before set' (4.3bsd grep, compile,
    ;;    lint part 1)
    ;; 'strcmp: variable # of args.      llib-lc(359)  ::  foo.c(8)' (4.3bsd
    ;;    lint part 2)
    ;; 'i defined( foo.c(4) ), but never used' (4.3bsd lint part 3)
    ;; 'i used( foo.c(144) ), but not defined' (4.3bsd lint part 3)
    ;;
    (".*\\(^\\| \\)\\([^( ?\n]+\\)\\??(\\([0-9]+\\))" 2 3 nil)
    )
  "*A list of lists consisting of the form
  \(\(rexexp filename-index line-index column-index\) ...\)
for parsing error messages.

Guidelines for adding regular expressions:
 - Regular expressions only match text at the beginning of a line, so prefix
   \".*\" to expressions intended to match beyond beginning-of-line.
 - For the same reason, \"^\" at the start of an expression is redundant, and
   should be omitted.
 - Always include \\n inside square-bracketed character ranges, to avoid
   confusing the parsing mechanism by matching beyond end-of-line.
 - If possible, modify an existing expression that almost performs the
   appropriate matches rather than creating a new one.
 - Position new regular expressions later in the list than more rigid regular
   expressions -- ones less prone to spurious matches -- and earlier than less
   rigid ones.
 - Set column-index to nil if no column information is available.
")

(defun gmake-parse-line (error-regexps)
  "Try and find an error matching one of the regular expressions in
the list ERROR-REGEXPS. Return nil if no match was found, or a list
(filename lineno column) if a match was found."

  (cond
   ((null error-regexps) nil)
   ((looking-at (car (car error-regexps)))
    (let* ((rule (car error-regexps))
	   (file-index (nth 1 rule))
	   (line-index (nth 2 rule))
	   (column-index (nth 3 rule)))
      (list
       (buffer-substring (match-beginning file-index)
			 (match-end file-index))
       (string-to-int(buffer-substring (match-beginning line-index)
				       (match-end line-index)))
       (cond (column-index
	      (1- (string-to-int (buffer-substring
				  (match-beginning column-index)
				  (match-end column-index)))))
	     (t 0)))))
   (t (gmake-parse-line (cdr error-regexps))))
)

(defun gmake-next-error ()
  "Find the next error by parsing the gmake compilation buffer.
Moves the error message to the top line of the compilation window,
putting the cursor at the beginning of the error source."
  (interactive)
  (if (null gmake-last-error) (error "No compilation in progress"))
  (let* ((parse nil)
	 (case-fold-search nil)
	 (compile-buffer (get-buffer-create "*compilation*"))
	 (compile-window (gmake-pop-up-buffer compile-buffer)))
    
    (save-excursion
      (set-buffer compile-buffer)
      (goto-char gmake-last-error) (forward-line 1)
      (while (and (not parse) (< (point) (point-max)))
	(setq parse (gmake-parse-line gmake-error-regexps))
	(setq gmake-last-error (point-marker))
	(forward-line 1)))
	 
    (cond
     ((null parse)
	(message "No error message found."))
     (t
      (set-window-start compile-window gmake-last-error)
      (let* ((file (nth 0 parse))
	     (line (nth 1 parse))
	     (column (nth 2 parse)))
	(cond ((file-readable-p file)
	       (switch-to-buffer (find-file-noselect file))
	       (goto-line line) (forward-char column))
	      (t
	       (error (concat "Can't read file " file)))))))))

(defun gmake-kill-compilation ()
  "Kill the currently executing compilation job"
  (interactive)
  (let ((p (get-buffer-process (get-buffer "*compilation*"))))
    (if p 
        (kill-process p))))

(provide 'gmake)
