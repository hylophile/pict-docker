;; pict.el. Major mode for editing Pict.
;; Hacked by David Turner from Olin Shivers' ML mode.
;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.

;; - Epoch/emacs 19 highlighting would be good for the error reporting.

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; If pict.el lives in some non-standard directory, you must tell emacs
;; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; Autoload pict-mode from file pict.el
;; (autoload 'pict-mode "pict" "Major mode for editing Pict programs." t)
;;
;; Files ending in ".pi" and ".src" are Pict source, so put
;; their buffers in pict-mode.
;; (setq auto-mode-alist
;;       (append '(("\\.pi$" . pict-mode) ("\\.src$" . pict-mode))
;; 		auto-mode-alist)
;; )

(provide 'pict)

;; CONSTANTS CONTROLLING THE MODE.
;;=============================================================================
;; These are the constants you might want to change

(defvar pict-indent-level 2
  "*Indentation of blocks in Pict.")

(defvar pict-paren-lookback 200
  "*Determines how far back (in chars) the indentation algorithm
should look for open parenthesis. High value means slow indentation
algorithm. A value of 200 (being the equivalent of 4-6 lines) should
suffice most uses. (A value of nil, means do not look at all)")

(defun install-pict-keybindings (map)
  (define-key map "\M-\t" 'pict-back-to-outer-indent)
  (define-key map "\C-j" 'newline-and-indent)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\M-\\" 'pict-indent-region)
  (define-key map "\t" 'pict-indent-line)
)

(defvar pict-mode-map nil "The mode map used in pict-mode.")

(cond ((not pict-mode-map)
       (setq pict-mode-map (make-sparse-keymap))
       (install-pict-keybindings pict-mode-map)))

;;; THIS NEEDS WORK
(defvar pict-mode-syntax-table nil "The syntax table used in pict-mode.")
(if pict-mode-syntax-table
    ()
  (setq pict-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" pict-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" pict-mode-syntax-table)
  (modify-syntax-entry ?+ "." pict-mode-syntax-table)
  (modify-syntax-entry ?- "." pict-mode-syntax-table)
  (modify-syntax-entry ?= "." pict-mode-syntax-table)
  (modify-syntax-entry ?% "." pict-mode-syntax-table)
  (modify-syntax-entry ?< "." pict-mode-syntax-table)
  (modify-syntax-entry ?> "." pict-mode-syntax-table)
  (modify-syntax-entry ?& "." pict-mode-syntax-table)
  (modify-syntax-entry ?| "." pict-mode-syntax-table)
  (modify-syntax-entry ?\\ "." pict-mode-syntax-table)
  (modify-syntax-entry ?\' "'" pict-mode-syntax-table))

(defun pict-mode ()
  "Major mode for editing Pict code.
Tab indents for Pict code.
Comments are delimited with {- ... -}.
Blank lines and form-feeds (^L's) separate paragraphs.
Delete converts tabs to spaces as it moves back.

Customisation: Entry to this mode runs the hooks on pict-mode-hook.

Variables controlling the indentation
=====================================

pict-indent-level (default 2)
    The indentation of a block of code.

pict-paren-lookback (default 200)
    Determines how far back (in chars) the indentation algorithm
    should look for open parenthesis. High value means slow indentation
    algorithm. A value of 200 (being the equivalent of 4-6 lines) should
    suffice most uses. (A value of nil, means do not look at all)

Mode map
========
\\{pict-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (pict-mode-variables)
  (use-local-map pict-mode-map)
  (make-local-variable 'pict-cursor)
  (setq major-mode 'pict-mode)
  (setq mode-name "Pict")
  (run-hooks 'pict-mode-hook)
)

(defun pict-mode-variables ()
  (set-syntax-table pict-mode-syntax-table)
  ;; A paragraph is separated by blank lines or ^L only.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[\t ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pict-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "{- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -}")
  (make-local-variable 'comment-column)
  (setq comment-column 40)		
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "{-+[ \t]*")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'pict-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pict-indent-region (begin end)
  "Indent region of Pict code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (pict-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region... done"))

(defun pict-indent-line ()
  "Indent current line of Pict code."
  (interactive)
  (let ((indent (pict-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(save-excursion
	  (let ((beg (progn (beginning-of-line) (point))))
	    (skip-chars-forward "\t ")
	    (delete-region beg (point))
	    (indent-to indent))))
    ;; If point is before indentation, move point to indentation
    (if (< (current-column) (current-indentation))
	(skip-chars-forward "\t "))))

(defun pict-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
	  (indent (current-column)))
      (if (> start-column 0)
	  (progn
	    (save-excursion
	      (while (>= indent start-column)
		(if (re-search-backward "^[^\n]" nil t)
		    (setq indent (current-indentation))
		  (setq indent 0))))
	    (backward-delete-char-untabify (- start-column indent)))))))

(defconst pict-indent-starters-reg
  "if\\b\\|then\\b\\|else\\b\\|local\\b\
\\|in\\b\\|def\\b\\|val\\b.*=$\\|and\\b\\|type\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst pict-starters-reg
  "\\btype\\b\\|\\bdef\\b\\|\\brun\\b\\|\\bval\\b\\|\\bnew\\b\\|\\bnow\\b"
  "The starters of new expressions.")

(defconst pict-end-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bif\\b\\|\\bchoose\\b\\|\\bbegin\\b\\|\\babs\\b"
  "Matching reg-expression for the \"end\" keyword.")

(defconst pict-starters-indent-after
  "local\\b"
  "Indent after these.")

(defun pict-calculate-indentation ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (if (bobp) 0 ; Indentation = 0 at beginning of buffer
	(skip-chars-forward "\t ")
	(cond
	 ;; Indentation for comments alone on a line, matches the
	 ;; proper indentation of the next line. Search only for the
	 ;; next "-}", not for the matching.
	 ((looking-at "{-")
	  (if (not (search-forward "-}" nil t)) (error "Comment not ended."))
	  (end-of-line)
	  (skip-chars-forward "\n\t ")
	  ;; If we are at eob, just indent 0
	  (if (eobp) 0 (pict-calculate-indentation)))
	 ;; Continued string ? (Added 890113 lbn)
	 ((looking-at "\\\\")
	  (save-excursion
	    (if (save-excursion (previous-line 1)
				(beginning-of-line)
				(looking-at "[\t ]*\\\\"))
		(progn (previous-line 1) (current-indentation))
	    (if (re-search-backward "[^\\\\]\"" nil t)
		(1+ (current-indentation))
	      0))))
	 ((looking-at "and\\b")
	  (if (pict-find-matching-starter pict-starters-reg)
	      (current-column)
	    0))
	 ((looking-at "in\\b") ; Match the beginning let/local
	  (pict-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\"))
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ((looking-at "else\\b") ; Match the previous if or else
	  (pict-find-match-indent
	   "else" "\\belse\\b" "\\bif\\b\\|\\belse\\b" t))
	 ((looking-at "then\\b") ; Match the previous if or else
	  (pict-find-match-indent
	   "then" "\\bthen\\b" "\\bif\\b\\|\\belse\\b" t))
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ((looking-at pict-starters-reg)
	  (let ((start (point)))
	    (pict-backward-sexp)
	    (if (and (looking-at pict-starters-indent-after)
		     (/= start (point)))
		(+ (progn (beginning-of-line)
			  (skip-chars-forward "\t ")
			  (current-indentation))
		   pict-indent-level)
	      (beginning-of-line)
	      (skip-chars-forward "\t ")
	      (if (and (looking-at pict-starters-indent-after)
		       (/= start (point)))
		  (+ (current-indentation) pict-indent-level)
		(goto-char start)
		(if (pict-find-matching-starter pict-starters-reg)
		    (current-column)
		  0)))))
	 (t
	  (let ((indent (pict-get-indent)))
	    (if pict-paren-lookback	; Look for open parenthesis ?
		(max indent (pict-get-paren-indent))
	      indent))))))))

(defun pict-get-indent ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (skip-chars-backward "\t\n; ")
      (if (looking-at ";") (pict-backward-sexp))
      (cond
       ((save-excursion (pict-backward-sexp) (looking-at "end\\b"))
	(- (current-indentation) pict-indent-level))
       (t
	(while (/= (current-column) (current-indentation))
	  (pict-backward-sexp))
	(skip-chars-forward "\t +")
	(let ((indent (current-column)))
	  (skip-chars-forward "\t (")
	  (cond
	   ;; Started val/fun/structure...
	   ((looking-at pict-indent-starters-reg)
	    (+ (current-column) pict-indent-level))
	   ;; Start of multi-line comment
	   ((looking-at "{-") (1+ indent))
	   ;; Indent after "->"
	   ((looking-at "=") (+ indent pict-indent-level))
	   ;; else keep the same indentation as previous line
	   (t indent))
))))))

(defun pict-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) pict-paren-lookback) (point-min))))
      (catch 'loop
	(while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1))
	  (if (re-search-backward "[][{}()]" backpoint t)
	      (if (not (pict-inside-comment-or-string-p))
		  (cond
		   ((looking-at "(") (setq levelpar (1+ levelpar)))
		   ((looking-at ")") (setq levelpar (1- levelpar)))
		   ((looking-at "\\[") (setq levelsqr (1+ levelsqr)))
		   ((looking-at "\\]") (setq levelsqr (1- levelsqr)))
		   ((looking-at "{") (setq levelcurl (1+ levelcurl)))
		   ((looking-at "}") (setq levelcurl (1- levelcurl)))))
	    (throw 'loop 0)))		; Exit with value 0
	(if (save-excursion
	      (forward-char 1)
	      (looking-at pict-indent-starters-reg))
	    (1+ (+ (current-column) pict-indent-level))
	  (1+ (current-column)))))))

(defun pict-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (progn
		(search-backward "{-")
		(search-forward "-}")
		(forward-char -1)	; A "-}" is not inside the comment
		(> (point) start))
	    (error nil)))
	t
      (let ((numb 0))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (progn (beginning-of-line) (point)) start)
	    (condition-case ()
		(while t
		  (search-forward "\"")
		  (setq numb (1+ numb)))
	      (error (if (and (not (zerop numb))
			      (not (zerop (% numb 2))))
			 t nil)))))))))

(defun pict-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((case-fold-search nil)
	  (level 1)
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (pict-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun pict-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (pict-find-match-backward unquoted-this this match))
    (if indented
	(current-column)
      (progn
	(beginning-of-line)
	(skip-chars-forward "\t ")
	(current-indentation)))))

(defun pict-find-matching-starter (regexp)
  (let ((case-fold-search nil)
	(start-let-point (pict-point-inside-let-etc))
	(start-up-list (pict-up-list))
	(found t))
    (if (pict-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (pict-up-list))
			 (/= start-let-point (pict-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun pict-point-inside-let-etc ()
  (let ((case-fold-search nil) (last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (pict-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (pict-find-match-backward "end" "\\bend\\b"
						  pict-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))

(defun pict-re-search-backward (regexpr)
  (let ((case-fold-search nil) (found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (pict-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun pict-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun pict-backward-sexp ()
  (condition-case ()
      (progn
	(let ((start (point)))
	  (backward-sexp 1)
	  (while (and (/= start (point)) (looking-at "\\{\\-"))
	    (setq start (point))
	    (backward-sexp 1))))
    (error (forward-char -1))))

(defun pict-comment-indent ()
  (if (looking-at "^\\{\\-")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	; Else indent at comment column
	   comment-column))))		; except leave at least one space.

;;; Do the user's customisation...

(defvar pict-load-hook nil
  "This hook is run when Pict mode is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'pict-load-hook)
