;; ml.el. Major mode for editing (Standard) ML.
;; Simplified by David Turner.
;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.
;; This file is under construction.

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ; If ml.el lives in some non-standard directory, you must tell emacs
;; ; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; ; Autoload ml and ml-mode from file ml.el
;; (autoload 'ml-mode "ml" "Major mode for editing ML source." t)
;;
;; ; Files ending in ".sml" are ML source, so put their buffers in ml-mode.
;; (setq auto-mode-alist
;;       (cons '("\\.sml$" . ml-mode) 
;;	       auto-mode-alist))   
;;
;; ; Define C-c t to run my favorite command in inferior ML mode:
;; (setq ml-load-hook
;;       '((lambda () (define-key inferior-ml-mode-map "\C-ct"
;;                                'favorite-cmd))))

(provide 'ml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change
;;; 

(defvar ml-indent-level 4
  "*Indentation of blocks in ML.")

(defvar ml-pipe-indent -2
  "*Extra (negative) indentation for lines beginning with |.")

(defvar ml-case-indent nil
  "*How to indent case-of expressions.
 If t:   case expr              If nil:   case expr of
           of exp1 => ...                     exp1 => ...
            | exp2 => ...                   | exp2 => ...

The first seems to be the standard in NJ-SML, but the second
is the default.")

(defvar ml-nested-if-indent nil
  "*If set to t, nested if-then-else expression will have the same
indentation as:
                 if exp1 then exp2
                 else if exp3 then exp4
                 else if exp5 then exp6
                      else exp7")

(defvar ml-type-of-indent t
  "*How to indent `let' `struct' etc.
 If t:  fun foo bar = let              If nil:  fun foo bar = let
                          val p = 4                 val p = 4
                      in                        in
                          bar + p                   bar + p
                      end                       end

Will not have any effect if the starting keyword is first on the line.")

(defvar ml-paren-lookback 200
  "*Determines how far back (in chars) the indentation algorithm
should look for open parenthesis. High value means slow indentation
algorithm. A value of 200 (being the equivalent of 4-6 lines) should
suffice most uses. (A value of nil, means do not look at all)")

(defvar ml-import-command "import \"%s\""
  "*Template for importing a file into the inferior ML.")

(defvar ml-prompt-regexp "^[\-=] *"
  "*Regexp used to recognise prompts in the inferior ML process.")

(defvar ml-temp-file (make-temp-name "/tmp/ml")
  "*Temp file that emacs uses to communicate with the ML process.
See ML-TEMP-THRESHOLD. Defaults to (MAKE-TEMP-NAME \"/tmp/ml\")")

;;; These bindings have the following advantages over sml.el:
;;; - C-M-\ is the standard emacs binding for indent-region.
;;; - Sml.el has several bindings of the form C-c <letter>.
;;;   These bindings are not supposed to be used by modes; they
;;;   are reserved for user customisation.
;;; - Bindings are more compatible with cmuscheme.el, cmulisp.el,
;;;   tea.el, hemlock, Zwei, and other process-in-buffer emacs
;;;   interfaces.

;;; Need: mark-defun, m-c-x

;;; Install the bindings common to the source and process modes:
(defun install-ml-keybindings (map)
  ;; Process commands:
  (define-key map "\C-c`" 'ml-next-error)
  (define-key map "\C-c=" 'ml-goto-error)
  ;; Text-formatting commands:
  (define-key map "\M-|" 'ml-electric-pipe)
  (define-key map "\M-\t" 'ml-back-to-outer-indent)
  (define-key map "\C-j" 'newline-and-indent)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\C-\M-\\" 'ml-indent-region)
  (define-key map "\t" 'ml-indent-line))

(defvar ml-mode-map nil "The mode map used in ml-mode.")
(cond ((not ml-mode-map)
       (setq ml-mode-map (make-sparse-keymap))
       (install-ml-keybindings ml-mode-map)
       ))

;;; THIS NEEDS WORK
(defvar ml-mode-syntax-table nil "The syntax table used in ml-mode.")
(if ml-mode-syntax-table
    ()
  (setq ml-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()1" ml-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" ml-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" ml-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" ml-mode-syntax-table)
  (modify-syntax-entry ?\' "'" ml-mode-syntax-table))

(defun ml-mode ()
  "Major mode for editing ML code.
Tab indents for ML code.
Comments are delimited with (* ... *).
Blank lines and form-feeds (^L's) separate paragraphs.
Delete converts tabs to spaces as it moves back.

Customisation: Entry to this mode runs the hooks on ml-mode-hook.

Variables controlling the indentation
=====================================

ml-indent-level (default 4)
    The indentation of a block of code.

ml-pipe-indent (default -2)
    Extra indentation of a line starting with \"|\".

ml-case-indent (default nil)
    Determine the way to indent case-of expression.
    If t:   case expr              If nil:   case expr of
              of exp1 => ...                     exp1 => ...
               | exp2 => ...                   | exp2 => ...

    The first seems to be the standard in NJ-SML. The second is the default.

ml-nested-if-indent (default nil)
    If set to t, nested if-then-else expression will have the same
    indentation as:
                     if exp1 then exp2
                     else if exp3 then exp4
                     else if exp5 then exp6
                          else exp7

ml-type-of-indent (default t)
    How to indent `let' `struct' etc.

    If t:  fun foo bar = let                If nil:  fun foo bar = let
                             val p = 4                   val p = 4
                         in                          in
                             bar + p                     bar + p
                         end                         end

    Will not have any effect if the starting keyword is first on the line.

ml-paren-lookback (default 200)
    Determines how far back (in chars) the indentation algorithm
    should look for open parenthesis. High value means slow indentation
    algorithm. A value of 200 (being the equivalent of 4-6 lines) should
    suffice most uses. (A value of nil, means do not look at all)

Mode map
========
\\{ml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (ml-mode-variables)
  (use-local-map ml-mode-map)
  (setq major-mode 'ml-mode)
  (setq mode-name "ML")
  (run-hooks 'ml-mode-hook))		; Run the hook

(defun ml-mode-variables ()
  (set-syntax-table ml-mode-syntax-table)
  ;; A paragraph is separated by blank lines or ^L only.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[\t ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ml-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)		
  (make-local-variable 'comment-start-skip)
  ;; This matches a start of comment (I sure hope!)
  (setq comment-start-skip "(\\*+[ \t]?")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ml-comment-indent)
  ;;
  ;; Adding these will fool the matching of parens. I really don't
  ;; know why. It would be nice to have comments treated as
  ;; white-space
  ;; 
  ;; (make-local-variable 'parse-sexp-ignore-comments)
  ;; (setq parse-sexp-ignore-comments t)
  )

(defconst ml-pipe-matchers-reg
  "\\bcase\\b\\|\\bfn\\b\\|\\bfun\\b\\|\\bhandle\\b\
\\|\\bdatatype\\b\\|\\babstype\\b\\|\\band\\b"
  "The keywords a `|' can follow.")

(defun ml-electric-pipe ()
  "Insert a \"|\". Depending on the context insert the name of
function, a \"=>\" etc."
  (interactive)
  (let ((case-fold-search nil)		; Case sensitive
	(here (point))
	(match (save-excursion
		 (ml-find-matching-starter ml-pipe-matchers-reg)
		 (point)))
	(tmp "  => ")
	(case-or-handle-exp t))
    (if (/= (save-excursion (beginning-of-line) (point))
	    (save-excursion (skip-chars-backward "\t ") (point)))
	(insert "\n"))
    (insert "|")
    (save-excursion
      (goto-char match)
      (cond
       ;; It was a function, insert the function name
       ((looking-at "fun\\b")
	(setq tmp (concat " " (buffer-substring
			       (progn (forward-char 3)
				      (skip-chars-forward "\t\n ") (point))
			       (progn (forward-word 1) (point))) " "))
	(setq case-or-handle-exp nil))
       ;; It was a datatype, insert nothing
       ((looking-at "datatype\\b\\|abstype\\b")
	(setq tmp " ") (setq case-or-handle-exp nil))
       ;; If it is an and, then we have to see what is was
       ((looking-at "and\\b")
	(let (isfun)
	  (save-excursion
	    (condition-case ()
		(progn
		  (re-search-backward "datatype\\b\\|abstype\\b\\|fun\\b")
		  (setq isfun (looking-at "fun\\b")))
	      (error (setq isfun nil))))
	  (if isfun
	      (progn
		(setq tmp
		      (concat " " (buffer-substring
				   (progn (forward-char 3)
					  (skip-chars-forward "\t\n ") (point))
				   (progn (forward-word 1) (point))) " "))
		(setq case-or-handle-exp nil))
	    (setq tmp " ") (setq case-or-handle-exp nil))))))
    (insert tmp)
    (ml-indent-line)
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (forward-char (1+ (length tmp)))
    (if case-or-handle-exp
	(forward-char -4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ml-indent-region (begin end)
  "Indent region of ML code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (ml-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region... done"))

(defun ml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((indent (ml-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(save-excursion			;; Added 890601 (point now stays)
	  (let ((beg (progn (beginning-of-line) (point))))
	    (skip-chars-forward "\t ")
	    (delete-region beg (point))
	    (indent-to indent))))
    ;; If point is before indentation, move point to indentation
    (if (< (current-column) (current-indentation))
	(skip-chars-forward "\t "))))

(defun ml-back-to-outer-indent ()
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

(defconst ml-indent-starters-reg
  "abstraction\\b\\|abstype\\b\\|and\\b\\|case\\b\\|datatype\\b\
\\|else\\b\\|fun\\b\\|functor\\b\\|if\\b\\|sharing\\b\
\\|in\\b\\|infix\\b\\|infixr\\b\\|let\\b\\|local\\b\
\\|nonfix\\b\\|of\\b\\|open\\b\\|raise\\b\\|sig\\b\\|signature\\b\
\\|struct\\b\\|structure\\b\\|then\\b\\|\\btype\\b\\|val\\b\
\\|while\\b\\|with\\b\\|withtype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst ml-starters-reg
  "\\babstraction\\b\\|\\babstype\\b\\|\\bdatatype\\b\
\\|\\bexception\\b\\|\\bfun\\b\\|\\bfunctor\\b\\|\\blocal\\b\
\\|\\binfix\\b\\|\\binfixr\\b\\|\\bsharing\\b\
\\|\\bnonfix\\b\\|\\bopen\\b\\|\\bsignature\\b\\|\\bstructure\\b\
\\|\\btype\\b\\|\\bval\\b\\|\\bwithtype\\b\\|\\bwith\\b"
  "The starters of new expressions.")

(defconst ml-end-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bsig\\b\\|\\bstruct\\b\\|\\bwith\\b"
  "Matching reg-expression for the \"end\" keyword.")

(defconst ml-starters-indent-after
  "let\\b\\|local\\b\\|struct\\b\\|in\\b\\|sig\\b\\|with\\b"
  "Indent after these.")

(defun ml-calculate-indentation ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (if (bobp)			; Beginning of buffer
	  0				; Indentation = 0
	(skip-chars-forward "\t ")
	(cond
	 ;; Indentation for comments alone on a line, matches the
	 ;; proper indentation of the next line. Search only for the
	 ;; next "*)", not for the matching.
	 ((looking-at "(\\*")
	  (if (not (search-forward "*)" nil t))
	      (error "Comment not ended."))
	  (end-of-line)
	  (skip-chars-forward "\n\t ")
	  ;; If we are at eob, just indent 0
	  (if (eobp) 0 (ml-calculate-indentation)))
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
	 ;; Are we looking at a case expression ?
	 ((looking-at "|.*=>")
	  (ml-skip-block)
	  (ml-re-search-backward "=>")
	  ;; Dont get fooled by fn _ => in case statements (890726)
	  ;; Changed the regexp a bit, so fn has to be first on line,
	  ;; in order to let the loop continue (Used to be ".*\bfn....")
	  ;; (900430).
	  (let ((loop t))
	    (while (and loop (save-excursion
			       (beginning-of-line)
			       (looking-at "[^ \t]+\\bfn\\b.*=>")))
	      (setq loop (ml-re-search-backward "=>"))))
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (cond
	   ((looking-at "|") (current-indentation))
	   ((and ml-case-indent (looking-at "of\\b"))
	    (1+ (current-indentation)))
	   ((looking-at "fn\\b") (1+ (current-indentation)))
	   ((looking-at "handle\\b") (+ (current-indentation) 5))
	   (t (+ (current-indentation) ml-pipe-indent))))
	 ((looking-at "and\\b")
	  (if (ml-find-matching-starter ml-starters-reg)
	      (current-column)
	    0))
	 ((looking-at "in\\b")		; Match the beginning let/local
	  (ml-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
	 ((looking-at "end\\b")		; Match the beginning
	  (ml-find-match-indent "end" "\\bend\\b" ml-end-starters-reg))
	 ((and ml-nested-if-indent (looking-at "else[\t ]*if\\b"))
	  (ml-re-search-backward "\\bif\\b\\|\\belse\\b")
	  (current-indentation))
	 ((looking-at "else\\b")	; Match the if
	  (ml-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
	 ((looking-at "then\\b")	; Match the if + extra indentation
	  (+ (ml-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	     ml-indent-level))
	 ((and ml-case-indent (looking-at "of\\b"))
	  (ml-re-search-backward "\\bcase\\b")
	  (+ (current-column) 2))
	 ((looking-at ml-starters-reg)
	  (let ((start (point)))
	    (ml-backward-sexp)
	    (if (and (looking-at ml-starters-indent-after)
		     (/= start (point)))
		(+ (if ml-type-of-indent
		       (current-column)
		     (if (progn (beginning-of-line)
				(skip-chars-forward "\t ")
				(looking-at "|"))
			 (- (current-indentation) ml-pipe-indent)
		       (current-indentation)))
		   ml-indent-level)
	      (beginning-of-line)
	      (skip-chars-forward "\t ")
	      (if (and (looking-at ml-starters-indent-after)
		       (/= start (point)))
		  (+ (if ml-type-of-indent
			 (current-column)
		       (current-indentation))
		     ml-indent-level)
		(goto-char start)
		(if (ml-find-matching-starter ml-starters-reg)
		    (current-column)
		  0)))))
	 (t
	  (let ((indent (ml-get-indent)))
	    (cond
	     ((looking-at "|")
	      ;; Lets see if it is the follower of a function definition
	      (if (ml-find-matching-starter
		   "\\bfun\\b\\|\\bfn\\b\\|\\band\\b\\|\\bhandle\\b")
		  (cond
		   ((looking-at "fun\\b") (- (current-column) ml-pipe-indent))
		   ((looking-at "fn\\b") (1+ (current-column)))
		   ((looking-at "and\\b") (1+ (1+ (current-column))))
		   ((looking-at "handle\\b") (+ (current-column) 5)))
		(+ indent ml-pipe-indent)))
	     (t
	      (if ml-paren-lookback	; Look for open parenthesis ?
		  (max indent (ml-get-paren-indent))
		indent))))))))))

(defun ml-get-indent ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (skip-chars-backward "\t\n; ")
      (if (looking-at ";") (ml-backward-sexp))
      (cond
       ((save-excursion (ml-backward-sexp) (looking-at "end\\b"))
	(- (current-indentation) ml-indent-level))
       (t
	(while (/= (current-column) (current-indentation))
	  (ml-backward-sexp))
	(skip-chars-forward "\t |")
	(let ((indent (current-column)))
	  (skip-chars-forward "\t (")
	  (cond
	   ;; Started val/fun/structure...
	   ((looking-at ml-indent-starters-reg)
	    (+ (current-column) ml-indent-level))
	   ;; Indent after "=>" pattern, but only if its not an fn _ =>
	   ;; (890726)
	   ((looking-at ".*=>")
	    (if (looking-at ".*\\bfn\\b.*=>")
		indent
	      (+ indent ml-indent-level)))
	   ;; else keep the same indentation as previous line
	   (t indent))))))))

(defun ml-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) ml-paren-lookback) (point-min))))
      (catch 'loop
	(while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1))
	  (if (re-search-backward "[][{}()]" backpoint t)
	      (if (not (ml-inside-comment-or-string-p))
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
	      (looking-at ml-indent-starters-reg))
	    (1+ (+ (current-column) ml-indent-level))
	  (1+ (current-column)))))))

;; This is too slow
;;
;; (defun ml-inside-comment-or-string-p ()
;;  (let ((state (parse-partial-sexp (point-min) (point))))
;;    (or (nth 4 state) (nth 3 state))))

(defun ml-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (progn
		(search-backward "(*")
		(search-forward "*)")
		(forward-char -1)	; A "*)" is not inside the comment
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

(defun ml-skip-block ()
  (let ((case-fold-search nil))
    (ml-backward-sexp)
    (if (looking-at "end\\b")
	(progn
	  (goto-char (ml-find-match-backward "end" "\\bend\\b"
					      ml-end-starters-reg))
	  (skip-chars-backward "\n\t "))
      ;; Here we will need to skip backward past if-then-else
      ;; and case-of expression. Please - tell me how !!
      )))

(defun ml-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((case-fold-search nil)
	  (level 1)
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (ml-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun ml-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (ml-find-match-backward unquoted-this this match))
    (if (or ml-type-of-indent indented)
	(current-column)
      (if (progn
	    (beginning-of-line)
	    (skip-chars-forward "\t ")
	    (looking-at "|"))
	  (- (current-indentation) ml-pipe-indent)
	(current-indentation)))))

(defun ml-find-matching-starter (regexp)
  (let ((case-fold-search nil)
	(start-let-point (ml-point-inside-let-etc))
	(start-up-list (ml-up-list))
	(found t))
    (if (ml-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (ml-up-list))
			 (/= start-let-point (ml-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun ml-point-inside-let-etc ()
  (let ((case-fold-search nil) (last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (ml-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (ml-find-match-backward "end" "\\bend\\b"
						  ml-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))

(defun ml-re-search-backward (regexpr)
  (let ((case-fold-search nil) (found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (ml-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun ml-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun ml-backward-sexp ()
  (condition-case ()
      (progn
	(let ((start (point)))
	  (backward-sexp 1)
	  (while (and (/= start (point)) (looking-at "(\\*"))
	    (setq start (point))
	    (backward-sexp 1))))
    (error (forward-char -1))))

(defun ml-comment-indent ()
  (if (looking-at "^(\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	; Else indent at comment column
	   comment-column))))		; except leave at least one space.

;;; Do the user's customisation...

(defvar ml-load-hook nil
  "This hook is run when ML is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'ml-load-hook)
