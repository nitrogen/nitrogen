(define-derived-mode nitrogen-mode erlang-mode "Nitrogen!"
  "Major mode for editing Nitrogen files."
  ;;   (set (make-local-variable 'erlang-calculate-stack-indents) 'nitrogen-calculate-stack-indent)
  (set (make-local-variable 'indent-line-function) 'nitrogen-indent-line)
  (set (make-local-variable 'indent-region-function) 'nitrogen-indent-region)
  (setq erlang-indent-level 4))


(defun nitrogen-indent-line ()
  (let ((result))
    (fset 'old-erlang-calculate-stack-indent (symbol-function 'erlang-calculate-stack-indent))
    (fset 'old-erlang-comment-indent (symbol-function 'erlang-comment-indent))
    (fset 'erlang-calculate-stack-indent (symbol-function 'nitrogen-calculate-stack-indent))
    (fset 'erlang-comment-indent (symbol-function 'nitrogen-comment-indent))
    (setq result (erlang-indent-line))
    (fset 'erlang-calculate-stack-indent (symbol-function 'old-erlang-calculate-stack-indent))
    (fset 'erlang-comment-indent (symbol-function 'old-erlang-comment-indent))
    result))

(defun nitrogen-indent-region (beg end)
  (let ((result))
    (fset 'old-erlang-calculate-stack-indent (symbol-function 'erlang-calculate-stack-indent))
    (fset 'old-erlang-comment-indent (symbol-function 'erlang-comment-indent))
    (fset 'erlang-calculate-stack-indent (symbol-function 'nitrogen-calculate-stack-indent))
    (fset 'erlang-comment-indent (symbol-function 'nitrogen-comment-indent))
    (setq result (erlang-indent-region beg end))
    (fset 'erlang-calculate-stack-indent (symbol-function 'old-erlang-calculate-stack-indent))
    (fset 'erlang-comment-indent (symbol-function 'old-erlang-comment-indent))
    result))

(defun erlang-looking-at-closing-token ()
  "Returns t if currently positioned after a closing token, and moves to next token."
  (while (looking-at "[\s\t].*") (forward-char))
  (cond
   ((looking-at "[\]\)\}]$") (forward-char) t)
   ((looking-at "[\]\)\}].*") (forward-char) t)
   ((looking-at "catch$") (forward-char 5) t)   
   ((looking-at "catch[,\.; ].*") (forward-char 5) t)
   ((looking-at "after$") (forward-char 5) t)
   ((looking-at "after[,\.; ].*") (forward-char 5) t)
   ((looking-at "end$") (forward-char 3) t)
   ((looking-at "end[,\.;\) ]") (forward-char 3) t)
   ((looking-at "\|\|$") (forward-char 2) t)
   ((looking-at "\|\| ") (forward-char 3) t)
   (t nil)))


(defun nitrogen-calculate-stack-indent (indent-point state)
  (let* ((stack (and state (car state)))
	 (token (nth 1 state))
	 (stack-top (and stack (car stack))))
    (cond 
     ;; No state
     ((null state) 0)

     ;; First line of something, or a guard
     ((null stack)
      (if (looking-at "when[^_a-zA-Z0-9]")
	  erlang-indent-guard 0))

     ;; Inside of a block
     (t
      ;; Count the number of indention points on distinct lines
      (let 
	  ((incount 0)
	   (collapsed-incount 0)
	   (outcount 0))
	
	;; Count how many levels we are nested.
	;; Put into collapsed-incount
	(save-excursion 
	  (back-to-indentation)
	  (let ((last-line (line-number-at-pos))
		(tstack stack))    
	    (while (not (null tstack))
	      (let* 
		  ((current-pos (nth 1 (car tstack)))
		   (current-line (line-number-at-pos current-pos)))
		(if (not (= last-line current-line))
		    (setq collapsed-incount (1+ collapsed-incount)))
		(setq incount (1+ incount))
		(setq last-line current-line)
		(setq tstack (cdr tstack))))))
	  
	;; Move through outdention points. Increment the outdent 
	;; counter for each distinct line jump.
	(save-excursion
	  (back-to-indentation)
	  (let ((last-line (line-number-at-pos))
		(tstack stack))

	    (while (erlang-looking-at-closing-token)
	      (let* ((current-pos (nth 1 (car tstack)))
		     (current-line (line-number-at-pos current-pos)))
		;; If we've moved to a different physical line, then outdent one.
		(if (not (= last-line current-line))
		    (setq outcount (1+ outcount)))
		
		;; If the previous level is an 'icr, and it's on a different line, then 
		;; outdent one place
		(let ((one-back (car (cdr tstack))))
		  (if (and 
		       (or
			(eq 'icr (nth 0 one-back))
			(and			 
			 (eq 'try (nth 0 one-back))
			 (not (eq 'try (nth 0 (car tstack))))))
		       (not (= current-line (line-number-at-pos (nth 1 one-back)))))		      
		      (setq outcount (1+ outcount))))
	    
		(setq last-line current-line)
		(setq tstack (cdr tstack)))))
	  
	  ;; If this is a double pipe, then back up one.
	  (if (eq '|| (nth 0 (car stack)))
	      (setq outcount (1+ outcount))))
	
	;; Return indention
;; 	(message "Stack: %S collapsed-incount: %i incount: %i outcount: %i column: %i" stack collapsed-incount incount outcount (* erlang-indent-level (- collapsed-incount outcount)))
	(let 
	    ((indent (* erlang-indent-level (- collapsed-incount outcount))))
	  (back-to-indentation)
	  indent))))))



(defun nitrogen-comment-indent ()
  (cond ((looking-at "%%%") 0)
	(t
	 (or (erlang-calculate-indent)
	     (current-indentation)))))

;; (add-to-list 'auto-mode-alist '("\\.erl\\'" . nitrogen-mode))
;; (add-to-list 'auto-mode-alist '("\\.hrl\\'" . nitrogen-mode))


(provide 'nitrogen-mode)