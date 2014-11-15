
;CL-USER 20 > (fc-production-system);

;"I am trying the rules again." 
;"I inferred : (ELLEN SEES MARK EATING CAKE)" 
;"I inferred : (JOHN HAS CAKE INGREDIENTS)" 
;"I inferred : (CAKE CONTAINS POISONED INGREDIENTS)" 
;"I am trying the rules again." 
;"I inferred : (ELLEN EATS CAKE)" 
;"I inferred : (MARK DIES)" 
;"I inferred : (JOHN BAKES CAKE)" 
;"I am trying the rules again." 
;"I inferred : (ELLEN DIES)" 
;"I inferred : (JOHN EATS CAKE)" 
;"I am trying the rules again." 
;"I inferred : (BILLY SEES JOHN EATING CAKE)" 
;"I inferred : (JOHN DIES)" 
;"I am trying the rules again." 
;"I inferred : (BILLY EATS CAKE)" 
;"I am trying the rules again." 
;"I inferred : (BILLY DIES)" 
;"I am trying the rules again." 
;"There is nothing else to be inferred" 
;"Short-term memory at end : ((BILLY DIES) (BILLY EATS CAKE) (JOHN DIES) (BILLY SEES JOHN EATING CAKE) (JOHN EATS CAKE) (ELLEN DIES) (JOHN BAKES CAKE) (MARK DIES) (ELLEN EATS CAKE) (CAKE CONTAINS POISONED INGREDIENTS) (JOHN HAS CAKE INGREDIENTS) (ELLEN SEES MARK EATING CAKE) (MARK EATS CAKE) (JOHN HAS OVEN) (JOHN HAS FLOUR) (JOHN HAS EGGS) (JOHN HAS MILK) (JOHN HAS SUGAR) (CAKE CONTAINS INGREDIENT FLOUR) (CAKE CONTAINS INGREDIENT EGGS) (CAKE CONTAINS INGREDIENT MILK) (CAKE CONTAINS INGREDIENT SUGAR) (BREAD CONTAINS MILK) (FLOUR IS AN INGREDIENT) (EGGS IS AN INGREDIENT) (MILK IS AN INGREDIENT) (SUGAR IS AN INGREDIENT) (CAKE CONTAINS FLOUR) (CAKE CONTAINS EGGS) (CAKE CONTAINS MILK) (CAKE CONTAINS SUGAR) (MILK IS POISONED) (BILLY LIVES WITH JOHN) (ELLEN LIVES WITH MARK))" 


;;;provide the while iteration construct for the main fc-production-system function
(defmacro while (test &rest body)
  `(do ()
    ((not ,test))
    ,@body))

;;;;does the list contain the element, returns reliable T/F result
(defun contains (element list) 
  (cond
    ((equal list nil) 'F)
    ((equal element (first list)) 'T)
    (t (contains element (rest list)))))

;;;helper to match5
(defun test-binds (x v binds) 
;; returns nil or the binds updated by the addition of the pair(x v) 
    (let ( (y nil) )
      (setf y  (assoc x binds)) 
      (cond (y (cond ((equal(second y) v) binds) 
		     (t nil)))
	    (t (setf binds (append binds (list (list x v))))))))

;;ex. (match5 (fourth (first *rule-base*)) '((BLOCK A) ON (BLOCK B))) 
;;returns (T ((X A) (Y B)))
(defun match5 (p s &optional binds) 
  (let ( (temp nil) ) 
    (cond ((atom p) (cond 
		      ((equal p s) (list t binds)) 
		      (t (list nil nil)))) 
  ((equal (first p) '?)  (setf temp (test-binds (second p) s binds))
                         (cond (temp (list t temp)) (t (list nil nil)))) 
  ((atom s) (list nil nil)) 
   (t ; both p and s are lists 
                          (setf temp (match5 (first p) (first s) binds)) 
			  ;temp = (flag binds) 
			  (cond ((first temp) 
			  ; (first p) and (first s) match 
				 (match5 (rest p) (rest s) (second temp))) 
			  (t (list nil nil)))))))

;;;check if arg1 is linear, the first time that you find out it is not then return nil
(defun linearp (arg1)
  (cond 
    ;;case 1, if arg1 is an atom then it is linear
    ((atom arg1)             T)
    ;;since you know you would not have got here if arg1 was just an atom, it just be a list.
    ;;now you can chec k to see if the first element of the list is a list -- if so, return nil
    ;;otherwise it will get caught be low and recurse into the function again to check the remainder
    ;;of the list
    ((listp (first arg1))    nil)
    ;;if you have got to the end of arg1's list and it is now nil, then recursion is complete 
    ;;and you can return T to indicate you never found a list within the list's contents
    ((eql nil arg1)          T)
    ;;recurse with the remainder of the list if you got to this predicate
    ((linearp (rest arg1))   T)))

;;;create a function that can take an expression with inner expressions of the form (? VAR) and use a list of expressions
;;;of the form (VAR REPLACE-EXPRESSION) to bind REPLACE-EXPRESSION into the string.
(defun instantiate (orig-expr bind-var-list)
  (cond     
    ;;if the orig-expr is an atom and bind-var-list is nil then return the atom
    ((atom orig-expr)                    orig-expr)
    ;;if the bind-var-list is empty, return the original string
    ;;((eql nil bind-var-list)           nil)
    ;;if orig-expr is nil then there is nothing to bind
    ((eql nil orig-expr)                 orig-expr)
    ;;if the orig-expr passed in is linear and it is of the form (? VAR) then find VAR in the bind-var-list
    ;;and perfrorm replace-vals
    ((and
      (linearp orig-expr)
      (equal '? (first orig-expr)))      (cond
					   ;;the expression is of the correct form, but nothing matched so just
					   ;;return the orig-expr
					   ((eql bind-var-list nil)                     orig-expr)
					   ;;otherwise the bind-var-list is not nil so try to match and recurse if not
					   ;;with the remainder of the list
					   ((equal 
					     (rest orig-expr)
					     (list (first (first bind-var-list))))     (first (rest (first bind-var-list))))
					   ;;recurse with the remainder of bind-var-list until it is nil or a match is found
					   (t                                                          
					    (instantiate orig-expr (rest bind-var-list)))))
    ;;if the orig-expr list that has been provided is linear then we are also finished, regardless of the bind-var-list
    ;;because that means there are no bind-variable substitution expressions of the form (? VAR) in orig-expr
    ((linearp orig-expr)                 orig-expr)
    ;;passed in a list that the contains a list
    (t                                   (cons 
					  (instantiate (first orig-expr) bind-var-list) 
					  (instantiate (rest orig-expr) bind-var-list)))))


;;;;store the facts that the system starts with or are ascertained through 
;;;;the chaining procedure
;(defvar *short-term-memory*
;  '((PAM IS A PARENT OF BOB)
;    (BOB IS A PARENT OF ANN)
;    (ANN IS A PARENT OF TOM)
;    (TOM IS A PARENT OF LIZ)))

;;;;store the inference rules that will be used by the system to infer new facts
;(defvar *rule-base*
;  '((PREDECESSOR1
;     IF
;     ((? X) IS A PARENT OF (? Y))
;     THEN
;     ((? X) IS A PREDECESSOR OF (? Y)))
;    (PREDECESSOR2
;     IF
;     ((? X) IS A PARENT OF (? Y))
;     ((? Y) IS A PREDECESSOR OF (? Z))
;     THEN
;     ((? X) IS A PREDECESSOR OF (? Z)))
;))

;;;a nested fact example list
;(setq *short-term-memory*
;      '(((BLOCK A) ON (BLOCK B))
;	((BLOCK B) ON (BLOCK C))
;	((BLOCK C) ON (BLOCK E))
;	((BLOCK E) ON (BLOCK F))
;	((BLOCK D) ON (BLOCK F))
;	((BLOCK C) ON (BLOCK D))
;	((BLOCK C) ON (BLOCK F))
;	((BLOCK F) ON (BLOCK G)))
;)

(defvar *short-term-memory*
;(setq *short-term-memory*
      '(
	;(BILLY DIES)
	;(JOHN DIES)
	;(ELLEN DIES)
	;(MARK DIES)
	;(JOHN EATS CAKE)
	;(ELLEN EATS CAKE)
	;(MARK EATS CAKE)
	;(JOHN BAKES CAKE)
	(JOHN HAS OVEN)
	(JOHN HAS FLOUR)
	(JOHN HAS EGGS)
	(JOHN HAS MILK)
	(JOHN HAS SUGAR)
	;(CAKE CONTAINS POISONED INGREDIENTS)
	(CAKE CONTAINS INGREDIENT FLOUR)
	(CAKE CONTAINS INGREDIENT EGGS)
        (CAKE CONTAINS INGREDIENT MILK)
	(CAKE CONTAINS INGREDIENT SUGAR)
	(BREAD CONTAINS MILK)
	(FLOUR IS AN INGREDIENT)
	(EGGS IS AN INGREDIENT)
	(MILK IS AN INGREDIENT)
	(SUGAR IS AN INGREDIENT)
	(CAKE CONTAINS FLOUR)
	(CAKE CONTAINS EGGS)
	(CAKE CONTAINS MILK)
	(CAKE CONTAINS SUGAR)
	(MILK IS POISONED)
	(BILLY LIVES WITH JOHN)
	(ELLEN LIVES WITH MARK)
	)
)


;(setq *short-term-memory*
;      '(
;	;((BLOCK A) ON (BLOCK B))
;	;((BLOCK B) ON (BLOCK C))
;        ;((BLOCK B) ON (BLOCK D))
;	((BLOCK B) ON (BLOCK E))
;	((BLOCK C) ON (BLOCK E))
;	((BLOCK E) ON BOTTOM)
;       )
;)

;(setf *rule-base* 
;      '(
;	;(RULE1
;	; IF
;	; ((BLOCK (? X)) ON (BLOCK (? Y)))
;	; ((BLOCK (? Y)) ON (BLOCK (? Z)))
;	; THEN
;	; ((BLOCK (? X)) ON (BLOCK (? Z))))
;	(RULE2
;	 IF
;	 ((BLOCK (? X)) ON (BLOCK (? Z)))
;	 ((BLOCK (? Z)) ON BOTTOM)
;	 THEN
;	 ((? X) SEES (? Z) ON BOTTOM))
;	)
;)

(defvar *rule-base*
      '((RULE1
	 IF
	 ((BLOCK (? X)) ON (BLOCK (? Y)))
	 ((BLOCK (? Y)) ON (BLOCK (? Z)))
	 ((BLOCK (? Z)) ON (BLOCK (? A)))
	 ((BLOCK (? A)) ON (BLOCK (? B)))
	 THEN
	 ((BLOCK (? X)) ON (BLOCK (? Z))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE2
	 IF
	 ((? X) HAS OVEN)
	 ((? X) HAS CAKE INGREDIENTS)
	 THEN
	 ((? X) BAKES CAKE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE3
	 IF
	 ((? X) BAKES CAKE)
	 THEN
	 ((? X) EATS CAKE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE4
	 IF
	 ((? X) EATS (? Y))
	 ((? Y) CONTAINS POISONED INGREDIENTS)
	 THEN
	 ((? X) DIES))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE5
	 IF
	 ((? X) IS AN INGREDIENT)
	 ((? X) IS POISONED)
	 ((? Y) CONTAINS INGREDIENT (? X))
	 THEN
	 ((? Y) CONTAINS POISONED INGREDIENTS))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE6
	 IF
	 ((? X) HAS FLOUR)
	 ((? X) HAS EGGS)
	 ((? X) HAS SUGAR)
	 ((? X) HAS MILK)
	 THEN
	 ((? X) HAS CAKE INGREDIENTS))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE7
	 IF
	 ((? X) LIVES WITH (? Y))
	 ((? Y) EATS CAKE)
	 THEN
	 ((? X) SEES (? Y) EATING CAKE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(RULE8
	 IF
	 ((? X) SEES (? Y) EATING CAKE)
	 THEN
	 ((? X) EATS CAKE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defvar *rule-base* '(
; (setf *rule-base*   '(
	)
)


;;;;return the list of consequents for a particular rule
(defun get-rule-consequents (rule &optional consequents) 
  (cond
    ((equal (second rule) 'IF) (get-rule-consequents (rest rule)))
    ((equal (first rule) 'IF) (get-rule-consequents (rest rule)))
    ((equal (first rule) 'THEN) consequents)
    (t (cons (first rule) (get-rule-consequents (rest rule) consequents)))))

;;;;return the list of antecedents for a particular rule
(defun get-rule-antecedents (rule &optional seen-then)
  (cond
    ((equal nil rule) nil)
    ((equal (first rule) 'THEN) (get-rule-antecedents (rest rule) 'T))
    ((equal seen-then 'T) (cons (first rule) (get-rule-antecedents (rest rule) 'T)))
    (t (get-rule-antecedents (rest rule) 'F))))

;;;return the binding list if the fact does match the consequent, nil otherwise
(defun check-fact-against-consequent (consequent fact)
   (cond
     ((equal (first (match5 consequent fact)) 'T) (rest (match5 consequent fact)))
     (t nil)))

;;;return all facts that match this particular consequent
(defun get-matching-facts (consequent) 
  (let ((matching-facts))
    (dolist (fact *short-term-memory* matching-facts) 
      (cond
	((equal (check-fact-against-consequent consequent fact) nil) nil)
	(t (setq matching-facts (cons fact matching-facts)))))))


;;;;;;;;;;;;;;;
;;;;binding list helper, to continue the chain starting with a list of the remainder of the consequents and some starter chains.
(defun binding-list-helper (consequents chain)
  (let ((new-chains) (finished-chains))
    (cond
      ;;if there are no consequents, return the chain
      ((equal consequents NIL) (progn
	 ;(print "no consequents, chain = ") (print chain)
	 (setq new-chains chain)))
      ;;if there is no matching chain, return nil
      ((equal chain nil) NIL)
      ;begin trying to get a start on a new chain 
      (t (dolist (matching-fact (get-matching-facts (instantiate (first consequents) chain)) new-chains)
	   (progn 
	     ;(print "chain = ")(print chain)
	     ;(print "new match list = ")(print  (match5 (instantiate (first consequents) chain) matching-fact))
	     (cond 
	       ;;is there a match for the current predicate
	       ((equal nil (first (match5 (instantiate (first consequents) chain) matching-fact))) nil)
	       (t (setq new-chains
			(cons
			 (append chain
			  (first (rest (match5 (instantiate (first consequents) chain) matching-fact))))
			 new-chains))))
	     ;(print "new-chains = ")(print new-chains)
	     ))))
    (cond 
      ((equal consequents NIL) (progn 
	 ;(print "consequents nil for binding-list-helper lower side, returning new-chains.") (print new-chains)
	 new-chains))
      (t
       ;;here the recursion is performed and the completed chains are collected to returned for instantiation
       (dolist (new-chain new-chains finished-chains)
	 (progn
	   ;(print "new-chain = ")(print new-chain)
	   ;(print "finished-chains-before = ")(print finished-chains)
	   (cond 
	     ((equal (binding-list-helper (rest consequents) new-chain) nil) nil)
	     (t 
	      (cond
		;;;LAST((and
		;;;LAST  (equal (length new-chain) 1)
		;;;LAST  (linearp (first new-chain)))
		((and
		  (equal (length new-chain) 1)
		  (linearp (first new-chain)))
		 (setq finished-chains (append (binding-list-helper (rest consequents) new-chain) finished-chains))) 
		((and
		  (linearp (first new-chain))
		  (equal (length finished-chains) 0))
		 (progn ;(print "new-chain = ")(print new-chain)
			 ;;;LAST(setq finished-chains (append (binding-list-helper (rest consequents) new-chain) finished-chains)))
		 (setq finished-chains (cons (binding-list-helper (rest consequents) new-chain) finished-chains))))
		(t (setq finished-chains
		      ;;;;;;LAST(cons (binding-list-helper (rest consequents) new-chain) finished-chains))))))
			 (append (binding-list-helper (rest consequents) new-chain) finished-chains))))))
	   ;(print "finished-chains = ")(print finished-chains)))))))
	   ))))))
;;;;;;;;;;;;;;;

;;;;collect all of the solutions for the rule based on the current memory -- generates all the complete chains and returns them to fc-cycle to be instantiated.
(defun get-binding-lists (rule)
  (let (binding-lists)
    ;;;get all of the facts that match only the first consequent,
    ;;;then take that fact and make it the head of a chain of facts that will satisfy all of the conditions.
    (cond
      ;(
      ;((and
	;(equal 1 (length (get-matching-facts (get-rule-consequents rule))))
    ;;cases where the helper is not needed, alrady have complete matching facts set, otherwise the last case allows chains which require additional bindings to be evaluated by helper
	((equal 1 (length (get-rule-consequents rule)))
	 (cond
	    ((equal (binding-list-helper 
		     (rest (get-rule-consequents rule)) 
		     (first (rest (match5 (first (get-rule-consequents rule))  (first (get-matching-facts (first (get-rule-consequents rule)))))))) nil) nil)
	    (t (progn
		 ;(print "(one consequent)rule consequents = ")
		 ;(print (get-rule-consequents rule))
		 ;(print "(one consequent)matching facts = ")
		 ;(print (get-matching-facts (first (get-rule-consequents rule))))
		 (dolist (fact (get-matching-facts (first (get-rule-consequents rule))) binding-lists)
		 (setq binding-lists (append 
				      (cons (binding-list-helper 
					      (rest (get-rule-consequents rule))
					      (first (rest (match5 (first (get-rule-consequents rule)) 
								   fact)))) nil) 
				      binding-lists)))))))
	    ;;))
	(t
       (dolist (chain-head (get-matching-facts (first (get-rule-consequents rule))) binding-lists)
	 ;(print "chain-head = ")
	 ;(print chain-head)
	 (cond
	   ((equal (binding-list-helper (rest (get-rule-consequents rule)) (first (rest (match5 (first (get-rule-consequents rule)) chain-head)))) nil) nil)
	   (t (setq binding-lists
		    (append
					;(cons
	     ;;;pass on to the helper the remainder of the consequents and the initial binding lists from the head element to try
	     ;;;and create a complete chain for all of the rule's conditions
		     (binding-list-helper (rest (get-rule-consequents rule)) (first (rest (match5 (first (get-rule-consequents rule)) chain-head))))
		     binding-lists)))))))))

;;;take off extra parentheses incurred through recursion from binding-list-helper
(defun fix-binding-list-depth (binding-list) 
  (cond
    ;;if first of first is linear it is one binding variable in a list
    ((linearp (first (first binding-list))) binding-list)
    (t (fix-binding-list-depth (first binding-list)))))

;;;;perform one cycle through the rules, trying to get all of the binding lists and then instantiating the rule with the binding lists.
;;;;all of the facts are collected (including those already in the memory) and then the top-level program sorts through to find if any new facts have been found.
(defun fc-cycle () 
  (let ((new-facts))
    ;;;for each one of the rules, try to get binding lists to create new rules.
    (dolist (rule *rule-base* new-facts)	
      ;;there are some binding-lists, try to instantiate each one with the rule's THEN portion
      (unless (equal (get-binding-lists rule) nil)
	(dolist (binding-list (get-binding-lists rule))
	  (progn
	    ;(print "before instantiation, binding-list = ")
	    ;(print (fix-binding-list-depth binding-list))
	    (if (linearp binding-list)
		(setq binding-list (cons binding-list nil)))
	    ;(setq binding-list (cons binding-list nil))
	    ;(dolist (one-list binding-list)
	      (progn
		;;to handle case of only one binding list
		(if (linearp (first binding-list))
	           (setq binding-list (cons binding-list nil)))
	      ;(print "binding-list = ")
	      ;(print (fix-binding-list-depth binding-list))
	      ;(print "instantiated-list = ")
	      ;(print (instantiate (get-rule-antecedents rule) (first (fix-binding-list-depth binding-list))))
		;;;new rules were found, instantiate the rule with the chain and add to the new facts.
	      (if (equal 'F (contains (first (instantiate (get-rule-antecedents rule) (first (fix-binding-list-depth binding-list)))) new-facts))
		  (setq new-facts (append (instantiate (get-rule-antecedents rule) (first (fix-binding-list-depth binding-list))) new-facts))
		  (setq new-facts new-facts)
		))))))))
		;))))))))

;;;;develop a forward-chaining production system
;;;;Need to match the IF part of a rule with the facts in the short-term-memory using match5
;;;;If there is a successful match, the THEN part of the rule needs to have the vars bound
;;;;and the result added to the short-term-memory -- use the instantiate function.
;;;;Use a special variable to define a flag which may indicate whether the short-term
;;;;memory has been changed during the current cycle.
;;;;In each cycle:
;;;; * Try to apply each rule
;;;; * For each rule
;;;; * Need to match all the conditions
;;;; * Need to find all posssible matchings between the conditions and the STM.
;;;;   --This may produce a list of binding lists.
;;;; * For each binding list instantiate the conclusion of the rule and insert it
;;;;   --into the STM, but only if it is not already there.
(defun fc-production-system ()
  (let ((finished-flag 'F)(new-facts))
    (while (equal finished-flag 'F)
      (print "I am trying the rules again.")
      (setq finished-flag 'T)
      ;;;new facts are collected for checking against those already in the knowledge base.
      (setq new-facts (append (funcall 'fc-cycle) new-facts))
      (cond
	((equal new-facts nil) (setq finished-flag 'T))
	(t 
	 (dolist (new-fact new-facts *short-term-memory*)
	   ;(print "length new-facts = ")
	   ;(print (length new-facts))
	   ;(print "short-term-memory = ")
	   ;(print *short-term-memory*)
	   (if (equal 'F (contains new-fact *short-term-memory*))
	       (progn 
	       	 (setq finished-flag 'F)
		 (print (format nil "I inferred : ~A" new-fact))
		 (setq *short-term-memory* (append (cons new-fact nil) *short-term-memory*))))))))
    (print "There is nothing else to be inferred")
    (print (format nil "Short-term memory at end : ~A" *short-term-memory*))))
