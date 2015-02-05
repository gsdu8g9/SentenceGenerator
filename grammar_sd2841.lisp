(defparameter *grammar*
  '((sentence -> (main-clause) (main-clause subordinate-clause) (subordinate-clause predicate) (subordinate-clause main-clause)(main-clause coordinate-clause))
    (main-clause -> (subject predicate)(subject))
    (subordinate-clause -> (Sub-Conjunction main-clause)(Sub-Conjunction predicate))
    (coordinate-clause -> (Cord-Conjunction main-clause))
    (subject -> (noun-phrase))
    (predicate -> (verb-phrase-present noun-phrase preposition-phrase) (verb-phrase-present preposition-phrase) (verb-phrase-present noun-phrase) (verb-phrase-past noun-phrase preposition-phrase) (verb-phrase-past preposition-phrase) (verb-phrase-past noun-phrase))
    (noun-phrase -> Noun (Adjective Noun)  (Article Adjective Noun) (Article Noun) (Pronoun Adjective Noun) (Pronoun Noun) (Pronoun) (Compound-Noun) (Article Compound-Noun) (Adjective Compound-Noun) (Article Adjective Compound-Noun)(Article Adjective-Number Noun-Plural) (Adjective-Number Noun-Plural) (Adverb-like-Noun)(compound-adjective Noun-Plural) (noun-phrase Cord-Conjunction-and noun-phrase)(Adjective Noun-Plural)(Noun-Plural))
    (verb-phrase-present -> (Verb-Present) (Auxilary Verb-Present) (Adverb Verb-Present) (Verb-Present Adverb) (Auxilary Verb-Present Adverb)(Auxilary Adverb Verb-Present) (Auxilary Negation Verb-Present)(Auxilary Imperative-be Adjective Infinitive Verb-Present)(compound-verb Adverb))
    (verb-phrase-past -> (Verb-Past) (Auxilary Verb-Past) (Adverb Verb-Past) (Verb-Past Adverb) (Auxilary Verb-Past Adverb) (Auxilary Adverb Verb-Past)(Auxilary Negation Verb-Past)(Auxilary Imperative-be Verb-Past))
    (preposition-phrase -> (Preposition) (Preposition noun-phrase) (Adjective Preposition noun-phrase)(Premodifier Preposition noun-phrase) )
    (infinitive-verb -> (Infinitive Verb-Present))
    (compound-verb -> (Verb-Past infinitive-verb))
    (Compound-Noun -> (Noun Noun)(Noun Noun-Plural) )
    (compound-adjective -> (Adjective Cord-Conjunction-and Adjective))
   
    (Noun-Plural -> Astronomers Telescopes gains Industries services Weeks)
    (Noun -> Education Narrative Polarization Strategy Intent Asteroid Eye Prosecution Death Penalty Case Prosecution job Health Care)
    (Adjective -> Higher Prevailing Flawed Desired Visible Naked Amateur Able Solid business professional)
    (Adjective-Number -> few several)
    (Article -> The a)
    (Verb-Present -> Counter Achieve be see Seek pay)
    (Verb-Past -> decided tend were)
    (Auxilary -> Must Will Should)
    (Preposition -> of to with in as)
    (Pronoun -> This It)
    (Adverb -> Hardly Will Well)
    (Adverb-like-Noun -> there)
    (Negation -> Not)
    (Cord-Conjunction -> and but)
    (Cord-Conjunction-and -> and)
    (Sub-Conjunction -> that whether)
    (Infinitive -> to)
    (Imperative-be -> be)
    (Premodifier -> such)
)
  "A grammar for a trivial subset of English.")

(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order

;(HIGHER EDUCATION MUST COUNTER THE PREVAILING NARRATIVE OF POLARIZATION>
(defparameter rules1 '( (sentence 0)(main-clause 0) (subject 0) (noun-phrase 1) (Adjective 0) (Noun 0) (predicate 0) (verb-phrase-present 1) (Auxilary 0) (Verb-Present 0) (noun-phrase 2) (Article 0) (Adjective 1) (Noun 1) (preposition-phrase 1) (Preposition 0) (noun-phrase 0) (Noun 2))) 

;(THIS FLAWED STRATEGY WILL HARDLY ACHIEVE THE DESIRED INTENT)
(defparameter rules2 '( (sentence 0)(main-clause 0) (subject 0) (noun-phrase 4) (Pronoun 0) (Adjective 2) (Noun 3) (predicate 2) (verb-phrase-present 5) (Auxilary 1) (Adverb 0) (Verb-Present 1) (noun-phrase 2)(Article 0) (Adjective 3) (Noun 4)))

;(THERE WERE SOLID JOB GAINS IN SEVERAL INDUSTRIES THAT TEND TO PAY WELL SUCH AS BUSINESS AND PROFESSIONAL SERVICES AND HEALTH CARE)
(defparameter rules3 '( (Sentence 1)(main-clause 0) (subject 0) (noun-phrase 13)(Adverb-like-Noun 0) (predicate 3) (verb-phrase-past 0) (verb-past 2) (noun-phrase 9)(Adjective 8) (Compound-Noun 1) (Noun 12) (Noun-Plural 2) (preposition-phrase 1)(Preposition 3) (Noun-Phrase 12) (Adjective-Number 1) (Noun-Plural 3)(subordinate-clause 1) (Sub-Conjunction 0)(predicate 1)(verb-phrase-present 8)(compound-verb 0)(Verb-Past 1)(infinitive-verb 0)(Infinitive 0)(Verb-Present 5)(Adverb 2)(preposition-phrase 3)(Premodifier 0) (Preposition 4)(noun-phrase 15)(noun-phrase 14)(compound-adjective 0) (Adjective 9) (Cord-Conjunction-and 0) (Adjective 10) (Noun-Plural 4)(Cord-Conjunction-and 0)(noun-phrase 7)(Compound-Noun 0)(Noun 13)(Noun 14)
))

;(THE ASTEROID WILL NOT BE VISIBLE TO THE NAKED EYE BUT AMATEUR ASTRONOMERS SHOULD BE ABLE TO SEE IT WITH TELESCOPES)
(defparameter  rules4 '( (sentence 4)(main-clause 0) (subject 0) (noun-phrase 3) (Article 0) (Noun 5) (predicate 1) (verb-phrase-present 6) (Auxilary 1) (Negation 0) (Verb-Present 2) (preposition-phrase 2) (Adjective 4) (Preposition 1) (noun-phrase 2) (Article 0) (Adjective 5) (Noun 6)(coordinate-clause 0) (Cord-Conjunction 1) (main-clause 0) (subject 0) (noun-phrase 16) (Adjective 6) (Noun-Plural 0) (Predicate 0) (verb-phrase-present 7) (Auxilary 2) (Imperative-be 0) (Adjective 7) (Infinitive 0) (Verb-Present 3) (noun-phrase 6) (Pronoun 1)(preposition-phrase 1) (Preposition 2) (noun-phrase 17) (Noun-Plural 1)))

;(WHETHER THE PROSECUTION WILL SEEK THE DEATH PENALTY IN THE CASE WILL BE DECIDED IN A FEW WEEKS)
(defparameter rules5 '( (sentence 2)(subordinate-clause 0) (Sub-Conjunction 1) (main-clause 0) (Subject 0) (noun-phrase 3) (Article 0)(Noun 7) (predicate 0) (verb-phrase-present 1) (Auxilary 1) (Verb-Present 4)(noun-phrase 8)(Article 0)(Compound-Noun 0)(Noun 8)(Noun 9) (preposition-phrase 1)(Preposition 3) (noun-phrase 3)(Article 0) (Noun 10) (predicate 4) (verb-phrase-past 7) (Auxilary 1) (Imperative-be 0) (Verb-Past 0) (preposition-phrase 1) (Preposition 3) (noun-phrase 11) (Article 1) (Adjective-Number 0) (Noun-Plural 5)))

;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)

(defun apply-rules (rules sentence)
  (cond 
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
   (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule))))))

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t 
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))
      

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun random-sentence (phrase)
  "Generate a random sentence or phrase"
  (cond 
	((listp phrase)
         (mappend #'random-sentence phrase))
        ((rewrites phrase)
         (random-sentence (random-elt (rewrites phrase))))
        (t (list phrase))
  )
 )

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun generate-tree2 (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (progn (list phrase)
		(princ phrase)
		)
)))


(setq depth 0)

(defun generate-tree1 (phrase depth)
   "Generate a random sentence or phrase,
   with a complete parse tree."
	(if (equal depth 0)
		(return-from generate-tree1)
  
  		(progn 
			(setq depth (+ depth 1))
    			(cond 
				((listp phrase)
         				(mapcar #'generate-tree phrase depth))
        			((rewrites phrase)
         				(cons phrase (generate-tree (random-elt (rewrites phrase)) depth )))
        			(t (list phrase))
			)
		)
	)
)


(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))
			
(random-sentence 'sentence)

; Function to write to file
(defun write-to-file (sentence file_name)
  (with-open-file (str file_name
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str sentence)))
(setq counter 0)
(defun run2 ()
  (let ((sent (random-sentence 'sentence)))
     (if (validp sent)
        (progn 
		(write-to-file (format nil "+ ~S ~%" sent) "~/hidden/41033217102991374/Project1/all_sentences_generated.txt")
		(write-to-file (format nil "~S ~%" sent) "~/hidden/41033217102991374/Project1/accepted_sentences.txt")
	)
        (progn
                (write-to-file (format nil "- ~S ~%" sent) "~/hidden/41033217102991374/Project1/all_sentences_generated.txt" )
		(if (< counter 101)
                	(progn 
				(write-to-file (format nil "~S ~%" sent)  "~/hidden/41033217102991374/Project1/q2_sd2841.txt" )
				(setq counter (+ counter 1))
			)
			(return-from run2)
		)
        )
      )
   )
)
; Function to run a loop around the run2 function that writes to file the accepted and rejected sentences
(defun loop-run (N)
  (loop for i from 1 to N do (run2)))

; The function to generate N number of random sentences and write in the to file "q1_sd2841.txt".
(defun loop-run-random (N)
 (loop for i from 1 to N do (write-to-file (format nil "~S ~%" (random-sentence 'sentence)) "~/hidden/41033217102991374/Project1/q1_sd2841.txt")))

; Function that returns the list of repeated words
(defun repeated (lst)
  (repeatedr lst '()))

(defun repeatedr (lst result)
  (if (null lst)
    result
    (if (member (first lst) (rest lst))
      (repeatedr (rest lst) (adjoin (first lst) result))
      (repeatedr (rest lst) result)
    )
  )
)

; Variable containing all the valid words
(setq valid-words (append
        (rule-rhs (assoc 'Premodifier *grammar*))
        (rule-rhs (assoc 'Imperative-be *grammar*))
        (rule-rhs (assoc 'Infinitive *grammar*))
        (rule-rhs (assoc 'Preposition *grammar*))
        (rule-rhs (assoc 'Auxilary *grammar*))
        (rule-rhs (assoc 'Article *grammar*))
        (rule-rhs (assoc 'Cord-Conjunction *grammar*))
        (rule-rhs (assoc 'Sub-Conjunction *grammar*))
        (rule-rhs (assoc 'Pronoun *grammar*))
))
; REJECTION-RULE 1: Function checking if there are any duplicates in the sentence other than the valid-words list
(defun check-duplicates (sentn)
 (progn
	
	(setq flag_duplicate 1)
	(setq duplicate-words (repeated sentn))
	(loop while (and duplicate-words (= flag_duplicate 1))
		do (progn (if (member (car duplicate-words) valid-words)
			(setq flag_duplicate 1)
			(setq flag_duplicate 0)
		)
		(setq duplicate-words (cdr duplicate-words))
	))
 )
 (return-from check-duplicates (values flag_duplicate))
)
	
; REJECTION-RULE 2: Function to check the length of sentence is greater than 10
(defun check-length (lst)

	( < (length lst) 20)
)

;REJECTION-RULE 3: Function to check if the length of the sentence is less that 1 and belonging to the valid-wordslist then reject it
(defun check-single-words (sentence)
	(if ( < (length sentence) 2)
		(if (member (car sentence) valid-words)
			(return-from check-single-words (values t))
			(return-from check-single-words (values nil))
		)
	)
)


;REJECTION-RULE 4: The function to check that the variables must not end with a word that belong to the valid-words
(defun check-end-word (sentence)
	(if( member (car (last sentence)) valid-words)
		(return-from check-end-word (values nil))
		(return-from check-end-word (values t))
	)	
)


;REJECTION-RULE 5: The function to check if the two consecutive words in the sentence are not the same
(defun check-consecutive (sentence)
	(progn 
	  (setq flag 0)
	  (setq phrase sentence)
	  (loop while (> (length phrase) 0) do
		(progn 
		   ( setq first_word (first phrase))
		   ( setq sec_word (first (rest phrase)))
		   ( if	(equal first_word sec_word)
			 	(setq flag 1))
		   ( setq phrase (rest phrase))
		)
	  )
	  (if ( = flag 0)
		(return-from check-consecutive (values t))
		(return-from check-consecutive (values nil))
	  )
	)
)

; Check if the sentence is valid or not for all the conditions
(defun validp (sentn)
  (if (check-length sentn)
	(if ( = (check-duplicates sentn) 1)
		;(if (equal (check-consecutive sentn) nil)
			(if (equal (check-single-words sentn) nil)
				(not (equal (check-end-word sentn) nil))
			)
	;	)
	)	
  )
)


