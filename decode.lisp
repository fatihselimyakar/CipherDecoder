; ********************************************* 
; *  Author: Fatih Selim Yakar                *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

;; constants
(defparameter DICTIONARY "dictionary2.txt")
(defparameter DOCUMENT "document1.txt")

;Reads the documents as list in character list.
(defun read-as-list (filename)
	(let ((word-list (list )))
		(with-open-file (file-stream filename)
			(when file-stream
				(loop for line = (read-preserving-whitespace file-stream nil)
					while line 
						do 
						(setq word-list (append word-list (list (coerce (string-downcase (string line)) 'list)))))
				
				(close file-stream)
			)
		)
		word-list
	)
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;makes new alphabet by using random function.Used in encoder
(defun make-new-alphabet()
	(let ((alphabet (make-list 26 :initial-element '0)) (current nil))
		(loop for i from 0 to 25 
			do 
			(setf current (i2c (random 26 (make-random-state t))))
			(if (equal (position current alphabet :test #'equal) nil)
				(setf (nth i alphabet) current)
			(decf i 1)
			)
		)
		alphabet
	)
)

;encodes the given string and writes given "filename".txt
(defun encode-and-write-file(string filename)
	(let ( (alphabet (make-new-alphabet )) (return-list (make-list (length string))) )
		(with-open-file (write-file filename :direction :output)
			(loop for i from 0 to (- (length string) 1)
				do
				(if (>= (c2i (char string i)) 0)
					(setf (nth i return-list) (nth (c2i (char string i)) alphabet))
				(setf (nth i return-list) #\Space)
				)	
			)
			(format write-file (coerce return-list 'string) )
		)
		(coerce return-list 'string)
	)
)

;reads the document for character counts in the document.Using together change-count-to-letter function for find letter frequency.
(defun find-char-counts (filename)
	(let ((list (make-list 26 :initial-element '0)))
		(with-open-file (file-stream filename)
			(when file-stream
				(loop for char = (read-char file-stream nil)
					while char 
						do 
						(if (and (/= (c2i char) -87) (/= (c2i char) -65))
							(setf (nth (c2i char) list) (incf (nth (c2i char) list) 1))
						)
				)
				(close file-stream)
			)
			list
		)
	)
)

;Changes the counts to letter in given list.Used for letter frequency.
(defun change-count-to-letter(list)
	(let ((max nil) (letter-list '(#\e #\t #\a #\o #\i #\n)) (return-list (make-list 26 :initial-element '0)))
		(loop for i from 0 to 5
			do
			(setq max (reduce #'max list))
			(when (/= max 0)
				(position max list :test #'equal)
				(setf (nth (position max list :test #'equal) return-list) (nth i letter-list))
				(setf (nth (position max list :test #'equal) list) 0)
			)
		)
		return-list
	)
)

;creates map using frequency analysis for 6 letter.Used in Gen-Decoder-B-0,1
(defun add-freq-letter-to-map(freq-list)
	(let ((return-map (make-hash-table :test 'equal)) (current nil))
		(loop for i from 0 to 25 
			do 
			(if (not (typep (nth i freq-list) 'integer))
				(setf (gethash (i2c i) return-map) (nth i freq-list))
			)
		)
		return-map
	)
)

;creates map that includes number of word recurrences in the paragraph
(defun create-freq-word-map(paragraph)
	(let ( (return-map (make-hash-table :test 'equal)) )
		(loop for element in paragraph
			do
			(if (equal (gethash element return-map) nil)
				(setf (gethash element return-map) 1)
			(incf (gethash element return-map) 1)
			)
		)
		return-map
	)
)

;Returns the word(key) that is maximum recurrences in the paragraph
(defun max-values-key(map)
	(let ((max-key nil))
		(loop for element being each hash-key of map
			do
			(if (equal max-key nil)
				(setq max-key element)
			)
			(if (< (gethash max-key map) (gethash element map))
				(setq max-key element)
			)
		)
		max-key
	)
)

;Finds the decoded word from semi decoded word.
(defun find-decoded-word(file-map semi-dec-word)
	(let ((flag nil))
		(loop for element being each hash-value of file-map
			do
			(setq flag t)
			(when (equal (length element) (list-length semi-dec-word))
				(loop for i from 0 to (- (length element) 1)
					do
					(if (not (equal (nth i semi-dec-word) nil))
						(if (not (equal (nth i semi-dec-word) (char element i)))
							(setq flag nil)
						)
					)
				)
				(if flag (return-from find-decoded-word element))
			)
		)
	)
)

;control is there same word in the list.If there are then returns T, if there are not returns nil
(defun spell-checker-0 (word filename)
	(with-open-file (file-stream filename)
		(when file-stream
			(loop for line = (read-line file-stream nil)
				while line 
					do 
					(if (equal line word) 
						(return-from spell-checker-0 T) 
					)
			)
			(close file-stream)
			nil
		)
	)
)

;creates a hash table and maps as key:hashcode,value:dictionary's word 
(defun spell-checker-1-helper (filename) 
	(let ((hmap (make-hash-table :test 'equal)))
		(with-open-file (file-stream filename)
			(when file-stream
				(loop for line = (read-line file-stream nil)
					while line 
						do 
						(setf (gethash (sxhash line) hmap) line)
				)
				(close file-stream)
			)
		)
		hmap
	)
)

;Controls is there stated word in the map.If there is returns t, else returns nil 
(defun spell-checker-1 (word hmap)
	(equal (gethash (sxhash word) hmap) word)
)

;Returns the number of word's different letters.
(defun letter-of-word(word)
	(let ((counter 0) (char nil) (list (list )))
		(loop for i from 0 to (- (list-length word) 1)
			do
			(setq char (pop word))
			(if (equal (position char word :test #'equal) nil)
				(setq list (append list (list char)))
			)
		)
		list
	)
)

;take the all of permutations for given list and returns list of list.
(defun all-permutations (list)
  	(cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar 
			 			(lambda (l) (cons element l))
                        (all-permutations (remove element list))
					)
			)
		)
	)
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

;Decodes the paragraph with only brute force
;Outer loop traverses the paragraphs's words one by one. In word takes the combination of the word according to the number of different letters. It takes permutations in the received combination and traverses that list and tries mappings.The new mapped word is checked through the spell-checker to see if it exists in the dictionary. Saves the list if available and meets the requirements, and finally returns the list of checked words
(defun Gen-Decoder-A (paragraph)
	(let ((dict-word (list )) (hmap (spell-checker-1-helper DICTIONARY)) (return-list (list )) (map (make-hash-table :test 'equal)) (mapped-word nil) (words-letter nil) (restricted-alphabet nil) (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
		(loop for i from 0 to (- (list-length paragraph) 1)
			do
			;gets the paragraph's ith element
			(setq word (nth i (copy-tree paragraph)))
			;if there is overlap the word's letter in hash-table,maps
			(setf mapped-word (make-list (list-length word)))
			(loop for m from 0 to (- (list-length word) 1)
				do
				(when (not (equal (gethash (nth m word) map) nil))
					(setf (nth m mapped-word) (gethash (nth m word) map))
				)
			)
			;gets the difference mapped letters map and words-letter
			(setq words-letter (letter-of-word word))
			(loop for element in words-letter
				do
				(if (not (equal (gethash element map) nil))
					(setq words-letter (remove element words-letter))
				)
			)
			;gets the difference mapped letters map and alphabet:burda hata buldum
			(setq restricted-alphabet (copy-tree alphabet))
			(loop for element in restricted-alphabet
				do
				(if (not (equal (gethash element map) nil))
					(setq restricted-alphabet (remove (gethash element map) restricted-alphabet))
				)
			)
			(setq m (list-length words-letter))

			;Getting combination for map the word
		  	(if (not (= m 0))
				(let ( (perm-list nil) (mini-map (make-hash-table :test 'equal)) (words-letter (copy-tree words-letter)) (flag nil) (control nil))
					(setq dict-word nil)
					(labels ((comb1 (l c m)
								(when  (and (>= (length l) m) (not control)) 
									(loop for e from 0 to (- (list-length perm-list) 1)
										do
										(loop for j from 0 to (- (list-length (nth e perm-list)) 1)
											do
											(setf (gethash (nth j words-letter) mini-map) (nth j (nth e perm-list)))
	
										)
										;creates a map with current permutations.
										(let ((new-mapped-word (copy-tree mapped-word)))
											(loop for m from 0 to (- (list-length word) 1)
												do
												(when (equal (nth m mapped-word) nil)
													(setf (nth m new-mapped-word) (gethash (nth m word) mini-map))
												)
											)
											;where permutated map is placed.											
											(when (setq control (spell-checker-1 (coerce new-mapped-word 'string) hmap))
												(loop for el being each hash-key of mini-map
													do
													(setf (gethash el map) (gethash el mini-map))
												)
												(setq dict-word new-mapped-word)
												(return-from comb1 new-mapped-word)
											)
										)
									)
									;Get the all permutations combined map
									( if (zerop m) ( return-from comb1 (setq perm-list (all-permutations c)) ) )
									(comb1 (cdr l) c m)
									(comb1 (cdr l) (cons (first l) c) (1- m))
								)
							))
						(comb1 restricted-alphabet nil m)
					)
				)
				(if (spell-checker-1 (coerce mapped-word 'string) hmap)
					(setq dict-word mapped-word)
				(setq dict-word nil)
				)
			)
			(if (equal dict-word nil)
				(setq return-list (concatenate 'string return-list "NIL "))
			(setq return-list (concatenate 'string return-list (coerce dict-word 'string) " "))	
			)
			 
		)
		return-list
		
	)
)

;decodes the paragraph with frequency analysis and brute force.
;Function have the same operations as the Gen-Decoder-A but alphabet addition according to frequency analysis. 
;Firstly it does frequency analysis and adds the found letters in map, whereupon runs bruteforce algorithm. 
(defun Gen-Decoder-B-0 (paragraph)
	(let ( (dict-word (list )) (hmap (spell-checker-1-helper DICTIONARY)) (return-list (list )) (map (add-freq-letter-to-map (change-count-to-letter (find-char-counts DOCUMENT)))) (mapped-word nil) (words-letter nil) (restricted-alphabet nil) (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
		(loop for i from 0 to (- (list-length paragraph) 1)
			do
			;gets the paragraph's ith element
			(setq word (nth i (copy-tree paragraph)))
			;if there is overlap the word's letter in hash-table,maps
			(setf mapped-word (make-list (list-length word)))
			(loop for m from 0 to (- (list-length word) 1)
				do
				(when (not (equal (gethash (nth m word) map) nil))
					(setf (nth m mapped-word) (gethash (nth m word) map))
				)
			)
			;gets the difference mapped letters map and words-letter
			(setq words-letter (letter-of-word word))
			(loop for element in words-letter
				do
				(if (not (equal (gethash element map) nil))
					(setq words-letter (remove element words-letter))
				)
			)
			;gets the difference mapped letters map and alphabet:burda hata buldum
			(setq restricted-alphabet (copy-tree alphabet))
			(loop for element in restricted-alphabet
				do
				(if (not (equal (gethash element map) nil))
					(setq restricted-alphabet (remove (gethash element map) restricted-alphabet))
				)
			)
			(setq m (list-length words-letter))

			;Getting combination for map the word
		  	(if (not (= m 0))
				(let ( (perm-list nil) (mini-map (make-hash-table :test 'equal)) (words-letter (copy-tree words-letter)) (flag nil) (control nil))
					(setq dict-word nil)
					(labels ((comb1 (l c m)
								(when  (and (>= (length l) m) (not control)) 
									(loop for e from 0 to (- (list-length perm-list) 1)
										do
										(loop for j from 0 to (- (list-length (nth e perm-list)) 1)
											do
											(setf (gethash (nth j words-letter) mini-map) (nth j (nth e perm-list)))
	
										)

										;creates a map with current permutations.
										(let ((new-mapped-word (copy-tree mapped-word)))
											(loop for m from 0 to (- (list-length word) 1)
												do
												(when (equal (nth m mapped-word) nil)
													(setf (nth m new-mapped-word) (gethash (nth m word) mini-map))
												)
											)
											;where permutated map is placed.											
											(when (setq control (spell-checker-1 (coerce new-mapped-word 'string) hmap))
												(loop for el being each hash-key of mini-map
													do
													(setf (gethash el map) (gethash el mini-map))
												)
												(setq dict-word new-mapped-word)
												(return-from comb1 new-mapped-word)
											)
											
										)
									)
									;Get the all permutations combined map 
									( if (zerop m) ( return-from comb1 (setq perm-list (all-permutations c)) ) )
									(comb1 (cdr l) c m)
									(comb1 (cdr l) (cons (first l) c) (1- m))
								)
							))
						(comb1 restricted-alphabet nil m)
					)
				)
				(if (spell-checker-1 (coerce mapped-word 'string) hmap)
					(setq dict-word mapped-word)
				(setq dict-word nil)
				)
			)
			(if (equal dict-word nil)
				(setq return-list (concatenate 'string return-list "NIL "))
			(setq return-list (concatenate 'string return-list (coerce dict-word 'string) " "))
			)	
		)
		return-list
		
	)
)

;Firstly saves the letters in map that is find in frequency analysis.Then finds words by looking at the number of repetitions. Changes the letters in the words according to frequency analysis.Looking at the letters of this semi-decoded word, it finds a match in the dictionary.The new letters in the found word are saved in the map. And so on until the paragraph ends.
(defun Gen-Decoder-B-1 (paragraph)
  	(let ( (return-list (list )) (max-rec-word (list )) (return-word nil) (semi-dec-word (list )) (count-map (create-freq-word-map paragraph)) (file-map (spell-checker-1-helper DICTIONARY)) (map (add-freq-letter-to-map (change-count-to-letter (find-char-counts DOCUMENT)))) )
		(loop for i from 0 to (- (hash-table-count count-map) 1)
			do
			(setq max-rec-word (max-values-key count-map))
			(setq semi-dec-word (make-list (list-length max-rec-word)))
			(loop for j from 0 to (- (list-length max-rec-word) 1)
				do
				(setf (nth j semi-dec-word) (gethash (nth j max-rec-word) map))
			)
			
			(when (not (equal (setq return-word (find-decoded-word file-map semi-dec-word)) nil))
				(setq return-list (concatenate 'string return-list return-word " "))
				(loop for k from 0 to (- (list-length semi-dec-word) 1)
					do
					(if (equal (nth k semi-dec-word) nil) 
						(setf (gethash (nth k max-rec-word) map) (char return-word k))
					)
				)
			)

			(remhash max-rec-word count-map)
		)
		return-list
	)
)

;Runs decoders on a paragraph.
(defun Code-Breaker (document decoder)
  	(funcall decoder (read-as-list document))
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(let ( (map (spell-checker-1-helper DICTIONARY)))
		(format t "~%Encoded paragraph: ~A" (encode-and-write-file "little girl live site love" DOCUMENT));directly create DOCUMENT.txt and decode this string. If do not want delete this then normally runs.
		
		(setq doc (read-as-list DOCUMENT))
		;read-as-list test the current DOCUMENT
		(format t "~%read-as-list: ~A" doc)
		
		;spell checker 0 test
		(format t "~%spell-checker-0: ~A" (spell-checker-0 "hello" DICTIONARY))

		;spell checker 1 test
		(format t "~%spell-checker-1: ~A" (spell-checker-1 "zurich" map))
		
		;works better in small dictionary(in big dictionary takes much time than small dictionary)
		(format t "~%Searching...")
		(format t "~%Code Breaker test(Gen-Decoder-A): ~A" (Code-Breaker DOCUMENT #'Gen-Decoder-A))

		(format t "~%Encoded paragraph: ~A" (encode-and-write-file "the foresee avid element a extent ability abater hood invoke taos" DOCUMENT))
		;e  t a o i n l b x r y m h d k v s d f
		;10 6 6 5 4 3 2 2 1 2 1 1 1 1 1 1 1 1 1 
		(format t "~%Searching...")
		(format t "~%Code Breaker test(Gen-Decoder-B-0): ~A" (Code-Breaker DOCUMENT #'Gen-Decoder-B-0)) 
		;(format t "~%Code Breaker test(Gen-Decoder-B-1): ~A" (Code-Breaker DOCUMENT #'Gen-Decoder-B-1)) ;I couldn't find the exact situation (word recurrence) but it works exactly described.I check step by step

	)
)



;; test code...
(test_on_test_data)


