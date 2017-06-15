


;General Approach:
;base pairs with each base followed with its complement. this function just define a list as base-pairs as a parameter

(defparameter base-pairs '((a t) (t a) (c g) (g c)))

;the base is associated with input, the function will find the pair matched with the base which is the input, then return the second argument of the sublist. 

 (defun complement-base (base)
  (second (assoc base base-pairs))) ; assoc will return the sublist started with base. 
 
;Evaluation of above function: 
;strength:  Compared with the  method  to use ( cond (eq base 'A) 'T), pre defined parameter is easy to modidy and more readable 
;weakness:  Easy to make mistake by forgetting to use assoc  


;General Approach:
;strand is an input list, return the new list that contains the complement for all original element in list. 
;using mapcar to implement the complement-base defined before.

(defun complement-strand(strand)
                  (mapcar #'complement-base strand))

;Evaluation of above function: 
;Strength: call the function complement-base that defined in the question one. function is reusable.
;Weakness: complement-strand can not be modified without changing the complement-base function. So it makes unportable 



;General Approach:
;define a function called make-double, and take input as a list. create a lsit that contains the first element of the input list then 
;append its complement by calling the complemnet-base function. 
(defun make-double (strand)
  (if (null strand)
      nil
    (cons (list (first strand)                      ; first element of the list
                  (complement-base (first strand))) ; cons means append the complement of the first element 
          (make-double (rest strand)))))            ; resursively go to the next element if any 


;Evaluation of above function: 
;Strength: this function implement the strand recursively, so program runs faster and code is simpler for long strand than using (cond (eq base 'A ) (List base 'T)) method. Also, the input is not associated with the code, so it makes this function reuseable and not limited to one data structure. 
;Weakness: not necessnary for a very short and simply strand. 

      
                                                                                                                                                                  ;                                                                                             
 
;General Approach:
;first using let to initialize all the counter to zero. Then declare a local function count-recursive using key word labels, if the base equal a then increase the counter for a. 
;using dolist loop to check all the elements, if the parameter element in the strand matches a in the list then return the value of count_a. if the next sublist is still a list
; recursively do the same steps listed above until the end of the strand. 
  (defun count-base (strand)
     (let( (count_a 0) (count_t 0) (count_g 0) (count_c 0))            ;initialize all the counters

       (labels (count_recursive (base)                                  ;declare the local function count-recursive 
            
                       (cond  ((eql base 'a) (incf count_a))           ;condition : if matched any of them then execute it and increase the counter
                              ((eql base 'g) (incf count_g))
                              ((eql base 't) (incf count_t))
                              ((eql base 'c) (incf count_c))))

                            (dolist (element strand (list (list 'a count_a) ;established the dolist parameters - element, strand is the list form, and the list is the result form 
                                                          (list 't count_t)
                                                          (list 'c count_c)
                                                          (list 'g count_g)))
                              (cond ((listp element)                         ;check if element is a list 
                                     (count-recursive (first element))       ;call function count-recursive and search the first element of next sublist
                                     (count-recursive (second element))
                                     (t (count-recursive(elelment)))))))))    ; program will always execute this statement.

;Evaluation for above function
;Strength: it implement it recursively, so it runs fast for a long strand. Also the code is readable. 
;Weakness: This function is not reuseable as the list return form is hard coded.                      
                      

;General Approach:
;This is a recursive function that to check if the first argument is the predix of the second argument
;simply check one element by element for the two list. 
(defun predixp (input strand)
  (if (null input)
      t
      (if (eql (first input) (first strand))
          ( predixp(rest input) ( rest strand))
          nil
       )
   )
)
  
;Evaluation for above function
;Strength: it implement it recursively, and the data structure is limited to one data type. Also this function can be reuseable.
;Weakness: for a large input, it will keep checking even if the strand is short. so it will reduce the ultilization. 


;General Approach:
;using condition method and call definited function predixp (input strand). it either can be predixed or the list can appear anywhere in the second list. 
(defun appearsp (input strand)
  (cond ((null input) t)
        ((null strand ) nil)
        (t (or (prefixp input strand)
               (appearsp input (rest strand))))))
    
                                                                 
;Evaluation for above function
;Strength: it implement it recursively, and the data structure is limited to one data type. Also this function can be reuseable.
;Weakness: for a large input, it will keep checking even if the strand is short. so it will reduce the ultilization. 

; not working 
(defun coverp (first_input second_input) 
   (cond ((null first_input) nil)
         ((null second_input ) t)
         (t (and (predixp first_input second_input)
               (coverp first_input (rest second_input))))))


;General Approach:
;Since the butlast only cut off the last n elements, so created a variable size, and get the length of the base. 
(defun prefix (n base)
  (setf size (length base))
  (if (null base)
      nil
    (butlast base (- size n)))) ; butlast cut off the last n element 
                        
;Evaluation
;Strength: the code is simply and efficient, and the logic is easy to understand. 
;Weakness: n has to be number , and there is no error checking. 


;not working 
(defun coverp (first_input second_input) 
   (cond
         ((null second_input ) t)
         ((eq (predixp first_input second_input) nil) nil)
         (t (coverp first_input (rest second_input)))))
           
)
; not working ! damn it ! fuck this 
(defun coverp (first_input second_input) 
  (if (null  second_input)
      t
    ( if  (predixp first_input second_input) 
         (coverp first_input (rest second_input))
         nil)))


(defun coverp (first_input second_input)
  (setf size (length first_input))
   (cond ((null second_input) t)
		 ((eq (predixp first_input second_input) nil) nil)
		 (t (coverp first_input (nthcdr size second_input)))))                      


(defun kernal (strand)
	(setf temp (prefix 1 strand))
	(do ((iterator 1))
		((cond ((eq (coverp temp strand) t) t)) temp)
		(incf iterator)
		(setf temp (prefix iterator strand))
	)
)

      
   
         
     