;;; A SIMPLE GENETIC ALGORITHM OPERATING OVER FLOATING-POINT VECTORS
;;; DUE: MIDNIGHT THE EVENING OF WEDNESDAY, MARCH 4

#|
SUBMISSION
Mail the TA your file as an attachment, including the report.
|#


;; Some utility Functions and Macros that you might find to be useful (hint)


(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
  then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
  are permitted (FUNCTION is repeatedly called until a unique
  new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
  given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))




;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS

;;; TOURNAMENT SELECTION


;; is this a good setting?  Try tweaking it (any integer >= 2) and see
(defparameter *tournament-size* 4)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  ;; Algorithm 32 of Essentials of Metaheuristics
  ; find the 'best' individual in the population
  (let ((best (random (length population))))
    (dotimes (n (- *tournament-size* 1))
      ; find another best 'next' in the population
      (let ((next (random (length population))))
        ; if fitness of nxt is better than fitness of best then best = next
	(if (> (elt fitnesses next) (elt fitnesses best)) (setf best next))))
    (elt population best))
  )


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"
  ; use the helper function to create a list of tournament selections
  (generate-list num
		 (lambda () (tournament-select-one population fitnesses)))
)


;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
  prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))


(defun evolve (generations pop-size
	       &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
  POP-SIZE, using various functions"

  ; call setup to declare variables
  (funcall setup)
  ; set all the variables as empty
  (setf *best* '())
  (setf *newpop* '())
  ; assign the best fit to an arbitraily large number for comparison
  (setf *bestfit* 10000)
  ; create the population by calling creator multiple times in a row
  (setf *pop* (generate-list pop-size (lambda () (funcall creator))))
  ; for generation times
  (dotimes (i generations)
    ; start off by setting the variables as empty 
    (setf *fit* '())
    (setf *newpop* '())
    ; evaluate the population and set thier fitness
    (setf *fit* (mapcar evaluator *pop*))
    ; if best is empty or if best fitness is lower than population best fitness
    (if (or (not *best*) (< *bestfit* (first (sort (copy-seq *fit*) #'>))))
	(progn
	   ; get the best and save it
	   (setf *best* (funcall selector 1 *pop* *fit*))
	   (setf *best* (elt *best* 0))
	   (setf *bestfit* (funcall evaluator *best*))))
    ; print the best of every population
    (funcall printer *pop* *fit*)
    ; for hlaf the size of population
    (dotimes (j (/ pop-size 2))
      ; select two from population
      (let ((selecttwo (funcall selector 2 *pop* *fit*)))
	; create two children modify them
	(let ((chlds (funcall modifier (elt selecttwo 0) (elt selecttwo 1))))
	  ; add them to the new list
	  (push (elt chlds 0) *newpop*)
	  (push (elt chlds 1) *newpop*))))
    ; set the population to new population
    (setf *pop* *newpop*))
  ; print the best ever
  (format t "~%Best Individual Ever ...~%Fitness: ~a~%Individual:~a~%"
	    *best* *bestfit*)
  
  
)




;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


(defparameter *float-vector-length* 10 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")


(defun new-rand ()
  ;using as a helper for lambda in float vector creator
  (+ *float-min* (random (* 2 *float-max*)))
)


(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
  UNIFORM random numbers in the range appropriate to the given problem"
  ; use the helper functions to create a new individual
  (generate-list *float-vector-length* 
		 (lambda () (new-rand)))
)


(defparameter *crossover-probability* 0.8
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.6
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.03
  "Per-gene mutation variance in gaussian convolution")


(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
  *crossover-probability* is the probability that any given allele will crossover.  
  The individuals are guaranteed to be the same length.  Returns NIL."
  ; for length of ind times check if they should be crossed over then cross over
  (dotimes (i (length ind1))
    (if (random? *crossover-probability*) (rotatef (elt ind1 i) (elt ind2 i))))
)


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
  Returns NIL."
  (let ((n 0))
    ; for length of ind times
    (dotimes (i (length ind))
      ; if there is a probability of mutation
      (if (random? *mutation-probability*)
	  (loop
	     do
	       ; keep selecting a n
	       (setf n (gaussian-random 0 *mutation-variance*))
	     until
	       ; until float min <= ind[i] + n <= float max
	       (and (>= (+ (elt ind i) n) *float-min*) (<= (+ (elt ind i) n) *float-max*))))
       ; set ind[i] as ind[i] + n
       (setf (elt ind i) (+ (elt ind i) n))))
)


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
  then mutates the children.  *crossover-probability* is the probability that any
  given allele will crossover.  *mutation-probability* is the probability that any
  given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
  ; copy individual 1 to child 1
  (let ((chld1 (copy-list ind1)))
    ; copy individual 2 to child 2
    (let ((chld2 (copy-list ind2)))
      ; crossover child 1 and child 2
      (uniform-crossover chld1 chld2)
      ; modify child 1 and child 2
      (gaussian-convolution chld1)
      (gaussian-convolution chld2)
      ; return the new copied and modified children as a list
      (list chld1 chld2)))
)


(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
  (ahem) various global variables which define the problem being evaluated
  and the floating-point ranges involved, etc.  I dunno."
  ; declare the global variables used in evolve
  (defvar *best*)
  (defvar *bestfit*)
  (defvar *pop*)
  (defvar *fit*)
  (defvar *newpop*)
  
)




;;; FITNESS EVALUATION FUNCTIONS


(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))




;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12


(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'step-f
	:printer #'simple-printer)


#|

short report


In the process of completing this project, I discovered many interesting things,
some personal but mostly relating to the code. I discovered skipping hw2 and starting 
late was not a good idea.

But on a more serious note, this project was very fun. It helped me dive headfirst 
into lisp coding and gave me an appreciation of the language. It has proven 
to be a very elegant yet very powerful language which is why I had so many errors,
 which is why I'm submitting it late.

Initially, my code was using global variables and lots of loops and had an extremely 
bad runtime. To optimize it I used functions such as mapcar and lambda. In the 
tournament selector, I changed the tournament size to 4 which helped the runtime a 
bit because it doesn't have to loop as often. In float vector creator I changed the 
length of an individual which helped me test the program much better because it had 
fewer alleles in the individual. I also found that changing the mutation probability 
helped get better results because below 0.5 its wasn't mutating much. I also changed 
the crossover probability but didn't notice much change in results. I did find that 
multiple runs using the same parameter setting sometimes produced worse results 
which makes sense because the algorithm is stochastic. 

I predominantly tested using sum-f and sphere-f and schwefel-f, no particular reasons
other than personal preference due to the sounds of their names. Sum-f had generally 
ok results but I found that schwefel-f did better than sum-f. what surprised me was 
the when using sphere-f my results didn't get too close to the expected results even 
though I thought it would.


|#
