;;;; Algs2.lisp

(in-package #:algs2) ; (ql:quickload "algs2") (in-package #:algs2)

;;; "algs2" goes here. Hacks and glory await!
(defparameter *mh-V-x* (make-instance 'binary-heap:binary-heap))
(defparameter *ht-V-x* (make-hash-table :test 'equal)) ;needed if we want to update a node

(defparameter *ht-x* (make-hash-table)) ;nodes visited and dijkstra score associated with each
(defparameter *graph* (make-hash-table));adjacency-list
(defparameter *edges* (make-hash-table :test 'equal));list of edges


(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun get-txt-file (path)
  (setf *graph* (make-hash-table))
  (setf *edges* (make-hash-table :test 'equal))
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof))
         (i 0 (incf i)))
        ((or (eql line 'eof)
             (> i 500)))
      (if (>= (length line) 1)
          (let* ((lst nil)                
                 (curr-vals (cl-ppcre:split "\\s+" line))
                 (tail (parse-integer (car curr-vals)))) ;for adjacency list representation
	    (dolist (path-dist (cdr curr-vals)) ;ie path-dist looks like "80,982" from tail node 1 
	      (let ((head (parse-integer (car (cl-ppcre:split "," path-dist))))
		    (dist (parse-integer (cadr (cl-ppcre:split "," path-dist)))))
		(if (not (equalp tail head)) ; ie we don't have a node pointing to itself
		    (progn
		      (add-to-heap *ht-v-x* *mh-v-x* 999999 head)
		      (add-to-heap *ht-v-x* *mh-v-x* 999999 tail)
		      (push (cons head dist) lst) ;for *graph*
		      (setf (gethash (cons tail head) *edges*);for *edges*
			    dist)))))
	    ;(add-to-heap *ht-v-x* *mh-v-x* 999999 tail); for heap and heaps's hash table
	    (setf (gethash tail *graph*) lst)))));for *graph*
  *graph*)

(defun dijstra-score (start-node finish-node path)
  (defparameter *mh-V-x* (make-instance 'binary-heap:binary-heap))
  (defparameter *ht-V-x* (make-hash-table :test 'equal)) ;needed if we want to update a node
  (defparameter *ht-x* (make-hash-table)) ;nodes visited and dijkstra score associated with each
  (defparameter *graph* (make-hash-table));adjacency-list
  (defparameter *edges* (make-hash-table :test 'equal));list of edges
  (get-txt-file path)
  (calc-shortest-path-scores start-node finish-node))
  

(defun qp-hashtable (ht)
  (maphash #'(lambda (k v)
	       (format t "~%k=~a,val=~a" k v))
	   ht))

(defun qp-heap (heap)
  (print (binary-heap::bin-heap-array heap)))

(defun qp-all ()
  (qp-hashtable *ht-x*)
  (qp-hashtable *ht-v-x*)
  (qp-heap *mh-v-x*))

;;makes sure hashtable and minheap have correct vals
(defun add-to-heap (htable heap heap-key heap-val)
  (if (null (gethash heap-val htable))
      (setf (gethash heap-val htable) 
	    (binary-heap:insert heap heap-key heap-val))))


;;;main idea, two regions X and V-x (where V is set of all nodes)
;;Think of it like a 2 round tourney to suck the next node into X
;;round 1 each node in V-x will keep a local score of the shortest path to it - that will be thrown on the heap so that the heap picks the min score
;;round 2 every time a new node is sucked into X we need to update the heap
;;since the heap needs to be the smallest dijkstra score
;;initialze everything in the heap to a max possible number

;;assumes that *graph* *edges* *ht-x* *mh-v-x* *ht-v-x* are all populated 
(defun calc-shortest-path-scores (start-node end-node)
  (setf (gethash start-node *ht-x*) 0);initilize X to be region that we have explored - so we need to add the first node
  (remhash start-node *ht-v-x*)
  (update-scores-neighbors start-node *ht-x* *mh-v-x* *ht-v-x*);add the start-nodes nighbors to the minimum heap
  (while (and ;(null (gethash end-node *ht-x*)) 
	      (> (binary-heap:heap-size *mh-v-x*) 0))
    (multiple-value-bind (node score) (binary-heap:extract-min *mh-v-x*)
      (remhash node *ht-v-x*);extracted node so remove it's link in the corresponding hash
      (if (null (gethash node *ht-x*)) (setf (gethash node *ht-x*) score)) ; add extracted node to X
      (update-scores-neighbors node *ht-x* *mh-v-x* *ht-v-x*)))
  (gethash end-node *ht-x*))

(defun update-scores-neighbors (source-node ht-x heap ht-heap)
  (dolist (neighbor-node-and-lengthtonode (gethash source-node *graph*))
    (let ((neighbor-node (car neighbor-node-and-lengthtonode))) ; (node . length-to-node) ; in the format 1: ((2 . 200) (3. 231)) ;; 2 = node 200 = length from 1 to 2 
      (if (null (gethash neighbor-node *ht-x*));only do this if tail in X and head in V-X 
	  (progn ;(qp-all)
		 ;(format  t "~%~%sourcenode ~a, neighbor-node=~a" source-node neighbor-node)
		
		 (update-node-score source-node neighbor-node ht-x heap ht-heap))))))
			  

(defun update-node-score (source-node target-node ht-x heap-v-x ht-v-x)
  (let* ((target-node-ptr (gethash target-node ht-v-x))
	 (score-source-node (gethash source-node ht-x)); A[s]
	 (curr-score-target-node (binary-heap::node-key target-node-ptr))
	 (len-source->target (gethash (cons source-node target-node) *edges*));Len-s->t; length from s to t
	 (new-score-target-node (+ score-source-node len-source->target))); A[s] + Len-s->t  
    (if (< new-score-target-node curr-score-target-node)
	(binary-heap:decrease-key heap-v-x target-node-ptr new-score-target-node))))

(defun node-score (node)
  (let ((node-ptr (gethash node *ht-v-x*)))
    (binary-heap::node-key node-ptr)))

(defun lower-node-score (node score)
  (if (or (null (gethash node *ht-x*))
	  (< score (gethash node *ht-x*)))
      (setf (gethash node *ht-x*) score)))
       

;;the heap data structure -- just for playing around with
(defparameter *heap* (make-instance 'binary-heap:binary-heap))
(defparameter *ht-heap* (make-hash-table :test 'equal))
(defun populate-test-heap ()
  (defparameter *heap* (make-instance 'binary-heap:binary-heap))
  (defparameter *ht-heap* (make-hash-table :test 'equal))
  (add-to-heap *ht-heap* *heap* 1 '(2 . 6))
  (add-to-heap *ht-heap* *heap* 2 '(3 . 10))
  (add-to-heap *ht-heap* *heap* 5 '(3 . 4))
  (add-to-heap *ht-heap* *heap* 2 '(10 . 8))
  (add-to-heap *ht-heap* *heap* 4 '(1 . 2))
  (add-to-heap *ht-heap* *heap* 6 '(3 . 9))
  (add-to-heap *ht-heap* *heap* 7 '(2 . 4))
  (add-to-heap *ht-heap* *heap* 2 '(7 . 8)))




;(defparameter *heap3* (make-instance 'cl-heap:fibonacci-heap :key #'first))
;(defun pop-heap3 (heap)
;  (cl-heap:add-to-heap heap '(1 (2 . 6)))
;  (cl-heap:add-to-heap heap '(2 (3 . 10)))
;  (cl-heap:add-to-heap heap '(5 (3 . 4)))
;  (cl-heap:add-to-heap heap '(2 (10 . 8)))
;  (cl-heap:add-to-heap heap '(4 (1 . 2)))
;  (cl-heap:add-to-heap heap '(6 (3 . 9)))
;  (cl-heap:add-to-heap heap '(7 (2 . 4)))
;  (cl-heap:add-to-heap heap '(2 (7 . 8))))

;(pop-heap3 *heap3*)



;;assume start-node not equal to finish-node
(defun calc-dijkstra-score (start-node finish-node)
  (let ((continue t))
    ;;do initial population to get the heap started
    (setf (gethash start-node *ht-x*) 0); starting node starts at 0
    (calc-greedy-score-for-heap start-node *graph* *ht-x* *mh-v-x* *ht-v-x*)
    (qp-all)
    (while (and (null (gethash finish-node *ht-x*))
		(< 0 (binary-heap:heap-size *mh-v-x*)))
      (multiple-value-bind (heap-key heap-val) (binary-heap:extract-min *mh-v-x*) ;;pop the min value
	(let ((tail (car heap-key)) ;from x
	      (head (cdr heap-key)));want this to be in v-x
	  (when (and (not (equalp finish-node head)) ;;if we haven't found the node
		     (null (gethash head *ht-x*))) ;;if tail in v-x (ie it's not in x)
	    (setf (gethash head *ht-x*) ;;calculate dijstra score A[head] + Ltail-node
		  (+ (gethash tail *ht-x*) heap-val)); A[x] + Lx-v where x = head which is the node in X which connects to the node pulled from the heap || v = current extracted node in V-x
	    (remhash (cons tail head) *ht-v-x*)
	    (calc-greedy-score-for-heap head *graph* *ht-x* *mh-v-x* *ht-v-x*)
	    (qp-all))
	  (if (and (equalp finish-node head) (null (gethash finish-node *ht-x*)))
	      (progn
		(setf continue nil)
		(format t "~%tail=~a, head=~a" tail head)
		(qp-all)
		(break)
		(setf (gethash head *ht-x*) ;;calculate dijstra score A[head] + Ltail-node
		      (+ (gethash tail *ht-x*) heap-val))
		(qp-all))))))
    (gethash finish-node *ht-x*)))
    
  


;;calcs greedy scores based on a new node
(defun calc-greedy-score-for-heap (tailnode-x graph ht-x heap heap-ht)
  ;(break)
  (dolist (headnode-dist (gethash tailnode-x graph))
    (let ((n (car headnode-dist))
	  (dist (cdr headnode-dist)))
      (if (null (gethash n ht-x))
	  (calc-greedy-score-for-heap-helper (cons tailnode-x n) dist heap heap-ht))))
  );(break))
				

(defun calc-greedy-score-for-heap-helper (edge dist-val heap ht)
  (let* ((node-ptr (gethash edge ht)));the value of that node - ie the key in the minheap
    (cond ((or (null node-ptr) 
	       (null (binary-heap::node-key node-ptr)))
	   (add-to-heap ht heap dist-val edge)) 
	  ((< dist-val (binary-heap::node-key node-ptr))
	   (remhash edge ht)
	   (binary-heap:decrease-key heap node-ptr dist-val))
	  (t nil))))


(mapcar #'(lambda (x) (gethash x *ht-x*))
	       '(7 37 59 82 99 115 133 165 188 197))
;(2599 2610 2947 2052 2367 2399 2029 2442 2505 3068) ;right answer



;;;;week 6 homework
(defparameter *ht-2sum* (make-hash-table)); hash table to hold the million numbers coming in
;(defparameter *ht-2sum-results* (make-hash-table)); going to use this for results
(defparameter *t-count* 0)

(defun get-2sum-list (path ht)
  (with-open-file (s path :direction :input)
    (do ((line (read-line s nil 'eof) (read-line s nil 'eof)));((vars)) (var - initial- update)
	((eql 'eof line)) ; ((stopping condtiion))
      (setf (gethash (parse-integer line) ht) 1)))
  ht)

;;x+y=t is our goal is to calculate y's that satisfy t=-10000 to t=10000
;;ie if x = 1 then a possible y
;;except t is a range [-10000 , 10000] 
;;say t1 is a 
(defun calc-2-sum-variant-y=t-x (x t1 t2 ht)
  (let ((target t1))
    (while (<= target t2)
      (when (eql (gethash (- target x) ht) 1) ;means it's the first time we're seeing it - we only want distinct values of x + y = target
	(format t "x=~a, y=~a, t=~a" x (gethash (- target x) ht) target)
	(incf *t-count*))
      (incf target))))

(defun calc-num-variants (ht)
  (maphash #'(lambda (k v)
	       (setf (gethash k ht) 0)
	       (calc-2-sum-variant-y=t-x k -10000 10000 ht)
	       (break))
	   ht))

;;;;PS greedy alg to calc minimized weighted sum of completion timesquestio
(defun get-jobs (path)
  (let ((joblist nil))
    (with-open-file (str path :direction :input)
      (do ((i 0 (incf i))
           (num-line (read-line str nil 'eof) (read-line str nil 'eof)))
          ((or (> i 10001) (eql 'eof num-line)) (nreverse joblist))
        (when (not (equalp num-line "10000"))
          ;(print num-line)
          (let* ((weight-comp (cl-ppcre:split " " num-line))
                 (weight (parse-integer (car weight-comp)))
                 (joblen    (parse-integer (cadr weight-comp))))
            (push (list i weight joblen (- weight joblen) (* 1.0 (/ weight joblen))) 
                  joblist)))))))

;;wi/li - optimal - note that we don't care how ties are broken (is good enough)
(defparameter *jobs-sorted-wi/li*  (sort (get-jobs "/home/bear/Downloads/jobs.txt") 
                             #'>
                             :key #'(lambda (x)
                                      (nth 4 x))))
 ;;wi-li - not optimal - note there is a special case for breaking ties
(defparameter *jobs-sorted-wi-li*  (sort (get-jobs "/home/bear/Downloads/jobs.txt") 
                                         #'(lambda (a b)
                                             (let ((wa (nth 1 a))
                                                   (wb (nth 1 b))
                                                   (wa-la (nth 3 a))
                                                   (wb-lb (nth 3 b)))
                                               (cond ((= wa-la wb-lb)
                                                      (if (> wa wb) t nil))
                                                     ((> wa-la wb-lb) t)
                                                     (t nil))))))

(defun calc-completion-time (jobs)
  (let ((weighted-completion-time 0)
        (prev-job-compleiton-time 0))
    (dolist (job jobs)
      (let ((wj (nth 1 job)) ;;weight current job
            (lj (nth 2 job))) ;;length of current job
        (setf prev-job-compleiton-time (+ prev-job-compleiton-time lj)) ;; Cj - completion-time-of-current-job = prevoius completion time + len of curr j
        (incf weighted-completion-time (* wj prev-job-compleiton-time))))
    weighted-completion-time))
 
; 
;(print (calc-completion-time *jobs-sorted-wi-li*))     ;69119377652  
;(print (calc-completion-time *jobs-sorted-wi/li*))     ;67311454237
;(print (- (calc-completion-time *jobs-sorted-wi-li*)   
;          (calc-completion-time *jobs-sorted-wi/li*))) ;1807923415





;;;;week 2 union-find alg - see if it's easier to put into the asdf def file

(load "/home/bear/lisp/clocc/src/ext/union-find/union-find.lisp")
(defparameter *vertices* '(a b c d e f g h i j))

(defun pprint-sets (partition verts)
  (let ((printed-sets (make-hash-table)))
    (declare (dynamic-extent printed-sets))
    (loop for v in verts
       for s = (find-set partition v)
       unless (gethash s printed-sets)
       do (setf (gethash s printed-sets) t)
       and collect (collect-set partition v)
       into sets
       finally (format t "~&Sets:~{ ~S~}~%" sets))))


(defun test-union-find ()
  (labels ((print-disjoint-sets (partition)
	     (let ((printed-sets (make-hash-table)))
	       (declare (dynamic-extent printed-sets))
	       (loop for v in *vertices*
		     for s = ( find-set partition v)
		     unless (gethash s printed-sets)
		       do (setf (gethash s printed-sets) t)
		       and collect (collect-set partition v)
		           into sets
		     finally (format t "~&Sets:~{ ~S~}~%" sets))))

	   (union-and-print (p v1 v2)
	     (format t "~%Joining set with ~A with set with ~S.~%" v1 v2)
	     ( uunion p ( find-set p v1) ( find-set p v2))
	     (print-disjoint-sets p))
	   )
    (let ((p ( make-partition :test #'eq)))
      (dolist (v *vertices*)
	( make-set p v))
      (print-disjoint-sets p)
      (union-and-print p 'b 'd)
      (union-and-print p 'e 'g)
      (union-and-print p 'a 'c)
      (union-and-print p 'h 'i)
      (union-and-print p 'a 'b)
      (union-and-print p 'e 'f)
      (union-and-print p 'b 'c)
      (union-and-print p 'h 'b)
      (print-disjoint-sets p)
      p
      )))


;;get a list of edges
(defparameter *vertices* nil)
(defparameter *partition* (make-partition))
(defparameter *edges* nil)

(defun get-edges-from-file (&optional (path "/home/bear/Downloads/clustering1.txt"))
  (defparameter *vertices* nil)
  (defparameter *partition* (make-partition))
  (defparameter *edges* nil)
  (for (j 1 500)
    (push j *vertices*)
    (make-set *partition* j))
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof))
         (i 0 (incf i)))
        ((eql line 'eof)   ;(or (eql line 'eof) (> i 100000))
	 (setf *edges* (sort *edges* #'< :key #'(lambda (x) (nth 2 x)))))
      (when (> i 0)
	(let* ((n1-n2-dist (cl-ppcre:split " " line))
	       (n1 (parse-integer (first n1-n2-dist)))
	       (n2 (parse-integer (second n1-n2-dist)))
	       (dist (parse-integer (third n1-n2-dist))))
	  (push (list n1 n2 dist) *edges*) )))))

(defun get-leader-node (n)
  (set-rep-item (find-set *partition* n)))

(defun same-leader-nodes? (n1 n2)
  (equal (get-leader-node n1)
	 (get-leader-node n2)))

;;this isn't exaclty the number of clusters you need, it's more like the starting point (so num of verticies)
(defun k-clustering (k)
  (dolist (n1-n2 *edges*)
    (let ((n1 (first n1-n2))
	  (dist (third n1-n2))
	  (n2 (second n1-n2)))
      (when (and (not (same-leader-nodes? n1 n2)) (>= k 4) )
	(uunion *partition* (find-set *partition* n1) (find-set *partition* n2)); join them into same set
	(format t "k=~a :: ~a . ~a :: ~a~%" k n1 n2 dist)
	(decf k))))
  k); reduce number of clusters by 1

;(get-edges-from-file)
;(pprint-sets *partition* *vertices*); compare the sets in union-find before ; ie Sets: (500) (499) (498) (497)....(1)
;(let ((e (get-edges-from-file)))
;  (k-clustering (length *vertices*))); look at the line where k = 4 - note at the end of this k will equal three since it runs for k = 4, we need to 
;(pprint-sets *partition* *vertices*) ; compare the sets after - it'll be 3 sets since there's an off by one error earlier -  

;;set k to 4
;;looking for the max spacing of a 4-clustering. ie the last edge added when we finally get to 4 clusters
;;so run it and notice what happens when the final number of clusters = 5 and then notice what edge gets added when the final number of clusters gets to 4 (ie when the leadernodes aren't the same etc)...
;(> k 4) and then (>= k 4) and we see the last edge to be added
;;can just run the above for 
;(k-clustering 500); look at the last printed statement it's the last thing entered k=4, edge 414.455, distance 106
;;is the max spacing so 106 in this case
;;;;ANSWER 106

;;what happens if we pick a number larger than 500 for k-clustering? it just makes it like one giant set (500 499 ... 1) but all in one set


;;;;;quesiton 2 - need all hamming distances
;;go through each hammin-code and put that into a hash
;;for each entry in hash - say x
;;  -create all possible constants that are 1 or 2 differences apart (about 600 constants)
;;  -for each of the 500 constants
;;    -see if that hamming-code is in the hash table
;;      -union x and that hamm code
;;      -mark that hash has been grabbed as part of some union

;;at the end of these just count the number of sets in the union find
;;this will be our K, or just the number of leaders

;(cl-ppcre:regex-replace-all "[^0-9]" "1 2 3 3 a b as   c" ""); "1233"
;(hash-table-count *ht-hamming-code*)

(defparameter *ht-hamming-code* (make-hash-table :test #'equal))
(defparameter *num-unions* 0)

(defun get-hamming-from-file (&optional (path "/home/bear/Downloads/clustering_big.txt"))
  (defparameter *ht-hamming-code* (make-hash-table :test #'equal))
  (defparameter *partition* (make-partition))
  (defparameter *num-unions* 0)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof))
         (i 0 (incf i)))
        ((or (eql line 'eof) (> i 300000))
	 (setf *num-unions* (hash-table-count *ht-hamming-code*)))
      (when (> i 0)
	(let* ((hamm-code (cl-ppcre:regex-replace-all "[^0-9]" line "")))
	  (make-set *partition* hamm-code)
	  (setf (gethash hamm-code *ht-hamming-code*) 0)))))) 


(defun invert-bit (str)
  (if (equalp str "0") "1" "0"))

(defun lst-of-1-offs (orig st end)
  (let ((results nil))
    (for (i st end)
      (let ((cpy (copy-seq orig)))
	(setf (subseq cpy i (1+ i))
	      (invert-bit (subseq cpy i (1+ i))))
	(push cpy results)))
    results))

(defun lst-1-or-2-offs-helper (orig st end)
  (let* ((r1 (lst-of-1-offs orig st end))
	 (r1-offs (copy-list r1))
	 (r2-offs nil))
    (dolist (1-off r1)
      (setf r2-offs (append (lst-of-1-offs 1-off 0 (1- (length 1-off))) r2-offs)))
    (append r1-offs r2-offs)))

(defun lst-1-or-2-offs (orig)
  (lst-1-or-2-offs-helper orig 0 (1- (length orig))))

;;greedly grab all points 1 and 2 clusters apart and union them
(defun greedy-clustering ()
  (maphash #'group-each-possible-hamm-code *ht-hamming-code*))

(defun group-each-possible-hamm-code (hamm-code-orig v)
  (let ((all-1-2-offs (lst-1-or-2-offs hamm-code-orig)))
    (dolist (potential-node all-1-2-offs)
      (when (and (gethash potential-node *ht-hamming-code*)
		 (not (same-leader-nodes? hamm-code-orig potential-node)))
	(uunion *partition* (find-set *partition* hamm-code-orig) (find-set *partition* potential-node))
	(decf *num-unions*)))))

(defun run-hamming-greedy ()
  (get-hamming-from-file)
  (greedy-clustering)
  *num-unions*); 6118 right answer


(defun make-leader-stats-fun ()
  (let ((ht (make-hash-table :test #'equal)))
    (maphash #'(lambda (node v)
		 (if (gethash (get-leader-node node) ht)
		     (incf (gethash (get-leader-node node) ht))
		     (setf (gethash (get-leader-node node) ht) 1)))
	     *ht-hamming-code*)
    ht))

;;try this for fun
;(time (sort (let ((lst nil))
;	       (maphash #'(lambda (k v)
;			    (push (list k v) lst))
;			(make-leader-stats-fun)) 
;	       lst)
;	     #'>
;	     :key #'(lambda (x) (nth 1 x))))


;;;;huffman code - week3 problem
(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elem n)))))
  elem (l0 nil) (r1 nil) (depth 0))


(defparameter *mh-nodes* (make-instance 'binary-heap:binary-heap))
(defparameter *ht-nodes* (make-hash-table))

;;get the file
(defun get-huffman-code (&optional (path "/home/bear/Downloads/huffman.txt"))
  (defparameter *mh-nodes* (make-instance 'binary-heap:binary-heap))
  (defparameter *ht-nodes* (make-hash-table))
  (with-open-file (s path :direction :input)
    (do ((i 0 (incf i))
	 (line (read-line s nil 'eof) (read-line s nil 'eof)))
	((or (> i 999999) (eql line 'eof)))
      (when (> i 0)
	(add-to-heap *ht-nodes* *mh-nodes* (parse-integer line)  (make-node :elem i))))))

(defun combine-nodes-internal-node (t1 t2)
  (let ((internal-node (make-node :elem -1 :l0 t1 :r1 t2)))
    (increase-depth (node-l0 internal-node) 1)
    (increase-depth (node-r1 internal-node) 1)
    internal-node))
    
(defun increase-depth (node depth-increase)
  (cond ((null node) t)
	(t 
	 (incf (node-depth node) depth-increase)
	 (increase-depth (node-l0 node) depth-increase)
	 (increase-depth (node-r1 node) depth-increase))))

(defun greedy-huffman-create-bst (heap)
  (while (> (binary-heap:heap-size heap) 1)
    (let* ((t1-w1 (multiple-value-list (binary-heap:extract-min heap)))
	   (t2-w2 (multiple-value-list (binary-heap:extract-min heap)))
	   (t1 (car t1-w1))
	   (w1 (second t1-w1))
	   (t2 (car t2-w2))
	   (w2 (second t2-w2))
	   (t3 (combine-nodes-internal-node t1 t2)))
      (binary-heap:insert heap (+ w1 w2) t3)))
  (if (= (binary-heap:heap-size heap) 1)
      (binary-heap:extract-min heap)))

(defun calc-huffman-code (path)
  (if (null path)
      (get-huffman-code)
      (get-huffman-code path))
  (greedy-huffman-create-bst *mh-nodes*))

;;;;answer is 19 and 9 for the max and min length of a code word
(sort (let ((acc nil))
  (maphash #'(lambda (k v)
	       (push (node-depth k) acc))
	   *ht-nodes*)
  acc) #'>)

;;;q3 - dynamic programming path-graph - max weight independent set
;;returns an array with the weights
(defun get-path-weights(&optional (path "/home/bear/Downloads/max-weight-independent-set-path-graph.txt"))
  (let ((weights (make-array '(1001) :initial-element 0)))
    (with-open-file (str path :direction :input)
      (do ((i 0 (incf i))
           (line (read-line str nil 'eof) (read-line str nil 'eof)))
          ((or (> i 10002) (eql 'eof line)) weights)
        (when (> i 0)
          (setf (aref weights i) (parse-integer line)))))))
 
(defparameter *mwis-weights* (get-path-weights))
(defparameter *mwis-optival* (make-array '(1001) :initial-element 0))

;returns the optimal value for each i in Gi. where Gi is the optimal value of the solution at index i.  G[1000] = 2955353732)
;still do not have if a particual
(defun get-optimal-vals ()
  (setf (aref *mwis-optival* 1) (aref *mwis-weights* 1))
  (for (j 2 1000) ;;dynamic programming part
    (setf (aref *mwis-optival* j) ;it's just grabbing previous values that we've stored in earlier steps
          (max (aref *mwis-optival* (- j 1)) ;picking the max of 1 of 2 optimal solutions
               (+ (aref *mwis-weights* j) (aref *mwis-optival* (- j 2)))))))

;;uses the previous optimal value array and determines which of these was actually 
(defun reconstruction-alg (weights optimal-vals)
  (let  ((acc nil) ;will be the set of Vis - and be the max weight IS of graph G
	 (i (- (length optimal-vals) 1))); start from right to left
    (while (>= i 1)
      (cond ((= 1 i)
	     (push i acc)
	     (decf i 2))
	    ((>= (aref optimal-vals (- i 1)) ;case 1 wins
		 (+ (aref optimal-vals (- i 2)); a[i-1] >= a[i-2] + wi
		    (aref weights i)))
	     (decf i 1))
	    (t (push i acc) ;;case 2 wins
	       (decf i 2))))
    acc))

(get-optimal-vals)
(defparameter *set-mwis-answer* (reconstruction-alg *mwis-weights* *mwis-optival*))
(mapcar #'(lambda (x)
	    (if (member x *set-mwis-answer*) 1 0))
	'(1 2 3 4 17 117 517 997)); (1 0 1 0 0 1 1 0)


;;;;WEEK 4 QUESTION 1 knapsack easy warmup - bottom-up

(defparameter *W* 10000)
(defparameter *vals* (make-array '(101) :initial-element 0))
(defparameter *weights* (make-array '(101) :initial-element 0))
(defparameter *res-arry* (make-array '(101 10001) :initial-element 0))

(defun get-knapsack (&optional (path "/home/bear/Downloads/knapsack1.txt"))
  (with-open-file (str path :direction :input)
    (do ((i 0 (incf i))
	 (line (read-line str nil 'eof) (read-line str nil 'eof)))
	((or (> i 1000000000) (eql line 'eof)) *vals*)
      (let* ((vi-wi (cl-ppcre:split " " line))
	     (vi (parse-integer (car vi-wi)))
	     (wi (parse-integer (cadr vi-wi))))
	(if (> i 0)
	    (setf (aref *vals* i) vi
		  (aref *weights* i) wi))))))
	    

(defun bot-up-knapsack (n W)
  (for (i 1 n)
    (for (x 0 W)
      (cond ((> (aref *weights* i) x)
	     (setf (aref *res-arry* i x) (aref *res-arry* (- i 1) x)))
	    (t (setf (aref *res-arry* i x)
		     (max (aref *res-arry* (- i 1) x)
			  (+ (aref *res-arry* (- i 1) (- x (aref *weights* i)))
			     (aref *vals* i))))))))
  (aref *res-arry* n W)) ; 2493893

;(get-knapsack)
;(bot-up-knapsack 2000 2000000)

;;use a 0/1 approach to this since we only need the previous column
(defparameter *vals* (make-array '(2001) :initial-element 0))
(defparameter *weights* (make-array '(2001) :initial-element 0))
(defparameter *res-col-0* (make-array '(2000001) :initial-element 0))
(defparameter *res-col-1* (make-array '(2000001) :initial-element 0))
;;do a second version of the knapsack problem but with
(defun bot-up-knapsack-0/1 (n W)
  (for (i 1 n)
    (setf *res-col-0* (copy-seq *res-col-1*)) ;copy col1 over to col0 every time we're on to a new column
    (for (x 0 W)
      (cond ((> (aref *weights* i) x) ; if wi > x then we can not use this since it's over the capacity
	     (setf (aref *res-col-1* x) (aref *res-col-0* x)))
	    (t (setf (aref *res-col-1* x) ;else A1[x] = max{ prev-col, currval + prev-col-x-wi  }
		     (max (aref *res-col-0* x) ;cae 1 - so item i excluded - capacity doesn't reduce but both cases the problem size gets smaller 
			  (+ (aref *res-col-0* (- x (aref *weights* i))) ;case 2 -or if item i is included, which means the value of item goes up (optimal solution)  AND capacity goes down since we added i)
			     (aref *vals* i))))))))
  (aref *res-col-1* W))

;(get-knapsack "/home/bear/Downloads/knapsack_big.txt")
;(bot-up-knapsack-0/1 2000 2000000); 4243395


(+ (* .05 7)
   (* .4 6)
   (* .08 5)
   (* .04 4)
   (* .1 3)
   (* .1 2)
   (* .23 1))

(defparameter *probs* #(0 .05 .40 .08 .04 .10 .10 .23))
(defparameter *A* (make-array '(8 8) :initial-element 0))

(defparameter *probs* #(0 80 10 10))
(defparameter *A* (make-array '(4 4) :initial-element 0))


(defparameter *probs* #(0 1 34 33 32))
(defparameter *A* (make-array '(5 5) :initial-element 0))

(defparameter *probs* #(0 2 23 73 1))
(defparameter *A* (make-array '(5 5) :initial-element 0))


(defun pk (probs i j)
  (let ((acc '(0)))
    (for (x i j)
      (push (aref probs x) acc))
    (apply #'+ acc)))

(defun binary-tree-helper (start end)
  (let ((p1 0)
	(t1 0)
	(t2 0)
	(acc '()))
    (when (<= start end)
      (for (r start end)
	(setf p1 (pk *probs* start end)  ;(aref *probs* r)
	      t1 0
	      t2 0)
	(if (<= start (- r 1))
	    (setf t1 (aref *A* start (- r 1))))
	(if (<= (+ r 1) end)
	    (setf t2 (aref *A* (+ r 1) end)))
	;(format t " <pk[~a to ~a]:~a + A[~a,~a]:~a + A[~a,~a]=~a> = ~a~%" start end p1 start (- r 1) t1 (+ r 1) end t2 (+ p1 t1 t2))  
	(push (+ p1 t1 t2) acc)))
	;(print acc)))
    ;(format t "-- ~a : ~A ~%~%" acc (first (sort acc #'<))) ;don't know why something to do with pointers but uncommenting this messes everything up
    (first (copy-seq (sort acc #'<)))))

;n = 7
(defun opt-binay-tree (n)
  (for (s 0 (- n 1))
    (for (i 1 n)
      ;(format t "s=~a, i=~a, i+s=~a A[~a,~a] ~%" s i (+ i s) i (+ i s))
      (when  (<= (+ i s) n)
	;(print "---------before---------")
	;(print *A*)
	(setf (aref *a* i (+ i s))
	      (binary-tree-helper i (+ i s)))
	;(print *A*)
	;(print "---------after-------")
	;(break)
	)))
  (aref *A* 1 n))

;(opt-binay-tree 4)
;(opt-binay-tree (1- (array-dimension *A* 0)))

(defparameter *probs* #(0 .22 .18 .2 .05 .25 .02 .08))
(defparameter *A* (make-array '(8 8) :initial-element 0))

(defparameter *probs* #(0 5 10 2 3 4))
(defparameter *A* (make-array '( 6 6  ) :initial-element 0))







;;;;;implement quicksort from previous
(defparameter *num-comparisons* 0) ;just a quick way to see how much the pivot moved

(defun qs (arr)
  (setf *num-comparisons* 0)
  (quicksort arr 0 (1- (length arr)) #'>=)
  (print *num-comparisons*)
  (print (- 73848 *num-comparisons*))) ;;compare it to just picking the first number
  


(defun qs2 (arr)
  (setf *num-comparisons* 0)
  (quicksort arr 0 (1- (length arr)) #'<=))

(defun quicksort (arr st end &optional (fn #'>=))
  (let ((pivot-index st)) ;just pick the st as a defult value for it
    (if (> (- end st) 0)
	(progn
	  (setf pivot-index (partition arr st end fn))
	  (incf *num-comparisons* (- end pivot-index)) ;;lets see how much the pivot moved - 50% chance we pick a good enough pivot so the split on both sides should be pretty good
	  (quicksort arr st (1- pivot-index) fn)
	  (quicksort arr (1+ pivot-index) end fn))))
  arr)

;destructive method that returns the pivot-index
(defun partition (arr st end &optional (fn #'>=))
  (let* ((pivot (choose-pivot arr st end)) ;(aref arr st)) ;pick first element for now; add choose pivot here to see how the pivot affects the output
	 (i st);assume pivot is or has been moved to the first position
	 (j (+ i 1)))
    (while (<= j end)
      (when (funcall fn pivot (aref arr j)) ;need the invariant to hold-- looking at the case where plllhhhLu with L being current element
	;(incf *num-comparisons*)
	(incf i)
	(swap arr i j)) ;; will make sure we have the following in the array plllhhhL -- pretend L is actually a value less than p and then notice after the swap it will look like plllLhhh, now we just need the pivot to go between the l(lower than pivot) and hs (higer than pivot) -- someting like Llllphhh
      (incf j))
    (swap arr st i);the final swap moves the pivot inbetween the low and highs ie Llllphhhh
    i));need to return the index of the pivot 

;destructive swap of elements in an array
(defun swap (arr i j)
  (let ((temp (aref arr i)))
    (setf (aref arr i) (aref arr j)
	  (aref arr j) temp)))
	   
(defun get-file-qs (path arr)
  (with-open-file (str path :direction :input)
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)) ; var initial update
	 (i 0 (incf i)))
	((eql line 'eof)) ;stopping condtions
	     ;(> i 20)))
      (push (parse-integer (cl-ppcre:scan-to-strings "\\d+" line)) arr)))
  (make-array (length arr) :initial-contents (nreverse arr)))


;destuctive method that chooses a pivot at random and then swap it to the first position 
(defun choose-pivot (arr st end)
  (let ((pivot-index (+ st (random (1+ (- end st))))))
    (swap arr pivot-index st)))

(defun choose-pivot1 (arr st end)
  (swap arr st st))

(defun choose-pivot-midpoint (arr st end)
  (let* ((a (sort (copy-seq (subseq arr st end)) #'<))
	 (midpoint-elem (aref a (floor (/ (length a) 2))))
	 (mid-index st))
    ;(print a)
    ;(print midpoint-elem)
    (for (x st end)
      (if (equalp midpoint-elem (aref arr x))
	  (setf mid-index x)))
    ;(print mid-index)
    ;(print (subseq arr st end))
    (swap arr st mid-index)))
	  
	       
;;note quicksort will affect the actual array
(defparameter *arr-qs* nil)
(time (setf *arr-qs* (get-file-qs "/home/bear/Downloads/QuickSort.txt" *arr-qs*)))
(time (qs *arr-qs*));; notice that using choose-pivot ends up reducing overall time and number of swaps necessary
;;run the last statement a few times in a row with the different pivot choices looks to me that just a random pivot seems best - also could do a median of 3

;;get the 5th order statictic from an array for example
;;key it's use partition and realize that it works kind of like binary search since the pivot-index choosen at random will always be in the right spot relative to where you're looking
(defun rselect (arr st end i)
  ;(format t "i:~a, st:~a, end:~a array=~a ~%"i st end (subseq arr st (1+ end)))
  (when (>= (- end st) 0)
    (let ((pivot-index (partition arr st end)))
      ;(format t "pivot-index:~a, array=~a ~%" pivot-index (subseq arr st (1+ end)) )
      (cond ((equalp i pivot-index)
	     (aref arr pivot-index))
	    ((> i pivot-index) ;xxxxpxixxx ;case where it's on the right hand side
	     ;(format t "i>pivot pivindex: ~a, a[~a]=~a, i:~a, st:~a, end:~a~%" pivot-index pivot-index (aref arr pivot-index) i st end )
	     (rselect arr (1+ pivot-index) end i)) ; (1- (- i pivot-index)))) ;note if it's on the rgith side we want the 
	    (t ;xxixxpxxxxxx ;ith order statistic is in the left side
	     ;(format t "i<pivot pivindex: ~a, a[~a]=~a, i:~a, st:~a, end:~a~%" pivot-index pivot-index (aref arr pivot-index) i st end )
	     (rselect arr st (1- pivot-index) i)))))) ; (- i st)) )))))
;;;note we're always passing the full array so i just stays the same - drew it out and don't need to mess with i if we're always passing the whole array through (espically sine partition messes up the order of the elements in the array

;;notice how this is better than sorting then then finding the xth element
;;faster than nlogn 
(defun ith-order-statistic (arr i)
  (rselect arr 0 (1- (length arr)) (1- i)))

;(ith-order-statistic #(10 8 2 4) 3); returns 8
;(ith-order-statistic *arr-qs* 3123); returns 3123 but doesn't sort the array!
;(ith-order-statistic *arr-qs* 10001) ; nil


