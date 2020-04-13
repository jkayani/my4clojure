(defn fizzbuzz [n] 
 (if (= 0 (rem n 15))
    (println "fizzbuzz")
    (if (= 0 (rem n 5))
        (println "buzz")
        (if (= 0 (rem n 3))
            (println "fizz")
            (println)
        )
    )
 )
 (if (= 1 n) "" (fizzbuzz (- n 1)))
)
        
(defn fizzbuzz2 [n]
    (let [
        rems (map #(rem n %) '(15 5 3))
        strings (map #(if (= %1 0) %2 nil) rems '("fizzbuzz" "buzz" "fizz"))
        str (reduce #(if (nil? %2) %1 (conj %2 %1)) [""] strings)
    ]
    (println (first str))))


; Lazily returns a list of intermediate "reductions" (reduce f v c)
(defn red [f & [v c]]
      (letfn [

        ; Used when fun, val, and collection are all explicitly defined
        (allDefined [fun val coll] 
          (lazy-seq 

            ; nxtArg is the second arg to `fun`
            (let [nxtArg (first coll)]
              (if (nil? nxtArg)

                ; No more args, so no more function calls needed. 
                ; Prepend the final value to the list to end the recursion
                (cons val '())

                ; Generate next value as `(fun val nxtArg)`
                ; Prepend the current value to the list, and recur with the next value
                ; and rest of the list
                (cons val 
                  (allDefined fun (fun val nxtArg) (rest coll)))))))  

        ; Used when only fun and coll are defined
        (partialDefined [fun coll]
          (lazy-seq 

          ; Take the first elm of `coll` as the val, and second elm as nxtArg
          (let [val (first coll)
                nxtArg (first (rest coll))]

            ; Same logic as above
            (if (nil? nxtArg)
              (cons val '())
              (cons val 

                ; Now that `val` is defined, we use mutual recursion to call `allDefined`
                (allDefined fun (fun val nxtArg) (rest (rest coll))))))))]

      (if (nil? (and f v c))

        ; `c` was missing, which means f is `fun` and v is `coll`
        (partialDefined f v)

        ; f is `fun`, v is `val`, and c is `coll`
        (allDefined f v c)))
)  
(red conj [1] [2 3 4])
(take 5 (red + 0 (rest (range))))
(take 5 (red + (range)))

; Calculates nth row of Pascal's triangle
(defn pascal [rowNum]
  (let [
    calc (fn [prevRow]
      (flatten 
        (conj 
          '(1)
          (map #(apply + %) (partition 2 1 prevRow))
          '(1))))]
    (last (take rowNum (iterate calc '(1))))))

; Rotates a sequence by factor of `n`
; If n is positive, it rotates from the left. Else, from the right
(defn rotate [n lst]
  (let [lstCount (count lst)
        stretch (mod n lstCount)]
        (->> (take (+ lstCount stretch) (cycle lst))
          (drop stretch)
          (take lstCount))))

(defn toilet [x]
  (let [
    remod (fn r [a b]
      (let [res (mod a b)]
        (if (zero? res) b #(r b res))))]
    (if (= 1 x)
      1
      (->> (range 1 x)
        (filter #(= 1 (trampoline remod x %)))
        (count)))))

; Tree traversals
(def tree 
  {
    :data 1
    :left 
      {
        :data 2
        :left
          {
            :data 4
            :left nil
            :right nil
          }
        :right 
          {
            :data 6
            :left nil
            :right nil
          }
      }
    :right 
      {
        :data 3
        :left 
          {
            :data 5
            :left nil
            :right nil
          }
        :right 
          {
            :data 7
            :left nil
            :right nil
          }
      }
  })

(defn preorder [tree fun]
  (letfn [
    
    (p [t f]
      (if (nil? (t :left))
        (fun t)
        (conj [] (f t) (p (t :left) f) (p (t :right) f))))]

    (flatten (p tree fun))))

(defn postorder [tree fun]
  (letfn [
    
    (p [t f]
      (if (nil? (t :left))
        (fun t)
        (conj [] (p (t :left) f) (p (t :right) f) (f t))))]

    (flatten (p tree fun))))

(defn inorder [tree fun]
  (letfn [
    
    (p [t f]
      (if (nil? (t :left))
        (fun t)
        (conj [] (p (t :left) f) (f t) (p (t :right) f))))]

    (flatten (p tree fun))))

; Breadth First Search of a tree
(defn bfs [tree fun]
  (letfn [

    (traverse [queue f]
      (let [curr (peek queue)]
        (when (not (nil? curr))
          (conj 
              [] 
              (f curr) 
              (traverse (conj (pop queue) (curr :left) (curr :right)) f)))))]

  (flatten (traverse (conj (clojure.lang.PersistentQueue/EMPTY) tree) fun))))

; Generates the powerset for `inset`
(defn powerset [s]
  (let [

    ; The set containing each element wrapped in a set
    first-gen
      (set (map #(conj #{} %) s))

    ; Generates the next iteration of the powerset
    ; @param i-set The previous iteration of the powerset
    next-generation
      (fn [i-set]

        ; Iterate over each member of the previous generation
        (reduce
          (fn [accum-set pset-member]
            (clojure.set/union 

              ; Create a set of sets containing each element combined with a previous generation set member
              (reduce
                (fn [out-set elm]
                  (conj 
                    out-set
                    (conj pset-member elm)))
                #{}
                s)

              ; Merge the above set with others created
              accum-set))
          #{}
          i-set))

    ; Filters out a set to only the subsets of size k
    size-k?
      (fn [s k] 
        (filter #(= k (count %)) s))]
  (conj
    (->> 

      ; Start with the size of the original set
      (count s)

      ; Increment it since we need to use it in a range
      (inc)

      ; Generate a range from 2 to the size of the original set, inclusive
      (range 2)

      ; Generate a sequence of the all powerset generations for size 2 to k
      (reduce
        #(cons 
          (next-generation
            (size-k? (first %1) %2))
          %1)
        (list (next-generation first-gen)))
      
      ; Put all the sets together
      (apply clojure.set/union))

    ; Add the empty set
    #{})))