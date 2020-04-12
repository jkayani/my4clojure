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
(defn powerset [inset]
  (letfn [

    ; Merge a list of vectors into a set
    ; @param coll list of 2D vectors like '( [[1 2] [1 3]] [[3 4]] )
    (merge-vecs [coll]
      (reduce
        #(apply conj %1 %2)
        #{}
        coll))

    ; Gets the portion of list after a value
    ; @param lst The list to pull from
    ; @param value The sentinel value 
    (after [lst value] (drop (inc (.indexOf lst value)) lst))

    ; Get the last item from a vector in O(1) time

    (lastv [v] (v (dec (count v))))

    ; Generate the next sequence of values for a powerset, given the previous sequence
    ; @param inseq A list of the original set's values
    ; @param last-level 
      ; A 2D vector where each inner vector is an element of the previous sequence,
      ; aka a member of the powerset
    (next-level [inseq last-level]

      (let [
        size (inc (count (last-level 0)))

        tails (mapv lastv last-level)

        chains (mapv (partial after inseq) tails)

        next-groups 
          (fn [total start values]
            (let [
              next-total 
                (reduce 
                  #(conj %1 (conj start %2))
                  total
                  values)]
              (if (< (count values) size)
                next-total
                (recur next-total [(first values)] (rest values)))))

        coll (map #(next-groups [] %1 %2) last-level chains)]
       
        (->> coll
          (filter (complement empty?))
          merge-vecs
          (vec))))]
  (let [
    as-seq (seq inset)
    base (mapv #(conj [] %) inset)
    subgroups
      (take-while 
        (complement empty?) 
        (iterate (partial next-level as-seq) base))]

    (->> (merge-vecs subgroups)
      (map set)
      (set)
      (clojure.set/union #{#{}}))))) 