(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
      (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         sequence a-seq]
    (cond
     (empty? sequence) nil
     (pred (first sequence)) i
     :else (recur (inc i) (rest sequence)))))

(defn avg [a-seq]
  (loop [a-sum 0
         a-count 0
         sequence a-seq]
    (if (empty? sequence)
      (/ a-sum a-count)
      (recur (+ a-sum (first sequence)) (inc a-count) (rest sequence)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [memo #{}
           sequence a-seq]
      (if (empty? sequence)
        memo
        (recur (toggle memo (first sequence)) (rest sequence))))))

(defn fast-fibo [n]
  (loop [f-n 1
         f-n1 0
         i 1]
    (cond
     (zero? n) f-n1
     (== i n) f-n
     :else (recur (+ f-n f-n1) f-n (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         sequence a-seq]
    (cond
     (empty? sequence) result
     (contains? (set result) (first sequence)) result
     :else (recur (conj result (first sequence))
                  (rest sequence)))))
