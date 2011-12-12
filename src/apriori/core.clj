(ns apriori.core
  (:use [clojure.set :only (subset? difference)])
  (:use [clojure.contrib.combinatorics :only (combinations)]))

(defn- get-k-subsets [s k]
  "Generate a set of k-itemsets from set s."
  (set (map #(apply sorted-set %) (combinations s k))))

(defn- satisfies-apriori-property? [s l-k k]
  "Every subset of a frequent itemset must also be a
frequent itemset."
  (every? #(contains? l-k %) (get-k-subsets s k)))

(defn- join [l-k k]
  (let [cs (combinations l-k 2)]
    (reduce 
     (fn [ret [a b]]
       (let [k-1 (dec k)
             ks (take k-1 a)
             found (= ks (take k-1 b))]
         (if found (conj ret (into a (difference b ks))) ret)))
     #{} cs)))

(defn- prune
  "Remove k-itemsets that do not satisfy the apriori-property." 
  [c-k l-k-1 k]
  (reduce
   (fn [ret x]
     (if (satisfies-apriori-property? x l-k-1 k) (conj ret x) ret))
   [] c-k))

(defn- generate-candidate-set
  "Generate k-th candidate set."
  [l-k-1 k]
  (prune (join l-k-1 (dec k)) l-k-1 (dec k)))

(defn- get-minimum-support-itemset
  "Produces minimum support itemset."
  [s min-support]
  (set (for [[k v] s :when (>= v min-support)] k)))

(defn- scan-D 
  "The transactions in D are scanned and the support
count for each candidate k-itemset in c-k is accumulated."
  [D c-k]
  (reduce 
   (fn [ret c] 
     (assoc ret c
            (reduce (fn [ret x]
                      (if (subset? c x)
                        (inc ret) 
                        ret))
                    0 D)))
   {} c-k))

(defn apriori-pass
  [l-k-1 D min-support k]
  "Calculates k-th apriori pass."
  (let [c-k (generate-candidate-set l-k-1 k)
        l-k (get-minimum-support-itemset (scan-D D c-k) min-support)]
    l-k))

(defn apriori-pass-1 [D min-support]
  (let [ks (map sorted-set (reduce #(into %1 %2) D))
        l-1 (get-minimum-support-itemset (scan-D D ks) min-support)]
    l-1))

(defn generate-frequent-itemsets [D min-support]
  (loop [f-itemsets []
         l-k (apriori-pass-1 D min-support)
         k 2]
    (if (empty? l-k)
      f-itemsets
      (recur
       (conj f-itemsets l-k)
       (apriori-pass l-k D min-support k)
       (inc k)))))
