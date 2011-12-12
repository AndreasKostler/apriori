(ns apriori.test.core
  (:use [apriori.core])
  (:use [clojure.test]))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(def +transactions-1+
  [#{:l1 :l2 :l5}
   #{:l2 :l4}
   #{:l2 :l3}
   #{:l1 :l2 :l4}
   #{:l1 :l3}
   #{:l2 :l3}
   #{:l1 :l3}
   #{:l1 :l2 :l3 :l5}
   #{:l1 :l2 :l3}])

(def +transactions-2+
  [#{:A :B :C}
   #{:A :B :C :D :E}
   #{:A :C :D}
   #{:A :C :D :E}
   #{:A :B :C :D}])

(with-private-fns [apriori.core [generate-k-subsets satisfies-apriori-property?])
  (deftest test-get-k-subsets
    (is (= #{#{:A :B}#{:A :C}#{:B :C}}
           (generate-k-subsetes #{:A :B :C} 2)))
    (is (= #{#{:A :B :C}#{:A :B :D}#{:B :C :D}}
           (generate-k-subsetes #{:A :B :C :D}))))
  (deftest test-satisfies-apriori-property?
    (is (true? (satisfies-apriorit-property?
                #{:A :B :C}
                #{#{:A :B}#{:A :C}#{:B :C}}
                2)))
    (is (false? (satisfies-apriorit-property?
                #{:A :B :D}
                #{#{:A :B}#{:A :C}#{:B :C}}
                2))))
  (deftest test-join
    (is (= #{(sorted-set :A :B :D)
             (sorted-set :A :B :E)
             (sorted-set :A :C :D)
             (sorted-set :A :C :E)
             (sorted-set :A :D :E)
             (sorted-set :B :C :D)
             (sorted-set :C :D :E)}
           (join #{
                   (sorted-set :A :B)
                   (sorted-set :A :C)
                   (sorted-set :A :E)
                   (sorted-set :B :C)
                   (sorted-set :B :D)
                   (sorted-set :C :D)
                   (sorted-set :C :E)
                   (sorted-set :D :E)} 2))
  
               

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))
