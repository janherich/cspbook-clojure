(ns cspbook-clojure.change-implementation)

(defn change 
  "Change event symbols of some process p with inverse-change function q"
  [g p]
  (fn [x]
    (if (= 'BLEEP (g x))
      'BLEEP
      (if (= 'BLEEP (p (g x)))
        'BLEEP
        (change g (p (g x)))))))

(defn label
  "Returns labeled version of some process p"
  [l p]
  (fn [y]
    (if (not (seq? y))
      'BLEEP
      (if (not= l (first y))
        'BLEEP
        (if (= 'BLEEP (p (second y)))
          'BLEEP
          (label l (p (second y))))))))
