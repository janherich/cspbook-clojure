(ns cspbook-clojure.concurrency-implementation)

(defn intersect
  "Creates new process as intersection of two concurrent processes p and q"
  [p q]
  (fn [z]
    (if (or (= 'BLEEP (p z)) (= 'BLEEP (q z)))
      'BLEEP
      (intersect (p z) (q z)))))
