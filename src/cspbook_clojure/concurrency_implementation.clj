(ns cspbook-clojure.concurrency-implementation
  (:require [cspbook-clojure.processes-implementation :refer [prefix choice2]]
            [cspbook-clojure.traces-implementation :refer [ismember]]))

(defn intersect
  "Creates new process as intersection of two concurrent processes p and q with same alphabets"
  [p q]
  (fn [z]
    (if (or (= 'BLEEP (p z)) (= 'BLEEP (q z)))
      'BLEEP
      (intersect (p z) (q z)))))

(def noisyvm (prefix 'COIN (prefix 'CLINK (prefix 'CHOC (prefix 'CLUNK noisyvm)))))

(def cust (prefix 'COIN (choice2 'TOFFEE cust 'CURSE (prefix 'CHOC cust))))

(def noisyvm-cust (prefix 'COIN (choice2 'CLINK (prefix 'CURSE (prefix 'CHOC (prefix 'CLUNK noisyvm-cust)))
                                         'CURSE (prefix 'CLINK (prefix 'CHOC (prefix 'CLUNK noisyvm-cust))))))


(defn concurrent
  "Creates new process as a process of two concurrent process with possibly different alphabets"
  [p a b q]
  (fn [x]
    (if (and (ismember x a) (ismember x b))
      (if (or (= 'BLEEP (p x)) (= 'BLEEP (q x)))
        'BLEEP
        (concurrent (p x) a b (q x)))
      (if (ismember x a)
        (if (= 'BLEEP (p x))
          'BLEEP
          (concurrent (p x) a b q))
        (if (ismember x b)
          (if (= 'BLEEP (q x))
            'BLEEP
            (concurrent p a b (q x)))
          'BLEEP)))))


