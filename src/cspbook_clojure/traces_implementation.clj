(ns cspbook-clojure.traces-implementation)

(comment
  "Functions defining operations on (process) traces")

(defn ismember
  "Returns true if symbol x is member of some list of symbols b"
  [x b]
  (if (seq b)
    (if (= x (first b))
      true
      (ismember x (rest b)))
    false))

(defn restrict
  "Restricts some trace s to members of some list of symbols b"
  [s b]
  (if (seq s)
    (if (ismember (first s) b)
      (cons (first s) (restrict (rest s) b))
      (restrict (rest s) b))
    '()))

(defn isprefix
  "Determines if some trace s is prefix of some trace t"
  [s t]
  (if (seq s)
    (if (seq t)
      (and (= (first s) (first t)) (isprefix (rest s) (rest t)))
      false)
    true))

(defn istrace
  "Determines if some list of symbols s could be the trace of some process p"
  [s p]
  (if (seq s)
    (if (= (p (first s)) 'BLEEP)
      false
      (recur (rest s) (p (first s))))
    true))
