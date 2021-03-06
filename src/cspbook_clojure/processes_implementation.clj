(ns cspbook-clojure.processes-implementation)

(comment
  "We should first define some 'primitives' from which more complicated processes are composed.
   Note that instead of LABEL feature of some LISP implementations, which is suggested in the 
   book to define recursive processes, we will implement it by delayed evaulation of expessions,
   this wouldn't be necessary in languages with lazy evaulation of function arguments, such as
   for example Haskell.")

(defmacro prefix
  "Define prefix process primitive, with c standing for allowed event and p for (quarded) process.
   This macro will return function representing the process guarded by some prefix event c."
  [c p]
  `(fn [~'x]
     (if (= ~'x ~c) ~p ~''BLEEP)))

(defmacro choice2
  "Define choice2 process primitice, with c standing form allowed event for process p and d standing
   for allowed event for process q. This macro will return function representing the process which
   models the choice between two successor processes, each with its own guard event."
  [c p d q]
  `(fn [~'x]
     (cond 
      (= ~'x ~c) ~p
      (= ~'x ~d) ~q
      :else ~''BLEEP)))

(comment 
  "Simple vending machine accepting recursive sequence of 'COIN 'CHOC 'COIN 'CHOC ...")

(def vms (prefix 'COIN (prefix 'CHOC vms))) 

(comment 
  "Process representing movement of the object in different levels, with possible movements on the
   ground (level 0) to be 'AROUND and 'UP and possible movements on each level above ground to be
   'UP and 'DOWN. Movement 'AROUND does not change anything and object stays on ground, while
   movements 'UP and 'DOWN change the object level one up and one down respectively.")

(defn ct [n]
  (if (= n 0)
    (choice2 'AROUND (ct 0) 'UP (ct 1))
    (choice2 'UP (ct (inc n)) 'DOWN (ct (dec n)))))

(comment
  "Auxiliary process functions")

(defn menu
  "Function, which returns sequence of valid events for some state of process p, given an sequence of
   all valid events (a) for the process p"
  [a p]
  (if (seq a)
    (if (= 'BLEEP (p (first a)))
      (menu (rest a) p)
      (cons (first a) (menu (rest a) p)))
    a))

(defn interact
  "Function describing interaction with some process - a is the sequence of all valid events for the
   process, p is the process in the starting state and k is the sequence of input events."
  [a p k]
  (cons (menu a p) (if (= 'END (first k))
                     nil
                     (if (= (p (first k)) 'BLEEP)
                       (cons 'BLEEP (interact a p (rest k)))
                       (interact a (p (first k)) (rest k))))))
