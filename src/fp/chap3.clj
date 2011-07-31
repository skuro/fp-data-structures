(ns fp.chap3
  (:use midje.sweet)
  (:import clojure.lang.PersistentQueue))

;; in this implementation the queue holds values
;; in two different buckets, front and rear.
;; front is empty only if rear is empty.
(defn make-queue
  [f r]
  {:f f
   :r r})

(defn emptyq
  []
  (make-queue '() '()))

;; the invariant for the queue is that f is empty only if also r is
;; empty, otherwise f is assigned (reverse r)
(defn honor-invariant
  [q]
  (if (empty? (:f q))
    (make-queue (reverse (:r q)) '())
    q))

;; returns the first element in the queue
(defn head
  [q]
  (first (:f q)))

;; normally adds to the front of rear, but to keep
;; the invariant it adds to front in case it's empty
(defn snoc
  [q x]
  (honor-invariant (make-queue (:f q)
                               (conj (:r q) x))))

(fact "multiple inserts"
  (-> (emptyq)
      (snoc 1)
      (snoc 2)
      (snoc 3)) => {:f '(1)
                    :r '(3 2)})

;.;. FAIL at (NO_SOURCE_FILE:2)
;.;.     Expected: {:f (10), :r ()}
;.;.       Actual: 10
(fact "insert on empty"
  (snoc (emptyq) 10) => {:f '(10)
                         :r '()})
;; normally takes out the head of front, but to keep
;; the invariant it reverses rear and moves it to front
;; in case front has only one value to pop
(defn tail
  [q]
  (honor-invariant (make-queue (rest (:f q))
                               (:r q))))

;; not part of the exercise, but useful to write tests
(defn consume
  [q f]
  (f (head q))
  (tail q))

(fact "consume an empty queue"
  (consume (emptyq) identity) => {:f '() :r '()})
