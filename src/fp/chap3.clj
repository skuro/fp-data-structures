(ns fp.chap3
  (:import clojure.lang.PersistentQueue))

;; in this implementation the queue holds values
;; in two different buckets, front and rear.
;; front is empty only if rear is empty.
(defn make-queue
  [f r]
  (atom {:f f
         :r r}))

(defn check-invariant
  [q]
  (if (empty? (:f @q))
    (swap! q #(make-queue (reverse (:r @q))
                          '()))))

;; returns the first element in the queue
(defn head
  [q]
  (first (:f @q)))

;; normally adds to the front of rear, but to keep
;; the invariant it adds to front in case it's empty
(defn snoc
  [q x]
  (check-invariant (make-queue (:f @q)
                               (conj (:r @q) x))))

;; normally takes out the head of front, but to keep
;; the invariant it reverses rear and moves it to front
;; in case front has only one value to pop
(defn tail
  [q]
  (check-invariant (make-queue (rest (:f @q))
                               (:r @q))))
