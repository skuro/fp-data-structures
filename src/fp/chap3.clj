(ns fp.chap3
  (:import clojure.lang.PersistentQueue))

;; in this implementation the queue holds values
;; in two different buckets, front and rear.
;; front is empty only if rear is empty.
(defn create-queue
  []
  (atom {:f '()
         :r '()}))

;; normally adds to the front of rear, but to keep
;; the invariant it adds to front in case it's empty
(defn snoc
  [q x]
  (let [k (if (empty? (:f @q)) :f :r)]
    (swap! q #(assoc % k (conj (% k) x)))))

;; normally takes out the head of front, but to keep
;; the invariant it reverses rear and moves it to front
;; in case front has only one value to pop
(defn tail
  [q]
  (let [res (head q)]
    (if (= 1 (count (:f @q)))
      (swap! q #(let [new-f (reverse (:r %))] {:f new-f :r '()})))
    res))

(defn head
  [q]
  (first (:f @q)))

(defn tail
  [q]
  (if (empty? (:r @q))
    (last (:f @q))
    (first (:r @q))))
