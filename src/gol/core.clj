(ns gol.core)

(def mini-board
  [[{:alive true} {:alive false} {:alive false}]
   [{:alive true} {:alive false} {:alive false}]
   [{:alive false} {:alive false} {:alive true}]])

(defn neighbour-indexes
  [[x y]]
  (let [vicinity (juxt dec identity inc)]
    (for [x (vicinity x) y (vicinity y)
          :when (not= x y)]
      [x y])))

(defn neighbour-count
  [board [x y]]
  (->> (neighbour-indexes [x y])
       (map (partial get-in board))
       (filter :alive)
       count))

(defn transform-cell
  [cell num-neighs]
  (assoc cell :alive 
    (if (:alive cell)
      (some? (#{2 3} num-neighs))
      (= 3 num-neighs))))

(defn get-dims
  [board]
  [(count board)
   (count (first board))])

(defn transform-board
  ([board [X Y]]
   (reduce
     (fn [acc [x y]]
       (assoc-in acc [x y] (transform-cell (get-in board [x y]) (neighbour-count board [x y]))))
     board
     (for [x (range X) y (range Y)] [x y])))
  ([board]
   (transform-board board (get-dims board))))
