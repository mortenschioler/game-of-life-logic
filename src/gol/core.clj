(ns gol.core)

(def mini-board
  [[{:alive true} {:alive false} {:alive false}]
   [{:alive true} {:alive false} {:alive false}]
   [{:alive false} {:alive false} {:alive true}]])

(defn neighbouring-indexes
  [[x y]]
  (let [vicinity (juxt dec identity inc)]
    (-> (for [x (vicinity x) y (vicinity y)] [x y])
        set
        (disj [x y]))))

(defn neighbours-where
  [pred board [x y]]
  (->> (neighbouring-indexes [x y])
       (map (partial get-in board))
       (filter pred)))

(def crowdedness
  (comp count (partial neighbours-where :alive)))

(defn lives?
  [is-alive crowdedness]
  (if is-alive
    (some? (#{2 3} crowdedness))
    (= 3 crowdedness)))

(defn get-dims
  [board]
  [(count board)
   (count (first board))])

(defn transform-board
  [board]
  (let [[X Y] (get-dims board)]
   (reduce
     (fn [acc [x y]]
       (update-in acc [x y :alive] lives? (crowdedness board [x y])))
     board
     (for [x (range X) y (range Y)] [x y]))))
