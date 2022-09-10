(ns awbc.shortest-path)

;; TODO
;;
(declare neighbors
         process-neighbour
         prepare-costs
         get-next-node
         unwind-path
         all-shortest-paths)

;; Implementation details

(defn prepare-costs
  "For given start node A ang graph prepare map of costs to start with
  (assign maximum value for all nodes and zero for starting one).
  Also save info about most advantageous parent.
  Example output: {2 [2147483647 7], 6 [2147483647 14]}
                   ^   ^         ^
                   |   |         |
                node   |         |
               cost-----         |
             parent---------------
  "
  [start graph]
  (assoc (zipmap (keys graph)
                 (repeat [##Inf nil]))
         start [0 start]))

(defn neighbors
  "Get given node's neighbors along with their own costs and costs of corresponding edges.
  Example output is: {1 [7 10] 2 [4 15]}
                      ^  ^  ^
                      |  |  |
   neighbour node label  |  |
        neighbour cost ---  |
             edge cost ------"
  [node graph costs]
  (->> (graph node)
       (map (fn [[neighbour edge-cost]]
              [neighbour [(first (costs neighbour)) edge-cost]]))
       (into {})))


(defn process-neighbour
  [parent
   parent-cost
   costs
   [neighbour [old-cost edge-cost]]]
  (let [new-cost (+ parent-cost edge-cost)]
    (if (< new-cost old-cost)
      (assoc costs
             neighbour
             [new-cost parent])
      costs)))


(defn get-next-node [costs unvisited]
  (->> costs
       (filter (comp unvisited first))
       (sort-by (comp first second))
       ffirst))

(defn unwind-path
  "Restore path from A to B based on costs data"
  [a b costs]
  (letfn [(f [a b costs]
            (when-not (= a b)
              (cons b (f a (second (costs b)) costs))))]
    (cons a (reverse (f a b costs)))))

(defn all-shortest-paths
  "Get shortest paths from start to all reachable nodes, along with their costs"
  [start costs]
  (let [paths (->> (keys costs)
                   (remove #{start})
                   (map (fn [n] [n (unwind-path start n costs)])))]
    (into (hash-map)
          (map (fn [[n p]]
                 [n [(first (costs n)) p]])
               paths))))

(defn dijkstra*
  "Given two nodes A and B, and graph, finds shortest path from point A to point B.
  Given one node and graph, finds all shortest paths to all other nodes.

  Graph example:
                 {1 {2 7, 3 9, 6 14}
                  2 {1 7, 3 10, 4 15}
                  3 {1 9, 2 10, 4 11, 6 2}
                  4 {2 15, 3 11, 5 6}
                  5 {6 9, 4 6}
                  6 {1 14, 3 2, 5 9}}
                  ^  ^  ^
                  |  |  |
         node label  |  |
    neighbour label---  |
          edge cost------
  From example in Wikipedia: https://en.wikipedia.org/wiki/Dijkstra's_algorithm

  Output example: [20 [1 3 6 5]]
                   ^  ^
                   |  |
  shortest path cost  |
       shortest path---"
  ([a b graph]
   (loop [costs (prepare-costs a graph)
          unvisited (set (keys graph))]
     (let [current-node (get-next-node costs unvisited)
           current-cost (first (costs current-node))]
       (cond (nil? current-node)
             (all-shortest-paths a costs)

             (= current-node b)
             [current-cost (unwind-path a b costs)]

             :else
             (recur (reduce (partial process-neighbour
                                     current-node
                                     current-cost)
                            costs
                            (filter (comp unvisited first)
                                    (neighbors current-node graph costs)))
                    (disj unvisited current-node))))))
  ([a graph] (dijkstra* a nil graph)))

;; adapt it to the thing
