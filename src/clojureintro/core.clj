(ns clojureintro.core
  (:use bakery.core))

(defn error [& rs]
  (do
    (apply println rs)
    :error))

(def baking {:recipes {:cake {:ingredients {:egg 2
                                            :flour 2
                                            :sugar 1
                                            :milk 1}
                              :steps [[:add :all]
                                      [:mix]
                                      [:pour]
                                      [:bake 25]
                                      [:cool]]}
                       :cookies {:ingredients {:egg 1
                                               :flour 1
                                               :butter 1
                                               :sugar 1}
                                 :steps [[:add :all]
                                         [:mix]
                                         [:pour]
                                         [:bake 30]
                                         [:cool]]}
                       :brownies {:ingredients {:egg 2
                                               :flour 2
                                               :butter 2
                                               :milk 1
                                               :cocoa 2
                                               :sugar 1}
                                 :steps [[:add :butter]
                                         [:add :sugar]
                                         [:add :cocoa]
                                         [:mix]
                                         [:add :egg]
                                         [:add :flour]
                                         [:add :milk]
                                         [:mix]
                                         [:pour]
                                         [:bake 35]
                                         [:cool]]}}
             :ingredients {:egg {:storage :fridge
                                 :usage :squeezed}
                           :milk {:storage :fridge
                                  :usage :scooped}
                           :flour {:storage :pantry
                                   :usage :scooped}
                           :butter {:storage :fridge
                                    :usage :simple}
                           :sugar {:storage :pantry
                                   :usage :scooped}
                           :cocoa {:storage :pantry
                                   :usage :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release))})

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
   (let [u (:usage (ingredient (:ingredients baking)))
         f (usage u)]
     (f ingredient amount))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {} (for [[ingredient amount] ingredients]
             [ingredient (* amount n)])))

(defn order->ingredients [order]
  (reduce add-ingredients (for [[item amount] (:items order)]
                            (multiply-ingredients amount (:ingredients (item (:recipes baking)))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map order->ingredients orders)))


(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (do
     (go-to (:storage (ingredient (:ingredients baking))))
     (load-up-amount ingredient amount)
     (go-to :prep-area)
     (unload-amount ingredient amount))))

(defn fetch-list [shopping-list]
  (let [with-storage (for [[ingredient amount] shopping-list]
                       {:ingredient ingredient
                        :amount amount
                        :storage (:storage (ingredient (:ingredients baking)))})]
    (doseq [[location ingredients](group-by :storage with-storage)]
      (go-to location)
      (doseq [ingredient ingredients]
        (load-up-amount (:ingredient ingredient) (:amount ingredient)))
      (go-to :prep-area)
      (doseq [[ingredient amount] shopping-list]
        (unload-amount ingredient amount)))))

(def actions {:cool (fn [recipe]
                      (cool-pan))
              :mix (fn [recipe]
                     (mix))
              :pour (fn [recipe]
                      (pour-into-pan))
              :bake (fn [recipe time]
                      (bake-pan time))
              :add (fn
                     ([recipe ingredient]
                      (cond
                        (= :all ingredient)
                        (doseq [[ingredient amount] (:ingredients recipe)]
                          (add ingredient amount))
                        (contains? (:ingredients recipe) ingredient)
                        (add ingredient (ingredient (:ingredients recipe)))))
                     ([recipe ingredient amount]
                      (add ingredient amount)))})

(defn perform
  [recipe step]
  (let [action (first step)
        f (action actions)]
    (apply f recipe (rest step))))

(defn bake-recipe [recipe]
  (last (for [step (:steps recipe)]
    (perform recipe step))))

(defn bake [item]
  (cond
   (contains? (:recipes baking) item)
   (bake-recipe (item (:recipes baking)))
   :else
   (error "I don't know how to bake " item)))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredient-list (orders->ingredients orders)]
    (fetch-list ingredient-list)
    (doseq [order orders]
      (let [items (:items order)
            racks (for [[item amount] items
                        i (range amount)]
                    (bake item))
            receipt {:orderid (:orderid order)
                     :address (:address order)
                     :rackids racks}]
        (delivery receipt)))))


(defn -main []
  (day-at-the-bakery))
