(ns loco-playground.core
  (:require [loco.core :refer :all]
            [clojure.pprint :refer :all]
            [loco.constraints :refer :all]))

;; http://bypacoman.blogspot.com.es/2013/08/desestructurando-el-puzzle-de-einstein.html

;; ---- Datos de referencia

(def domain
  {:color    [:red       :blue   :green     :yellow :white   ]
   :country  [:england   :spain  :france    :norway :japan   ]
   :beverage [:tea       :milk   :tomato    :coffee :beer    ]
   :animal   [:dog       :horse  :fox       :cat    :zebra   ]
   :job      [:detective :doctor :architect :lawyer :engineer]})

(defn ->val [prop name]
  (.indexOf (domain prop) name))

(defn ->name [[prop value]]
  (get-in domain [prop value]))

;; --- Funciones de ayuda para construir las restricciones

(defn all-distinct [prop]
  ($distinct (map (partial vector prop) (range 5))))

(defn fact
  "Fija una propiedad [:color 4 :red] o enlaza dos [:color :red] <=> [:country :england]"
  ([[prop idx name]]
   ($= [prop idx] (->val prop name)))
  ([[prop1 name1] [prop2 name2]]
   (apply $or (map #($and ($= [prop1 %] (->val prop1 name1)) ($= [prop2 %] (->val prop2 name2)))
                   (range 5)))))

(defn right-fact
  "Enlaza una propiedad con el vecino de la derecha "
  [[prop1 name1] [prop2 name2]]
  (apply $or (map #($and (fact [prop1 % name1]) (fact [prop2 (inc %) name2])) (range 4))))

(defn side-fact 
  "Enlaza una propiedad con algún vecino (derecha o izquierda)"
  [[prop1 name1] [prop2 name2]]
  (apply $or
         (concat
          (map #($and (fact [prop1 % name1]) (fact [prop2 (inc %) name2])) (range 4))
          (map #($and (fact [prop1 % name1]) (fact [prop2 (dec %) name2])) (range 1 5)))))


;; ---- Creación del modelo a resolver

(def model

  (conj

   ;; Variables [:country i] [:animal i] [:color i] [:beverage i] [:job i] con i <- [0,5)
   (for [prop (keys domain)
         idx (range 5)]
     ($in [prop idx] (range 5)))

   ;; Todas las variables de cada propiedad deben ser diferentes
   (all-distinct :color)
   (all-distinct :animal)
   (all-distinct :beverage)
   (all-distinct :country)
   (all-distinct :job)

   ;; La casa del inglés es roja
   (fact [:country :england] [:color :red])
   
   ;; El español tiene un perro
   (fact [:country :spain] [:animal :dog])
   
   ;; El japonés es detective
   (fact [:job :detective] [:country :japan])
   
   ;; El francés bebe té
   (fact [:country :france] [:beverage :tea])
   
   ;; La casa blanca está inmediatamente a la derecha de la casa verde
   (right-fact [:color :green] [:color :white])
   
   ;; En la casa del medio se bebe leche
   (fact [:beverage 2 :milk])
   
   ;; La casa del noruego es la primera a la izquierda
   (fact [:country 0 :norway])
   
   ;; La casa vecina al noruego es azul
   (side-fact [:country :norway] [:color :blue])
   
   ;; La casa amarilla es del médico
   (fact [:color :yellow] [:job :doctor])
   
   ;; El vecino del médico tiene un caballo
   (side-fact [:job :doctor] [:animal :horse])
   
   ;; El vecino del inglés es arquitecto
   (side-fact [:country :england] [:job :architect])
   
   ;; El vecino del arquitecto tiene un zorro
   (side-fact [:job :architect] [:animal :fox])
   
   ;; El abogado bebe zumo de tomate
   (fact [:job :lawyer] [:beverage :tomato])
   
   ;; El ingeniero tiene un gato
   (fact [:job :engineer] [:animal :cat])
   
   ;; En la casa verde se bebe café
   (fact [:color :green] [:beverage :coffee])))


(defn solve []
  (letfn [(format-prop [[[name idx] val]]
            [name (->name [name val])])]
    (->> (solution model)
         (group-by (fn [[[name idx] _]] idx))
         (map (fn [[idx props]] [idx (sort-by first (map format-prop props))]))
         (sort-by first))))

(print "\n\n------ SOLUTION -------\n\n")
(pprint (solve))

