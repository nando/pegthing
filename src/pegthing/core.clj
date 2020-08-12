(ns pegthing.core
  (:require [pegthing.board :as board :refer [print-board alpha-start new-board]]
            [pegthing.pegs :as pegs]))

(declare successful-move prompt-move game-over prompt-rows)

;;;;
;; Interaction
;;;;
(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  "Given a string, return a collection consisting of each individual
  character"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (pegs/remove-peg board (letter->pos (get-input "e"))) board false))

(defn performance-category
  [remaining-pegs]
  (cond
    (>= remaining-pegs 5) "Abysmal! Is this your first time?"
    (= remaining-pegs 4) "Poor. Try harder."
    (= remaining-pegs 3) "OK. You could use some strategy."
    (= remaining-pegs 2) "Good. See if you can do better."
    (= remaining-pegs 1) "AMAZING! You win."
    :else (format "Remaining pegs (%s) is an unexpected number" remaining-pegs)))

(defn print-performance
  [remaining-pegs]
  (let [result (performance-category remaining-pegs)]
    (println (format "Result: %s" result))))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (print-performance remaining-pegs)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn successful-move
  [board old-board]
  (if (pegs/can-move? board)
    (prompt-move board old-board)
    (game-over board)))

(defn prompt-rows
  []
  (println "Five or six rows board? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (if (contains? #{5 6} rows)
      (prompt-empty-peg board)
      (do
        (println "Sorry, only 5 or 6 rows board is supported for now!! ¯\\_(ツ)_/¯")
        (prompt-rows)))))

(defn restore-board
  [board old-board can-restore]
  (if can-restore
    (do
      (println "Restoring previous board...")
      (prompt-move old-board))
    (do
      (println "Unable to restore previous board. Using existing board...")
      (prompt-move board))))

(defn prompt-move
  ([board] (prompt-move board board false))
  ([board old-board] (prompt-move board old-board true))
  ([board old-board can-restore]
   (println "\nHere's your board:")
   (print-board board)
   (println "Move from where to where? Enter two letters:")
   (def raw-input (get-input))
   (let [input (map letter->pos (characters-as-strings raw-input))]
     (if-let [new-board (pegs/make-move board (first input) (second input))]
       (successful-move new-board board)
       (if (or (= raw-input "b") (= raw-input "back"))
         (restore-board board old-board can-restore)
         (do
           (println (format "\nInvalid move (%s). Try again:\n" raw-input))
           (prompt-move board old-board)))))))

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
  (prompt-rows))
