(import testament :prefix "" :exit true)
(import ../moneta/debts)

(defn arr-sort [x] (sorted x (fn [x y] (< (hash x) (hash y)))))

(deftest order_balance
  (assert-deep-equal
    {:crediters @[["karl" 10] ["john" 100]] :debiters @[["peter" -50] ["valentina" -60]]}
    (let [res (debts/order_balance @{"karl" 10 "peter" -50 "john" 100 "valentina" -60})]
      {:crediters (arr-sort (res :crediters)) :debiters (arr-sort (res :debiters))})))

(deftest check_balance
  (debts/check_balance @[["karl" 10] ["john" 100]] @[["valentina" -60] ["peter" -50]]))

(deftest check_balance_error
  (assert-thrown-message
    "Unsolvable balance"
    (debts/check_balance @[["karl" 10] ["john" 150]] @[["valentina" -60] ["peter" -50]] )))

(deftest reduce_balance
  (assert-deep-equal
    (arr-sort @[["valentina" 60 "john"] ["peter" 40 "john"] ["peter" 10 "karl"]])
    (arr-sort (debts/reduce_balance @[["valentina" -60] ["peter" -50]] @[["karl" 10] ["john" 100]]))))

(deftest settle1
  (assert-deep-equal
    @[["peter" 40 "john"] ["peter" 10 "karl"] ["valentina" 60 "john"]]
    (arr-sort (debts/settle @{"karl" 10 "peter" -50 "john" 100 "valentina" -60}))))

(deftest settle2
  (assert-deep-equal
    @[["svetlana" 10 "peter"] ["svetlana" 10 "brian"]]
    (arr-sort (debts/settle @{"peter" 10 "brian" 10 "svetlana" -20}))))

(deftest test-big-debts-are-solved-properly
  (def balance @{"simon" 778.06
                 "laura" -28.78
                 "pauline" 126.60
                 "valentin" -445.9
                 "colin" -134.28
                 "remy" 163.41
                 "elsa" 339.45
                 "alexis" 611.77
                 "ter-ter" -298.33
                 "chez-francine" -176.82
                 "la-lanterne" -240.75
                 "cece-manu" -184.85
                 "thorigne" -28.67
                 "amour-de-coloc" -151.26
                 "fleurs-bleues" -37.78
                 "treguinguette" -291.87})
  # (def expected_result
  #   (arr-sort @[["thorigne" 28.67 "pauline"]
  #               ["laura" 28.78 "pauline"]
  #               ["fleurs-bleues" 37.78 "pauline"]
  #               ["colin" 31.37 "pauline"]
  #               ["colin" 102.91 "remy"]
  #               ["amour-de-coloc" 60.50 "remy"]
  #               ["amour-de-coloc" 90.76 "elsa"]
  #               ["chez-francine" 176.82 "elsa"]
  #               ["cece-manu" 71.87 "elsa"]
  #               ["cece-manu" 112.98 "alexis"]
  #               ["la-lanterne" 240.75 "alexis"]
  #               ["treguinguette" 258.04 "alexis"]
  #               ["treguinguette" 33.83 "simon"]
  #               ["ter-ter" 298.33 "simon"]
  #               ["valentin" 445.90 "simon"]]))
  (do
    (def order_balanced (debts/order_balance balance))
    (def bank @{})
    (each item (array ;(order_balanced :debiters) ;(order_balanced :crediters))
      (put bank (item 0) (item 1)))
    (def solution (debts/settle balance))
    (each step solution
      (+= (bank (step 0)) (step 1))
      (-= (bank (step 2)) (step 1)))
    (each account bank
      (if (>= (math/abs account) 0.01)
          (do (pp bank)
              (error "balance does not check out"))))))

(deftest test_person_in_both_sides
  (assert-deep-equal
    @[["fred" 200 "remy"] ["alexis" 300 "remy"]]
    (arr-sort (debts/settle @{"fred" -200 "alexis" -300 "remy" 500}))))

(run-tests!)
