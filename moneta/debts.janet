(defn check_balance [debiters crediters]
  (if (>= (math/abs (+ (sum (map |($0 1) crediters)) (sum (map |($0 1) debiters)))) 0.01)
      (error "Unsolvable balance"))
  {:debiters debiters :crediters crediters})

(defn order_balance [balances]
  (def ret {:crediters @[] :debiters @[]})
  # (def m @{})
  # (each item balances
  #   (if (m (item 0))
  #     (+= (m (item 0)) (item 1))
  #     (put m (item 0) (item 1))))
  (eachk key balances
    (if (> (balances key) 0)
        (array/push (ret :crediters) [key (balances key)])
        (array/push (ret :debiters) [key (balances key)])))
  ret)

(defn reduce_balance [debiters crediters &opt results]
  (default results @[])
  (if (or (= (length crediters) 0) (= (length debiters) 0))
      results
      (do (def crediters (sort-by |($0 1) crediters))
          (def debiters (reverse (sort-by |($0 1) debiters)))
          (def crediter (array/pop crediters))
          (def debiter (array/pop debiters))

          (var amount 0)
          (if (> (math/abs (debiter 1)) (math/abs (crediter 1)))
            (set amount (math/abs (crediter 1)))
            (set amount (math/abs (debiter 1))))
          (def new_results (array ;results))
          (def due_amount amount) # TODO round the returned value to hide float inprecision?
          (if (>= due_amount 0.01) # account for float inprecision
              (array/push new_results [(debiter 0) due_amount (crediter 0)]))

          (def new_debiter_balance (+ (debiter 1) amount))
          (if (< new_debiter_balance 0)
            (array/push debiters [(debiter 0) new_debiter_balance]))

          (def new_crediter_balance (- (crediter 1) amount))
          (if (> new_crediter_balance 0)
            (array/push crediters [(crediter 0) new_crediter_balance]))

          (reduce_balance (reverse (sort-by |($0 1) debiters)) (reverse (sort-by |($0 1) crediters)) new_results))))

(defn settle
  "accepts a table of accounts and returns an array of operations to balance all the accounts"
  [balances]
  (-> (order_balance balances)
      (|(check_balance ($0 :debiters) ($0 :crediters)))
      (|(reduce_balance (array ;($0 :debiters)) (array ;($0 :crediters))))))
