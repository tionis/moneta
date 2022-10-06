(defn check_balance_solvable [debiters crediters]
  (if (>= (math/abs (+ (sum (map |($0 1) crediters)) (sum (map |($0 1) debiters)))) 0.01)
      (error "Unsolvable balance"))
  {:debiters debiters :crediters crediters})

(defn split_balance [balances]
  (def out {:crediters @[] :debiters @[]})
  (eachk key balances
    (if (> (balances key) 0)
        (array/push (out :crediters) [key (balances key)])
        (array/push (out :debiters) [key (balances key)])))
  out)

(defn solve_balance [debiters crediters &opt results]
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
          (let [due_amount amount] # TODO round the returned value to hide float inprecision?
            (if (>= due_amount 0.01) # account for float inprecision
                (array/push new_results [(debiter 0) due_amount (crediter 0)])))

          (let [new_debiter_balance (+ (debiter 1) amount)]
            (if (< new_debiter_balance 0)
                (array/push debiters [(debiter 0) new_debiter_balance])))

          (let [new_crediter_balance (- (crediter 1) amount)]
            (if (> new_crediter_balance 0)
                (array/push crediters [(crediter 0) new_crediter_balance])))

          (solve_balance (reverse (sort-by |($0 1) debiters)) (reverse (sort-by |($0 1) crediters)) new_results))))

(defn settle
  "accepts a table of accounts and returns an array of operations to balance all the accounts"
  [balances]
  (-> (split_balance balances)
      (|(check_balance_solvable ($0 :debiters) ($0 :crediters)))
      (|(solve_balance (array ;($0 :debiters)) (array ;($0 :crediters))))))
