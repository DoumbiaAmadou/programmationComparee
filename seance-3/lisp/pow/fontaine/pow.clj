(defn pow
  "creates a function which takes an `x` and return `x^n`"
  [n]
  (fn [x]
    (Math/pow x n)))
