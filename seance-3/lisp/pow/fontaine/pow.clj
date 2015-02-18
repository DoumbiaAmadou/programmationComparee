(defmacro pow-macro
  "creates a function which takes an `x` and return `x^n`"
  [n]
  `(fn [x#]
    (Math/pow x# '~n)))

(defn pow-fun
  "returns a function which takes an `x` and return `x^n`"
  [n]
  #(Math/pow % n))
