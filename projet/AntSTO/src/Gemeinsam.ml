type ('a, 'b) either =
  | Correct of 'a
  | Error   of 'b

let ( @$ ) f x = f x
