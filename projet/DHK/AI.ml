open Data

module AI : sig
    type t
      
    (* Etat initial de l'automate *)
    val start : t
    (* Fait une transition de l'automate en renvoyant
       la commande a executer *)
    val step : t -> environment -> (command * t)

end = struct
    type t = F of (environment -> (command * t))
	
    let step (F f) env = f env

    let rec start = F init 
    
    and init env = (Move,F init)

end
