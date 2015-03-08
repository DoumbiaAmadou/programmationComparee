module AI : sig
    type t
      
    (* Etat initial de l'automate *)
    val start : t
    (* Fait une transition de l'automate en renvoyant
       la commande a executer *)
    val step : t -> Data.environment -> (Data.command * t)

end = struct
    
    type t = F of (Data.environment -> (Data.command * t))
	
    let step (F f) env = f env

    let rec start = F init 
    
    and init env = (Data.Move,F init)

end
