module type AI = sig
    type t
      
    (* Etat initial de l'automate *)
    val start : t
    (* Fait une transition de l'automate en renvoyant
       la commande a executer *)
    val step : t -> Data.ant -> (Data.command * t)

end

module Stoopid : AI = struct
    
    type t = F of (Data.ant -> (Data.command * t))
	
    let step (F f) ant = f ant

    let rec start = F init 
    
    and init ant = (Data.Move,F init)

end

module Test : AI = struct

    type t = F of (Data.ant -> (Data.command *t))

    let step (F f) ant = f ant

    let rec start = F init

    and init ant = 
      match other ant with 
      | Some(pos,Data.Corpse) -> hack pos ant
      | Some(pos,Data.Enemy) -> confront pos ant
      | _ -> move ant

    and other ant = 
      let f = function
	| Some (_, Some(Data.Enemy)) -> Some(Data.Enemy)
	| Some (_, Some(Data.Corpse)) -> Some(Data.Corpse)
	| _ -> None
      in
      Data.search_first f Data.basic_order ant
	
    and move ant = (Data.Move,F init)

    and hack pos ant = (Data.Move,F init)
      
    and confront pos ant = (Data.Move,F init)

end
