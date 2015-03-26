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

    let z_program = ""

    let step (F f) ant = f ant

    let rec start = F init

    and init ant = Data.(match ant with
      | Ally(s,_) when s.energy < (s.max_energy / 10) ->
	 critical 10 ant
      | Ally(s,_) when s.energy < (s.max_energy/2) ->
	 begin
	   match other ant with 
	   | Some(pos,Corpse) -> hack pos ant
	   | _ -> move ant
	 end
      | _ ->
	 begin
	   match other ant with
           | Some(pos,Corpse) -> hack pos ant
	   | Some(pos,Enemy) -> confront pos ant
	   | _ -> move ant
         end)

    and critical n ant =
      if n==0 
      then init ant
      else (Data.Rest,F(critical (n-1)))

    and other ant = 
      let f = function
	| Some (_, Some(Data.Enemy)) -> Some(Data.Enemy)
	| Some (_, Some(Data.Corpse)) -> Some(Data.Corpse)
	| _ -> None
      in
      Data.search_first f Data.basic_order ant
	
    and move ant = match Data.environment ant Data.Front with
      | Some(Data.Grass,None) 
      | Some(Data.Food _,None) -> (Data.Move, F init)
      | _ -> (Data.TurnRight, F init)

    and targeted action step pos ant = 
      Data.( 
	match pos with
	| On -> init ant (*Not supposed to happen*)
	| Front -> (action,F init)
	| FrontLeft -> (Move, F(step Left))
	| FrontRight -> (Move,F(step Right))
	| Left -> (TurnLeft,F(step Front))
	| Right -> (TurnRight,F(step Front))
	| _ -> (TurnRight, F init)
      )      
	
    and hack pos ant = targeted (Data.Zombify z_program) hack pos ant

    and confront pos ant = targeted (Data.Attack 50) confront pos ant

end
