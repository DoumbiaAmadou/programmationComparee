open Data
open AI 
open Program
open IO

(* type army = (int * ant) list *)
(* type ia_army=  (int * Test.t) list *)

(* let rec turn ia_army army (cmd,ias) = *)
(*   match army with *)
(*   | [] -> (cmd,ias) *)
(*   |(id,a)::t ->let (c,aut) = Test.step a (List.assoc id ia_army) in *)
(*    turn ia_army t (id^":"^(string_of_command c)^cmd,(id,aut)::ias) *)

(* let begi id_game nb_ant cookie=  *)
(*   let rec fst_turn i =  *)
(*     match i with  *)
(*     | 0 -> "" *)
(*     | n -> (string_of_int (n-1))^":"^(string_of_command (Rest))^"," *)
(*        ^(aux (i-1)) *)
(*   in  *)
(*   play cookie id_game (fst_turn nb_ant) *)

(* let game id_game nb_ant cookie = *)
(*   let army = begi id_game nb_ant cookie in  *)
(*   let rec aux ia_army army= *)
(*     match army with  *)
(*     | [] -> () *)
(*     | l -> let (cmd,ia_army) = turn ia_army army ("",[]) *)
(* 	   in aux ia_army (play cookie id_game cmd) *)
(*   in aux (List.mapi (fun i a -> (i,Test.start)) army) army *)
	   


(* module Html = Dom_html *)
  
(* let get_element_by_id id = *)
(*   Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false) *)
    
(* let make_test_request id = *)
(*   let button = get_element_by_id "test" in *)
(*   button##onclick <- Html.handler ( fun _ -> *)
(*       let _ = IO.auth_user "afk" "dersimantep627"; *)
(*         Js._true) *)


let () = 
  let cookie = auth_user "afk" "dersimantep627" in
  ()
  (* let id = gameid_of_string "4098919538429027416006485239929202" in *)
  (* (\* let _ = play cookie id "4:hack@[L1>left;jump!L1;],3:hack@[L1>left;jump!L1;], *)2:hack@[L1>left;jump!L1;],1:hack@[L1>left;jump!L1;],0:hack@[L1>left;jump!L1;]" in *)
  (* () *)

      
