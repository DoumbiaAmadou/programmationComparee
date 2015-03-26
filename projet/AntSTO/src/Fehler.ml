open Gemeinsam

(** Fichier contenant les codes d'erreurs possibles *)

(** Type representant les differentes erreurs possibles retournees par l'API *)
type t =
  | EAlreadyJoined
  | EGameIsNotOver
  | EGameIsNotPlaying
  | EInvalidAntIdentifier
  | EInvalidArgument
  | EInvalidCommand
  | EInvalidGameIdentifier
  | EInvalidLogin
  | EMustBeLogged
  | EMustJoinFirst
  | ENoMoreSlot
  | ENoPermission
  | EUnknownUser
  | EUserAlreadyExists

exception UnknownErrorCode of int

(** Fonction qui, a partir du code d'erreur sous forme d'entier, renvoie l'erreur *)
let of_int = function
  | 443034632  -> EAlreadyJoined
  | 808783211  -> EGameIsNotOver
  | 357629463  -> EGameIsNotPlaying
  | 973268714  -> EInvalidAntIdentifier
  | 677388348  -> EInvalidArgument
  | 4313039    -> EInvalidCommand
  | 513589683  -> EInvalidGameIdentifier
  | 114981602  -> EInvalidLogin
  | 32403037   -> EMustBeLogged
  | 523177601  -> EMustJoinFirst
  | 1001223883 -> ENoMoreSlot
  | 842913797  -> ENoPermission
  | 502441794  -> EUnknownUser
  | 334269347  -> EUserAlreadyExists
  | otherwise  -> raise @$ UnknownErrorCode otherwise

(** Fonction qui, a partir de l'erreur, renvoie le code d'erreur sous forme d'entier *)
let to_int = function
  | EAlreadyJoined         -> 443034632
  | EGameIsNotOver         -> 808783211
  | EGameIsNotPlaying      -> 357629463
  | EInvalidAntIdentifier  -> 973268714
  | EInvalidArgument       -> 677388348
  | EInvalidCommand        -> 4313039
  | EInvalidGameIdentifier -> 513589683
  | EInvalidLogin          -> 114981602
  | EMustBeLogged          -> 32403037
  | EMustJoinFirst         -> 523177601
  | ENoMoreSlot            -> 1001223883
  | ENoPermission          -> 842913797
  | EUnknownUser           -> 502441794
  | EUserAlreadyExists     -> 334269347
