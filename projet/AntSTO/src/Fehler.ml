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
  | 306276868  -> EAlreadyJoined
  | 1016528498 -> EGameIsNotOver
  | 318351321  -> EGameIsNotPlaying
  | 231632226  -> EInvalidAntIdentifier
  | 995492770  -> EInvalidArgument
  | 395281878  -> EInvalidCommand
  | 397948155  -> EInvalidGameIdentifier
  | 465204434  -> EInvalidLogin
  | 785135324  -> EMustBeLogged
  | 779856578  -> EMustJoinFirst
  | 184050103  -> ENoMoreSlot
  | 290490648  -> ENoPermission
  | 598036704  -> EUnknownUser
  | 1041390133 -> EUserAlreadyExists
  | otherwise  -> raise @$ UnknownErrorCode otherwise

(** Fonction qui, a partir de l'erreur, renvoie le code d'erreur sous forme d'entier *)
let to_int = function
  | EAlreadyJoined         -> 306276868
  | EGameIsNotOver         -> 1016528498
  | EGameIsNotPlaying      -> 318351321
  | EInvalidAntIdentifier  -> 231632226
  | EInvalidArgument       -> 995492770
  | EInvalidCommand        -> 395281878
  | EInvalidGameIdentifier -> 397948155
  | EInvalidLogin          -> 465204434
  | EMustBeLogged          -> 785135324
  | EMustJoinFirst         -> 779856578
  | ENoMoreSlot            -> 184050103
  | ENoPermission          -> 290490648
  | EUnknownUser           -> 598036704
  | EUserAlreadyExists     -> 1041390133
