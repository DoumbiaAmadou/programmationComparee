(**
 * Use to used to store anagrams
 *)
module SS :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val cardinal : t -> int
  end
(**
 * list of functions (alphabetical order) with their description
 *)

(**
 * adding a new word in the hashtable containing anagrams
 *)
val add_word: (string, SS.t) Hashtbl.t -> string -> unit

(**
 * adding a list of words in the hashtable containing anagrams
 *)
val add_words: (string, SS.t) Hashtbl.t -> SS.t -> unit

(**
 * compare to char (useful to sort a string)
 *)
val compare_char: char -> char -> int

(**
 * create a dictionnary with a list of words
 *)
val create_dic: SS.t -> (string, SS.t) Hashtbl.t
(**
 * create a list containint all the lines of the file given in parameter
 *)
val file2list: string -> SS.t

(**
 * give anagrams of each word given in parameter.
 *)
val find_anagrams_from_list: SS.t -> (string, SS.t) Hashtbl.t -> unit

(**
 * give anagrams of the word given in parameter.
 *)
val find_anagrams_from_string: string -> (string, SS.t) Hashtbl.t -> SS.t

(**
 * convert a char list to string
 *)
val list2string: char list -> string
  
(**
 * function to print result
 *)
val print_result: string -> SS.t -> unit

(**
 * sort a string (using functions defined above)
 *)
val sort_word: string -> string

(**
 * convert a string to char list
 * copy of : http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings
 *)
val string2list: string -> char list
