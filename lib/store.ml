module type StoreType = sig
  type 'a t = 'a list
  val count_products: 'a t -> int
  val all_products: 'a t -> 'a t
  val import_goods: 'a t -> 'a t
  val sell_goods: 'a t -> 'a t
  val popular_goods: 'a t -> 'a t
  val on_sale_goods: 'a t -> 'a t
  val in_stock_goods: 'a t -> 'a t
  val out_of_stock_goods: 'a t
end

module Store (Bag: BagOfGoods) : StoreType = struct

module Bag:BagOfGoods = StoreProducts
type 'a t = 'a StoreProducts.t list 

(**Allows user to see list of all StoreProducts in given Store [st]. Returns a list
of StoreProducts*)
let all_products st = st

(**Keep a list that tracks out_of_stock_goods each time we sell a good*)
let out_of_stock_goods = []

(**Count the number of products in a given store [st].*)
let count_products st = 
   match st with 
  | [] -> 0
  | h::t -> h.count_elements + count_products t

 (**FIX LATER: Given a list of BagOfGoods [b_lst] and a store [st], 
  add all goods of [b] to the store [st]. Q: need to account for fact 
  that item might already exist*)
  let import_goods b_lst st = 
    
     st @ b_lst
  

(**FIX LATER: probably will be able to use more built-in functions 
from item or BagofGoods later. 
Sell [dec] amount of an item [b] within a given store [st]. 
  Requires that [b] is a good in [st] already.*)

let sell_goods dec b st =
    match st with
      | [] -> []
      | h::t -> let h =
      let update_bag b = 
      match h with
      | []-> []
      | h1::t1 -> if h1.get_name() == b then 
      if (h1.get_quantity() - dec)>0 then
          h1.change_quantity(h1.get_quantity()-dec)::t1 else t1
      else h1 :: update_bag b t1
      in update_bag b in h::sell_goods dec b t 
  
  
  let determine_popular bag_goods lim =
      match bag_goods with 
      | [] -> []
      | h::t -> if (h.get_quantity())<=lim then h:: 
        determine_popular t lim else determine_popular t lim

  let popular_goods lim st = match st with
      | [] -> []
      | h :: t -> (determine_popular h lim) @ popular_goods lim t
  
(*talk about this: how to determine if good is on sale: add a on_sale field to BagOfGoods?*)
  
 let in_stock_goods st = st 

end 

(**********************************************************************)

(** Sanitize a string by removing non-alphanumeric symbols. Return the
    list of words obtained by splitting on spaces. *)
    let sanitize (s : string) : string list =
      s
      |> Str.global_replace (Str.regexp "[ \t\r\n]") " "
      |> Str.global_replace (Str.regexp "[^a-zA-Z0-9' ]") ""
      |> String.lowercase_ascii |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
    
    (** Build a random Ngram model from a training corpus consisting of a string
        [input] and an integer parameter [n]. Requires: [n > 0]. *)
    let build_rand_ngram (input : string) (ngram_len : int) : Store =
      input |> sanitize |> Rand_ngram.build ngram_len
    
    (** Build a most-frequent Ngram model from a training corpus consisting of a
        string [input] and an integer parameter [n]. Requires: [n > 0]. *)
    let build_freq_ngram (input : string) (ngram_len : int) : Freq_ngram.t =
      input |> sanitize |> Freq_ngram.build ngram_len
    
    (** Build an interpolated Ngram model from a training corpus consisting of a
        string [input] and an integer parameter [n]. Requires: [n > 0]. *)
    let build_interp_ngram (input : string) (ngram_len : int) : Interp_ngram.t
        =
      input |> sanitize |> Interp_ngram.build ngram_len
    
    (** Given a maximum number [N] of words to generate and a prompt, generate up to
        [N] words using a random Ngram model. Requires: [N >= 0]. *)
    let create_rand_sequence (dist : Rand_ngram.t) (max_len : int)
        (prompt : string) : string =
      prompt
      |> sanitize
      |> Rand_ngram.sample_sequence dist max_len
      |> String.concat " "
    
    (** Given a maximum number [N] of words to generate and a prompt, generate up to
        [N] words using a most-frequent Ngram model. Requires: [N >= 0]. *)
    let create_freq_sequence (dist : Freq_ngram.t) (max_len : int)
        (prompt : string) : string =
      prompt
      |> sanitize
      |> Freq_ngram.sample_sequence dist max_len
      |> String.concat " "
    
    (** Given a maximum number [N] of words to generate and a prompt, generate up to
        [N] words using an interpolated Ngram model. Requires: [N >= 0]. *)
    let create_interp_sequence (dist : Interp_ngram.t) (max_len : int)
        (prompt : string) : string =
      prompt
      |> sanitize
      |> Interp_ngram.sample_sequence dist max_len
      |> String.concat " "
    