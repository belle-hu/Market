open Bagofgoods
open Items
module type StoreType = sig
  type 'a t
  val empty: 'a t
  val count_products: 'a t -> int
  val all_products: 'a t -> 'a t 
  val sell_goods: int -> (Item.t) -> 'a t -> 'a t
  val popular_goods: int -> 'a t -> 'a t
  val in_stock_goods: 'a t -> 'a t
  val of_list: FrequencyBagGoods.t list -> 'a t
  val to_list: 'a t -> FrequencyBagGoods.t list
  val map: (FrequencyBagGoods.t -> FrequencyBagGoods.t) -> FrequencyBagGoods.t list -> FrequencyBagGoods.t list
  val filter: (FrequencyBagGoods.t -> bool) -> FrequencyBagGoods.t list -> FrequencyBagGoods.t list
  val item_by_name: (string -> 'a t -> Item.t option)
end


module Store : StoreType = struct


type 'a t = FrequencyBagGoods.t list


(**Allows user to see list of all StoreProducts in given Store [st]. Returns a
list of StoreProducts*)
let all_products st: FrequencyBagGoods.t list = st


let empty = []
 (*[count_bag bg] counts the number of items in [bg]*)
let rec count_bag bg =
  match FrequencyBagGoods.to_list bg with
  | []-> 0
  | h::t -> Item.get_quantity h + count_bag (FrequencyBagGoods.of_list t)


(**Count the number of products in a given store [st].*)
let rec count_products st =
  match st with
  | [] -> 0
  | h :: t -> count_bag h + count_products t


(**Given a list of FrequencyBagGoods [b_lst] and a store [st], add all
goods of [bg] to the store [st].*)
let import_goods bg st =
  (*Check for uniqueness of items?*)
  FrequencyBagGoods.to_list bg :: st

(**Keep a list that tracks out_of_stock_goods each time we sell a good*)
let out_of_stock_goods = ref []


  (*Get bag of deleted items from a bag*)
let rec get_del_items bg =
  match (FrequencyBagGoods.to_list bg) with
  | [] -> out_of_stock_goods := List.append !out_of_stock_goods []
  | h::t ->
    if Item.get_quantity h <= 0 then 
    out_of_stock_goods :=  (List.append !out_of_stock_goods [h])
   else
  get_del_items (FrequencyBagGoods.of_list t)
(*[del_items bg] deletes items of a bag [bg] if their quantity is less than
   or equal to zero.*)
let rec del_items bg =
  get_del_items bg;
  match (FrequencyBagGoods.to_list bg) with
  | [] -> FrequencyBagGoods.empty
  | h::t -> if Item.get_quantity h <= 0 then 
    del_items (FrequencyBagGoods.of_list t) else
  FrequencyBagGoods.join (FrequencyBagGoods.of_list [h]) (del_items (FrequencyBagGoods.of_list t))
   
(*[store_change_quan dec item bg] sells [dec] amount of [item] in [bg]
   and returns a new bag with the updated amount of [item] *)
let rec store_change_quan dec (item:Item.t) bg =
  match FrequencyBagGoods.to_list bg with
  | [] -> FrequencyBagGoods.empty
  | h :: t->
  if (Item.get_name item == Item.get_name h) then
  let new_item = Item.change_quantity h (-1 * dec)
  in (FrequencyBagGoods.of_list (new_item::t) )
  else
    FrequencyBagGoods.join (FrequencyBagGoods.of_list [h]) (store_change_quan dec item (FrequencyBagGoods.of_list t))
       
(**Sell [dec] amount of an item [item] within a given store
[st]. Requires that [b] is a good in [st] already.*)
let rec sell_goods dec (item:Item.t) st =
match st with
| [] -> []
| h :: t->
  let b1 = (store_change_quan dec item h) in
  let b2 = del_items b1 in b2:: (sell_goods dec item t)


let rec delete_bag (bg: FrequencyBagGoods.t) st =
  match st with
  | [] -> []
  | h::t -> if h == bg then t else delete_bag bg t


(*[clean st] cleans the store of empty bags*)
let rec clean st =
  match st with
  | [] -> empty
  | h::t -> if h == FrequencyBagGoods.empty then clean t
  else h :: clean t


let rec determine_popular bag_goods lim =
  match FrequencyBagGoods.to_list bag_goods with
  | [] -> FrequencyBagGoods.empty
  | h :: t
-> if Item.get_quantity h <= lim then
  FrequencyBagGoods.join (FrequencyBagGoods.of_list [h]) (determine_popular (FrequencyBagGoods.of_list t) lim)
    else determine_popular (FrequencyBagGoods.of_list t) lim


(*[popular_goods] returns all the bags of goods in [st] with only popular items in them,
   determined by whether the quantity of an item is less than or equal to the [lim]*)
let rec popular_goods lim st =
  match st with | [] -> [] | h :: t ->
(determine_popular h lim :: popular_goods lim t) |> clean


(*talk about this: how to determine if good is on sale: add a on_sale field to
FrequencyBagGoods?*)
let in_stock_goods st = st


(*Given a list of bag of goods list, convert to a store*)
let rec of_list (bg_list:FrequencyBagGoods.t list) = bg_list
 let to_list (bg_list:FrequencyBagGoods.t list) = bg_list


let rec map (f: FrequencyBagGoods.t -> FrequencyBagGoods.t) st =
  match st with
  | [] -> []
  | h::t -> f h :: map f t
 let rec filter(f: FrequencyBagGoods.t -> bool) st =
  List.filter f st

  let item_by_nam_aux nam bg = 
    let lst = FrequencyBagGoods.to_list bg in
    let rec r1 nam lst = 
      match lst with 
      | [] -> None
      | h::t -> if Item.get_name h = nam then Some h else r1 nam t
    in
    r1 nam lst 

  (*[item_by_name nam st] retrieves the Item of name [nam] in store [st]*)
  let rec item_by_name nam st = 
    let lst = to_list st in 
    let rec r11 nam lst =
    match st with 
    | [] -> None
    | h::t -> let r1 = item_by_nam_aux nam h in 
    if r1 <> None then r1 else item_by_name nam t
    in 
    r11 nam lst
end

