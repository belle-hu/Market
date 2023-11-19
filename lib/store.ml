  open Bagofgoods
  open Items
  module type StoreType = sig 
    type 'a t val count_products: 'a t ->
    int val all_products: 'a t -> 'a t  val
    sell_goods: int -> 'a -> 'a t -> 'a t 
     val popular_goods: 'a t -> 'a t 
    val in_stock_goods: 'a t -> 'a t val out_of_stock_goods: 'a t
    val popular_goods: int -> 'a t -> 'a t
    end 
 

  module Store:StoreType = struct

  type 'a t = Item.t list list

  (**Allows user to see list of all StoreProducts in given Store [st]. Returns a
  list of StoreProducts*) 
  let all_products st = st

  (**Keep a list that tracks out_of_stock_goods each time we sell a good*) 
  let
  out_of_stock_goods = []

  let to_list st = st

  let rec count_products_aux l= 
  match l with 
  | [] -> 0 
  | h :: t -> Item.get_quantity h + (count_products_aux t)
  
  (**Count the number of products in a given store [st].*) 
  let rec count_products st = 
    match st with 
    | [] -> 0 
    | h :: t -> count_products_aux h + count_products t

  (**FIX LATER: Given a list of BagOfGoods [b_lst] and a store [st], add all
  goods of [b] to the store [st]. Q: need to account for fact that item might
  already exist*) 
  let import_goods b_lst st = st :: b_lst

  (*[del_items bg] deletes items of a bag [bg] if their quantity is less than
     or equal to zero.*)
  let rec del_items bg = 
    match bg with 
    | [] -> []
    | h::t -> if Item.get_quantity h <= 0 then  
      del_items t else 
      h:: (del_items t)

  (*[store_change_quan dec item bg] sells [dec] amount of [item] in [bg]
     and returns a new bag with the updated amount of [item] *)
  let rec store_change_quan dec item bg = 
    match bg with 
    | [] -> []
    | h :: t-> let old_quan = Item.get_quantity h in
    if (Item.get_name h == Item.get_name h) then 
    let new_item = Item.change_quantity h (old_quan - dec)
    in new_item::t 
    else h :: store_change_quan dec item t
    
  (**Sell [dec] amount of an item [item] within a given store
  [st]. Requires that [b] is a good in [st] already.*)
  let rec sell_goods dec item st = 
  match st with 
  | [] -> []
  | h :: t-> 
    let b1 = store_change_quan dec item h in 
    let b2 = del_items b1 in import_goods (sell_goods dec item t) b2

  let rec determine_popular bag_goods lim = 
    match bag_goods with 
    | [] -> [] 
    | h :: t
  -> if Item.get_quantity h <= lim then h :: determine_popular t lim 
  else determine_popular t lim

  let rec popular_goods lim st = match st with | [] -> [] | h :: t ->
  determine_popular h lim :: popular_goods lim t

  (*talk about this: how to determine if good is on sale: add a on_sale field to
  BagOfGoods?*)

  let in_stock_goods st = st 

end 
