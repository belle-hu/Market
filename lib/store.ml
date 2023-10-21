(*open Bagofgoods

  module type StoreType = sig type t

  val count_products : t -> int val all_products : t -> t val import_goods : t
  -> t val sell_goods : t -> t val popular_goods : t -> t val on_sale_goods : t
  -> t val in_stock_goods : t -> t val out_of_stock_goods : t end

  module Store (Bag : SampleGoodsType) : StoreType = struct module Bag :
  SampleGoodsType = StoreProducts

  type t = StoreProducts.t list

  (**Allows user to see list of all StoreProducts in given Store [st]. Returns a
  list of StoreProducts*) let all_products st = st

  (**Keep a list that tracks out_of_stock_goods each time we sell a good*) let
  out_of_stock_goods = []

  (**Count the number of products in a given store [st].*) let count_products st
  = match st with | [] -> 0 | h :: t -> h.count_elements + count_products t

  (**FIX LATER: Given a list of BagOfGoods [b_lst] and a store [st], add all
  goods of [b] to the store [st]. Q: need to account for fact that item might
  already exist*) let import_goods b_lst st = st @ b_lst

  (**FIX LATER: probably will be able to use more built-in functions from item
  or BagofGoods later. Sell [dec] amount of an item [b] within a given store
  [st]. Requires that [b] is a good in [st] already.*)

  let sell_goods dec b st = match st with | [] -> [] | h :: t -> let h = let
  update_bag b = match h with | [] -> [] | h1 :: t1 -> if h1.get_name () == b
  then if h1.get_quantity () - dec > 0 then h1.change_quantity (h1.get_quantity
  () - dec) :: t1 else t1 else h1 :: update_bag b t1 in update_bag b in h ::
  sell_goods dec b t

  let determine_popular bag_goods lim = match bag_goods with | [] -> [] | h :: t
  -> if h.get_quantity () <= lim then h :: determine_popular t lim else
  determine_popular t lim

  let popular_goods lim st = match st with | [] -> [] | h :: t ->
  determine_popular h lim @ popular_goods lim t

  (*talk about this: how to determine if good is on sale: add a on_sale field to
  BagOfGoods?*)

  let in_stock_goods st = st end *)
