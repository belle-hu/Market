open Bagofgoods
open Items

module type StoreType = sig 
  type 'a t 
  val count_products: 'a t -> int 
  val all_products: 'a t -> 'a t  
  val sell_goods: int -> (Item.t) -> 'a t -> 'a t 
  val popular_goods: int -> 'a t -> 'a t
  val in_stock_goods: 'a t -> 'a t
  val of_list: BagOfGoods.t list -> 'a t 
  val to_list: 'a t -> BagOfGoods.t list
  val map: (BagOfGoods.t -> BagOfGoods.t) -> BagOfGoods.t list -> BagOfGoods.t list 
  val filter: (BagOfGoods.t -> bool) -> BagOfGoods.t list -> BagOfGoods.t list 
end 

module Store : StoreType 