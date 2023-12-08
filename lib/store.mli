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


module Store : StoreType

