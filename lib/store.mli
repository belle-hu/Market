open Bagofgoods
open Items


module type StoreType = sig

 type 'a t
(*Type representing a store*)
 val empty: 'a t
 (*[empty] represents an empty store*)
 val count_products: 'a t -> int
(*Returns the number of all products in store*)
 val all_products: 'a t -> 'a t 
 (*Returns the store*)
 val sell_goods: int -> (Item.t) -> 'a t -> 'a t
 (*[sell_goods dec item st] sells [dec] of an item in store [st]*)
 val popular_goods: int -> 'a t -> 'a t
 (*[popular_goods] returns all the bags of goods in [st] with only popular
    items in them, determined by whether the quantity of an item is less than or
    equal to the [lim]*)
 val of_list: FrequencyBagGoods.t list -> 'a t
 (*Converts a list of FrequencyBagGoods to a store.*)
 val to_list: 'a t -> FrequencyBagGoods.t list
 (*Converts a store to a list.*)
 val map: (FrequencyBagGoods.t -> FrequencyBagGoods.t) -> FrequencyBagGoods.t list -> FrequencyBagGoods.t list
 (*Applies a function to each FrequencyBagGood in a store.*)
 val filter: (FrequencyBagGoods.t -> bool) -> FrequencyBagGoods.t list -> FrequencyBagGoods.t list
  (*Filters out the valid FrequencyBagGood in a store based on function.*)
 val item_by_name: (string -> 'a t -> Item.t option)
 (*Finds an item in store by its name.*)
end


module Store : StoreType

