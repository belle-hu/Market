module type StoreType = sig 
   type 'a t val count_products: 'a t ->
   int val all_products: 'a t -> 'a t  val
   sell_goods: int -> 'a -> 'a t -> 'a t 
    val popular_goods: 'a t -> 'a t 
   val in_stock_goods: 'a t -> 'a t val out_of_stock_goods: 'a t
   val popular_goods: int -> 'a t -> 'a t
   end 
