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
