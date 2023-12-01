open Items

module type SampleGoodsType = sig
  type t
  (**Type representing the data in the bag*)

  val empty : t
  (**An empty bag*)

  val to_list : t -> Item.t list
  (**Convert a sampleable bag to a list of items*)

  val of_list : Item.t list -> t
  (**Convert a list of items into a sampleable bag*)

  val join : t -> t -> t
  (**Join two bags together*)

  val join_many : t list -> t
  (**Join a list of bags together into one bag*)

  val sample : t -> Item.t option
  (**Draw an item from the sampleable bag. Return [None] if bag is empty*)

  val count_elems : t -> int
  (**Counts number of items in a bag*)

  val update_price : t -> string -> int -> t
  (**Update the price of the provided item in the bag. Requires: The item
     required to update should be already in the bag*)

  val update_quantity : t -> string -> int -> t
  (**Update the quantity of the provided item in the bag. Requires: The item
     required to update should be already in the bag*)

  val to_string : t -> string 
  (**Converts a bag of goods to string format*)
end

module BagOfGoods : SampleGoodsType

module FrequencyBagGoods : SampleGoodsType
(**type 'a t = (float * int * 'a) list, corresponds to price, quantity, item*)
(*module FrequencyPriceGoods : SampleGoodsType*)
