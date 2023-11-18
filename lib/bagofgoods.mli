open Items

module type SampleGoodsType = sig
  type t
  (**Type representing the data in the bag*)

  val to_list : t -> Item.t list
  (**Convert a sampleable bag to a list of items*)

  val of_list : Item.t list -> t
  (**Convert a list of items into a sampleable bag*)

  val join : t -> t -> t
  (**Join two bags together*)

  val sample : t -> Item.t option
  (**Draw an item from the sampleable bag. Return [None] if bag is empty*)

  val count_elems : t -> int
  (**Counts number of items in a bag*)
end

module BagOfGoods : SampleGoodsType

module FrequencyBagGoods : SampleGoodsType
(**type 'a t = (float * int * 'a) list, corresponds to price, quantity, item*)
(*module FrequencyPriceGoods : SampleGoodsType*)
