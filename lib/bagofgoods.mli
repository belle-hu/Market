module type SampleGoodsType = sig
  type 'a t
  (**Type representing the data in the bag*)

  val to_list : 'a t -> 'a list
  (**Convert a sampleable bag to a list of items*)

  val of_list : 'a list -> 'a t
  (**Convert a list of items into a sampleable bag*)

  val sample : 'a t -> 'a option
  (**Draw an item from the sampleable bag. Return [None] if bag is empty*)

  val count_elems : 'a t -> int
  (**Counts number of items in a bag*)
end

module FrequencyPriceGoods : SampleGoodsType
(**type 'a t = (float * int * 'a) list, corresponds to price, quantity, item*)
