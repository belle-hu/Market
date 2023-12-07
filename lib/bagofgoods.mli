open Items

module type SampleGoodsType = sig
  type t
  (**Type representing the data in the bag*)

  val empty : t
  (**An empty bag*)

  val to_list : t -> Item.t list
  (**Convert a sampleable bag to a list of items*)

  val of_list : Item.t list -> t
  (**Convert a list of items into a sampleable bag. Requires that multiple Item
     objects cannot have the same name. This includes Items with the same name
     and different costs as well as Items with the same name and same cost. For
     example, a bag with one "apple" of price 1 and another with price 2 cannot
     be created. Instead, users must use the update_price function. A list with
     an "apple" of price 1 twice occuring is also forbidden. *)

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

  val contains : t -> string -> bool
  (**Returns true if the bag contains an element*)

  val to_string : t -> string
  (**Returns the string representation of a bag.*)

  (*new stuff*)

  val remove : t -> string -> t
  (**Remove an item from the bag by its name*)

  val names : t -> string list
  (**Returns a list of names of items in the bag*)

  val total_quantity : t -> int
  (**Returns the total number of items in the bag*)

  val total_cost : t -> int
  (**Returns the total cost of all items in the bag*)

  val map : t -> (Item.t -> Item.t) -> t
  (**Applies a function to all items in the bag*)

  val filter : t -> (Item.t -> bool) -> t
  (**Filters items in the bag based on a predicate*)

  val intersection : t -> t -> t
  (**Returns a bag with elements that exist in both input bags*)

  val difference : t -> t -> t
  (**Returns a bag with elements existing in the first bag but not the second*)

  val get_price : t -> string -> int option
  (**Return the price of an item wrapped in [Some] given a bag and a name.
     Returns [None] if item does not exist in the bag. *)
end

module BagOfGoods : SampleGoodsType

module FrequencyBagGoods : SampleGoodsType
(**type 'a t = (float * int * 'a) list, corresponds to price, quantity, item*)
(*module FrequencyPriceGoods : SampleGoodsType*)
