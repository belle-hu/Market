module type ExpenditureType = sig
  type 'a t
  (** Type representing the collection of items bought*)

  val of_list : 'a list -> 'a t
  (** Convert a list of items into a collection of items*)

  val to_list : 'a t -> 'a list
  (** Convert a collection of items into a list of items*)

  val add_item : 'a -> 'a t -> 'a t
  (** add an item to the collection of items previously bought*)

  val total_expenditure : 'a t -> int
  (** computes the total expenditure. That is, the cost of all the items, taking
      into account their multiplicities*)

  val distinct_items_list : 'a t -> 'b list
  (** returns a list of all the distinct items bought*)

  val total_items : 'a t -> int
  (** returns the total number of items bought, accounting for the
      multiplicities of each item*)
  val count_elems: 'a t -> int
  (** counts the number of distinct items bought*)
end
