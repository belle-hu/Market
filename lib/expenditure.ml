open Items

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

module Expenditure= struct
  (** module representing a list of items bought, including information about
      how many and amount of money spent. This log of items bought can be for
      both the store and individual customers.*)
  type 'a t = Item.t list
  (** note: I tried to do Item list, but it said unbound*)

  let of_list (list_of_expenditures : 'a list) : 'a t = list_of_expenditures
  let to_list (bag_of_expenditures : 'a t) : 'a list = bag_of_expenditures

  (** determine whether item is already in the list. COME BACK TO*)
  (* let find_item (item: 'a)(item_list: 'a list): bool = match item_list with |
     [] -> false | h::t -> *)

  (** add an item to the collection of items previously bought*)
  let add_item (item : 'a) (bag : 'a t) : 'a t = item :: to_list bag

  (** computes the total expenditure. That is, the cost of all the items, taking
      into account their multiplicities*)
  let total_expenditure (item_list : 'a t) : int  =
    List.fold_left
      (fun acc  item -> Item.(get_price item * get_quantity item) + acc)
      0 item_list

  (** returns a list of all the distinct items bought*)
  let distinct_items_list item_list =
    List.map (fun item -> Item.get_name item) item_list

  (** returns the total number of items bought, accounting for the
      multiplicities of each item*)
  let total_items (item_list : 'a t) : int =
    List.fold_left
      (fun acc item -> Item.get_quantity item+ acc)
      0 item_list

  (** counts the number of distinct items bought*)
  let count_elems (bag_of_expenditures : 'a t) : int =
    List.length bag_of_expenditures
end
