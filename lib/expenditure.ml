module Expenditure = struct
  (** module representing a list of items bought, including information about
      how many and amount of money spent. This log of items bought can be for
      both the store and individual customers.*)

  type p = {
    name : string;
    price : float;
    quantity : float;
  }

  type 'a t = p list
  (** note: I tried to do Item list, but it said unbound*)

  let to_list (bag_of_expenditures : 'a t) : 'a list = bag_of_expenditures
  let of_list (list_of_expenditures : 'a list) : 'a t = list_of_expenditures

  (** determine whether item is already in the list. COME BACK TO*)
  (* let find_item (item: 'a)(item_list: 'a list): bool = match item_list with |
     [] -> false | h::t -> *)

  (** add an item to the collection of items previously bought*)
  let add_item (item : 'a) (bag : 'a t) : 'a t = item :: to_list bag

  (** counts the number of distinct items bought*)
  let count_elems (bag_of_expenditures : 'a t) : int =
    List.length bag_of_expenditures

  (** computes the total expenditure. That is, the cost of all the items, taking
      into account their multiplicities*)
  let total_expenditure (item_list : 'a t) : float =
    List.fold_left
      (fun acc { name = n; price = p; quantity = q } -> (p *. q) +. acc)
      0.0 item_list

  (** returns the most frequent item bought*)
  let most_frequent_item item_list =
    match item_list with
    | [] -> None
    | h :: t -> Some h

  (** returns a list of all the distinct items bought*)
  let distinct_items_list item_list =
    List.map (fun { name = n; price = p; quantity = q } -> n) item_list

  (** returns the total number of items bought, accounting for the
      multiplicities of each item*)
  let total_items (item_list : 'a t) : float =
    List.fold_left
      (fun acc { name = n; price = p; quantity = q } -> q +. acc)
      0.0 item_list
end
