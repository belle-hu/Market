open Items

(** The signature of sampleable bags (multisets). *)

module type SampleGoodsType = sig
  type t

  val to_list : t -> Item.t list
  val of_list : Item.t list -> t
  val join : t -> t -> t
  val sample : t -> Item.t option
  val count_elems : t -> int
end

(** Sampleable bag such that sample returns elements with probability
    proportional to their multiplicity. *)
module BagOfGoods : SampleGoodsType = struct
  type t = Item.t list

  let to_list (b : t) : Item.t list = b
  let of_list (lst : Item.t list) : t = lst
  let join (b1 : t) (b2 : t) : t = b1 @ b2

  let sample (b : t) : Item.t option =
    List.nth_opt b (Random.int (List.length b))

  let count_elems1 (b : t) : int =
    let rec count_elements_acc list acc =
      match list with
      | [] -> acc
      | _ :: rest -> count_elements_acc rest (acc + 1)
    in
    count_elements_acc b 0

  let count_elems (b : t) : int = List.length b
end

(** Sampleable bag such that sample always returns the element of highest
    multiplicity. Ties are broken arbitrarily. *)
module FrequencyBagGoods : SampleGoodsType = struct
  type freq_record = {
    element : Item.t;
    freq : int;
  }

  type t = freq_record list

  let rec to_list (b : t) : Item.t list =
    match b with
    | [] -> []
    | rec1 :: t -> rec1.element :: to_list t

  (** Either updates a record's frequency field by [freq] if the element is
      already in the list, or adds a new record with [elt] and freq of 1 if the
      element is not in the list.*)
  let rec update_freq lst elt frq =
    match lst with
    | [] -> [ { element = elt; freq = frq } ]
    | rec1 :: t
      when Item.get_name rec1.element = Item.get_name elt
           && Item.get_price rec1.element = Item.get_price elt ->
        {
          element = Item.change_quantity rec1.element frq;
          freq = rec1.freq + frq;
        }
        :: t
    | rec1 :: t -> rec1 :: update_freq t elt frq

  (** Compares 2 records and returns 0 if they are the same frequency, a
      positive integer if r1 is smaller, and negative if r1 is larger. This
      comparator helps List.sort prefer descending order over ascending order.*)
  let compare_record r1 r2 = r2.freq - r1.freq

  let rec of_list (lst : Item.t list) : t =
    match lst with
    | [] -> []
    | elt :: t ->
        let record_list = of_list t in
        let updated_list =
          update_freq record_list elt (Item.get_quantity elt)
        in
        List.sort compare_record updated_list

  let rec join (b1 : t) (b2 : t) : t =
    match b1 with
    | [] -> b2
    | rec1 :: t ->
        let combine_rec1 = join t b2 in
        let combined_all = update_freq combine_rec1 rec1.element rec1.freq in
        List.sort compare_record combined_all

  let sample (b : t) : Item.t option =
    match b with
    | [] -> None
    | rec1 :: t -> Some rec1.element

  let count_elems (b : t) : int =
    List.fold_left
      (fun sum record -> Item.get_quantity record.element + sum)
      0 b
end
