open Items

(** The signature of sampleable bags (multisets). *)

module type SampleGoodsType = sig
  type t

  val empty : t
  val to_list : t -> Item.t list
  val of_list : Item.t list -> t
  val join : t -> t -> t
  val join_many : t list -> t
  val sample : t -> Item.t option
  val count_elems : t -> int
  val update_price : t -> string -> int -> t
  val update_quantity : t -> string -> int -> t
end

(** Sampleable bag such that sample returns elements with probability
    proportional to their multiplicity. *)
module BagOfGoods : SampleGoodsType = struct
  type t = Item.t list

  let empty = []
  let to_list (b : t) : Item.t list = b
  let of_list (lst : Item.t list) : t = lst
  let join (b1 : t) (b2 : t) : t = b1 @ b2
  let join_many (lst : t list) : t = List.fold_left join empty lst

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

  let update_price (b : t) (nam : string) (pri : int) : t =
    let rec update_p_helper bg na pr =
      match to_list bg with
      | [] -> b
      | h :: t ->
          if Item.get_name h = nam then
            let update_item = Item.change_price h pri in
            join [ update_item ]
              (List.filter
                 (fun ele -> if Item.get_name ele = nam then false else true)
                 b)
          else update_p_helper t nam pri
    in
    update_p_helper b nam pri

  let rec update_quantity (b : t) (nam : string) (quan : int) : t =
    match to_list b with
    | [] -> b
    | h :: t ->
        if Item.get_name h = nam then
          let update_item = Item.change_quantity h quan in
          join [ update_item ]
            (List.filter
               (fun ele -> if Item.get_name ele = nam then false else true)
               b)
        else update_quantity (of_list t) nam quan
end

(** Sampleable bag such that sample always returns the element of highest
    multiplicity. Ties are broken arbitrarily. *)
module FrequencyBagGoods : SampleGoodsType = struct
  type freq_record = {
    element : Item.t;
    freq : int;
  }

  type t = freq_record list

  let empty = []

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
    | rec1 :: t when Item.get_name rec1.element = Item.get_name elt ->
        { element = elt; freq = rec1.freq + frq } :: t
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

  let join_many (lst : t list) : t = failwith "unimplemented"

  let sample (b : t) : Item.t option =
    match b with
    | [] -> None
    | rec1 :: t -> Some rec1.element

  let count_elems (b : t) : int =
    List.fold_left
      (fun sum record -> Item.get_quantity record.element + sum)
      0 b

  let rec update_price (b : t) (nam : string) (pri : int) : t =
    let rec update_p_helper bg na pr =
      match to_list bg with
      | [] -> b
      | h :: t ->
          if Item.get_name h = nam then
            let update_item = Item.change_price h pri in
            join (of_list [ update_item ]) b
          else update_p_helper (of_list t) nam pri
    in
    update_p_helper b nam pri

  let rec update_quantity (b : t) (nam : string) (quan : int) : t =
    let rec update_p_helper bg na qu =
      match to_list bg with
      | [] -> b
      | h :: t ->
          if Item.get_name h = nam then
            let update_item = Item.change_quantity h qu in
            join (of_list [ update_item ]) b
          else update_p_helper (of_list t) nam qu
    in
    update_p_helper b nam quan
end
