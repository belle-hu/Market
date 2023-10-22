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

(* Sampleable bag such that sample always returns the element of highest
    multiplicity. Ties are broken arbitrarily. *)
(*module FrequencyPriceGoods : SampleGoodsType = struct
  type 'a t = (float * int * 'a) list

  (**FIX: PLACEHOLDER PRICE*)
  let rec to_list (b : 'a t) : 'a list =
    match b with
    | [] -> []
    | (price, freq, elem) :: rest ->
        let repeated_elements = List.init freq (fun _ -> elem) in
        repeated_elements @ to_list rest

  (*helper method for of_list*)

  (**FIX: PLACEHOLDER 0.0*)
  let rec of_list_helper (lst : 'a list) : 'a t =
    match lst with
    | [] -> []
    | hd :: tl ->
        let count, rest = List.partition (fun x -> x = hd) tl in
        (0.0, List.length count + 1, hd) :: of_list_helper rest

  let of_list (lst : 'a list) : 'a t = of_list_helper lst

  (*Fix implementation later*)
  let join (b1 : 'a t) (b2 : 'a t) : 'a t = b1 @ b2

  let sample (b : 'a t) : 'a option =
    match b with
    | [] -> None
    | (p, q, e) :: t -> Some e

  let count_elems (b : 'a t) : int =
    let rec count_elements_acc items acc =
      match items with
      | [] -> acc
      | (_, frequency, _) :: rest -> count_elements_acc rest (acc + frequency)
    in
    count_elements_acc b 0
end
*)
