(** The signature of sampleable bags (multisets). *)
module type SampleGoodsType = sig
  type 'a t

  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val join : 'a t -> 'a t -> 'a t
  val sample : 'a t -> 'a option
  val count_elems : 'a t -> int
end

(** Sampleable bag such that sample returns elements with probability
    proportional to their multiplicity. *)
module BagOfGoods : SampleGoodsType = struct
  type 'a t = 'a list

  let to_list (b : 'a t) : 'a list = b
  let of_list (lst : 'a list) : 'a t = lst
  let join (b1 : 'a t) (b2 : 'a t) : 'a t = b1 @ b2

  let sample (b : 'a t) : 'a option =
    List.nth_opt b (Random.int (List.length b))

  let count_elems (b : 'a t) : int =
    let rec count_elements_acc list acc =
      match list with
      | [] -> acc
      | _ :: rest -> count_elements_acc rest (acc + 1)
    in
    count_elements_acc b 0
end

(** Sampleable bag such that sample always returns the element of highest
    multiplicity. Ties are broken arbitrarily. *)
module FrequencyPriceGoods : SampleGoodsType = struct
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
