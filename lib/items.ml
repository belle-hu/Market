(**The signature of items. *)
module type ItemType = sig
  type t

  val create : string -> int -> int -> t
  val get_name : t -> string
  val get_price : t -> int
  val get_quantity : t -> int
  val change_price : t -> int -> t
  val change_quantity : t -> int -> t
  val to_string : t -> string
end

(** A single item that has name, price, and quantity. *)
module Item : ItemType = struct
  type t = {
    name : string;
    price : int;
    quantity : int;
  }

  let create (nam : string) (pri : int) (quan : int) =
    if pri < 0 then failwith "Error: the price of an item cannot be less than 0"
    else if quan < 0 then
      failwith "Error: the quantity of an item cannot be less than 0"
    else { name = nam; price = pri; quantity = quan }

  let get_name (i : t) = i.name
  let get_price (i : t) = i.price
  let get_quantity (i : t) = i.quantity

  let change_price (i : t) (p : int) =
    if i.price + p < 0 then
      failwith "Error: the price of an item cannot be less than 0"
    else { name = i.name; price = i.price + p; quantity = i.quantity }

  let change_quantity (i : t) (q : int) =
    if i.quantity + q < 0 then
      failwith "Error: the quantity of an item cannot be less than 0"
    else { name = i.name; price = i.price; quantity = i.quantity + q }

  let to_string (i : t) =
    let name = get_name i in
    let price = string_of_int (get_price i) in
    let quantity = string_of_int (get_quantity i) in
    "{name = " ^ name ^ "; " ^ "price = " ^ price ^ "; quantity = " ^ quantity
    ^ "}"
end
