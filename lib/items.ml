(**The signature of items. *)
module type ItemType = sig
  type t

  val create : string -> int -> int -> t
  val get_name : t -> string
  val get_price : t -> int
  val get_quantity : t -> int
  val change_price : t -> int -> t
  val change_quantity : t -> int -> t
end

(** A single item that has name, price, and quantity. *)
module Item : ItemType = struct
  type t = {
    name : string;
    price : int;
    quantity : int;
  }

  let create (nam : string) (pri : int) (quan : int) =
    { name = nam; price = pri; quantity = quan }

  let get_name (i : t) = i.name
  let get_price (i : t) = i.price
  let get_quantity (i : t) = i.quantity

  let change_price (i : t) (p : int) =
    { name = i.name; price = i.price + p; quantity = i.quantity }

  let change_quantity (i : t) (q : int) =
    { name = i.name; price = i.price; quantity = i.quantity + q }
end
