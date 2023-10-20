(**The signature of items. *)
module type ItemType = sig
  type t

  val create : string -> float -> float -> t
  val get_name : t -> string
  val get_price : t -> float
  val get_quantity : t -> float
  val change_price : t -> float -> t
  val change_quantity : t -> float -> t
end

(** A single item that has name, price, and quantity. *)
module Item : ItemType = struct
  type t = {
    name : string;
    price : float;
    quantity : float;
  }

  let create (nam : string) (pri : float) (quan : float) =
    { name = nam; price = pri; quantity = quan }

  let get_name (i : t) = i.name
  let get_price (i : t) = i.price
  let get_quantity (i : t) = i.quantity

  let change_price (i : t) (p : float) =
    { name = i.name; price = i.price +. p; quantity = i.quantity }

  let change_quantity (i : t) (q : float) =
    { name = i.name; price = i.price; quantity = i.quantity +. q }
end
