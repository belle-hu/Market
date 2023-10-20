(**The signature of items. *)
module type ItemType = sig
  type t
  (** Type representing the information (name, price, and quantity) of the item. *)

  val create : string -> float -> float -> t
  (** Create a item with provided name, price, and quantity. *)

  val get_name : t -> string
  (**Get the name of the item. *)

  val get_price : t -> float
  (**Get the price of the item. *)

  val get_quantity : t -> float
  (**Get the quantity of the item. *)

  val change_price : t -> float -> t
  (**Adding the provided change of proce to the price of the item. (The change
     can be positive or negative.) *)

  val change_quantity : t -> float -> t
  (**Adding the provided change of quantity to the quantity of the item. (The
     change can be positive or negative.) *)
end

module Item : ItemType
(** A single item that has name, price, and quantity. *)
