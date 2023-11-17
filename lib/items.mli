(**The signature of items. *)
module type ItemType = sig
  type t
  (** Type representing the information (name, price, and quantity) of the item. *)

  val create : string -> int -> int -> t
  (** Create a item with provided name, price, and quantity. *)

  val get_name : t -> string
  (**Get the name of the item. *)

  val get_price : t -> int
  (**Get the price of the item. *)

  val get_quantity : t -> int
  (**Get the quantity of the item. *)

  val change_price : t -> int -> t
  (**Adding the provided change of proce to the price of the item. (The change
     can be positive or negative.) *)

  val change_quantity : t -> int -> t
  (**Adding the provided change of quantity to the quantity of the item. (The
     change can be positive or negative.) *)

  val to_string : t -> string
  (**Return the information of an item in a string. For example, an item named
     apple with price 1 and quantity 2 is represented as "apple (Price: 1,
     Quantity: 2)"*)
end

module Item : ItemType
(** A single item that has name, price, and quantity. *)
