open Grocery

(*open Store*)
open Items
open Bagofgoods
open Expenditure

let create_item () =
  print_endline
    "Please enter the name, price, and quantity of the item you want to create \
     in the format of \"[name] [price] [quantity]\" (eg. \"apple 1 2\")\n\
    \ (Price and quantity should be integers.):";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      print_endline
        ("Successfully created item: " ^ Item.get_name item1 ^ " with price: "
        ^ string_of_int (Item.get_price item1)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity item1))
  | _ -> failwith "Invalid input"

let change_price () =
  print_endline
    "\n\
     Please enter the name, price, quantity, and the price_change of the item \
     you want to create in the format of \"[name] [price] [quantity] \
     [price_change]\" (eg. \"apple 1 2 3\")\n\
    \ (Price, quantity, and price_change should all be integers.\n\
    \ Price_change can be a positive or negative integer, and the new price of \
     the item will be [price + price_change]):";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan; new_price ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      let price = int_of_string new_price in
      let new_item = Item.change_price item1 price in
      print_endline
        ("Successfully changed item: " ^ Item.get_name new_item
       ^ " with price: "
        ^ string_of_int (Item.get_price new_item)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity new_item))
  | _ -> failwith "Invalid input"

(**Helper method: converts all the information in an item into a single string*)
let item_to_string (item : Item.t) : string =
  let name = Item.get_name item in
  let price = string_of_int (Item.get_price item) in
  let quantity = string_of_int (Item.get_quantity item) in
  name ^ " (Price: " ^ price ^ ", Quantity: " ^ quantity ^ ")"

let create_item_add_to_bag () =
  print_endline
    "\n\
     Please enter the name, price, and quantity of the item you want to create \
     in the format of \"[name] [price] [quantity]\" (eg. \"apple 1 2\")\n\
    \ (Price and quantity should be integers):";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      print_endline
        ("Successfully created item: " ^ Item.get_name item1 ^ " with price: "
        ^ string_of_int (Item.get_price item1)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity item1));
      let bag = [ item1 ] in
      let item_to_string (item : Item.t) : string =
        let name = Item.get_name item in
        let price = string_of_int (Item.get_price item) in
        let quantity = string_of_int (Item.get_quantity item) in
        name ^ " (Price: " ^ price ^ ", Quantity: " ^ quantity ^ ")"
      in
      print_endline
        ("Bag now contains: " ^ String.concat "," (List.map item_to_string bag))
  | _ -> failwith "Invalid input"

let () =
  print_endline "\n\nWelcome to your new grocery store! \n";
  print_endline "What would you like to name your store?\n";
  match read_line () with
  | x -> (
      print_endline ("Welcome to " ^ x ^ "!");
      print_endline "1. Create new item 2. Change item price";
      print_endline "\nPlease enter your choice of 1 or 2\n";
      match read_line () with
      | "1" -> create_item_add_to_bag ()
      | "2" -> change_price ()
      | _ -> failwith "Invalid input")
