open Grocery

(*open Store*)
open Items
open Bagofgoods
open Expenditure

let create_item() =
  print_endline "\n\nWelcome to Grocery. \n";
  print_endline
    "Please enter the name, price, and quantity of the item you want to create:";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      print_endline
        ("Successfully created item: " ^ Item.get_name item1 ^ " with price: "
        ^ string_of_int (Item.get_price item1)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity item1))
  | _ -> failwith "Invalid input"

let change_price() =
  print_endline "\n\nWelcome to Grocery. \n";
  print_endline
    "Please enter the name, price, quantity, and new_price of the item you \
     want to create:";
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

  let () =
    print_endline "\n\nWelcome to Grocery. \n";
    print_endline
      "1. Create new item 2. Change item price";
    print_endline 
      "\nPlease enter your choice of 1 or 2\n";
    match read_line() with
    | "1" -> create_item()
    | "2" -> change_price()
    | _ -> failwith "Invalid input"
