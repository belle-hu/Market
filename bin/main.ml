open Grocery

(*open Store*)
open Items
open Bagofgoods

let () =
  print_endline "\n\nWelcome to Grocery. \n";
  print_endline
    "Please enter the name, price, and quantity of the item you want to create:";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      print_endline
        (Item.get_name item1 ^ " "
        ^ string_of_int (Item.get_price item1)
        ^ " "
        ^ string_of_int (Item.get_quantity item1))
  | _ -> failwith "Invalid input"
