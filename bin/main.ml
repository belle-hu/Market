open Grocery

open Store
open Items
open Bagofgoods
open Expenditure

(**Store history*)
let bag = ref FrequencyBagGoods.empty

(**Choice 2*)

(*Helper method: 
   [update_bag bg item_name quan price] searches through a Bag of goods [bg]
   for an item with the same [item_name] and updates price and/or quantity accordingly. 
   If the same item exists, we return an updated item 
  with a quantity [quan] and new price [price + item's old price]. 
  If the item doesn't already exist, we'll return a new item instead. *)
let rec search_bag bg item_name quan price_change= 
  match (FrequencyBagGoods.to_list) bg with 
  | [] ->  Item.create item_name quan price_change  
  | h::t -> if Item.get_name h == item_name 
    then let p_change = Item.change_price h price_change
  in Item.change_quantity p_change quan else 
    search_bag (FrequencyBagGoods.of_list t) item_name quan price_change


let change_price () =
  print_endline
    "\n\
     Please enter the name, price, and the price_change of the item \
     you want to create in the format of \"[name] [quantity] \
     [new_price]\" (eg. \"apple 2 3\")\n\
    \ (Quantity and new_price should all be positive integers.)\n\
    ";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; quan; new_price ] ->
      let updated_item = search_bag (!bag) nam (int_of_string quan) (int_of_string new_price)
      in
      bag := FrequencyBagGoods.(join !bag (of_list [ updated_item ]));
      print_endline
        ("Successfully changed item: " ^ Item.get_name updated_item
       ^ " with price: "
        ^ string_of_int (Item.get_price updated_item)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity updated_item))
  | _ -> failwith "Invalid input"

(**Helper method: converts all the information in an item into a single string*)
let item_to_string (item : Item.t) : string =
  let name = Item.get_name item in
  let price = string_of_int (Item.get_price item) in
  let quantity = string_of_int (Item.get_quantity item) in
  name ^ " (Price: " ^ price ^ ", Quantity: " ^ quantity ^ ")"

(**Choice 1*)
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
      (bag := FrequencyBagGoods.(join !bag (of_list [ item1 ])));
      print_endline
        ("Bag now contains: "
        ^ String.concat ","
            (List.map Item.to_string (FrequencyBagGoods.to_list !bag)))
  | _ -> failwith "Invalid input"

(**Welcome message*)
let welcome_msg () =
  print_endline "\n\nWelcome to your new grocery store! \n";
  print_endline "What would you like to name your store?\n";
  let x = read_line () in
  let _ = print_endline ("\nWelcome to " ^ x ^ "!") in
  let _ =
    print_endline "\nPlease provide a brief description of your store.\n"
  in
  let y = read_line () in
  print_endline ("Congrats on opening your new store, " ^ x ^ ": " ^ y ^ "\n")

let dont_use () =
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

(**Main interface*)
let rec work () =
  print_endline
    "****************************************************************";
  print_endline
    "Your store can do several things!\n\
    \ 1. Create new item 2. Change item price 3. Show all items in the store \
     4. Quit";
  print_endline "\nPlease enter your choice of 1, 2, 3, or 4\n";
  match read_line () with
  | "1" ->
      create_item_add_to_bag ();
      work ()
  | "2" ->
      change_price ();
      work ()
  | "3" ->
      print_endline
        ("Bag now contains: "
        ^ String.concat ","
            (List.map Item.to_string (FrequencyBagGoods.to_list !bag)));
      work ()
  | "4" ->
      print_endline "Thank you for using our grocery system! Bye!";
      ()
  | _ -> failwith "Invalid input"

let () =
  welcome_msg ();
  work ()
