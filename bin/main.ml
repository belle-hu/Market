open Grocery
open Store
open Items
open Bagofgoods

(**Store history*)
let bag = ref FrequencyBagGoods.empty

module StringMap = Map.Make (String)

let empt_map = StringMap.empty

(*[category_map represents a map of category strings to bag values]*)
let category_map = ref empt_map

(*Functions used to help implement store functions*)
let rec category_list mp =
  let bd = StringMap.bindings mp in
  let rec cat_list_aux bd =
    match bd with
    | [] -> []
    | (p1, p2) :: t -> p2 :: cat_list_aux t
  in
  cat_list_aux bd

let map_to_string mp =
  let bd = StringMap.bindings mp in
  let rec r1 bd =
    match bd with
    | [] -> ""
    | (p1, p2) :: t ->
        "Category: " ^ p1 ^ FrequencyBagGoods.to_string p2 ^ "|" ^ r1 t
  in
  r1 bd

let cat_bg_to_string e =
  "Category: " ^ fst e ^ FrequencyBagGoods.to_string (snd e) ^ "|"

let store_name = ref ""
let our_store = ref Store.empty

(*[pop_goods_by_freq freq_lim] returns the most popular goods in bag [bg]. If
  the good has been sold at least [freq_lim] number of times, it is considered
  popular. *)
let pop_goods_by_freq_aux freq_lim bg =
  (*frequency bag of goods= list of records, need each product and its
    frequency!*)
  let lst = FrequencyBagGoods.to_list bg in
  let rec r1 acc lst =
    match lst with
    | [] -> acc
    | h :: t ->
        if FrequencyBagGoods.get_frequency bg h >= freq_lim then r1 (h :: acc) t
        else r1 acc t
  in
  r1 [] lst

(*[pop_goods_by_freq freq_lim] returns the most popular goods in store. If the
  good has been sold at least [freq_lim] number of times, it is considered
  popular. *)
let pop_goods_by_freq freq_lim =
  let store_lst = Store.to_list !our_store in
  let rec r1 acc store_lst =
    match store_lst with
    | [] -> acc
    | h :: t -> r1 (pop_goods_by_freq_aux freq_lim h @ acc) t
  in
  r1 [] store_lst

type ty =
  | Price
  | Quantity
  | Discard

type transac = {
  typ : ty;
  amount : int;
  na : string;
}

type customer_transac = {
  name : string;
  amount : int;
  price : int;
}

let transaction_history = ref ([], 0)
let reciept = ref ([], 0)

(**Choice 2*)

(*Helper method: [update_bag bg item_name quan price] searches through a Bag of
  goods [bg] for an item with the same [item_name] and updates price and/or
  quantity accordingly. If the same item exists, we return an updated item with
  a quantity [quan] and new price [price + item's old price]. If the item
  doesn't already exist, we'll return a new item instead. *)
let rec search_bag bg item_name quan price_change =
  match FrequencyBagGoods.to_list bg with
  | [] -> Item.create item_name quan price_change
  | h :: t ->
      if Item.get_name h == item_name then
        let p_change = Item.change_price h price_change in
        Item.change_quantity p_change quan
      else search_bag (FrequencyBagGoods.of_list t) item_name quan price_change

(*[change_price_aux nam price_change cat cat_mp] is a helper method that changes
  the price of an item of name [nam] in a category [cat] in our category map
  [cat_mp]*)
let change_price_aux nam price_change cat cat_mp =
  match StringMap.find_opt cat cat_mp with
  | None -> failwith "Category not found!"
  | Some v ->
      let v' = FrequencyBagGoods.update_price v nam price_change in
      let m' = StringMap.add cat v' cat_mp in
      category_map := m';
      our_store := Store.of_list (category_list !category_map)

let rec change_price () =
  print_endline
    "\n\
     Please enter the name, the price_change, and category of the item you \
     want to change in the format of \"[name] [price_change] [category]\" (eg. \
     \"apple 2 fruit\"). \n\
    \ The new price of the item will be original price + price_change. \n\
    \ (Requirements: 1. price_change should be an integer. \n\
    \ It can be negative, but the original price + price_change cannot be \
     smaller than or equal to 0. \n\
    \ 2. The name of the item you enter should already be created.)\n";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; price_change; cat ] ->
      (bag :=
         FrequencyBagGoods.(update_price !bag nam (int_of_string price_change)));
      let trans_list, sale = !transaction_history in

      transaction_history :=
        ( trans_list
          @ [ { na = nam; amount = int_of_string price_change; typ = Price } ],
          sale );
      change_price_aux nam (int_of_string price_change) cat !category_map;
      print_endline
        ("Successfully changed item: " ^ nam ^ " with price_change: "
       ^ price_change ^ " in " ^ cat);
      print_endline
        ("Store now contains: "
        ^ String.concat ","
            (List.map FrequencyBagGoods.to_string (Store.to_list !our_store)))
  | _ ->
      print_endline
        "The format of the input is not correct. Please enter your input with \
         correct format again";
      change_price ()

(*Choice 3*)

(*[change_quantity_aux nam quantity_change cat cat_mp] is a helper method that
  changes the quantity of an item of name [nam] in a category [cat] in our
  category map [cat_mp]*)
let change_quantity_aux nam quantity_change cat cat_mp =
  match StringMap.find_opt cat cat_mp with
  | None -> failwith "Category not found!"
  | Some v ->
      let v' = FrequencyBagGoods.update_quantity v nam quantity_change in
      let m' = StringMap.add cat v' cat_mp in
      category_map := m';
      our_store := Store.of_list (category_list !category_map)

let rec change_quantity () =
  print_endline
    "\n\
     Please enter the name, the quantity, and the category of the item you \
     want to import in the format of \"[name] [imported_quantity] [category]\" \
     (eg. \"apple 2 fruit\"). \n\
    \ The new quantity of the item will be original quantity + \
     imported_quantity. \n\
    \ (Requirements: 1. imported_quantity should be an positive integer. \n\
    \ 2. The name of the item you enter should already be created.)\n";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; quantity_change; cat ] ->
      (bag :=
         FrequencyBagGoods.(
           update_quantity !bag nam (int_of_string quantity_change)));

      let trans_list, sale = !transaction_history in

      transaction_history :=
        ( trans_list
          @ [
              {
                na = nam;
                amount = int_of_string quantity_change;
                typ = Quantity;
              };
            ],
          sale );
      change_quantity_aux nam (int_of_string quantity_change) cat !category_map;
      print_endline
        ("Successfully changed item: " ^ nam ^ " with quantity_change: "
       ^ quantity_change ^ " in " ^ cat)
  | _ ->
      print_endline
        "The format of the input is not correct. Please enter your input with \
         correct format again";
      change_quantity ()

(**Helper method: converts all the information in an item into a single string*)
let item_to_string (item : Item.t) : string =
  let name = Item.get_name item in
  let price = string_of_int (Item.get_price item) in
  let quantity = string_of_int (Item.get_quantity item) in
  name ^ " (Price: " ^ price ^ ", Quantity: " ^ quantity ^ ")"

(*[add_to_category cat bg] adds binding key [cat] to value [bg] in map
  [cat_mp]*)
let add_to_category cat bg =
  let v1 = StringMap.find_opt cat !category_map in
  match v1 with
  | None ->
      category_map := StringMap.add cat bg !category_map;
      our_store := Store.of_list (category_list !category_map)
  | Some v ->
      let joined_bg = FrequencyBagGoods.join v bg in
      category_map := StringMap.add cat joined_bg !category_map;
      our_store := Store.of_list (category_list !category_map)

(**Choice 1*)
let rec create_item_add_to_store () =
  print_endline
    "\n\
     Please enter the name, price, quantity, category of the item you want to \
     create in the format of \"[name] [price] [quantity] [category]\" (eg. \
     \"apple 1 2 fruit\")\n\
     Also, please note that you should not be creating a new item with the \
     same name as an item you have already created. For example, if you \
     previously created \"apple 1 2 fruit\", please do not create a new \
     \"apple 5 7 fruit\". Instead, you should use the change_price or \
     change_quantity functionalities. \n\
    \     (Price and quantity should be integers):";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; pri; quan; cat ] ->
      let item1 = Item.create nam (int_of_string pri) (int_of_string quan) in
      let bag1 = FrequencyBagGoods.of_list [ item1 ] in
      print_endline
        ("Successfully created item: " ^ Item.get_name item1 ^ " with price: "
        ^ string_of_int (Item.get_price item1)
        ^ " and quantity: "
        ^ string_of_int (Item.get_quantity item1)
        ^ " in " ^ cat);
      (bag := FrequencyBagGoods.(join !bag (of_list [ item1 ])));
      let trans_list, sale = !transaction_history in

      transaction_history :=
        ( trans_list
          @ [ { na = nam; amount = int_of_string quan; typ = Quantity } ],
          sale );
      add_to_category cat bag1;
      print_endline ("The store now contains: " ^ map_to_string !category_map)
  | _ ->
      print_endline
        "The format of the input is not correct. Please enter your input with \
         correct format again";
      create_item_add_to_store ()

(*Choice 4*)
let show_category_and_items () = print_endline (map_to_string !category_map)

(*Choice 5*)
let show_item_names () =
  let item_names = FrequencyBagGoods.names !bag in
  print_endline "The list of items are:";
  List.iter (fun name -> print_endline ("- " ^ name)) item_names

(*Choice 6*)
let show_total_quantity () =
  let total_quant = FrequencyBagGoods.total_quantity !bag in
  print_endline
    ("The total quantity of all items in the store is: "
   ^ string_of_int total_quant)

(*Choice 7*)
let show_total_cost () =
  let total_cost = FrequencyBagGoods.total_cost !bag in
  print_endline
    ("The total value of all items in the store is: $"
   ^ string_of_int total_cost)

(*Choice 8*)
(*[sell_items_aux cat nam quan_change cat_map] is a helper function that sells
  [quan_change] of an item by name [nam] in store*)
let discard_items_aux cat nam quan_change st =
  let item_opt = Store.item_by_name nam st in
  match item_opt with
  | None -> failwith "Sorry, item doesn't exist in store."
  | Some i -> (
      our_store := Store.sell_goods ~-quan_change i st;
      match StringMap.find_opt cat !category_map with
      | None -> failwith "Category not found!"
      | Some v ->
          let v' = FrequencyBagGoods.update_quantity v nam (1 * quan_change) in
          let m' = StringMap.add cat v' !category_map in
          category_map := m')

let rec discard_items () =
  print_endline
    "\n\
     Please enter the name, the quantity, and the category of the item you \
     want to remove in the format of \"[name] [quantity_removed] [category] \" \
     (eg. \"apple 1 fruit\"). \n\
    \ The new quantity of the item will be original quantity - \
     quantity_removed. \n\
    \ (Requirements: 1. quantity_removed should be an positive integer. \n\
    \ 2. The name of the item you enter should already be created.)\n";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; quantity_change; cat ] ->
      (bag :=
         FrequencyBagGoods.(
           update_quantity !bag nam ~-(int_of_string quantity_change)));
      let trans_list, sale = !transaction_history in

      transaction_history :=
        ( trans_list
          @ [
              {
                na = nam;
                amount = int_of_string quantity_change;
                typ = Discard;
              };
            ],
          sale );
      discard_items_aux cat nam ~-(int_of_string quantity_change) !our_store;
      print_endline
        ("Successfully discard item: " ^ nam ^ " of " ^ quantity_change
       ^ " amount" ^ " in " ^ cat)
  | _ ->
      print_endline
        "The format of the input is not correct. Please enter your input with \
         correct format again";
      discard_items ()

let remove_item () =
  print_endline "Please enter the name of the item you want to remove:";
  let item_name = read_line () in
  try
    bag := FrequencyBagGoods.remove !bag item_name;
    print_endline ("Successfully removed item: " ^ item_name);
    print_endline
      ("Bag now contains: "
      ^ String.concat ","
          (List.map Item.to_string (FrequencyBagGoods.to_list !bag)))
  with Failure msg -> print_endline msg

(*Choice 9*)
let show_almost_out_goods () =
  let almost_out_goods = Store.almost_out 10 !our_store in
  print_endline
    ("Items that are almost out include: "
    ^ String.concat ","
        (List.map FrequencyBagGoods.to_string (Store.to_list almost_out_goods))
    )

(*Choice 10*)
(* let show_popular_goods () = let popular_goods = pop_goods_by_freq 100 in
   print_endline ("Our popular items in store are: " ^ String.concat ","
   (List.map Item.to_string (popular_goods))) *)

(*Choice 11*)
let show_transaction_history () =
  print_endline "Here are the transaction history:";
  let rec history_helper (lst, s) =
    match lst with
    | [] ->
        print_endline
          ("Transaction history has all been displayed.\nThe total sale is "
         ^ string_of_int s)
    | h :: t -> (
        match h.typ with
        | Price ->
            if h.amount < 0 then
              print_endline
                ("You decrease the price of " ^ h.na ^ " by "
               ^ string_of_int ~-(h.amount))
            else
              print_endline
                ("You increase the price of " ^ h.na ^ " by "
               ^ string_of_int h.amount);
            history_helper (t, s)
        | Quantity ->
            if h.amount < 0 then
              print_endline
                ("You sold " ^ string_of_int ~-(h.amount) ^ " of " ^ h.na)
            else
              print_endline
                ("You imported " ^ string_of_int h.amount ^ " of " ^ h.na);
            history_helper (t, s)
        | Discard ->
            print_endline
              ("You discard " ^ string_of_int h.amount ^ " of " ^ h.na);
            history_helper (t, s))
  in
  history_helper !transaction_history

(*Customer choice 1*)
let rec customer_buy () =
  print_endline
    "Please enter the name, the quantity, and the category of the item you \
     want to buy in the format of \"[name] [quantity] [category]\" (eg. \
     \"apple 1 fruit\"):";
  print_endline
    "Notice: \n\
    \     1. The name of the item you enter must be already in the store";
  match String.split_on_char ' ' (read_line ()) with
  | [ nam; quantity_change; cat ] ->
      (bag :=
         FrequencyBagGoods.(
           update_quantity_fre !bag nam ~-(int_of_string quantity_change)));

      let price_opt = FrequencyBagGoods.get_price !bag nam in
      let product_price =
        match price_opt with
        | None -> 0
        | Some v -> v
      in
      let trans_list, sale = !transaction_history in
      transaction_history :=
        ( trans_list
          @ [
              {
                na = nam;
                amount = ~-(int_of_string quantity_change);
                typ = Quantity;
              };
            ],
          sale + (product_price * int_of_string quantity_change) );
      let price_opt = FrequencyBagGoods.get_price !bag nam in
      let product_price =
        match price_opt with
        | None -> 0
        | Some v -> v
      in
      let total = snd !reciept in
      let new_total = total + (int_of_string quantity_change * product_price) in
      reciept :=
        ( fst !reciept
          @ [
              {
                name = nam;
                amount = int_of_string quantity_change;
                price = product_price;
              };
            ],
          new_total );
      discard_items_aux cat nam ~-(int_of_string quantity_change) !our_store;
      print_endline
        ("Successfully purchased item: " ^ nam ^ " with quantity: "
       ^ quantity_change ^ " in " ^ cat)
  | _ ->
      print_endline
        "The input you enter is unvalid. Please enter the input again in \
         correct format.";
      customer_buy ()

(*print out reciept*)
let rec print_reciept_helper tup =
  match tup with
  | [], n -> print_endline "Done."
  | h :: t, v ->
      let n = h.name in
      let a = h.amount in
      let p = h.price in
      print_endline
        ("Product: " ^ n ^ ", Amount: " ^ string_of_int a ^ ", Price: "
       ^ string_of_int p);
      print_reciept_helper (t, v)

let print_reciept () =
  print_endline "Here is your reciept: ";
  print_reciept_helper !reciept;
  print_endline ("Your total is: " ^ string_of_int (snd !reciept))

(*Welcome message*)
let welcome_msg () =
  print_endline "\n\nWelcome to your new grocery store! \n";
  print_endline "What would you like to name your store?\n";
  let rec get_store_name () =
    let x = read_line () in
    if String.trim x = "" then (
      print_endline
        "You cannot leave your store name blank! Please enter a name:";
      get_store_name ())
    else x
  in
  store_name := get_store_name ();
  let _ = print_endline ("\nWelcome to " ^ !store_name ^ "!") in
  let _ =
    print_endline "\nPlease provide a brief description of your store.\n"
  in
  let y = read_line () in
  print_endline
    ("Congrats on opening your new store, " ^ !store_name ^ ": " ^ y ^ "\n")

let dont_use () =
  print_endline "\n\nWelcome to your new grocery store! \n";
  print_endline "What would you like to name your store?\n";
  match read_line () with
  | x -> (
      print_endline ("Welcome to " ^ x ^ "!");
      print_endline "1. Create new item 2. Change item price";
      print_endline "\nPlease enter your choice of 1 or 2\n";
      match read_line () with
      | "1" -> create_item_add_to_store ()
      | "2" -> change_price ()
      | _ -> failwith "Invalid input")

(**Main interface*)
let rec work () =
  print_endline
    "****************************************************************";
  print_endline
    "Your store can do several things!\n\
     1. Create new item \n\
     2. Change item price \n\
     3. Import certain quantity of an item \n\
     4. Show all items with their categories in store \n\
     5. Show all item names in the store \n\
     6. Show total quantity of all items in the store \n\
     7. Show total value of all items in the store  \n\
     8. Discard certain quantity of an item \n\
     9. Take a look at our items that have almost run out! \n\
     10. Switch to customer mode \n\
     11. Show transaction history \n\
     Q. Quit";
  print_endline "\nPlease enter your choice of 1-11, or Q\n";
  match read_line () with
  | "1" ->
      create_item_add_to_store ();
      work ()
  | "2" ->
      change_price ();
      work ()
  | "3" ->
      change_quantity ();
      work ()
  | "4" ->
      show_category_and_items ();
      work ()
  | "5" ->
      show_item_names ();
      work ()
  | "6" ->
      show_total_quantity ();
      work ()
  | "7" ->
      show_total_cost ();
      work ()
  | "8" ->
      discard_items ();
      work ()
  | "9" ->
      show_almost_out_goods ();
      work ()
  | "10" -> customer_mode ()
  | "11" ->
      show_transaction_history ();
      work ()
  | "Q" ->
      print_endline
        ("Thank you for using our grocery system, " ^ !store_name ^ "! Bye!");
      ()
  | _ -> failwith "Invalid input"

and customer_mode () =
  let rec customer_mode_helper () =
    print_endline
      "****************************************************************";
    print_endline
      "Welcome to customer mode!\n\
       Dear customer, here are the items currently in the store:";
    show_category_and_items ();
    print_endline
      "****************************************************************";
    print_endline
      "Do you want to buy some items? \n\
       Please enter your choice of 1 or 2: \n\
       1. Yes, I want to buy something.\n\
       2. I want to checkout. (Switch back to the store owner mode)";
    match read_line () with
    | "1" ->
        customer_buy ();
        customer_mode_helper ()
    | "2" ->
        print_endline ("Thank you for shopping at " ^ !store_name);
        print_reciept ();
        print_endline "We will switch back to store mode.";
        work ()
    | _ ->
        print_endline
          "The format of the input is not correct. Please enter your input \
           with correct format again";
        customer_mode_helper ()
  in
  customer_mode_helper ()

let () =
  welcome_msg ();
  work ()
