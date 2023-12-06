open OUnit2
open Grocery

(*open Chat open Ngrams open Model open Bag *)
open Store
open Bagofgoods
open Items

(********************************************************************
   Here are some helper functions for your testing of bag-like lists.
 ********************************************************************)

(** [cmp_bag_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent bag-like lists. That means checking that they they contain the
    same elements with the same number of repetitions, though not necessarily in
    the same order. *)
let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

(** [pp_int i] pretty-prints int [i]. *)
let pp_int i = "\"" ^ string_of_int i ^ "\""

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_bag_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] );
    (* Uncomment this test to see what happens when a test case fails. ( "counts
       must be the same" >:: fun _ -> assert_equal ~cmp:cmp_bag_like_lists
       ~printer:(pp_list pp_string) ["foo"; "foo"] ["foo"]); *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* let item_create_test out in1 in2 in3 _ = assert_equal out Item.(create in1
   in2 in3 |> to_string) *)

let item_create_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(fun s -> s) out in1

let item_get_name_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(fun s -> s) out (Item.get_name in1)

let item_get_price_test msg out in1 =
  msg >:: fun _ ->
  assert_equal ~printer:(fun s -> string_of_int s) out (Item.get_price in1)

let item_get_quantity_test msg out in1 =
  msg >:: fun _ ->
  assert_equal ~printer:(fun s -> string_of_int s) out (Item.get_quantity in1)

let item_change_price_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal
    ~printer:(fun s -> s)
    out
    Item.(change_price in1 in2 |> to_string)

let item_change_quantity_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal
    ~printer:(fun s -> s)
    out
    Item.(change_quantity in1 in2 |> to_string)

let item_to_list_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(fun s -> s) out (Item.to_string in1)

let item_change_price_exception_test msg in1 in2 =
  msg >:: fun _ ->
  assert_raises (Failure "Error: the price of an item cannot be less than 0")
    (fun () -> Item.change_price in1 in2)

let item_change_quantity_exception_test msg in1 in2 =
  msg >:: fun _ ->
  assert_raises (Failure "Error: the quantity of an item cannot be less than 0")
    (fun () -> Item.change_quantity in1 in2)

let item_create_price_exception_test msg in1 in2 in3 =
  msg >:: fun _ ->
  assert_raises (Failure "Error: the price of an item cannot be less than 0")
    (fun () -> Item.create in1 in2 in3)

let item_create_quantity_exception_test msg in1 in2 in3 =
  msg >:: fun _ ->
  assert_raises (Failure "Error: the quantity of an item cannot be less than 0")
    (fun () -> Item.create in1 in2 in3)

let item1 = Item.create "item1" 3 10
let item2 = Item.create "item2" 10 100
let item3 = Item.create "item3" 50 500
let item4 = Item.create "item4" 20 1000
let item5 = Item.create "item5" 100 10000

let items_tests =
  [
    (*create tests*)
    item_create_test "Create item: apple w/ price 1 & quantity 2"
      "{name = apple; price = 1; quantity = 2}"
      (Item.create "apple" 1 2 |> Item.to_string);
    item_create_test "Create item: banana w/ price 3 & quantity 100"
      "{name = banana; price = 3; quantity = 100}"
      (Item.create "banana" 3 100 |> Item.to_string);
    item_create_test "Create item: pear w/ price 5 & quantity 90"
      "{name = pear; price = 5; quantity = 90}"
      (Item.create "pear" 5 90 |> Item.to_string);
    (*get_name tests*)
    item_get_name_test "Get the name of an item1" "apple"
      (Item.create "apple" 1 2);
    item_get_name_test "Get the name of an item2" "Candy"
      (Item.create "Candy" 4 5);
    item_get_name_test "Get the name of an item3" "" (Item.create "" 6 7);
    item_get_name_test "Get the name of item1" "item1" item1;
    item_get_name_test "Get the name of item2" "item2" item2;
    item_get_name_test "Get the name of item3" "item3" item3;
    item_get_name_test "Get the name of item4" "item4" item4;
    item_get_name_test "Get the name of item5" "item5" item5;
    (*get_price tests*)
    item_get_price_test "Get the price of an item1" 3
      (Item.create "banana" 3 10);
    item_get_price_test "Get the price of an item2" 1 (Item.create "peach" 1 30);
    item_get_price_test "Get the price of an item3" 10 (Item.create "milk" 10 5);
    item_get_price_test "Get the price of item1" 3 item1;
    item_get_price_test "Get the price of item2" 10 item2;
    item_get_price_test "Get the price of item3" 50 item3;
    item_get_price_test "Get the price of item4" 20 item4;
    item_get_price_test "Get the price of item5" 100 item5;
    (*get_quantity tests*)
    item_get_quantity_test "Get the quantity of an item1" 10
      (Item.create "cookie" 50 10);
    item_get_quantity_test "Get the quantity of an item2" 8
      (Item.create "some item" 100 8);
    item_get_quantity_test "Get the quantity of an item3" 497
      (Item.create "potato" 5 497);
    item_get_quantity_test "Get the quantity of item1" 10 item1;
    item_get_quantity_test "Get the quantity of item2" 100 item2;
    item_get_quantity_test "Get the quantity of item3" 500 item3;
    item_get_quantity_test "Get the quantity of item4" 1000 item4;
    item_get_quantity_test "Get the quantity of item5" 10000 item5;
    (*change_price tests*)
    item_change_price_test "Increase the price of an item"
      "{name = item1; price = 5; quantity = 10}" item1 2;
    item_change_price_test "Decrease the price of an item"
      "{name = item1; price = 1; quantity = 10}" item1 ~-2;
    item_change_price_test "Leave the price of an item unchanged"
      "{name = item1; price = 3; quantity = 10}" item1 0;
    (*change_quantity tests*)
    item_change_quantity_test "Increase the quantity of an item"
      "{name = item2; price = 10; quantity = 110}" item2 10;
    item_change_quantity_test "Decrease the quantity of an item"
      "{name = item2; price = 10; quantity = 80}" item2 ~-20;
    item_change_quantity_test "Leave the quantity of an item unchanged"
      "{name = item2; price = 10; quantity = 100}" item2 0;
    (*to_list tests*)
    item_to_list_test "to_list of item1"
      "{name = item1; price = 3; quantity = 10}" item1;
    item_to_list_test "to_list of item1 after changing the price"
      "{name = item1; price = 6; quantity = 10}"
      (Item.change_price item1 3);
    item_to_list_test "to_list of item1 after changing the quantity"
      "{name = item1; price = 3; quantity = 5}"
      (Item.change_quantity item1 ~-5);
    item_to_list_test "to_list of item2"
      "{name = item2; price = 10; quantity = 100}" item2;
    item_to_list_test "to_list of item2 after changing the price"
      "{name = item2; price = 15; quantity = 100}"
      (Item.change_price item2 5);
    item_to_list_test "to_list of item2 after changing the quantity"
      "{name = item2; price = 10; quantity = 50}"
      (Item.change_quantity item2 ~-50);
    item_to_list_test "to_list of an item after more than one changes1"
      "{name = item1; price = 6; quantity = 15}"
      Item.(change_quantity (change_price item1 3) 5);
    item_to_list_test
      "to_list of an item after more than one changes2 (Property: the sequence \
       of changes shouldn't influence the final state of an item)"
      "{name = item1; price = 6; quantity = 15}"
      Item.(change_price (change_quantity item1 5) 3);
    item_to_list_test
      "to_list of an item after many changes (Property: increase the price of \
       an item by 1 and then decrease the price by 1 will not change the price \
       of the item)"
      "{name = item1; price = 3; quantity = 20}"
      Item.(change_price (change_quantity (change_price item1 ~-1) 10) 1);
    (*change_price_exception tests*)
    item_change_price_exception_test
      "Raise exception if the price is below 0 after change_price item1" item1
      ~-10000;
    item_change_price_exception_test
      "Raise exception if the price is below 0 after change_price item2" item2
      ~-11;
    item_change_price_exception_test
      "Raise exception if the price is below 0 after change_price item3" item3
      ~-51;
    (*change_quantity_exception tests*)
    item_change_quantity_exception_test
      "Raise exception if the quantity is below 0 after change_price item1"
      item1 ~-10000;
    item_change_quantity_exception_test
      "Raise exception if the quantity is below 0 after change_price item2"
      item2 ~-101;
    item_change_quantity_exception_test
      "Raise exception if the quantity is below 0 after change_price item3"
      item3 ~-501;
    (*create_price_exception tests*)
    item_create_price_exception_test
      "Raise exception if an item is created with price < 0: test 1" "banana"
      ~-1 10;
    item_create_price_exception_test
      "Raise exception if an item is created with price < 0: test 2" "apple"
      ~-20 50;
    item_create_price_exception_test
      "Raise exception if an item is created with price < 0: test 3" "sugar"
      ~-30 60;
    item_create_price_exception_test
      "Raise exception if an item is created with price < 0: test 4" "grapes"
      ~-40 80;
    item_create_price_exception_test
      "Raise exception if an item is created with price < 0: test 5 (The \
       quantity is also below 0)"
      "pocky" ~-100 ~-10000;
    (*create_quantity_exception tests*)
    item_create_quantity_exception_test
      "Raise exception if an item is created with quantity < 0: test 1" "banana"
      1 ~-10;
    item_create_quantity_exception_test
      "Raise exception if an item is created with quantity < 0: test 2" "apple"
      20 ~-50;
    item_create_quantity_exception_test
      "Raise exception if an item is created with quantity < 0: test 3" "grapes"
      30 ~-60;
    item_create_quantity_exception_test
      "Raise exception if an item is created with quantity < 0: test 4" "sugar"
      40 ~-80;
    item_create_quantity_exception_test
      "Raise exception if an item is created with quantity < 0: test 5" "pear"
      100 ~-10000;
  ]

(** Regular Bag of Goods Tests*)
let items1 = Item.create "cake" 1 2

let items1plus1quant = Item.create "cake" 1 3
let items1plus1price = Item.create "cake" 2 2
let items1plusquant_lst = [ items1plus1quant ]
let items1more = Item.create "cake" 5 4
let items1lots = Item.create "cake" 1 6
let items2 = Item.create "cookie" 1 3
let items2plus1quant = Item.create "cookie" 1 4
let items2plus1price = Item.create "cookie" 2 3
let items2_bag = BagOfGoods.of_list [ items2 ]
let items3 = Item.create "pie" 5 1
let items3plus1quant = Item.create "pie" 5 2
let items3plus1price = Item.create "pie" 6 1
let items3_lst = [ items3 ]
let items3_bag = BagOfGoods.of_list [ items3 ]
let items4 = Item.create "milk" 4 2
let items4_bag = BagOfGoods.of_list [ items4 ]
let empty_bag = BagOfGoods.of_list []
let items_lst = [ items1; items2; items3 ]
let items_bag = BagOfGoods.of_list items_lst
let items_reverse_lst = [ items3; items2; items1 ]
let items_reverse_bag = BagOfGoods.of_list items_reverse_lst
let items_half1_lst = [ items1; items2 ]
let items_half1_bag = BagOfGoods.of_list items_half1_lst
let items_half2_lst = [ items3; items4 ]
let items_half2_bag = BagOfGoods.of_list items_half2_lst
let items1_lst = [ items1 ]
let items2_lst = [ items2 ]
let items1_bag = BagOfGoods.of_list items1_lst
let itemsmore_lst = [ items1; items2; items3; items4 ]
let itemsmore_bag = BagOfGoods.join items_bag items4_bag

let itemsplusquant_lst =
  [ items1plus1quant; items2plus1quant; items3plus1quant ]

let itemsplusprice_lst =
  [ items1plus1price; items2plus1price; items3plus1price ]

let bag_to_list_test msg out in1 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out in1

let bag_of_list_test msg out in1 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out in1

let bag_join_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list (BagOfGoods.join in1 in2))

let bag_join_many_test msg out in1 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list (BagOfGoods.join_many in1))

let bag_count_test msg out in1 =
  msg >:: fun _ -> assert_equal out (BagOfGoods.count_elems in1)

let bag_update_price_test msg out in1 in2 in3 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    BagOfGoods.(update_price in1 in2 in3 |> to_list)

let bag_update_quantity_test msg out in1 in2 in3 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    BagOfGoods.(update_quantity in1 in2 in3 |> to_list)

let bag_contains_test msg out in1 in2 =
  msg >:: fun _ -> assert_equal out BagOfGoods.(contains in1 in2)

let bag_to_string_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(fun x -> x) out in1

let bag_remove_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list (BagOfGoods.remove in1 in2))

let bag_remove_fail_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_raises
    (Failure
       "Item with the name provided does not exist in the bag. Please make \n\
       \    sure that the name of the product is spelled correctly with proper \
        capitalization.") (fun () -> BagOfGoods.remove in1 in2)

let bag_names_test msg out in1 =
  msg >:: fun _ -> assert_equal out BagOfGoods.(names in1)

let bag_total_quantity_test msg out in1 =
  msg >:: fun _ -> assert_equal out BagOfGoods.(total_quantity in1)

let bag_total_cost_test msg out in1 =
  msg >:: fun _ -> assert_equal out BagOfGoods.(total_cost in1)

let bag_map_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list (BagOfGoods.map in1 in2))

let add_one_to_quantity item = Item.change_quantity item 1
let add_one_to_price item = Item.change_price item 1

let bag_filter_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list (BagOfGoods.filter in1 in2))

let bag_intersection_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list BagOfGoods.(intersection in1 in2))

let bag_difference_test msg out in1 in2 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    (BagOfGoods.to_list BagOfGoods.(difference in1 in2))

let trial_item1 = Item.create "hello" 1000 10000
let trial_item2 = Item.create "hi" 500000 5000000
let trial_items_lst = [ trial_item1; trial_item2 ]
let trial_items_bag = BagOfGoods.of_list trial_items_lst

let string_bag =
  "|{name = cake; price = 1; quantity = 2}; {name = cookie; price = 1; \
   quantity = 3}; {name = pie; price = 5; quantity = 1}|"

(*Regular Bag Tests*)
let bagofgoods_tests =
  [
    (*to_string tests*)
    bag_to_string_test "to_string empty bag" "||"
      (BagOfGoods.to_string empty_bag);
    bag_to_string_test "to_string non-empty bag" string_bag
      (BagOfGoods.to_string items_bag);
    (*to_list tests*)
    bag_to_list_test "to_list empty" [] (BagOfGoods.to_list empty_bag);
    bag_to_list_test "to_list 1 item" items1_lst (BagOfGoods.to_list items1_bag);
    bag_to_list_test "to_list 3 items" items_lst (BagOfGoods.to_list items_bag);
    bag_to_list_test "to_list 3 items reverse" items_lst
      (BagOfGoods.to_list items_reverse_bag);
    bag_to_list_test "to_list trial"
      [ Item.create "hello" 5000 5000; Item.create "hi" 50000 500000 ]
      (BagOfGoods.to_list
         (BagOfGoods.of_list
            [ Item.create "hello" 5000 5000; Item.create "hi" 50000 500000 ]));
    bag_to_list_test "to_list trial pt2" trial_items_lst
      (BagOfGoods.to_list trial_items_bag);
    bag_to_list_test "to_list trial reverse"
      [ trial_item2; trial_item1 ]
      (BagOfGoods.to_list trial_items_bag);
    (*of_list tests*)
    bag_of_list_test "of_list empty" [] [];
    bag_of_list_test "of_list 1 item" [ items1 ]
      (BagOfGoods.of_list items1_lst |> BagOfGoods.to_list);
    bag_of_list_test "of_list 3 items pt1" [ items1; items2; items3 ]
      (BagOfGoods.of_list items_lst |> BagOfGoods.to_list);
    bag_of_list_test "of_list 3 items pt2" items_lst
      (BagOfGoods.of_list items_lst |> BagOfGoods.to_list);
    bag_of_list_test "of_list 3 items pt2 reverse" [ items3; items2; items1 ]
      (BagOfGoods.of_list items_lst |> BagOfGoods.to_list);
    bag_of_list_test "of_list trial pt1"
      [ Item.create "hello" 5000 5000; Item.create "hi" 50000 500000 ]
      (BagOfGoods.of_list
         [ Item.create "hello" 5000 5000; Item.create "hi" 50000 500000 ]
      |> BagOfGoods.to_list);
    bag_of_list_test "of_list trial pt2" trial_items_lst
      (BagOfGoods.of_list trial_items_lst |> BagOfGoods.to_list);
    bag_of_list_test "of_list trial pt3" trial_items_lst
      (BagOfGoods.to_list (BagOfGoods.of_list trial_items_lst));
    bag_of_list_test "of_list trial pt3 reverse"
      [ trial_item2; trial_item1 ]
      (BagOfGoods.to_list (BagOfGoods.of_list trial_items_lst));
    (*join tests*)
    bag_join_test "join both empty" [] empty_bag empty_bag;
    bag_join_test "join 1 empty pt1" items1_lst empty_bag items1_bag;
    bag_join_test "join 1 empty pt2" items1_lst items1_bag empty_bag;
    bag_join_test "join 1 empty pt3" items_lst empty_bag items_bag;
    bag_join_test "join 1 empty pt4" items_lst items_bag empty_bag;
    bag_join_test "join 1 empty pt4 reverse" items_reverse_lst items_bag
      empty_bag;
    bag_join_test "join both unempty pt1" items_half1_lst items1_bag items2_bag;
    bag_join_test "join both unempty pt2 (reverse)" items_half1_lst items2_bag
      items1_bag;
    bag_join_test "join both unempty pt3" itemsmore_lst items_bag items4_bag;
    bag_join_test "join both unempty pt4" itemsmore_lst items_half1_bag
      items_half2_bag;
    (*join_many tests*)
    bag_join_many_test "join_many empty" [] [ empty_bag ];
    bag_join_many_test "join_many 1 empty pt1" items1_lst
      [ empty_bag; items1_bag ];
    bag_join_many_test "join_many 1 empty pt2" items1_lst
      [ items1_bag; empty_bag ];
    bag_join_many_test "join_many 3 pt1" items_lst
      [ items1_bag; items2_bag; items3_bag ];
    bag_join_many_test "join_many 3 pt2" items_lst
      [ empty_bag; items1_bag; items2_bag; items3_bag ];
    bag_join_many_test "join_many 3 pt3" items_lst
      [ items3_bag; items1_bag; empty_bag; items2_bag ];
    bag_join_many_test "join_many 3 pt4" items_lst
      [
        empty_bag;
        items1_bag;
        items2_bag;
        items3_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
        empty_bag;
      ];
    bag_join_many_test "join_many 4 pt1" itemsmore_lst
      [ items_half1_bag; items_half2_bag ];
    bag_join_many_test "join_many 4 pt2" itemsmore_lst
      [ items_half2_bag; items_half1_bag ];
    bag_join_many_test "join_many 4 pt3" itemsmore_lst
      [ items1_bag; items2_bag; items3_bag; items4_bag ];
    bag_join_many_test "join_many 4 pt4" itemsmore_lst
      [ items4_bag; items2_bag; items1_bag; items3_bag ];
    (*count_elems tests*)
    bag_count_test "count empty" 0 empty_bag;
    bag_count_test "count 1 pt1" 1 items1_bag;
    bag_count_test "count 1 pt2" 1 (BagOfGoods.of_list items1_lst);
    bag_count_test "count 1 pt3" 1
      (BagOfGoods.of_list [ Item.create "hello" 50000 500000 ]);
    bag_count_test "count 2 pt1" 2 items_half1_bag;
    bag_count_test "count 2 pt2" 2 items_half2_bag;
    bag_count_test "count 3 pt1" 3 items_bag;
    bag_count_test "count 3 pt2" 3 items_reverse_bag;
    bag_count_test "count 4 pt1" 4 itemsmore_bag;
    bag_count_test "count 4 pt2" 4
      (BagOfGoods.join items_half1_bag items_half2_bag);
    bag_update_price_test "update_price: items1_bag"
      [ Item.create "cake" 3 2 ]
      items1_bag "cake" 2;
    bag_update_price_test "update_price: items_half1_bag"
      [ items1; Item.create "cookie" 6 3 ]
      items_half1_bag "cookie" 5;
    bag_update_price_test "update_price: itemsmore_bag"
      [ items1; items2; items4; Item.create "pie" 3 1 ]
      itemsmore_bag "pie" ~-2;
    bag_update_quantity_test "update_quantity: items2_bag"
      [ Item.create "cookie" 1 2 ]
      items2_bag "cookie" ~-1;
    bag_update_quantity_test "update_quantity: items_half2_bag"
      [ items4; Item.create "pie" 5 3 ]
      items_half2_bag "pie" 2;
    bag_update_quantity_test "update_quantity: itemsmore_bag"
      [ Item.create "milk" 4 6 ]
      itemsmore_bag "milk" 4;
    (*contains test*)
    bag_contains_test "contains: false empty bag" false empty_bag "cake";
    bag_contains_test "contains: true one item bag" true items1_bag "cake";
    bag_contains_test "contains: true multiple item bag" true items_bag "cake";
    bag_contains_test "contains: true multiple item bag" true items_bag "cookie";
    bag_contains_test "contains: true multiple item bag" true items_bag "pie";
    bag_contains_test "contains: false one item bag" false items1_bag "pie";
    bag_contains_test "contains: false multiple item bag" false items_bag "hi";
    bag_contains_test "contains: true multiple item bag joined" true
      BagOfGoods.(join items_half1_bag items_half2_bag)
      "pie";
    (*remove test*)
    bag_remove_test "remove: one item to empty" [] items1_bag "cake";
    bag_remove_test "remove: two items to one 1" items2_lst items_half1_bag
      "cake";
    bag_remove_test "remove: two items to one 2" items1_lst items_half1_bag
      "cookie";
    bag_remove_test "remove: bigger case" items_lst itemsmore_bag "milk";
    bag_remove_fail_test "remove: fail 1" () items1_bag "hi";
    bag_remove_fail_test "remove: fail 2" () items_half1_bag "pie";
    bag_remove_fail_test "remove: fail 3" () itemsmore_bag "ADSLFJADS";
    bag_remove_fail_test "remove: fail capitalization" () items_half1_bag "Cake";
    (*names test*)
    bag_names_test "names: empty" [] empty_bag;
    bag_names_test "names: 1 item" [ "cake" ] items1_bag;
    bag_names_test "names: multiple items" [ "cake"; "cookie"; "pie" ] items_bag;
    bag_names_test "names: multiple items reverse"
      [ "pie"; "cookie"; "cake" ]
      items_reverse_bag;
    (*total quantity test*)
    bag_total_quantity_test "quantity: empty" 0 empty_bag;
    bag_total_quantity_test "quantity: 1 item 1 quantity" 1
      BagOfGoods.(of_list [ Item.create "cake" 2 1 ]);
    bag_total_quantity_test "quantity: 1 item multiple quantity" 2 items1_bag;
    bag_total_quantity_test "quantity: multiple items" 6 items_bag;
    (*total cost test*)
    bag_total_cost_test "cost: empty" 0 empty_bag;
    bag_total_cost_test "cost: 1 item 1 quantity $1" 1
      BagOfGoods.(of_list [ Item.create "cake" 1 1 ]);
    bag_total_cost_test "cost: 1 item 1 quantity $1" 2
      BagOfGoods.(of_list [ Item.create "cake" 2 1 ]);
    bag_total_cost_test "cost: 1 item 1 quantity $1" 2
      BagOfGoods.(of_list [ Item.create "cake" 1 2 ]);
    bag_total_cost_test "cost: multiple items" 10 items_bag;
    bag_total_cost_test "cost: multiple items more" 18 itemsmore_bag;
    (*map test*)
    bag_map_test "map: 1 item add 1 quantity" items1plusquant_lst items1_bag
      add_one_to_quantity;
    bag_map_test "map: add 1 quantity" itemsplusquant_lst items_bag
      add_one_to_quantity;
    bag_map_test "map: add 1 price" itemsplusprice_lst items_bag
      add_one_to_price;
    (*filter test*)
    bag_filter_test "filter: 1 item price less than 5" [] items1_bag
      (fun item -> Item.get_price item > 4);
    bag_filter_test "filter: 1 item price more than 4" items1_lst items1_bag
      (fun item -> Item.get_price item < 4);
    bag_filter_test "filter: price less than 5" items3_lst items_bag
      (fun item -> Item.get_price item > 4);
    bag_filter_test "filter: quantity more than 1" items_half1_lst items_bag
      (fun item -> Item.get_price item < 2);
    (*intersection test*)
    bag_intersection_test "intersection: empty + empty" [] empty_bag empty_bag;
    bag_intersection_test "intersection: empty + nonempty" [] empty_bag
      items1_bag;
    bag_intersection_test "intersection: some overlap" items1_lst items1_bag
      itemsmore_bag;
    (*difference test*)
    bag_difference_test "difference: empty + empty" [] empty_bag empty_bag;
    bag_difference_test "difference: empty + nonempty" [] empty_bag items1_bag;
    bag_difference_test "difference: nonempty + empty" items1_lst items1_bag
      empty_bag;
    bag_difference_test "difference: some overlap" [ items4 ] itemsmore_bag
      items_bag;
  ]

(*Frequency Bag Test*)
let freqbag_oflist_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list Item.to_string) out in1

let freqbag_tolist_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list Item.to_string) out in1

let freqbag_sample_test msg out in1 = msg >:: fun _ -> assert_equal out in1

let freqbag_count_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:string_of_int out in1

let freqbag_update_price_test msg out in1 in2 in3 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    FrequencyBagGoods.(update_price in1 in2 in3 |> to_list)

let freqbag_update_quantity_test msg out in1 in2 in3 =
  msg >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list Item.to_string) out
    FrequencyBagGoods.(update_quantity in1 in2 in3 |> to_list)

let apple_item = Item.create "apple" 1 2
let apple_item_expensive = Item.create "apple" 5 4
let orange_item = Item.create "orange" 2 3
let grapes_item = Item.create "grapes" 3 1
let apple_item_big = Item.create "apple" 1 6
let fruits_lst = [ apple_item; orange_item; grapes_item ]
let fruits_bag = FrequencyBagGoods.of_list fruits_lst
let big_apple_lst = [ apple_item_big ]
let big_apple_bag = FrequencyBagGoods.of_list big_apple_lst
let apple_bag = FrequencyBagGoods.of_list [ apple_item; apple_item; apple_item ]
let empty_bag = FrequencyBagGoods.of_list []
let diff_apple_lst = [ apple_item; apple_item_expensive ]

let diff_apple_bag =
  FrequencyBagGoods.of_list [ apple_item; apple_item_expensive ]

let mixed_bag =
  FrequencyBagGoods.of_list
    [
      apple_item;
      apple_item;
      apple_item;
      orange_item;
      apple_item_expensive;
      grapes_item;
    ]

let fbagofgoods_tests =
  [
    (*of_list tests*)
    freqbag_oflist_test "freqbag of_list 3 distinct items"
      [ orange_item; apple_item; grapes_item ]
      (FrequencyBagGoods.of_list fruits_lst |> FrequencyBagGoods.to_list);
    freqbag_oflist_test "freqbag of_list 3 of the same item & same multiplicity"
      big_apple_lst
      (FrequencyBagGoods.of_list [ apple_item; apple_item; apple_item ]
      |> FrequencyBagGoods.to_list);
    freqbag_oflist_test "freqbag of_list on empty list" []
      (FrequencyBagGoods.of_list [] |> FrequencyBagGoods.to_list);
    freqbag_oflist_test "freqbag of_list on singleton list" [ apple_item ]
      (FrequencyBagGoods.of_list [ apple_item ] |> FrequencyBagGoods.to_list);
    freqbag_oflist_test
      "freqbag of_list where 2 items same name but diff prices"
      [ apple_item_expensive; apple_item ]
      (FrequencyBagGoods.of_list [ apple_item; apple_item_expensive ]
      |> FrequencyBagGoods.to_list);
    (*to_list tests*)
    freqbag_tolist_test "freqbag to_list on fruits_bag" fruits_lst
      (FrequencyBagGoods.to_list fruits_bag);
    freqbag_tolist_test "freqbag to_list on big_apple_bag" big_apple_lst
      (FrequencyBagGoods.to_list big_apple_bag);
    freqbag_tolist_test "freqbag to_list on empty bag" []
      (FrequencyBagGoods.to_list empty_bag);
    freqbag_tolist_test "freqbag to_list on diff_apple_bag" diff_apple_lst
      (FrequencyBagGoods.to_list diff_apple_bag);
    (*sample tests*)
    freqbag_sample_test "freqbag sample on fruits_bag" (Some orange_item)
      (FrequencyBagGoods.sample fruits_bag);
    freqbag_sample_test "freqbag sample on apple_bag" (Some apple_item_big)
      (FrequencyBagGoods.sample apple_bag);
    freqbag_sample_test "freqbag sample on empty bag" None
      (FrequencyBagGoods.sample empty_bag);
    freqbag_sample_test "freqbag sample on diff_apple_bag"
      (Some apple_item_expensive)
      (FrequencyBagGoods.sample diff_apple_bag);
    (*count_elems tests*)
    freqbag_count_test "freqbag count on empty bag" 0
      (FrequencyBagGoods.count_elems empty_bag);
    freqbag_count_test "freqbag count on fruits_bag" 6
      (FrequencyBagGoods.count_elems fruits_bag);
    freqbag_count_test "freqbag count on apple bag" 6
      (FrequencyBagGoods.count_elems apple_bag);
    freqbag_sample_test "freqbag count on diff_apple_bag" 6
      (FrequencyBagGoods.count_elems diff_apple_bag);
    freqbag_update_price_test "freqbag update_price: fruits_bag"
      [ Item.create "orange" 4 3; apple_item; grapes_item ]
      fruits_bag "orange" 2;
    freqbag_update_price_test "update_price: big_apple_bag"
      [ Item.create "apple" 6 6 ]
      big_apple_bag "apple" 5;
  ]

let baseball = Item.create "baseball" 10 100
let baseball_updated = Item.create "baseball" 10 90
let basketball = Item.create "basketball" 10 99
let tennis_racket = Item.create "tennis racket" 11 1000
let sport_bag_lst = [ baseball; basketball; tennis_racket ]
let fruits_normal_bag = BagOfGoods.of_list fruits_lst
let apple_item_a = Item.create "apple" 1 1
let pencil = Item.create "pencil" 1 1000000
let bookbag = Item.create "bookbag" 19 10000
let pen = Item.create "pen" 1 10000
let eraser = Item.create "eraser" 1 100000
let highlighter = Item.create "highlighter" 1 100000
let highlighter_u = Item.create "highlighter" 1 10000
let school_supplies_lst = [ pencil; bookbag; pen; eraser; highlighter ]
let school_supplies_bag = BagOfGoods.of_list school_supplies_lst

let updated_fruits_bag =
  BagOfGoods.of_list [ apple_item_a; orange_item; grapes_item ]

let updated_sports_bag =
  BagOfGoods.of_list [ baseball_updated; basketball; tennis_racket ]

let updated_school_supplies_bag =
  BagOfGoods.of_list [ pencil; bookbag; pen; eraser; highlighter_u ]

let updated_store1 = Store.of_list [ updated_fruits_bag ]
let store2_updated = Store.of_list [ updated_sports_bag; fruits_normal_bag ]
let sport_bag = BagOfGoods.of_list sport_bag_lst
let store1 = Store.of_list [ fruits_normal_bag ]
let store2_lst = [ sport_bag; fruits_normal_bag ]
let store2 = Store.of_list store2_lst
let store3_lst = [ sport_bag; fruits_normal_bag; school_supplies_bag ]
let store3_u_lst = [ sport_bag; fruits_normal_bag; updated_school_supplies_bag ]
let store3_u = Store.of_list store3_u_lst
let store3 = Store.of_list store3_lst

let store_count_products_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:pp_int out in1

let store_sell_goods_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list BagOfGoods.to_string) out in1

let store_all_products_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list BagOfGoods.to_string) out in1

let store_popular_goods_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list BagOfGoods.to_string) out in1

let store_of_list_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list BagOfGoods.to_string) out in1

let store_tests =
  [
    store_of_list_test "store_of_list for store of one bag"
      (Store.to_list store1)
      (Store.to_list (Store.of_list [ fruits_normal_bag ]));
    store_of_list_test "store_of_list for store of one bag updated"
      (Store.to_list updated_store1)
      (Store.to_list (Store.of_list [ updated_fruits_bag ]));
    store_of_list_test "store_of_list for store of two bags"
      (Store.to_list store2)
      (Store.to_list (Store.of_list store2_lst));
    store_of_list_test "store_of_list for store of three bags"
      (Store.to_list store3)
      (Store.to_list (Store.of_list store3_lst));
    store_of_list_test "store_of_list for updated store of two bags"
      (Store.to_list store2_updated)
      (Store.to_list (Store.of_list [ updated_sports_bag; fruits_normal_bag ]));
    store_of_list_test "store_of_list for updated store of three bags"
      (Store.to_list store3_u)
      (Store.to_list
         (Store.of_list
            [ sport_bag; fruits_normal_bag; updated_school_supplies_bag ]));
    store_count_products_test "store count_products for store of one bag" 6
      (Store.count_products store1);
    store_count_products_test "store_count_products for store of two bags" 1205
      (Store.count_products store2);
    store_count_products_test "store_count_products for store of 3 bags" 1221205
      (Store.count_products store3);
    store_sell_goods_test
      "store sell_goods_test for item apple in store of one bag"
      (Store.to_list updated_store1)
      (Store.to_list (Store.sell_goods 1 apple_item store1));
    store_sell_goods_test "store sell_goods_test for item baseball "
      (Store.to_list store2_updated)
      (Store.to_list (Store.sell_goods 10 baseball store2));
    store_sell_goods_test "store sell_goods_test for item highlighter "
      (Store.to_list store3_u)
      (Store.to_list (Store.sell_goods 90000 highlighter store3));
    store_count_products_test
      "store_count_products for updated store after selling apple" 5
      (Store.count_products updated_store1);
    store_count_products_test
      "store_count_products for updated store after selling baseball" 1195
      (Store.count_products store2_updated);
    store_count_products_test
      "store_count_products for updated store after selling highlighters"
      1131205
      (Store.count_products store3_u);
    store_all_products_test "store all products for store of one bag"
      (Store.to_list store1)
      (Store.to_list (Store.all_products store1));
    store_all_products_test
      "store all products for store of one bag after selling apple"
      (Store.to_list updated_store1)
      (Store.to_list (Store.all_products updated_store1));
    store_all_products_test "store all products for store of two bags"
      (Store.to_list store2)
      (Store.to_list (Store.all_products store2));
    store_all_products_test
      "store all products for store of two bags after selling baseballs"
      (Store.to_list store2_updated)
      (Store.to_list (Store.all_products store2_updated));
    store_all_products_test "store all products for store of three bags"
      (Store.to_list store3)
      (Store.to_list (Store.all_products store3));
    store_all_products_test
      "store all products for store of three bags after selling highlighters"
      (Store.to_list store3_u)
      (Store.to_list (Store.all_products store3_u));
    store_popular_goods_test
      "store popular goods, goods are popular if only 10 or less in stock"
      (Store.to_list store1)
      (Store.to_list (Store.popular_goods 10 store1));
    store_popular_goods_test
      "store popular goods, goods are popular if only 10 or less in stock"
      (Store.to_list store1)
      (Store.to_list (Store.popular_goods 10 store2));
    store_popular_goods_test
      "store popular goods, goods are popular if only 10 or less in stock"
      (Store.to_list store1)
      (Store.to_list (Store.popular_goods 10 store3));
  ]

let ngram_tests = []

let suite =
  "test suite for Grocery"
  >::: List.flatten
         [
           cmp_demo;
           items_tests;
           bagofgoods_tests;
           (* fbagofgoods_tests; *)
           store_tests;
         ]

let () = run_test_tt_main suite
