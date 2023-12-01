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

let item1 = Item.create "item1" 3 10
let item2 = Item.create "item2" 10 100

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
    (*get_price tests*)
    item_get_price_test "Get the price of an item1" 3
      (Item.create "banana" 3 10);
    item_get_price_test "Get the price of an item2" 1 (Item.create "peach" 1 30);
    item_get_price_test "Get the price of an item3" 10 (Item.create "milk" 10 5);
    (*get_quantity tests*)
    item_get_quantity_test "Get the quantity of an item1" 10
      (Item.create "cookie" 50 10);
    item_get_quantity_test "Get the quantity of an item2" 8
      (Item.create "some item" 100 8);
    item_get_quantity_test "Get the quantity of an item3" 497
      (Item.create "potato" 5 497);
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
  ]

(** Regular Bag of Goods Tests*)
let items1 = Item.create "cake" 1 2

let items1more = Item.create "cake" 5 4
let items1lots = Item.create "cake" 1 6
let items2 = Item.create "cookie" 1 3
let items2_bag = BagOfGoods.of_list [ items2 ]
let items3 = Item.create "pie" 5 1
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
let items1_bag = BagOfGoods.of_list items1_lst
let itemsmore_lst = [ items1; items2; items3; items4 ]
let itemsmore_bag = BagOfGoods.join items_bag items4_bag

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

let trial_item1 = Item.create "hello" 1000 10000
let trial_item2 = Item.create "hi" 500000 5000000
let trial_items_lst = [ trial_item1; trial_item2 ]
let trial_items_bag = BagOfGoods.of_list trial_items_lst

(*Frequency Bag Tests*)
let bagofgoods_tests =
  [
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
  ]

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
    (*freqbag_tolist_test "freqbag to_list on fruits_bag" fruits_lst
      (FrequencyBagGoods.to_list fruits_bag);*)
    freqbag_tolist_test "freqbag to_list on big_apple_bag" big_apple_lst
      (FrequencyBagGoods.to_list big_apple_bag);
    freqbag_tolist_test "freqbag to_list on empty bag" []
      (FrequencyBagGoods.to_list empty_bag);
    (*freqbag_tolist_test "freqbag to_list on diff_apple_bag" diff_apple_lst
      (FrequencyBagGoods.to_list diff_apple_bag);*)
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
    (*freqbag_update_price_test "freqbag update_price: fruits_bag" [ Item.create
      "orange" 4 3; apple_item; grapes_item ] fruits_bag "orange" 2;
      freqbag_update_price_test "update_price: big_apple_bag" [ Item.create
      "apple" 6 6 ] big_apple_bag "apple" 5;*)
  ]


  let baseball = Item.create "baseball" 10 100 

  let baseball_updated = Item.create "baseball" 10 90
  let basketball = Item.create "basketball" 10 99 
  let tennis_racket = Item.create "tennis racket" 11 1000 
  let sport_bag_lst = [baseball; basketball; tennis_racket]

  let fruits_normal_bag = BagOfGoods.of_list fruits_lst 
  let apple_item_a = Item.create "apple" 1 1 

  let updated_fruits_bag = BagOfGoods.of_list [grapes_item; orange_item; apple_item_a]

  let updated_sports_bag = BagOfGoods.of_list [baseball_updated; basketball; tennis_racket]

  let updated_store1 = Store.of_list [updated_fruits_bag]
  
  let store2_updated = Store.of_list [updated_sports_bag; fruits_normal_bag]
  let sport_bag = BagOfGoods.of_list sport_bag_lst
  let store1 = Store.of_list [fruits_normal_bag]
  let store2_lst = [sport_bag; fruits_normal_bag]
  let store2 = Store.of_list store2_lst 
  let store_count_products_test msg out in1 = 
    msg >:: fun _ -> assert_equal ~printer:pp_int out in1
  let store_sell_goods_test msg out in1 = 
    msg >:: fun _ -> assert_equal ~printer: (pp_list BagOfGoods.to_string) out in1


let store_tests = [

    store_count_products_test "store count_products for store of one bag" 
    6 (Store.count_products store1);

    store_count_products_test "store_count_products for store of two bags" 1205 
    (Store.count_products store2);

    store_sell_goods_test "store sell_goods_test for item apple in store of one bag" 
    (Store.to_list updated_store1) (Store.to_list (Store.sell_goods 1 apple_item store1));

    store_sell_goods_test "store sell_goods_test for item baseball " 
    (Store.to_list store2_updated) (Store.to_list (Store.sell_goods 10 baseball store2));
    
]
let ngram_tests = []

let suite =
  "test suite for Grocery"
  >::: List.flatten
         [
           cmp_demo;
           items_tests;
           bagofgoods_tests;
           fbagofgoods_tests;
           store_tests;
         ]

let () = run_test_tt_main suite
