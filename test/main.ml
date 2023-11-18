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

let items_tests =
  [
    (*create tests*)
    item_create_test "Create item: apple w/ price 1 & quantity 2"
      "{name = apple; price = 1; quantity = 2}"
      (Item.create "apple" 1 2 |> Item.to_string);
    (*get_name tests*)
    (*get_price tests*)
    (*get_quantity tests*)
    (*change_price tests*)
    (*change_quantity tests*)
  ]

let freqbag_oflist_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list Item.to_string) out in1

let freqbag_tolist_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:(pp_list Item.to_string) out in1

let freqbag_sample_test msg out in1 = msg >:: fun _ -> assert_equal out in1

let freqbag_count_test msg out in1 =
  msg >:: fun _ -> assert_equal ~printer:string_of_int out in1

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

let bagofgoods_tests =
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
  ]

let store_tests = []
let ngram_tests = []

let suite =
  "test suite for Grocery"
  >::: List.flatten [ cmp_demo; items_tests; bagofgoods_tests; store_tests ]

let () = run_test_tt_main suite
