open OUnit2
open Grocery

(*open Chat open Ngrams open Model open Bag *)
open Store
open Bagofgoods

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
 let items_tests = [
  (**create tests*)
  (**get_name tests*)
  (**get_price tests*)
  (**get_quantity tests*)
  (**change_price tests*)
  (**change_quantity tests*)
]
let bagofgoods_tests = [
  (**to_list tests*)
  (**of_list tests*)
  (**sample tests*)
  (**count_elems tests*)
]
let store_tests = []
let ngram_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ cmp_demo; items_tests; bagofgoods_tests; store_tests]

let () = run_test_tt_main suite
