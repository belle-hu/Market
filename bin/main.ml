open Grocery

(*open Store*)
open Item

let () =
  print_endline "\n\nWelcome to Grocery. \n";
  let f = read_line () in
  print_endline f;
  print_endline "Please enter the "
