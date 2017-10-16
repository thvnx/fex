open Printf
open Fex

let horner xx =
  let coef = [
      2.373046875000000e-01; -4.192382812500000e+00;
      3.467285156250000e+01; -1.781982421875000e+02;
      6.370019531250000e+02; -1.679423828125000e+03;
      3.378095703125000e+03; -5.288271484375000e+03;
      6.511538085937500e+03; -6.327524414062500e+03;
      4.836465820312500e+03; -2.877295898437500e+03;
      1.306113281250000e+03; -4.373437500000000e+02;
      1.018750000000000e+02; -1.475000000000000e+01;
      1.000000000000000e+00] in
  let coef = List.rev coef in

  let rec eval x acc c =
    match c with
    | h :: t ->
       eval x (acc *. x +. h) t
    | []     ->
       acc
  in
  fprintf stdout "%e\n" (eval xx (List.hd coef) (List.tl coef))
;;

let hornerE xx =
    let coef = Fex.float_to_fex_list [
      2.373046875000000e-01; -4.192382812500000e+00;
      3.467285156250000e+01; -1.781982421875000e+02;
      6.370019531250000e+02; -1.679423828125000e+03;
      3.378095703125000e+03; -5.288271484375000e+03;
      6.511538085937500e+03; -6.327524414062500e+03;
      4.836465820312500e+03; -2.877295898437500e+03;
      1.306113281250000e+03; -4.373437500000000e+02;
      1.018750000000000e+02; -1.475000000000000e+01;
      1.000000000000000e+00] in
  let coef = List.rev coef in

  let rec eval x acc c =
    match c with
    | h :: t ->
       eval x (acc *.. x +.. h) t
    | []     ->
       acc
  in
  print_endline (eval xx (List.hd coef) (List.tl coef))#to_string
;;



let _ =
  let x = float_of_string (Array.get Sys.argv 1) in
  horner x;
  hornerE (Fex.float_to_fex x)
