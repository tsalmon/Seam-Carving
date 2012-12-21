(*ocamlc graphics.cma ppm.cmo distance.ml main.ml -o projet && ./projet test-1.ppm*)
open Distance;;
open Graphics;;
open Chemins;;
(* Largeur image sous forme textuelle*)
let str_largeur img =
  string_of_int(Array.length img.(0));;
(* Hauteur image sous forme textuelle*)
let str_hauteur img = 
  string_of_int(Array.length img);;

(* Main *******************************************************************)

let img = (Ppm.load Sys.argv.(1));;

Graphics.open_graph (" " ^ string_of_int(width img) ^ "x" ^ string_of_int(height img) ^ "") ;;

browser ( trace_path (lum_matrix img) ) 0 ( point_de_depart 0 0 0 (img) ) img ;;

Graphics.draw_image (Graphics.make_image img) 0 0;;

ignore(Graphics.read_key());;
Graphics.close_graph() ;;

(*
let tab = 
  [|
    [| 1; 4; 3; 5; 2 |]
    [| 3; 2; 5; 2; 3 |];
    [| 5; 2; 4; 2; 1 |];
  |]
;;

print_tab img;;
print_string('\n');;
let tab = trace_path img ;;
print_tab img;;
print_string('\n');;
Graphics.draw_image(browser img ((height img)-1) 1);;*)

