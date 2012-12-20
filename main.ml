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

(* Main ********************************************************************
Draw the energy of the image
let img = Ppm.load Sys.argv.(1);;
Graphics.open_graph (" " ^ (str_largeur img) ^ "x" ^ (str_hauteur img) ^ "");;

Graphics.draw_image (Graphics.make_image (lum_matrix img)) 0 0;;

ignore(Graphics.read_key());
Graphics.close_graph() ;;
*)

let tab = 
  [|
    [| 1; 4; 3; 5; 2 |];
    [| 3; 2; 5; 2; 3 |];
    [| 5; 2; 4; 2; 1 |];
  |]
;;

print_tab tab;;
print_string("\n");;
let tab = algo tab ;;
print_tab tab;;
print_string("\n");;
print_string(parcours tab 2 1);;
