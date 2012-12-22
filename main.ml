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

let wait milli = 
  let sec = milli /. 1000. in
  let tml = Unix.gettimeofday () in
  while Unix.gettimeofday () -. tml < sec do () done
;;

(* Main *****************************************************************)

let img = (Ppm.load Sys.argv.(1));;

Graphics.open_graph (" " ^ string_of_int(width img) ^ "x" ^ string_of_int(height img) ^ "") ;;
for i = 0 to 100 do
  let trace = trace_path (lum_matrix img) in
    let pdt = point_de_depart 0 0 0 trace in
      animation trace  0 pdt img ;
      Graphics.draw_image (Graphics.make_image img) 0 0;
      suppression trace 0 pdt img;
      Graphics.draw_image (Graphics.make_image img) 0 0;
done;;
ignore(Graphics.read_key());;
Graphics.close_graph() ;;

(*
let tab = 
  [|
    [| 1; 2; 3; 12|];
    [| 4; 5; 0; 11|];
    [| 7; 8; 9; 10|];
  |]
;;

print_int( min_tab_coord tab 2 0);;
print_string("\n");;*)
(* note attention a lum_rgb et il faut faire les methodes rgb 4 et 8 *)
