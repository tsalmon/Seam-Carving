open Graphics;;
let soi x = string_of_int(x);;
let min a b = if( a > b) then b else a ;;

let print_tab_1 t = 
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      let (a, b, c) = t.(i).(j) in
      print_string("( " ^ soi(a) ^ ", " ^ soi(b) ^ ", " ^ soi(c) ^ "); ")
    done;
    print_string("\n")
  done;
  print_string("\n")
;;

let print_tab_2 t = 
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      print_string(soi(t.(i).(j)) ^ " ")
    done;
    print_string("\n")
  done;
  print_string("\n")
;;

(* 
   Call by : 
   
*)
let min_tab t i j =
  let a = t.(i).(j) in
  if( (j > 0) && (j < (Array.length t.(0))-1)) then
    let b = t.(i).(j-1) in
    let c = t.(i).(j+1) in 
    if( (min a b) = (min a c)) then
      a
    else
      min b c
  else
    if(j = 0) then
      min a t.(i).(j+1)
    else
      min a t.(i).(j-1)
;;

let min_tab_coord t i j =
  let a = t.(i).(j) in
  if ((j > 0) && (j < (Array.length t.(0)-1))) then
    let b = t.(i).(j-1) in
    let c = t.(i).(j+1) in 
    if( (min a b) = (min a c)) then
      j
    else
      if(b > c) then
	j+1
      else
	j-1
  else
    if(j = 0) then
      if(t.(i).(j+1) > t.(i).(j)) then
	j
      else
	j+1
    else
      if(t.(i).(j-1) > t.(i).(j)) then
	j
      else
	j-1
;;

(*
 * Call by main
 * Use : allow energy of neighboors
 *)
let trace_path t =
  let resultat = Array.make_matrix (Array.length t) (Array.length t.(0)) 0 in
  for i = 0 to (Array.length t-1) do
    for j = 0 to (Array.length t.(0)-1) do
      let a = Array.length t - 1 - i in
      let b = Array.length t.(0) - 1 - j in
      if(a = Array.length t - 1) then
	resultat.(a).(b) <- t.(a).(b)
      else
        resultat.(a).(b) <- t.(a).(b)+ min_tab resultat (a+1) (b)
    done
  done;
  resultat
;;

let color_to_rgb c = (c asr 16,  (c asr 8) land 255, c land 255);;

let rec point_de_depart s x y t =
  if (y < Array.length t.(0)) then
    if(t.(x).(s) >= t.(x).(y)) then
      point_de_depart y x (y+1) t
    else
      point_de_depart s x (y+1) t
  else
    s
;;

(* 
 * Call by : suppression
 * Use : decal pixel to the right
 * Contexte : pixel deleting
 *)
let rec decale x y img =
  if ( y+1 < Array.length img.(0) ) then
    begin
      img.(x).(y) <- img.(x).(y+1);
      decale x (y+1) img;
    end
  else
    img.(x).(y) <- Graphics.rgb 255 255 255
;;
(* 
 * Call by : main
 * Use : delete the min way
 *)
let rec suppression t x y img =
  decale x y img ;
  if (x+1 < (Array.length t)) then
    suppression t (x+1) (min_tab_coord t (x+1) y ) img
  else
    ()
;;

(*
 * Call by : main
 * Use : color the 'best way' in red
 *)
let rec animation t x y img =
  img.(x).(y) <- Graphics.rgb 250 0 0; 
  if (x+1 < (Array.length t)) then
    animation t (x+1) (min_tab_coord t (x+1) y ) img
  else
    ()
;;
