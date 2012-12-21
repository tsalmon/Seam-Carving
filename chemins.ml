open Graphics;;
let soi x = string_of_int(x);;
let min a b = if( a > b) then b else a ;;

let print_tab t = 
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      print_string(string_of_int(t.(i).(j)) ^ " ")
    done;
    print_string("\n")
  done
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
  if ((j > 0) && (j < (Array.length t.(0))-1)) then
    let b = t.(i).(j-1) in
    let c = t.(i).(j+1) in 
    print_string(soi(b) ^ ", " ^ soi(a) ^ ", " ^ soi(c) ^ "\n");
    if( (min a b) = (min a c)) then
      j
    else
      if(t.(i).(j+1) > t.(i).(j-1)) then
	j-1
      else
	j+1
  else
    if(j = 0) then
      j+1
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
  print_string( "t[" ^ soi(x) ^ "][" ^ soi(s) ^ "] = " ^ soi(t.(x).(s)) ^ "\n");
  if (y < Array.length t.(0)) then
    if(t.(x).(s) > t.(x).(y)) then
      point_de_depart y x (y+1) t
    else
      point_de_depart s x (y+1) t
  else
    s
;;

(*
 * Call by : main
 * Use : browse the 'best way'
 *)
let rec browser t x y img =
  img.(x).(y) <- Graphics.rgb 250 0 0; 
  if (x+1 < (Array.length t)) then
    browser t (x+1) (min_tab_coord t (x+1) y ) img
  else
    ()
;;
