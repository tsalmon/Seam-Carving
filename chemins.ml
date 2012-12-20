let min a b = if( a > b) then b else a ;;

let print_tab t = 
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      print_string(string_of_int(t.(i).(j)) ^ " ")
    done;
    print_string("\n")
  done
;;

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
  if( (j > 0) && (j < (Array.length t.(0))-1)) then
    let b = t.(i).(j-1) in
    let c = t.(i).(j+1) in 
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


let algo t  = 
  let resultat = Array.make_matrix (Array.length t) (Array.length t.(0)) 0 in
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      if(i = 0) then
	resultat.(i).(j) <- t.(i).(j)
      else
	resultat.(i).(j) <- t.(i).(j) + min_tab resultat (i-1) j 
    done
  done;
  resultat;
;;

let rec parcours t x y =
  if( x = 0 ) then
    "tab[" ^ string_of_int(x) ^ "][" ^ string_of_int(y) ^ "] : " ^ (string_of_int t.(x).(y)) ^ "\n"
  else
    "tab[" ^ string_of_int(x) ^ "][" ^ string_of_int(y) ^ "] : " ^ (string_of_int t.(x).(y)) ^ "\n" ^ (parcours t (x-1) (min_tab_coord t (x-1) y))
;;
