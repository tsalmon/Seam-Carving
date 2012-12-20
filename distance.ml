let print_tab t = 
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t.(0))-1 do
      print_string(string_of_int(t.(i).(j)) ^ " ")
    done;
    print_string("\n")
  done
;;

let min a b = if( a > b) then b else a ;;

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

let color_to_rgb c = (c asr 16,  (c asr 8) land 255, c land 255);;

let soi x = 
  string_of_int (x)
;;


(* Largeur image sous forme numerique*)
let largeur img =
  Array.length img.(0)
;;
(* Hauteur image sous forme numerique*)
let hauteur img = 
  Array.length img
;;


(* calculation of the formula *)
let lum_rgb i j m=
  let iof x = int_of_float(x) in
  let foi y = float_of_int(y) in
  let (r, g, b) = color_to_rgb m.(i).(j) in 
  iof((0.2126 *. foi(r)) +. (0.7152 *. foi(g)) +. (0.0722 *. foi(b)))
;;

(************************************************************
 * give in back of theses 8 functions the result of lum_rgb *
 * between p and his neighbors                              *
 ************************************************************)
let distance_lum_haut i j m =
  if((i+1) >= (hauteur m)-1) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) j m)) 	
;;

let distance_lum_haut_gauche i j m =
  if(((i+1) >= (hauteur m)-1) || (j-1 < 0)) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) (j-1) m)) 	
;;

let distance_lum_gauche i j m =
  if((j-1) < 0 ) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb i (j-1) m))
;;

let distance_lum_bas_gauche i j m =
  if((i-1 < 0 ) || ( j - 1 < 0 )) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) (j-1) m))
;;

let distance_lum_bas i j m =
  if((i-1) < 0) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) j m))
;;

let distance_lum_bas_droite i j m =
  if((i-1 < 0) || (j+1 >= (largeur m)-1)) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) (j+1) m))
;;


let distance_lum_droite i j m =
  if((j+1) >= (largeur m)-1) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb i (j+1) m))
;;

let distance_lum_haut_droite i j m =
  if((j+1 >= (largeur m)-1) || (i+1 >= (hauteur m)-1))then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) (j+1) m))
;;

let is_noneg x =
  if(x !=  -1) then
    1
  else
    0
;;

(*
 * Try to know for h b d and g if this parameters are empty or not  
 *)
let nb_voisins h b d g =
  (is_noneg h) + (is_noneg b) + (is_noneg d) + (is_noneg g)
;;

(* 
   methode de calcul de la distance de luminosite pour 4 Voisins
   Appel: matrice_energie
*)
let distance_lum_4 i j m =
  let haut = distance_lum_haut i j m in
  let bas = distance_lum_bas i j m in
  let droite = distance_lum_droite i j m in
  let gauche = distance_lum_gauche i j m in
  (haut + bas + droite + gauche)/(nb_voisins haut bas droite gauche)
;;

(*
 * PS : i m not satisfied of this function,
 * will try to do better
 *)
let distance_lum_8 i j m =
  let haut = distance_lum_haut i j m in
  let haut_gauche = distance_lum_haut_gauche i j m in
  let gauche = distance_lum_gauche i j m in
  let bas_gauche = distance_lum_bas_gauche i j m in
  let bas = distance_lum_bas i j m in
  let bas_droite = distance_lum_bas_droite i j m in
  let droite = distance_lum_droite i j m in
  let haut_droite = distance_lum_haut_droite i j m in
  let a = haut_gauche + haut_droite + bas_droite + bas_gauche in
  let b = nb_voisins haut_gauche haut_droite bas_droite bas_gauche in
  let c = haut + bas + droite + gauche in
  let d = nb_voisins haut  bas droite  gauche in
  (a + c)/(b + d)
;;

(* parcours des pixels de l'image et application d'une fonction f*)
let matrice_luminosite m = 
  let aux = Array.make_matrix (hauteur m) (largeur m) m.(0).(0) in
  for i = 0 to (hauteur m)-1 do
    for j = 0 to (largeur m)-1 do
      let  d = (distance_lum_4 i j m) in
      aux.(i).(j) <- Graphics.rgb d d d;
    done;
  done;
  aux
;;
