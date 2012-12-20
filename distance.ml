let width img =
  Array.length img.(0)
;;

let height img = 
  Array.length img
;;

let color_to_rgb c = (c asr 16,  (c asr 8) land 255, c land 255);;

let soi x = 
  string_of_int (x)
;;

(*----------------------------------------------------------*
 *                  DISTANCE LUMINOSITY                     * 
 * Abbrevation:                                             *
 * lum : luminosity                                         *
 * dist: distance                                           *
 * neg : negative                                           *
 * nb  : number                                             *
 * neig: neighbors                                          *
 *----------------------------------------------------------*)

(*   
 * Call by: dist_lum_top, ..., dist_lum_top_right
 * Context: pixel p and p' are not out of the matrix
 * Use: calculation of the formula 
 *)
 
let lum_rgb i j m =
  let iof x = int_of_float(x) in
  let foi y = float_of_int(y) in
  let (r, g, b) = color_to_rgb m.(i).(j) in 
  iof((0.2126 *. foi(r)) +. (0.7152 *. foi(g)) +. (0.0722 *. foi(b)))
;;

(*
 * For all this function to 'dist_lum_top' to 'dist_lum_top_right':
 * Call by: dist_lum_4, dist_lum_8
 * Use :  give the dist between a pixel p and p'
 *)
let dist_lum_top i j m =
  if((i+1) >= (height m)-1) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) j m)) 	
;;

let dist_lum_top_left i j m =
  if(((i+1) >= (height m)-1) || (j-1 < 0)) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) (j-1) m)) 	
;;

let dist_lum_left i j m =
  if((j-1) < 0 ) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb i (j-1) m))
;;

let dist_lum_bottom_left i j m =
  if((i-1 < 0 ) || ( j - 1 < 0 )) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) (j-1) m))
;;

let dist_lum_bottom i j m =
  if((i-1) < 0) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) j m))
;;

let dist_lum_bottom_right i j m =
  if((i-1 < 0) || (j+1 >= (width m)-1)) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i-1) (j+1) m))
;;

let dist_lum_right i j m =
  if((j+1) >= (width m)-1) then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb i (j+1) m))
;;

let dist_lum_top_right i j m =
  if((j+1 >= (width m)-1) || (i+1 >= (height m)-1))then
    -1
  else
    abs((lum_rgb i j m) - (lum_rgb (i+1) (j+1) m))

(*
  Call by : nb_neig
  Use     : check if a number is negative
*)
let is_not_neg x =
  if(x !=  -1) then
    1
  else
    0
;;

(*
 *  Call by : dist_lum_4, dist_lum_8
 *  Use     : Try to know for h b d and g if this parameters are empty or not  
 *)
let nb_neig h b d g =
  (is_not_neg h) + (is_not_neg b) + (is_not_neg d) + (is_not_neg g)
;;

(* 
   Call by: lum_matrix
   Use : Calcul the energy between a pixel and his 4 neig
*)
let dist_lum_4 i j m =
  let top = dist_lum_top i j m in
  let bottom = dist_lum_bottom i j m in
  let right = dist_lum_right i j m in
  let left = dist_lum_left i j m in
  (top + bottom + right + left)/(nb_neig top bottom right left)
;;

(*
 *  PS : i m not satisfied of this function,
 *         will try to do better
 *  Call by : lum_matrix
 *  Use: Calcul the energy between a pixel and his 8 neig
 *)
let dist_lum_8 i j m =
  let top = dist_lum_top i j m in
  let top_left = dist_lum_top_left i j m in
  let left = dist_lum_left i j m in
  let bottom_left = dist_lum_bottom_left i j m in
  let bottom = dist_lum_bottom i j m in
  let bottom_right = dist_lum_bottom_right i j m in
  let right = dist_lum_right i j m in
  let top_right = dist_lum_top_right i j m in
  let a = top_left + top_right + bottom_right + bottom_left in
  let b = nb_neig top_left top_right bottom_right bottom_left in
  let c = top + bottom + right + left in
  let d = nb_neig top  bottom right  left in
  (a + c)/(b + d)
;;

(*
  Call by: main
  Use: give the energy of each pixel
*)
let lum_matrix m = 
  let aux = Array.make_matrix (height m) (width m) m.(0).(0) in
  for i = 0 to (height m)-1 do
    for j = 0 to (width m)-1 do
      let  d = (dist_lum_4 i j m) in
      aux.(i).(j) <- Graphics.rgb d d d;
    done;
  done;
  aux
;;
