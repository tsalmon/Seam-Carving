(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999, 2004                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ppm.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

(* Manipulating images in portable format: PPM, PGM, and PBM.

PPM: portable pixmap (pixels (picture element) map).
PGM: portable greymap (grey scale map).
PBM: portable bitmap (binary digit map).

modified by Matthias Puech to be standalone.

*)

open Graphics

(* Reading PPM images. *)

type ppm_magic_number = | P1 | P2 | P3 | P4 | P5 | P6;;
 (* Magic numbers for PPM images.
    P1 and P4 indicate bitmaps (P1 is ascii encoding, P4 is raw encoding).
    P2 and P5 indicate greymaps, in raw or ascii encoding.
    P3 and P6 indicate pixmaps (P3 is ascii encoding, P6 is raw encoding).

    The library systematically saves images in raw form (which is more compact).
 *)

let magic_number_of_string = function
  | "P1" (* BITMAP, ASCII form *) -> P1
  | "P2" (* BITMAP, ASCII form *) -> P2
  | "P3" (* PIXMAP, ASCII form *) -> P3
  | "P4" (* BITMAP, RAW form *) -> P4
  | "P5" (* BITMAP, ASCII form *) -> P5
  | "P6" (* PIXMAP, RAW form *) -> P6
  | s -> invalid_arg ("Unknown magic number for PPM image: " ^ s);;

let read_ppm_magic_number ic = magic_number_of_string (input_line ic);;

(* Auxiliaries: skipping comments and reading numbers into strings. *)
let rec skip_comment ic =
 let rec r0 () =
  match input_char ic with
  | '#' -> r1 ()
  | ' ' -> r0 ()
  | '\n' -> r0 ()
  | c -> c
 and r1 () =
  match input_char ic with
  | '\n' -> r0 ()
  | _ -> r1 () in
 r0 ();;

(* Read a sequence of digits eventually followed by a single space. *)
let read_int_accu accu ic =
 let rec read accu =
 match input_char ic with
 | '0' .. '9' as c -> read1 (10 * accu + int_of_char c - 48)
 | ' ' -> read accu
 | '\n' -> read accu
 | _ -> invalid_arg "read_int"

 and read1 accu =
 match input_char ic with
 | '0' .. '9' as c -> read1 (10 * accu + int_of_char c - 48)
 | _ -> accu in
 read accu;;

let read_int ic = read_int_accu 0 ic;;

let read_dims c ic =
 let cols = read_int_accu (int_of_char c - 48) ic in
 let lines = read_int ic in
 cols, lines;;

let read_max ic = read_int ic;;

let read_ppm_header ic =
 (* Reads something like
    P6
    # CREATOR: XV Version 3.10  Rev: 12/16/94
    256 162
    255
 *)
 let mn = read_ppm_magic_number ic in
 let char = skip_comment ic in
 let c, l = read_dims char ic in
 mn, l, c;;

(* Reading pixmaps. *)
let read_raw_pixel24 ic =
 let r = input_byte ic in
 let g = input_byte ic in
 let b = input_byte ic in
 rgb r g b

let read_ascii_pixel24 ic =
 let r = read_int ic in
 let g = read_int ic in
 let b = read_int ic in
 rgb r g b

let read_raw_ppm_ic ic l c _max =
 let img = Array.init l (fun _ -> Array.make c red) in
 for i = 0 to l - 1 do
  for j = 0 to c - 1 do
   Array.set img.(i) j (read_raw_pixel24 ic)
  done
 done;
 img;;

let read_ascii_ppm_ic ic l c _max =
 let img = Array.init l (fun _ -> Array.make c red) in
 for i = 0 to l - 1 do
  for j = 0 to c - 1 do
   Array.set img.(i) j (read_ascii_pixel24 ic)
  done
 done;
 img;;

(* Reading greymaps. *)
let read_raw_grey x =
  let c = input_byte x in
  rgb c c c
;;

let read_ascii_grey x =
  let c = read_int x in
  rgb c c c
;;

let max_byte = 255;;

let read_raw_gen_ic read_pixel ic l c max =
  let img = Array.init l (fun _ -> Array.make c red) in
  for i = 0 to l - 1 do
    for j = 0 to c - 1 do
      Array.set img.(i) j (read_pixel ic * max_byte / max)
    done
  done;
  img;;

let read_raw_pgm_ic ic = read_raw_gen_ic read_raw_grey ic;;

let read_ascii_pgm_ic ic = read_raw_gen_ic read_ascii_grey ic;;

(* Reading bitmaps. *)
let read_raw_pbm_ic ic l c =
  let img = Array.init l (fun _ -> Array.make c red) in
  for i = 0 to l - 1 do
    let rec loop j bn byte =
      if j = c then () else
        if bn = 8 then loop j 0 (input_byte ic) else
          let color =
            match byte land 0x80 with
            | 0 -> white
            | _ -> black in
          Array.set img.(i) j color;
          let new_byte = byte lsl 1 in
          loop (j + 1) (bn + 1) new_byte
    in
    loop 0 0 (input_byte ic)
  done;
 img;;

let rec read_ascii_bit ic =
    match input_char ic with
    | '0' -> white
    | ' ' -> read_ascii_bit ic
    | '\n' -> read_ascii_bit ic
    | _ -> black;;

let read_ascii_pbm_ic ic l c = read_raw_gen_ic read_ascii_bit ic l c max_byte;;

let rec read_ppm_ic ic =
 let mn, l, c = read_ppm_header ic in
 let img =
   match mn with
   | P1 -> read_ascii_pbm_ic ic l c
   | P4 -> read_raw_pbm_ic ic l c
   | P2 | P3 | P5 | P6 ->
       let max = read_max ic in
       match mn with
       | P2 -> read_ascii_pgm_ic ic l c max
       | P3 -> read_ascii_ppm_ic ic l c max
       | P5 -> read_raw_pgm_ic ic l c max
       | _ -> read_raw_ppm_ic ic l c max in
 img;;

let load s : color array array =
 let ic = open_in_bin s in
 try
  let img = read_ppm_ic ic in
  close_in ic;
  img
 with End_of_file ->
  close_in ic; invalid_arg "read_ppm: premature end of file";;

let save s img = 
  let oc = open_out_bin s in
  output_string oc "P6\n";
  let v = Array.length img in
  if v = 0 then failwith "Ppm.save:empty array";
  let h = Array.length img.(0) in 
  output_string oc ((string_of_int h)^" "^(string_of_int v)^"\n255\n");
  for j = 0 to v - 1 do
    for i = 0 to h - 1 do
      let c = img.(j).(i) in
      let (r,g,b) =  (c asr 16, (c asr 8) land 255, c land 255) in
      output_byte oc r;
      output_byte oc g; 
      output_byte oc b;	
    done
  done;
  close_out oc
;;
  
