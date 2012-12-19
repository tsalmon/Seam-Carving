#load "graphics.cma";;
#use "topfind";;
#require "camlimages.all_formats";;
#require "camlimages.graphics";;


open Graphics;;

let height m =
  Array.length m
;;

let width m =
  Array.length (m.(0))
;;

let load filename = 
	Graphic_image.array_of_image (Images.load filename [] )
;;

let save filename m = 
	Images.save filename None [Images.Save_Quality 255] 
	(Images.Rgb24(Graphic_image.image_of (Graphics.make_image m)))
;;

let draw image =
  Graphics.draw_image (Graphics.make_image image ) 0 0
;;

let blank h w =
  Array.make_matrix h w (Graphics.rgb 0 0 0)
;;


let show image =
  open_graph (" " ^ (string_of_int(width image)) ^ "x" ^ (string_of_int(height image)));
  Graphics.set_window_title "TP 7: exercice 1";
  draw image;
  ignore(Graphics.read_key());
  close_graph()
;;


show (  blank 200 200);;
