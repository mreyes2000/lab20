type imageType = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold (img : imageType) (t : float) : imageType =
  let inner_threshold = List.map (fun cond -> if cond <= t then 0. else 1.) in
  List.map inner_threshold img;;
       
(* dither max image -- dithered image *)
let dither (img : imageType) : imageType =
  let inner_dither= List.map (fun cond -> if cond > Random.float 1. then 1. else 0.) in 
  List.map inner_dither img;;

let depict (img : imageType) : unit =
  Graphics.open_graph ""; 
  Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in 
  Graphics.resize_window x y;
  let depict_pix (l, w : size) (v : float)  : unit  = 
    let lvl = int_of_float (255. *. (1. -. v)) in 
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
    plot w (y - l) in
  List.iteri (fun l_iter-> List.iteri (fun w_iter -> depict_pix (l_iter, w_iter))) img;
  Unix.sleep 2; 
  Graphics.close_graph () ;;
    
let mona = Monalisa.image ;;
depict mona ;;
    
let mona_threshold = threshold mona 0.75 ;;
depict mona_threshold ;;
      
let mona_dither = dither mona ;;
depict mona_dither ;;
           
