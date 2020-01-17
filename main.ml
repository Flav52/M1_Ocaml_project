(* Nous ne sommes pas parvenus à compiler ce fichier avec ocamlc en incluant ses librairies *)
(* L'encodage est fonctionnel *)
(* En décommentant le bloque suivant, le programme est testable dans l'interpreteur *)

(*#directory "/usr/lib/ocaml/lablgtk2/";;
#load "lablgtk.cma";;

#directory "/usr/lib/ocaml/camlimages/";;
#load "graphics.cma";;
#load "camlimages_core.cma";;
#load "camlimages_png.cma";;
#load "camlimages.cma";;
#load "nums.cma";;*)

open Images;;
open OImages;;
open Info;;
open Graphics;;
open Graphic_image;;
open Num;;

let zero=num_of_int 0;;
let deux=num_of_int 2;;
let un=num_of_int 1;;

let transformation img =
  let w, h = (img#width, img#height) in
  let img_mat = (Array.make_matrix w h 0) in
    for i = 0 to (w - 1) do
      for j = 0 to (h - 1) do
	let pixel = img#get i j in
          img_mat.(i).(j) <- (Graphics.rgb pixel.r pixel.g pixel.b)
      done;
    done;
    img_mat
;;

(*Fonction faisant les opérations bits-à-bits pour récupérer les composantes RGB d'un pixel*)
let get_rgb x =
    let r = (x land 0xFF0000) lsr (2*8) in (*lsr décalle les bits vers la droite*)
    let g = (x land 0x00FF00) lsr (1*8) in
    let b = (x land 0x0000FF) in
    (r,g,b)

let get_rgb x =
    let octet n = (x lsr (8*n)) land 0x0000FF in
    (octet 2, octet 1, octet 0)


(*Création d'un tableau 3D RGB*)
let tableau3D_RGB img long larg =
    let t = Array.make long [||] in
        for i = 0 to (long-1) do
                let colonne = Array.make larg (0,0,0) in
                for j = 0 to (larg-1 ) do
                    colonne.(j) <- get_rgb img.(i).(j)
                done;
                t.(i) <- colonne
            done;
            t;;

let rec miroir l=
  match l with
  | [] -> []
  | t::q -> (miroir q)@[t];;

let b10 l=
let rec bas l i m=
    if(i=62) then (List.nth l i)
    else (List.nth l i)*/m+/(bas l (i+1) (m*/deux))
in
bas (miroir l) 0 un;;

let encode chiffre =
    if(chiffre = 255) then
        chiffre - 1
    else
        chiffre + 1
    ;;


let encodage chiffre bit =
    let mo= chiffre mod 2 in
    match mo , bit with
    0 , 0 ->chiffre;
    |0,1 -> chiffre+1;
    |1,0 -> encode chiffre;
    |1,1 -> chiffre;;

(*Cryptage d'un tableau RGB*)
let cryptage tableauRGB list_bin long larg =
    let taille_bin = List.length list_bin in
    let t = Array.make long [||] in
            for i = 0 to (long-1) do
                    let colonne = Array.make larg (0,0,0) in
                    for j = 0 to (larg-1 ) do
                        let (r,v,b) = tableauRGB.(i).(j) in
                        for k = 3 to (taille_bin-1) do
                            let newR = encodage r (List.nth list_bin (k-2)) in
                            let newV = encodage v (List.nth list_bin (k-1)) in
                            let newB = encodage b (List.nth list_bin k) in
                            colonne.(j) <- (newR,newV,newB);
                            k+3
                        done
                    done;
                    t.(i) <- colonne
                done;
                t;;
(*
let decryptage tableauCrypte long larg =
    let f x = num_of_int x in
    let res = ref [] in
    let intermediaire = ref [] in
    let t = Array.make long [||] in
            for i = 0 to (long-1) do
                    let colonne = Array.make larg (0,0,0) in
                    for j = 0 to (larg-1 ) do
                        for k=0 to 21 do
                                let (r,v,b) = tableauCrypte.(i).(j) in
                                intermediaire := !intermediaire@[r mod 2];
                                intermediaire := !intermediaire@[v mod 2];
                                intermediaire := !intermediaire@[b mod 2];
                         done;
                         (*intermediaire := List.map f !intermediaire;*)
                         res := !res@[b10 !intermediaire]; (*res prends la valeur en base 10 de notre suite de bit*)
                         intermediaire := [];
                    done;
                done;
                res;;
*)

let to_int_tab mess =
  let l = String.split_on_char ';' mess in
  let f x= num_of_string x in
  List.map f l ;;


(*Renvoie l'entier en binaire dans un tableau*)
let b2 n =
  let rec nxt l n=
  if n=/zero then l
  else
  nxt (int_of_num((mod_num n deux))::l) (quo_num n deux)
  in
nxt [] n;;


let rec makeList l n=
if n=0 then l
else (makeList l (n-1))@[0];;

let rec bin_to_63b l=
let li= List.length l in
let need= 63-li in
(makeList ([]) need)@l;;




(*dimensions de l’image a construire*)
let hauteur = 600 and largeur = 1440 and longueur_c = 3;;
(*
(*ouverture d’une fenetre graphiqueavec les dimensions de l’image*)
Graphics.open_graph (" "^(string_of_int largeur)^"x"^(string_of_int hauteur)) ;;
(*etablissement du texte du bandeau de cette fenetre*)
Graphics.set_window_title "Mon image" ;;
*)

let image = (OImages.rgb24 (OImages.load "test22.png" []));;
let img = (transformation image);;
let tableauRGB = (tableau3D_RGB img hauteur largeur);;
let numero = num_of_int 123456789;;
let list_b = b2 numero;;
let list_bin_63b = bin_to_63b list_b;;
let test_cryptage = (cryptage tableauRGB list_bin_63b hauteur largeur);;
(*let test_decryptage = (decryptage test_cryptage hauteur largeur);;*)

(*Affiche l'image en sortie sur la fenêtre graphique*)
(*let image_sortie = Graphics.make_image img;;

(*dessin de l’image dans la fenetre graphiquedepuis le coin inferieur gauche*)
Graphics.draw_image image_sortie 0 0 ;;*)