(* Nous ne sommes pas parvenus à compiler ce fichier avec ocamlc en incluant ses librairies *)
(* L'encodage est fonctionnel *)
(* En décommentant le bloque suivant, le programme est testable dans l'interpreteur *)

#directory "/usr/lib/ocaml/lablgtk2/";;
#load "lablgtk.cma";;

#directory "/usr/lib/ocaml/camlimages/";;
#load "graphics.cma";;
#load "camlimages_core.cma";;
#load "camlimages_png.cma";;
#load "camlimages.cma";;
#load "nums.cma";;

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

let transformation_apres_cryptage tabRGB largeur longueur =
  let img_mat = (Array.make_matrix largeur longueur 0) in
    for i = 0 to (largeur - 1) do
      for j = 0 to (longueur - 1) do
          let (r,v,b) = tabRGB.(i).(j) in
          img_mat.(i).(j) <- (Graphics.rgb r v b)
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
                let ligne = Array.make larg (0,0,0) in
                for j = 0 to (larg-1 ) do
                    ligne.(j) <- get_rgb img.(i).(j)
                done;
                t.(i) <- ligne
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

(*Fonction allant avec la fonction encodage, elle prends en compte l'exemption du chiffre étant à 255*)
let encode chiffre =
    if(chiffre = 255) then
        chiffre - 1
    else
        chiffre + 1
    ;;

(*Fonction permettant l'encodage d'un chiffre suivant si celui-ci est paire ou impaire, par comparaison avec un bit mis en paramètre*)
let encodage chiffre bit =
    let mo= chiffre mod 2 in
    match mo , bit with
    0 , 0 ->chiffre;
    |0,1 -> chiffre+1;
    |1,0 -> encode chiffre;
    |1,1 -> chiffre;;

(*Cryptage d'un tableau RGB de type -> (int * int * int) Array Array *)
let cryptage tableauRGB list_bin long larg =
    let taille_bin = List.length list_bin in
    let t = Array.make long [||] in
            let compteur = ref 2 in
                for i = 0 to (long-1) do
                    let colonne = Array.make larg (0,0,0) in
                    for j = 0 to (larg-1 ) do
                        let (r,v,b) = tableauRGB.(i).(j) in
                        if !compteur < taille_bin-1  then
                            let newR = encodage r (List.nth list_bin (!compteur-2)) in
                            let newV = encodage v (List.nth list_bin (!compteur-1)) in
                            let newB = encodage b (List.nth list_bin !compteur) in
                            colonne.(j) <- (newR,newV,newB);
                            compteur := !compteur + 3;
                            Printf.printf " cryptage ((%i-%i-%i) -> (%i-%i-%i)) bin : (%i-%i-%i) compteur : %i taille bin :%i \n" r v b newR newV newB (List.nth list_bin (!compteur-2)) (List.nth list_bin (!compteur-1)) (List.nth list_bin !compteur) !compteur taille_bin;
                         else if !compteur = taille_bin-1 then
                            let newR = encodage r (List.nth list_bin (!compteur-2)) in
                            let newV = encodage v (List.nth list_bin (!compteur-1)) in
                            let newB = encodage b (List.nth list_bin !compteur) in
                            colonne.(j) <- (newR,newV,newB);
                            compteur := !compteur + 1;
                         else colonne.(j) <- (r,v,b);
                   done;
                    t.(i) <- colonne
                done;
                t;;

(*Decryptage d'un message binaire dans un tableau 2D ayant dans chaque cellule un tuple (r,g,b) *)
let decryptage tabRGB largeur longueur =
  let resultat = ref [] in
  let message = ref [] in
  let compteur = ref 0 in
    for i = 0 to (largeur - 1) do
        for j = 0 to (longueur - 1) do
            if !compteur < 63 then
              let (r,v,b) = tabRGB.(i).(j) in
                  message := !message@[mod_num (num_of_int r) deux];
                  message := !message@[mod_num (num_of_int v) deux];
                  message := !message@[mod_num (num_of_int b) deux];
                  (*Printf.printf " (%i-%i-%i) -> decryptage -> (%i-%i-%i)  \n" r v b (mod_num (num_of_int r) deux) (v mod 2) (b mod 2);*)
                  compteur := !compteur + 3;
        done;
    done;
    resultat := !resultat@[b10 !message];
    resultat;;

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

(*Fonction permettant de convertir un message binaire sous 63 bit*)
let rec bin_to_63b l=
let li= List.length l in
let need= 63-li in
(makeList ([]) need)@l;;

(*
let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l
*)



(*dimensions de l’image a construire*)
let hauteur = 600 and largeur = 1440 ;;

let image = (OImages.rgb24 (OImages.load "test22.png" []));;
let img = (transformation image);;
let tableauRGB = (tableau3D_RGB img hauteur largeur);;
let numero = num_of_int 123456789;;
let list_b = b2 numero;;
let list_bin_63b = bin_to_63b list_b;;
let test_cryptage = (cryptage tableauRGB list_bin_63b hauteur largeur);;
let test_decryptage = (decryptage test_cryptage hauteur largeur);;

let img_crypte = (transformation_apres_cryptage test_cryptage hauteur largeur);;



(*
(*ouverture d’une fenetre graphiqueavec les dimensions de l’image*)
Graphics.open_graph (" "^(string_of_int largeur)^"x"^(string_of_int hauteur)) ;;
(*etablissement du texte du bandeau de cette fenetre*)
Graphics.set_window_title "Mon image" ;;

(*Affiche l'image en sortie sur la fenêtre graphique*)
let image_sortie = Graphics.make_image img_crypte;;
let image_sortie = Graphics.make_image img;;
(*dessin de l’image dans la fenetre graphiquedepuis le coin inferieur gauche*)
Graphics.draw_image image_sortie 0 0 ;;
*)