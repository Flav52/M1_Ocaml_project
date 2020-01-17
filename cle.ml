open Num;;
open Big_int;;
open Random;;
open Printf;;

open Tux;;
open Primes;;

(*
#load "primes.ml";;
#load "tux.ml";;
*)
Random.self_init();;



let zero=num_of_int 0;;
let un=num_of_int 1;;
let deux=num_of_int 2;;
let print_num n = Format.printf "%s" (string_of_num n);;

let isPair n =
  if mod_num n deux =/ zero then true
  else false;;

(*Génère un Num aléatoire*)
let rec genNumRand ?(s="") i=
  Random.self_init();
  let gen=string_of_int (Random.int 10) in
  let s=gen^s in
  match i with
  |0 -> num_of_string s;
  |_ -> genNumRand ~s (i-1);;

(*Renvoie le num en binaire dans un tableau*)
let b2 n =
  let rec nxt l n=
  if n=/zero then l
  else
  nxt ((mod_num n deux)::l) (quo_num n deux)
  in
nxt [] n;;

(* Renvoie la liste l en miroir*)
let rec miroir l=
  match l with
  | [] -> []
  | t::q -> (miroir q)@[t];;

let rec taille l =
  match l with
  |[] -> 0
  |t::q->1+taille q;;

(* renvoie l'element d'index i dans la liste l*)
let rec ieme l i=
  assert(i>=0);
  match l with
  | [] -> failwith "Liste vide"
  | t::q -> if 0=i then t
            else ieme q(i-1);;
  
(* Exponentiation modulaire*)
let expMod a b n =
  let c=ref 0 in
  let d=ref un in
  let b=miroir(b2 b) in
  let rec boucle i=
      c:= 2*(!c);
      d:=mod_num (!d*/(!d)) n;
    if (ieme b i)=/un then 
      begin
        c:=!c+1;
        d:=mod_num (!d*/a) n;
      end;
  if i>0 then boucle (i-1); 
  in
  boucle (taille(b)-1);
  !d;;

(* Algorithme pseudo-primaire*)
let pseudoPrim n=
  if (expMod deux (pred_num n) n)=/un then
    true 
  else 
    false;;

(* Témoin de Miller *)
let temoin a n =
  let u = ref (pred_num n) in
  let t = ref 0 in
  let prev = ref zero in
  let x = ref (expMod a (!u) n) in
  let res=ref false in

  while (mod_num !u deux)=/zero do
    u := !u//deux;
    t := !t + 1;
  done;
 
  for i=1 to !t do
    prev:=!x;
    x:=mod_num (!x**/deux) n;
    if (!x=/un)&&(!prev<>/un)&&(!prev<>/pred_num n) then res:=true;
  done;

  if !x<>/un then res:=true;
  !res;;

(*Longueur du num en digit*)
let lgt n =
  let str= string_of_num n in
  String.length str;;

(*Num aléatoire inférieur à n*)
let rec random n =
  let res=genNumRand (lgt(n)) in
  if res</n then res
  else random n;;

let millerRabin n s =
  let res=ref true in
  for j = 1 to s do
    if (temoin (random(n-/un)) (n)) then
    res:=false;
  done;
  !res;;

(* Renvoie true si n est dans la liste l*)
let rec isIn n l =
  match l with
  |t::q when t=n->true;
  |t::q ->isIn n q;
  |[]-> false;;

(* Vérifie si n est premier. On regarde si il est
 dans la liste fournie avant de langer Miller-Rabin*)
let isPrime n =
  let test=(n</(num_of_int 4096))&&(isIn (int_of_num n) primes)in
  if test then
    true
  else
    millerRabin n 20;;

(* Génère un entier aléatoire jusqu'à en trouver un premier*)
let rec prime n=
  let a=ref (genNumRand n)in
  let res= ref (isPrime !a)in
  if !res then !a else prime n;;

(* Interactions avec les fichiers*)
let writeToFile mess name =
  let oc=open_out name in
  fprintf oc "%s" mess;
  close_out oc;;

let readFromFile name =
  let ic = open_in name in
  let line = input_line ic in   
  flush stdout;               
  close_in ic ;
  line;;

(* Fonction phi d'Euler*)
let phi p q =
  (pred_num p)*/(pred_num q);;

(* calcul du pgcd de a et b*)
let gcd_num a b=
  let c=big_int_of_num a in
  let d=big_int_of_num b in
  let e=gcd_big_int c d in
  num_of_big_int e;;

(* calcul de e*)
let find_e n =
  let rec find n e =
    if (gcd_num n e)=/un then
      e
    else
      find n (e+/deux)
  in find n (num_of_int 3);;

(* Lis le nombre de paires de clés générées*)
let getKeyIndex =
  let str=ref(readFromFile "indexK") in
  int_of_string !str;;

(* 2 suivantes: interfaces pour l'Algorithme d'euclide Bézout (tux.ml)*)
(*La fonction rend 0 avec a=Num.3 et n et Num.20
 alors que le résultat attendu est 7            *)
let i_binv_mod_BROKEN a n=
  num_of_big_int(binv_mod ((big_int_of_num a),(big_int_of_num a)));;

(* On a recours à de multiples casts,
 certes ce n'est pas très propre, 
 mais le seul moyen que nous avons trouvé. *)
let i_binv_mod a n=
let ba=big_int_of_string (string_of_num a) in
let bn=big_int_of_string (string_of_num n) in
  num_of_big_int(binv_mod (ba,bn));;

(* Exponentiation rapide*)
let rec expRapide x n=
  if(n=/un) then x
  else 
    begin
      if(mod_num n deux)=/zero then (expRapide (x*/x) (n//deux))
      else
        x*/(expRapide (x*/x) ((pred_num n)//deux));
    end

(* renvoie une liste des caractères de s*)
  let explode s=
  let l=ref [] in
  for i = 0 to (String.length s)-1 do
    l:=!l@[s.[i]];
    done;
    !l;;

(* lance le cryptage de mess avec exposant e et modulo m*)
let crypt e m mess=
  let l = explode mess in
  let crp char= expMod (num_of_int(Char.code char)) e m in
  let rec loop l=
  match l with
    [] -> "";
    |[a] -> (string_of_num(crp a));
    |t::q-> (string_of_num(crp t))^(";")^(loop q);
  in
  loop l;;

(* lance le decryptage de mess avec exposant e et modulo m*)
  let decrypt d m mess=
  let l = String.split_on_char ';' mess in
  let f x= int_of_string x in
  let codes= List.map f l in
  let dcrp char= int_of_string(string_of_num(expMod (num_of_int(char)) d m)) in
  let num_to_string x= String.make 1 (Char.chr(x)) in
  let rec loop o=
  match o with
  []->[];
    |[a] -> [(dcrp a)];
    |t::q-> (dcrp t)::(loop q);
  in
    let res = List.map num_to_string (loop codes) in
    res;;

(* retourne les clés stockées de numéro n*)
let getkeys n=
  let line1 = readFromFile ("KEYS/"^(string_of_int n)^".pub") in
  let line2 = readFromFile ("KEYS/"^(string_of_int n)^".prv") in
  let couple1= String.split_on_char ' ' line1 in
  let couple2= String.split_on_char ' ' line2 in
  let pub=ieme couple1 0 in
  let priv=ieme couple2 0 in
  let modu=ieme couple1 1 in
  (num_of_string pub,num_of_string priv,num_of_string modu);;

(* génération des clés*)
let genKeys nb=
  let ind= (getKeyIndex+1) in
  let p=  prime nb in
  let q=  prime nb in
  let n= p*/q in
  let e= find_e (phi p q)in
  let d= i_binv_mod e (phi p q) in
  let strPu =ref ((string_of_num e)^" "^(string_of_num n)) in
  let strPr =ref ((string_of_num d)^" "^(string_of_num n)) in
  let file=ref ("KEYS/"^(string_of_int ind)) in
  writeToFile !strPu ((!file)^".pub");
  writeToFile !strPr ((!file)^".prv");
  writeToFile (string_of_int (ind)) "indexK";;

(* concatène les chaînes de l en une seule*)
  let rec strlist_to_str l =
  match l with
  [] -> "";
  |[t] -> t;
  |t::q -> t^(strlist_to_str q);;

(* Menu *)
let afficherAide ()= Printf.printf("Aide: \n
  -c: Lancer le cryptage\n
  -d: Lancer de décryptage\n
  -k n: Lancer la génération des clés. Peut prendre plus de 10s.\n
  La taille des nombres premiers ainsi que le numero du jeu de clés doivent
être précisé dans la méthode main avant compilation.\n\n") ;;


let main ()=
  let numero_paire = ref 1 in
  let taille_seed = ref 15 in
  let (pub,priv,modu)=getkeys (!numero_paire) in
  let matched= ref false in


match Sys.argv.(1) with
      x when x = "-c" ->
      begin
          matched :=true;
          let message= "Ceci est un message secret" in
          let message_crypte = crypt pub modu message in
          writeToFile message_crypte "crypt.txt";
          Printf.printf "Cryptage terminé: %s\n"  message_crypte
      end
      |x when x = "-d" ->
        begin
          matched :=true;     (* Message par défaut avant le couplage avec l'autre partie*)
        let message_crypte = "1350125107;10510100501;9509900499;12762815625;33554432;10510100501;20113571875;21003416576;33554432;21924480357;16105100000;33554432;15386239549;10510100501;20113571875;20113571875;8587340257;11592740743;10510100501;33554432;20113571875;10510100501;9509900499;19254145824;10510100501;21003416576" in 
        let message = decrypt priv modu message_crypte in
        writeToFile (strlist_to_str message) "decrypt.txt";
        Printf.printf "Décryptage terminé: %s\n" (strlist_to_str message)
        end

      |x when x = "-k" ->
        begin
        matched :=true;
        genKeys !taille_seed;
        Printf.printf("Génération terminée\n")
        end
      
      |_ -> afficherAide();;

let _= if Array.length (Sys.argv) < 1 then
afficherAide ()
else
main();
;;