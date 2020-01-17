open Big_int;;

(* Code tiré du site http://informathix.tuxfamily.org/?q=node/32 *)
let (&+) a b = Big_int.add_big_int a b;;
let (&*) a b = Big_int.mult_big_int a b;;
let (&%) a n = Big_int.mod_big_int a n;;
let (&/) a b = Big_int.div_big_int a b;;
let (&-) a b = Big_int.sub_big_int a b;;
let (&=) a b = Big_int.eq_big_int a b ;;
 
let big n = Big_int.big_int_of_int n;;

let bzero = zero_big_int;;
 
let bun = unit_big_int ;;
 
let bdeux = big(2) ;;

(*-Big Euclide II / calcul du PGCD + coeff de Bézout version récursive-*)
 
let bbezout(a,b) =
  let rec euc(u,v,r,u',v',r') =
    if r' &= bzero then
      [|u;r|]
    else
      let q = r &/ r' in 
        euc(u',v',r',u &- (q &* u'), v &- (q &* v'),r &- (q &* r'))
  in  euc(bun,bzero,a,bzero,bun,b);;
 
(*---------Fonction qui recherche l'inverse de a modulo n---------*)
 
let binv_mod(a,n) =
  let bb = bbezout(a,n) in
    bb.(0) &% n