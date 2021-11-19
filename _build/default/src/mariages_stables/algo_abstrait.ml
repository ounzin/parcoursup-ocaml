(* open Definitions *)

module type PIOCHE = sig
  type 'a t
    val of_list: 'a list -> 'a t 
    val pioche : 'a t -> 'a option 
     val defausse : 'a -> 'a t -> unit 
end



module Pile : PIOCHE = struct
  (* <- on choisit d'utiliser une liste  *)
  type 'a t = {
    mutable liste : 'a list;
  } 

  let of_list l = match l with
  | [] -> {
    liste = [];
  }
  | _ -> {
    liste = l;
  }

  let pioche p = match p.liste with
  | [] -> None
  | hd::_ -> 
      let res = hd in
      p.liste <- List.tl p.liste;
      Some res

  let defausse x p = p.liste <- x::p.liste
end



module File : PIOCHE = struct

  (* <- on choisit une liste *)
  type 'a t = {
    mutable liste : 'a list;
  } 

  let of_list l = {
    liste = l;
  }

  let pioche p = match p.liste with
  | [] -> None
  | _-> 
    let liste_rev = List.rev p.liste in
    let res = List.hd liste_rev in
    p.liste <- List.rev (List.tl liste_rev);
    Some res

  let defausse x p = p.liste <- p.liste @ [x]
end




module Algo(P:PIOCHE) = struct
  
  let run entree = 
    ignore entree;
    failwith "non implémenté"

end
