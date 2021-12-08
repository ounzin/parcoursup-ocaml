open Definitions

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

  let defausse x p = 
    p.liste <- x::p.liste;
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
  | hd::_-> 
    let res = hd in
      p.liste <- List.tl p.liste;
      Some res

  let defausse x p = 
    p.liste <- p.liste @ [x];
end

module Algo(P:PIOCHE) = struct

  let run entree = 

    let n = entree.n in
    let x = ref None in
    let _x = ref None in
    let res = ref [] in
    let hommes = ref [] in
    let omega = None in

    for i=0 to n-1 do
      hommes := i::!hommes;
    done;
    hommes := List.rev !hommes; (* <- init hommes [0;1;...;n-1]*) 
    
    
    let _pioche = P.of_list !hommes in

    (* fiancer toutes les femmes à Ω; *)
    let config = {
      rang_appel_de = Array.make n 0;
      fiance_de = Array.make n omega;
    } in
  
    x := P.pioche _pioche; (* <- homme pioché *)

    while !x <> None do
      _x := Some entree.liste_appel_de.(Option.get !x).(config.rang_appel_de.(Option.get !x)); (* <- une fiancée *)

      if config.fiance_de.(Option.get !_x) = None 
        then begin
          config.fiance_de.(Option.get !_x) <- !x
      end
      else begin
          let current_fiance = Option.get (config.fiance_de.(Option.get !_x)) in

          if entree.prefere.(Option.get !_x) (Option.get !x) current_fiance (* si x préfère X à son fiancé *)
            then begin
              config.fiance_de.(Option.get !_x) <- !x;
              config.rang_appel_de.(current_fiance) <- config.rang_appel_de.(current_fiance) + 1;
              P.defausse current_fiance _pioche;
            end
          else begin
            config.rang_appel_de.(Option.get !x) <- config.rang_appel_de.(Option.get !x) + 1;
            P.defausse (Option.get !x) _pioche;
          end
      end;
      x := P.pioche _pioche; (* maj homme pioché *)  
    done;

  
    (*célébrer n mariages*)
    for i=0 to n-1 do
      res := (Option.get config.fiance_de.(i),(i:femme))::!res
    done;
    !res
end