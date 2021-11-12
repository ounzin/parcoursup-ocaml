open Definitions 

let algo ?(affiche_config=true) entree =

  if entree_valide entree = false then failwith "Mauvais format d'entrée"
  else begin
    (* Init de mes variables*)
    let n = entree.n in  
    let k = ref 0 in
    let x = ref None in (* prétendant *)
    let _x = ref None in (* femme a qui le pret à fait une avance*)
    let res = ref [] in
    
     
    let omega = None in

    (* fiancer toutes les femmes à Ω; *)
    
    let config = {
      rang_appel_de = Array.make n 0;
      fiance_de = Array.make n omega;
    } in

    while !k < n do
      x := Some !k;
      while !x <> omega do
        _x := Some entree.liste_appel_de.(Option.get !x).(config.rang_appel_de.(Option.get !x));

        if config.fiance_de.(Option.get !_x) = None (* <- pire cas possible, X sera toujours préférable à omega *)
        then
          begin
            let tmp = config.fiance_de.(Option.get !_x) in
            config.fiance_de.(Option.get !_x) <- !x;
            x := tmp;
          end
        else begin
          let current_fiance = Option.get (config.fiance_de.(Option.get !_x)) in
          if entree.prefere.(Option.get !_x) (Option.get !x) current_fiance (* si x préfère X à son fiancé *)
            then begin (* fiancer x et X *)
              let tmp = config.fiance_de.(Option.get !_x) in
              config.fiance_de.(Option.get !_x) <- !x;
              x := tmp;
            end;
        end;

        if !x <> omega then (* si X ≠ Ω alors retirer x de la liste de X *)
          begin
            let tmp = config.rang_appel_de.(Option.get !x) in
            config.rang_appel_de.(Option.get !x) <- tmp + 1
          end;

        if not affiche_config = true
        then print_configuration config
      done;
      k := !k + 1;
    done;

    (*célébrer n mariages*)

    for i=0 to n-1 do
      res := (Option.get config.fiance_de.(i), i)::!res
    done;
    !res
  end