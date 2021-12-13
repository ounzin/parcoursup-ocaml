open Definitions

let algo ?(affiche_config=false) entree =

  if entree_valide entree = false then failwith "Mauvais format d'entrée"
  else begin
    match entree.n with
    | 0 -> []
    | 1 -> [(0,0)]
    | _  ->
        
        (* Init de mes variables*)
        let n = entree.n in  
        let x = ref None in (* prétendant *)
        let _x = ref None in (* femme a qui le prétendant à fait une avance*)
        let res = ref [] in
        let omega = None in

        (* initialisation hommes  *)

        let init_hommes nbr_hommes = 
          let res_init_homme = ref [] in
          for j=0 to nbr_hommes-1 do
            res_init_homme :=  j::!res_init_homme;
          done;
          res_init_homme := List.rev !res_init_homme;
          res_init_homme;
        in
        let hommes_celibataire = init_hommes n in (* liste regroupant les hommes célibataires *)
        let next_hommes_celibataire = hommes_celibataire in

        (* initialisation tableau de prétendants *)
        let pretendants = ref (Array.make n []) in

        (* fiancer toutes les femmes à Ω; *)
        let config = {
          rang_appel_de = Array.make n 0;
          fiance_de = Array.make n omega;
        } in


        while List.length !hommes_celibataire <> 0 do
      
            let homme_courant = ref 0 in
            let next_women = ref 0 in
            let index_women = ref 0 in

          for k=0 to (List.length !hommes_celibataire)-1 do

            homme_courant := List.nth !hommes_celibataire k;
            next_women := config.rang_appel_de.(!homme_courant); (* <- femme suivante dans le rang d'appel *)
            index_women := entree.liste_appel_de.(!homme_courant).(!next_women); (* <- index de la liste de preference d'un homme *)
            
            !pretendants.(!index_women) <- !pretendants.(!index_women) @ [!homme_courant]
          done;

          (* pour toute femme x ayant reçu une proposition *)

          let rang_a_avancer = ref [] in

          for i=0 to (Array.length !pretendants)-1 do
            let current_list = ref (!pretendants.(i)) in
            if List.length !current_list <> 0
            then begin
              (* X := prétendant que x préfère *)

              let rec meilleur_pretendant l r k = match l with (* l = current liste, r =  *)
                | [] -> invalid_arg "error" (*ce cas n'arrivera pas car on s'est assuré que la liste contient quelque chose*)
                | [a] -> a
                | hd::tl ->
                  if entree.prefere.(k) hd !r
                    then begin
                      meilleur_pretendant tl (ref hd) k
                    end
                  else begin
                    meilleur_pretendant tl r k
                  end;
                in

                let meilleur_pretendant_actuel = ref (List.hd !current_list) in
                x := Some (meilleur_pretendant !current_list meilleur_pretendant_actuel i); (* <- voici X, le prétendant que x préfère *)
                
                (*
                si x préfère X à son fiancé actuel, ou si x est célibataire, faire début
                fiancer x et X (X n'est plus célibataire, l'éventuel ancien fiançé de x le devient)
                avancer le rang d'appel de l'éventuel ancien fiancé 
                fin
                *)

                let actuel_fiance_de__x = config.fiance_de.(i) in

                if actuel_fiance_de__x = None
                  then begin
                    config.fiance_de.(i) <- !x;
                    hommes_celibataire := List.filter (fun a -> a <> (Option.get !x)) !hommes_celibataire (* retirer X de la liste des célibataires *)
                    
                  end
                else begin
                  if entree.prefere.(i) (Option.get !x) (Option.get actuel_fiance_de__x)
                    then begin
                      config.fiance_de.(i) <- !x;
                      next_hommes_celibataire := List.filter (fun a -> a <> (Option.get !x)) !next_hommes_celibataire; (* retirer X de la liste des célibataires *)
                      next_hommes_celibataire := (Option.get actuel_fiance_de__x)::!next_hommes_celibataire;
                      
                    end
                  else begin
                    if not (List.mem (Option.get !x) !rang_a_avancer)
                      then begin
                        rang_a_avancer := (Option.get !x)::!rang_a_avancer
                      end
                  end;
                end;        
            end;
          done;


          if (List.length !rang_a_avancer) <> 0 && (List.length !next_hommes_celibataire) <> 0
          then begin
            for t=0 to (List.length !rang_a_avancer)-1 do
              let a_avancer = List.nth !rang_a_avancer t in
              if config.rang_appel_de.(a_avancer) < n 
                then begin
                  config.rang_appel_de.(a_avancer) <- config.rang_appel_de.(a_avancer) + 1;
                end       
            done;
          end;

          hommes_celibataire := !next_hommes_celibataire; (* <- maj hommes celibataire *)

          if (List.length !rang_a_avancer)<> 0
            then begin
              if affiche_config = true
                then begin
                  print_configuration config
                end;
            end
        done;
        
        (*let rang = ref 0 in
        for p=0 to n-1 do
          rang := config.rang_appel_de.(p);
          if !rang <> 0
            then begin
              config.rang_appel_de.(p) <- !rang-1;
            end;
        done;*)

          
        (*célébrer n mariages*)
        for z=0 to n-1 do
          res := (Option.get config.fiance_de.(z),z)::!res
        done;
        !res  
  end