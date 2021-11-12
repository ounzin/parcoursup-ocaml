open Definitions

let algo ?(affiche_config=false) entree =
  if entree_valide entree = false then failwith "Mauvais format d'entrée"
  else begin
    (* Init de mes variables*)
    let n = entree.n in  
    let x = ref None in (* prétendant *)
    let _x = ref None in (* femme a qui le pret à fait une avance*)
    let res = ref [] in
    let omega = None in


    
    (* initialisation hommes  *)
    let init_hommes nbr_hommes = 
      let res_init_homme = ref [] in
      for i=0 to nbr_hommes-1 do
        res_init_homme :=  !res_init_homme @ [i];
      done;
      res_init_homme;
    in
    let hommes_celibataire = init_hommes n in (* liste regroupant les hommes célibataires *)

    (* initialisation tableau de prétendants *)

    let init_pretendants nbr_femmes =
      let res_init_pretendants = ref [||] in
      let empty_liste = [] in
      for i=0 to nbr_femmes do
        res_init_pretendants := Array.append !res_init_pretendants [|empty_liste|];
      done;
      res_init_pretendants
    in
    let pretendants = init_pretendants n in



    (* fiancer toutes les femmes à Ω; *)
    
    let config = {
      rang_appel_de = Array.make n 0;
      fiance_de = Array.make n omega;
    } in


    while List.length !hommes_celibataire <> 0 do

      (* pour tout homme célibataire X faire début
        x := prochaine femme sur la liste d'appel de X
        ajouter X aux prétendants de x
      fin *)
      
      for i=0 to List.length !hommes_celibataire do
        let index_next_women_liste_appel_x = config.rang_appel_de.(List.nth !hommes_celibataire i) in
        !pretendants.(index_next_women_liste_appel_x) <- !pretendants.(index_next_women_liste_appel_x) @ [List.nth !hommes_celibataire i]
      done;

      (* pour toute femme x ayant reçu une proposition *)

      for i=0 to n-1 do
        let current_list = !pretendants.(i) in
        if List.length current_list <> 0
        then begin
          if List.length current_list = 1
            then x := List.hd current_list;
        end
    done;
  end;