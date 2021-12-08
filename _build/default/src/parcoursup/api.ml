type candidat = {
  mutable name : string;
  mutable repondeur_automatique : string array;
  mutable voeux : string array;
  mutable voeux_en_attente : (string * int) array;
  mutable proposition_en_attente : string list;
}

type formation = {
  mutable name : string;
  mutable capacite : int;
  mutable candidatures : string list; 
  mutable liste_dappel : string list; 
  mutable rang_dappel : int;
  mutable places_restante : int;
  mutable acceptations : string list;
  mutable renonces : string list;
} 
type commission = {
  mutable formation : string;
  mutable algo_local : (candidat1: string -> candidat2: string -> bool);
}

type session = {
  mutable candidats : candidat array;
  mutable formations : formation array;
  mutable commissions : commission array;
}

(* mes fonctions usuelles *)
let get_voeux_name tab = 
  if Array.length tab = 0
    then begin
      []
    end else begin
    let res = ref [] in
    for i=0 to (Array.length tab - 1) do
      res := fst(tab.(i))::!res;
    done;
    !res
  end

(* fin fonction usuelles *)

let nouvelle_session () = {
  candidats = [||];
  formations = [||];
  commissions = [||];
}

let ajoute_candidat session ~nom_candidat = 
  let new_candidat = {
    name = nom_candidat;
    repondeur_automatique = [||];
    voeux = [||];
    voeux_en_attente = [||];
    proposition_en_attente = [];
  } in
  session.candidats <- Array.append session.candidats [|new_candidat|]

let ajoute_formation session ~nom_formation ~capacite =
  let new_formation = {
    name = nom_formation;
    capacite = capacite;
    candidatures = [];
    liste_dappel = [];
    rang_dappel = 0;
    places_restante = capacite;
    acceptations = [];
    renonces = [];
  } in
  session.formations <- Array.append session.formations [|new_formation|]

let ajoute_voeu session ~rang_repondeur ~nom_candidat ~nom_formation =  
  
  (* récupération de mon candidat courant grace à nom candidat *)

  let current_candidat = ref session.candidats.(0) in
  let index_candidat = ref 0 in
  for i=0 to (Array.length (session.candidats)-1) do
    if session.candidats.(i).name = nom_candidat
      then begin
        current_candidat := session.candidats.(i);
        index_candidat := i;
      end
  done;

  (* récupération de la formation à laquelle il souhaite candidaté *)

  let current_formation = ref session.formations.(0) in
  let index_formation = ref 0 in
  for i=0 to (Array.length (session.formations)-1) do
    if session.formations.(i).name = nom_formation
      then begin
        current_formation := session.formations.(i);
        index_formation := i;
      end
  done;

  (* ajouter nom de la formation aux voeux du candidat et ajouter celui du candidat aux candidatures de la formation *)

  !current_candidat.voeux <- Array.append !current_candidat.voeux [|nom_formation|];
  !current_formation.candidatures <- nom_candidat::!current_formation.candidatures;
  session.candidats.(!index_candidat) <- !current_candidat;
  session.formations.(!index_formation) <- !current_formation;

  (* mise à jour du repondeur automatique : cas ou rang_repondeur est précisé *)

  if rang_repondeur <> None (* <- un rang a été précisé *)
    then begin
      let repondeur_size = Array.length !current_candidat.repondeur_automatique in

      if ((Option.get rang_repondeur) < repondeur_size && repondeur_size > 0) (* rang spécifié est plus petit que la taille du répondeur actuel *)
        then begin (* j'insère mon voeux dans le répondeur et je décale ceux qui sont moins bien classés *)
          let old_repondeur_head = ref (Array.sub !current_candidat.repondeur_automatique 0 (Option.get rang_repondeur)) in (* je recupere les elements du premier jusqu'au ou je veux inserer*)
          old_repondeur_head := Array.append !old_repondeur_head [|nom_formation|]; (* <- j'insère la formation *)
          old_repondeur_head := Array.append !old_repondeur_head (Array.sub !current_candidat.repondeur_automatique (Option.get rang_repondeur) (repondeur_size-1)); (* <- je décale les autres *)
          !current_candidat.repondeur_automatique <- !old_repondeur_head; (* <- je mets à jour le répondeur auto avec ma formation déjà insérée*)
          (* mise a jour du candidat dans la session *)
          session.candidats.(!index_candidat) <- !current_candidat;
        end
      else begin (* <- les rang est plus grand que la taille du répondeur je le mets à la fin *)
        !current_candidat.repondeur_automatique <- Array.append !current_candidat.repondeur_automatique [|nom_formation|];
        session.candidats.(!index_candidat) <- !current_candidat;
      end
    end


let ajoute_commission session ~nom_formation ~fonction_comparaison = 
  let new_commission = {
    formation = nom_formation;
    algo_local = fonction_comparaison;
  } in
  session.commissions <- Array.append session.commissions [|new_commission|]

let reunit_commissions session =
  let les_commissions = session.commissions in
  let size = Array.length les_commissions in

  for i=0 to size-1 do (* Pour chacune des formations nous allons récupérer la liste des canditats, les triés et maj la liste d'appel *)
    let current_commission = session.commissions.(i) in (* <- commission courante *)
    let nom_current_formation = current_commission.formation in (* <- nom de la formation que nous utiliserons pour récupérer la formation dans session.formations *)
    let fonction_current_formation = current_commission.algo_local in (* <- algo de tri boolean *)
    let current_formation = ref session.formations.(0) in (* <- variable pour récupérer la formation courante *)
    let current_candidatures = ref session.formations.(0).candidatures in (* <- variable pour récupérer les candidatures courantes *)
    let index_formation = ref 0 in (* <- variable pour récupérer l'index de la formation courante *)

    (* récuperation de la formation courante *)
    for j=0 to (Array.length session.formations - 1) do
      if session.formations.(j).name = nom_current_formation
        then begin
          current_formation := session.formations.(j);
          index_formation := j;
        end
    done; (* <- fin recup formation courante *)


    current_candidatures := !current_formation.candidatures; (* <- maj candidatures courante *)

    (* Utilisation de la fonction de comparaison pour générer la liste d'appel *)
    
    let algo_trieur a b =  (* on modifie notre algo de tri pour pouvoir gérer des entiers *)
      if fonction_current_formation ~candidat1:a ~candidat2:b  then -1 else 1 in
    
    !current_formation.liste_dappel <- List.sort algo_trieur !current_candidatures; (* <- on génère notre liste d'appel *)
    session.formations.(!index_formation) <- !current_formation; (* maj de formation dans la session *)
    
  done (* <- fin iter sur commissions *)

let nouveau_jour session =

  (* Définition de mes variables *)
  let current_candidat = ref session.candidats.(0) in
  let current_formation = ref session.formations.(0) in
  let propositions_envoyees = ref [] in

  (* Proposition des formations aux étudiants en tenant compte des renonces *)

  for i=0 to (Array.length session.formations -1) do 
    current_formation := session.formations.(i); (* <- maj formation courante *)

    (* Pour chacune des formation nous allons envoyer des candidatures en fonction du nombre de places restantes et des renonces ensuite nous allons mettre à jour les voeux en attente *)
    while !current_formation.places_restante > 0 && !current_formation.rang_dappel < (List.length !current_formation.liste_dappel) do (* <- tant qu'il reste des places libreset qu'on peut avancer*)
  
    let nom_current_candidat_liste_dappel = List.nth !current_formation.liste_dappel !current_formation.rang_dappel in

      if (List.mem nom_current_candidat_liste_dappel !current_formation.renonces ) = true (* <- s'il s'agit d'un candidat qui a renoncé *)
        then begin
          !current_formation.rang_dappel <- !current_formation.rang_dappel + 1;
      end else begin (* un potentiel candidat *)
      
          (* on recupere le potentiel candidat *)
          
          let potentiel_candidat = ref session.candidats.(0) in
          let index_potentiel_candidat = ref 0 in
          for j=0 to (Array.length session.candidats -1) do
            if nom_current_candidat_liste_dappel = session.candidats.(j).name 
              then begin
                potentiel_candidat := session.candidats.(j);
                index_potentiel_candidat := j
              end
          done;

          (*  on ajoute la formation à ses propositions en attente et on l'efface des voeux en attentes si elle s'y trouve *)
          let voeux_en_attente_potentiel_candidat = get_voeux_name !potentiel_candidat.voeux_en_attente in

          if (List.mem !current_formation.name voeux_en_attente_potentiel_candidat) = true
            then begin
              (* on retire la formation des voeux en attente si elle y est *)
                let new_voeux_en_attente = ref [||] in
                for j=0 to (Array.length !potentiel_candidat.voeux_en_attente -1) do
                  if !current_formation.name <> fst (!potentiel_candidat.voeux_en_attente.(j))
                    then begin
                      new_voeux_en_attente := Array.append !new_voeux_en_attente [|!current_candidat.voeux_en_attente.(j)|] 
                    end
                done;
                !potentiel_candidat.voeux_en_attente <- !new_voeux_en_attente;
                !potentiel_candidat.proposition_en_attente <- !potentiel_candidat.proposition_en_attente @ [!current_formation.name];
            end else begin
              !potentiel_candidat.proposition_en_attente <- !potentiel_candidat.proposition_en_attente @ [!current_formation.name];          
            end;
          propositions_envoyees := !potentiel_candidat.name::!propositions_envoyees;
          session.candidats.(!index_potentiel_candidat) <- !potentiel_candidat;
          !current_formation.places_restante <- !current_formation.places_restante - 1; (* <- on reduit le nombre de place libre *)
          !current_formation.rang_dappel <- !current_formation.rang_dappel + 1; (* <- on avance le rang d'appel *)
          
      end
    done; (* <- fin des propositions de la formation courante *)


    (* Mise à jour des voeux en attentes des candidats restants sur la liste d'appel *)
    
    if !current_formation.rang_dappel <> (List.length !current_formation.liste_dappel)
      then begin
        for p = 0 to (List.length !current_formation.liste_dappel - !current_formation.rang_dappel ) do
          let nom_candidat_restant_sur_liste = ref (List.nth !current_formation.liste_dappel ( 0 + !current_formation.rang_dappel))in
          let current_candidat_restant = ref session.candidats.(0) in
          let index_current_candidat_restant  = ref 0 in
    
          for k=0 to (Array.length session.candidats - 1) do
            if !nom_candidat_restant_sur_liste = session.candidats.(k).name
              then begin
                current_candidat_restant := session.candidats.(k);
                index_current_candidat_restant := k;
              end
          done;
    
          let new_voeux_en_attente = (!current_formation.name, p) in 
          let liste_names_voeux = get_voeux_name !current_candidat_restant.voeux_en_attente in
    
          if List.mem !current_formation.name liste_names_voeux <> true
            then begin
              !current_candidat_restant.voeux_en_attente <- (Array.append !current_candidat_restant.voeux_en_attente [|new_voeux_en_attente|]);
            end;
          session.candidats.(!index_current_candidat_restant) <- !current_candidat_restant;
        done;
      end;
    

    (* Pour chacun des candidats nous allons analyser les propositions reçues *)
    
    for i=0 to (Array.length session.candidats - 1) do
      
      current_candidat := session.candidats.(i);

      let testeur_repondeur_auto = try Some (!current_candidat.repondeur_automatique.(0)) with _ -> None in

          if testeur_repondeur_auto <> None && (List.length !current_candidat.proposition_en_attente > 0) (* <- le répondeur automatique est activé : on choisit le meilleur choix et on renonce aux autres *)
            then begin
              let a_renoncer = ref [] in
              let map_choix = ref [] in
              let meilleur_choix = ref "" in
              let index_meilleur_choix = ref 0 in
            
    
              (* création d'une map (formation, position_dans_repondeur)*)
              for c=0 to (List.length !current_candidat.proposition_en_attente - 1) do
                let current_proposition = List.nth !current_candidat.proposition_en_attente c in
                for k=0 to (Array.length !current_candidat.repondeur_automatique - 1) do
                  if current_proposition = !current_candidat.repondeur_automatique.(k)
                    then begin
                      map_choix := (!current_candidat.repondeur_automatique.(k), k)::!map_choix;
                    end
                done;
              done;
    
              (* recupération du meilleur choix *)
              if (List.length !map_choix) = 1
                then begin
                  meilleur_choix := fst(List.nth !map_choix 0);
                  index_meilleur_choix := snd(List.nth !map_choix 0);
                end else begin
                  let best_in_map = ref (snd(List.nth !map_choix 0)) in
                  let index_best_in_map = ref 0 in
                  for k=1 to (List.length !map_choix - 1) do
                    if (snd(List.nth !map_choix k) < !best_in_map) then
                      begin
                        best_in_map := snd(List.nth !map_choix k);
                        index_best_in_map := k;
                      end
                  done;
                  meilleur_choix := fst(List.nth !map_choix !index_best_in_map);
                  index_meilleur_choix := snd(List.nth !map_choix !index_best_in_map);
              end;
    
              (* <- on remplit notre liste de formation à refuser *)
              for t=(!index_meilleur_choix+1) to (Array.length !current_candidat.repondeur_automatique -1) do
                a_renoncer := !current_candidat.repondeur_automatique.(t)::!a_renoncer;
              done;
              
    
              (*<- on renonce aux formations moins bien classés :: maj dans formations puis dans candidat *)
    
              (* Dans formation*)
              for z=0 to (List.length !a_renoncer - 1) do
                let formation_refusee = ref session.formations.(0) in (* pour chaque formation courante à laquelle on renonce on la récupère et on la met à jour *)
                let index_formation_refusee = ref 0 in
                for y=0 to (Array.length session.formations - 1) do
                  if session.formations.(y).name = (List.nth !a_renoncer z)
                    then
                      begin
                        formation_refusee := session.formations.(y);
                        index_formation_refusee := y;
                      end
                done;
                !formation_refusee.renonces <- !current_candidat.name::!formation_refusee.renonces;
                session.formations.(!index_formation_refusee) <- !formation_refusee; (* maj de la formation *)
              done;
    
              (* Dans candidat *)
    
              (* on retire la formation dans les voeux en attentes *)
    
              for p=0 to (List.length !a_renoncer - 1) do
                let formation_refusee = (List.nth !a_renoncer p) in
                let maj_voeux = ref (Array.to_list !current_candidat.voeux_en_attente) in
                if (List.mem formation_refusee (get_voeux_name(!current_candidat.voeux_en_attente))) = true
                  then begin
                    !current_candidat.voeux_en_attente <- Array.of_list (List.filter (fun x -> fst(x)=formation_refusee) !maj_voeux);
                  end
              done; 

              (* on retire la formation dans les propostions *)

              for p=0 to (List.length !a_renoncer - 1) do
                let formation_refusee = (List.nth !a_renoncer p) in
                let maj_propositions = ref (!current_candidat.proposition_en_attente) in
                if (List.mem formation_refusee !current_candidat.proposition_en_attente) = true
                  then begin
                    !current_candidat.proposition_en_attente <- List.filter (fun x -> x<>formation_refusee) !maj_propositions;
                  end
                done;
            end;
    done;
    
    session.formations.(i) <- !current_formation; (* maj de ma formation courante *)
  done (* <- fin des propostions de toutes les formations *)


let renonce session ~nom_candidat ~nom_formation = 

  (* récupération de du candidat courant *)
  let current_candidat = ref session.candidats.(0) in
  let index_candidat = ref 0 in
  for i=0 to (Array.length session.candidats - 1) do
    if nom_candidat = session.candidats.(i).name
      then begin 
        current_candidat := session.candidats.(i);
        index_candidat := i;
      end
  done;

  (* récupération de la formation courante *)
  let current_formation = ref session.formations.(0) in
  let index_formation = ref 0 in
  for i=0 to (Array.length session.formations - 1) do
    if nom_formation = session.formations.(i).name
      then begin 
        current_formation := session.formations.(i);
        index_formation := i;
      end
  done;

  (* on retire la formation des voeux en attente si elle y est *)
  let new_voeux_en_attente = ref [||] in
  for j=0 to (Array.length !current_candidat.voeux_en_attente -1) do
    if nom_formation <> fst (!current_candidat.voeux_en_attente.(j))
      then begin
        new_voeux_en_attente := Array.append !new_voeux_en_attente [|!current_candidat.voeux_en_attente.(j)|] 
      end
  done;
  !current_candidat.voeux_en_attente <- !new_voeux_en_attente; (* <- maj voeux en attente pour le candidat courant *)

  
  (* on retire la formation des propositions si elle y est *)
  let new_propositions_en_attente = ref [] in
  for k=0 to (List.length !current_candidat.proposition_en_attente -1) do
    
    if nom_formation <> (List.nth !current_candidat.proposition_en_attente k)
      then begin
        new_propositions_en_attente := (List.nth !current_candidat.proposition_en_attente k)::!new_propositions_en_attente;
      end else begin
        if !current_formation.places_restante < !current_formation.capacite
          then begin
            !current_formation.places_restante <- !current_formation.places_restante + 1;
          end
      end
  done;

  !current_candidat.proposition_en_attente <- !new_propositions_en_attente; (* <- maj des propositions pour le candidat courant *)
  !current_formation.renonces <- nom_candidat::!current_formation.renonces; (* <- ajout du candidat aux renoncement des formations *)
  session.candidats.(!index_candidat) <- !current_candidat;
  session.formations.(!index_formation) <- !current_formation

let consulte_propositions session ~nom_candidat =
  let current_candidat = ref session.candidats.(0) in
  for i=0 to (Array.length session.candidats - 1) do
    if session.candidats.(i).name = nom_candidat
      then begin
      current_candidat := session.candidats.(i);
      end
  done;
  !current_candidat.proposition_en_attente

let consulte_voeux_en_attente session ~nom_candidat = 
  let current_candidat = ref session.candidats.(0) in
  for i=0 to (Array.length session.candidats - 1) do
    if session.candidats.(i).name = nom_candidat
      then begin
      current_candidat := session.candidats.(i);
      end
  done;
  (Array.to_list !current_candidat.voeux_en_attente)
