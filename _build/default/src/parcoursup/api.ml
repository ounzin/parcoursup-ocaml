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
  mutable candidatures : string list; (* <- Céline, Adam, Ahmed *)
  mutable liste_dappel : string list; (* <- Adam, Ahmed, Céline *)
  mutable rang_dappel : int; (* rang max par jour = capacité *)
  mutable places_restante : int;
  mutable acceptations : string list;
  mutable renonces : string list;
} 

type commission = {
  mutable formation : string;
  mutable algo_local : (candidat1: string -> candidat2: string -> bool);
}

type session = {
  (*mutable name : string;*)
  mutable candidats : candidat array;
  mutable formations : formation array;
  mutable commissions : commission array;
}

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

let ajoute_voeu session ~rang_repondeur ~nom_candidat ~nom_formation =  (* <- reste si formation existe déjà *)
  
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
  !current_candidat.voeux_en_attente <- Array.append !current_candidat.voeux_en_attente [|(nom_formation, (List.length !current_formation.candidatures))|];
  session.candidats.(!index_candidat) <- !current_candidat;
  session.formations.(!index_formation) <- !current_formation;

  (* mise à jour du repondeur automatique : cas ou rang_repondeur est précisé ou non *)

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
  (* else begin  <- aucun rang n'est précisé 
      !current_candidat.repondeur_automatique <- Array.append !current_candidat.repondeur_automatique [||];
      session.candidats.(!index_candidat) <- !current_candidat;*)

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
      if fonction_current_formation ~candidat1:a ~candidat2:b  then 1 else -1 in
    
    !current_formation.liste_dappel <- List.sort algo_trieur !current_candidatures; (* <- on génère notre liste d'appel *)
    session.formations.(!index_formation) <- !current_formation; (* maj de formation dans la session *)

    (* maj du rang d'attente dans la liste voeux en attentes de candidats de la liste d'appel *)

    let nom_current_candidat = ref (List.nth !current_formation.liste_dappel 0)in (* <- on récupère le nom du premier candidat de notre liste d'appel *)

    for k=0 to (List.length !current_formation.liste_dappel - 1) do (* on applique le bloc à chaque element de notre liste d'appel *)
     
    nom_current_candidat := (List.nth !current_formation.candidatures k); (* <- maj nom candidat courant *)

      for t=0 to (Array.length session.candidats)-1 do 
        if !nom_current_candidat = session.candidats.(t).name (* on identifie notre candidat dans le tableau des candidats *)
          then begin (* <- on récupère son tableau de voeux en attente qu'on mettra à jour *)
            for z=0 to (Array.length session.candidats.(t).voeux_en_attente - 1) do
              if nom_current_formation = fst (session.candidats.(t).voeux_en_attente.(z)) 
                then session.candidats.(t).voeux_en_attente.(z) <- (nom_current_formation, k)
            done
          end
      done;
    done; (* <- fin de la maj des voeux en attentes *)
  done (* <- fin iter sur commissions *)

let nouveau_jour session =

  (* Définition de mes variables *)
  let current_candidat = ref session.candidats.(0) in
  let current_formation = ref session.formations.(0) in

  (* Proposition des formations aux étudiants en tenant compte des renonces *)

  for i=0 to (Array.length session.formations -1) do
    current_formation := session.formations.(i);
    (* Pour chacune des formation nous allons envoyer des candidatures en fonction du nombre de places restantes et des renonces *)
    
    while !current_formation.places_restante > 0 && !current_formation.rang_dappel < (List.length !current_formation.liste_dappel) do (* <- tant qu'il reste des place libre et qu'on peut avancer*)
     
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
          (*  on ajoute la formation à ses propositions en attente *)
          !potentiel_candidat.proposition_en_attente <- !current_formation.name::!potentiel_candidat.proposition_en_attente;
          !current_formation.rang_dappel <- !current_formation.rang_dappel + 1; (* <- on avance le rang d'appel *)
          !current_formation.places_restante <- !current_formation.places_restante - 1; (* <- on reduit le nombre de place libre *)
        end
    done; (* <- fin des propositions de la formation courante *)

    session.formations.(i) <- !current_formation; (* maj de ma formation courante *)
  done; (* <- fin des propostions de toutes les formations *)

  (* Pour chacun des candidats nous allons analyser les propositions reçues *)

  for i=0 to (Array.length session.candidats - 1) do
    current_candidat := session.candidats.(i);
    let testeur_repondeur_auto = try Some (!current_candidat.repondeur_automatique.(0)) with _ -> None in

    if testeur_repondeur_auto <> None (* <- le répondeur automatique est activé : on choisit le meilleur choix et on renonce aux autres *)
      then begin
        let a_renoncer = ref [] in
        let meilleur_choix = ref "" in
        let index_meilleur_choix = ref 0 in
        let meilleur_choix_trouve = ref false in

        for k=0 to (Array.length !current_candidat.repondeur_automatique -1) do (* <- on recupere l'index du meilleur choix *)
          if (!meilleur_choix_trouve <> false)
            then begin
              if List.mem !current_candidat.repondeur_automatique.(k) !current_candidat.proposition_en_attente
                then begin
                meilleur_choix := !current_candidat.repondeur_automatique.(k);
                index_meilleur_choix := k;
                meilleur_choix_trouve := true;
                end
            end;
        done;

        (* <- on remplit notre liste de formation à refuser *)
        for t=(!index_meilleur_choix) to (Array.length !current_candidat.repondeur_automatique -1) do
          a_renoncer := !current_candidat.repondeur_automatique.(t)::!a_renoncer;
        done; 

        (*<- on renonce aux formations moins bien classés*)

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

      end
  done

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

  (* on retire la formation des proposition si elle y est *)

  let new_propositions_en_attente = ref [] in
  for k=0 to (List.length !current_candidat.proposition_en_attente -1) do
    
    if nom_candidat <> (List.nth !current_candidat.proposition_en_attente k)
      then begin
        new_propositions_en_attente := (List.nth !current_candidat.proposition_en_attente k)::!new_propositions_en_attente;
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
