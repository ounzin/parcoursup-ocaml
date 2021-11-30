type candidat = {
  mutable name : string;
  mutable repondeur_automatique : string array;
  mutable voeux : string array;
  mutable voeux_en_attente : string array;
  mutable proposition_en_attente : string array;
}

type formation = {
  mutable name : string;
  mutable capacite : int;
  mutable candidatures : string list; (* <- Céline, Adam, Ahmed *)
  mutable liste_dappel : string list; (* <- Adam, Ahmed, Céline *)
  mutable rang_dappel : int; (* rang max par jour = capacité *)
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
    proposition_en_attente = [||];
  } in
  session.candidats <- Array.append session.candidats [|new_candidat|]

let ajoute_formation session ~nom_formation ~capacite =
  let new_formation = {
    name = nom_formation;
    capacite = capacite;
    candidatures = [];
    liste_dappel = [];
    rang_dappel = 0;
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
  else begin (* <- aucun rang n'est précisé *)
      !current_candidat.repondeur_automatique <- Array.append !current_candidat.repondeur_automatique [|nom_formation|];
      session.candidats.(!index_candidat) <- !current_candidat;
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
      if fonction_current_formation ~candidat1:a ~candidat2:b  then 1 else -1 in
    
    !current_formation.liste_dappel <- List.sort algo_trieur !current_candidatures; (* <- on génère notre liste d'appel *)
    session.formations.(!index_formation) <- !current_formation; (* maj de formation dans la session *)
  done (* <- fin iter sur commissions *)

let nouveau_jour session =
  ignore session;
  failwith "non implémenté nouveau jour"

let renonce session ~nom_candidat ~nom_formation = 
  ignore session;
  ignore nom_candidat;
  ignore nom_formation;
  failwith "non implémenté renonce"

let consulte_propositions session ~nom_candidat =
  ignore session;
  ignore nom_candidat;
  failwith "non implémenté consulte_propositions"

let consulte_voeux_en_attente session ~nom_candidat = 
  ignore session;
  ignore nom_candidat;
  failwith "non implémenté consulte_voeux_en_attente"
