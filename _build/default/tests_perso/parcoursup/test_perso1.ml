open Test_parcoursup_utils2

let session = nouvelle_session ()

let () = 
  ajoute_candidat session ~nom_candidat:"Nabil";
  ajoute_candidat session ~nom_candidat:"Uriel";
  ajoute_formation session ~nom_formation:"Université Côte d'Azur" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université de Toulon" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Nabil" 
    ~nom_formation:"Université Côte d'Azur";
    ajoute_voeu session 
    ~rang_repondeur:(Some 2) 
    ~nom_candidat:"Nabil" 
    ~nom_formation:"Université de Toulon";
    ajoute_voeu session 
    ~rang_repondeur:(Some 1) 
    ~nom_candidat:"Uriel" 
    ~nom_formation:"Université Côte d'Azur";
    ajoute_voeu session 
    ~rang_repondeur:(Some 2) 
    ~nom_candidat:"Uriel" 
    ~nom_formation:"Université de Toulon";
  ajoute_commission session 
    ~nom_formation:"Université Côte d'Azur" 
    ["Nabil";"Uriel"]; 
    ajoute_commission session 
    ~nom_formation:"Université de Toulon" 
    ["Nabil";"Uriel"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Nabil";
  affiche_propositions_en_attente session "Nabil";
  affiche_voeux_en_attente session "Uriel";
  affiche_propositions_en_attente session "Uriel";
  renonce session  ~nom_candidat:"Nabil"  ~nom_formation:"Université de Toulon";
  renonce session  ~nom_candidat:"Nabil"  ~nom_formation:"Université Côte d'Azur";
  renonce session  ~nom_candidat:"Uriel"  ~nom_formation:"Université de Toulon";
  renonce session  ~nom_candidat:"Uriel"  ~nom_formation:"Université Côte d'Azur";
  nouveau_jour session;
  affiche_voeux_en_attente session "Nabil";
  affiche_propositions_en_attente session "Nabil";
  affiche_voeux_en_attente session "Uriel";
  affiche_propositions_en_attente session "Uriel";