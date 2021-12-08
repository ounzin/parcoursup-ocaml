open Test_parcoursup_utils2
let session = nouvelle_session ()

let () =  
  ajoute_candidat session ~nom_candidat:"Adam";
  ajoute_candidat session ~nom_candidat:"Lisa";
  ajoute_formation session ~nom_formation:"Université de Monaco" ~capacite:1;
  ajoute_formation session ~nom_formation:"ENSE3 Grenoble" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Adam" 
    ~nom_formation:"Université de Monaco";
    ajoute_voeu session 
    ~rang_repondeur:(None)
    ~nom_candidat:"Adam" 
    ~nom_formation:"ENSE3 Grenoble";
    ajoute_voeu session 
    ~rang_repondeur:None
    ~nom_candidat:"Lisa" 
    ~nom_formation:"Université de Monaco";
    ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Lisa" 
    ~nom_formation:"ENSE3 Grenoble";
  ajoute_commission session 
    ~nom_formation:"Université de Monaco" 
    ["Lisa";"Adam"]; 
    ajoute_commission session 
    ~nom_formation:"ENSE3 Grenoble" 
    ["Adam"; "Lisa"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Adam";
  affiche_propositions_en_attente session "Adam";
  affiche_voeux_en_attente session "Lisa";
  affiche_propositions_en_attente session "Lisa";