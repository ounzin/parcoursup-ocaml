open Test_parcoursup_utils2
let session = nouvelle_session ()

let () =  
  ajoute_candidat session ~nom_candidat:"Ahmed";
  ajoute_candidat session ~nom_candidat:"Houefa";
  ajoute_formation session ~nom_formation:"IUT Lokossa" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université Grenoble Alpes" ~capacite:1;
  ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Ahmed" 
    ~nom_formation:"IUT Lokossa";
    ajoute_voeu session 
    ~rang_repondeur:(None)
    ~nom_candidat:"Ahmed" 
    ~nom_formation:"Université Grenoble Alpes";
    ajoute_voeu session 
    ~rang_repondeur:(None)
    ~nom_candidat:"Houefa" 
    ~nom_formation:"IUT Lokossa";
    ajoute_voeu session 
    ~rang_repondeur:(None) 
    ~nom_candidat:"Houefa" 
    ~nom_formation:"Université Grenoble Alpes";
  ajoute_commission session 
    ~nom_formation:"IUT Lokossa" 
    ["Houefa";"Ahmed"]; 
    ajoute_commission session 
    ~nom_formation:"Université Grenoble Alpes" 
    ["Houefa";"Ahmed"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Ahmed";
  affiche_propositions_en_attente session "Ahmed";
  affiche_voeux_en_attente session "Houefa";
  affiche_propositions_en_attente session "Houefa";
  renonce session  ~nom_candidat:"Houefa"  ~nom_formation:"IUT Lokossa";
  nouveau_jour session;
  affiche_voeux_en_attente session "Ahmed";
  affiche_propositions_en_attente session "Ahmed";
  affiche_voeux_en_attente session "Houefa";
  affiche_propositions_en_attente session "Houefa";