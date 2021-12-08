open Test_parcoursup_utils2

let session = nouvelle_session ()

let () =
  ajoute_candidat session ~nom_candidat:"Valérian";
  ajoute_candidat session ~nom_candidat:"Elodie";
  ajoute_formation session ~nom_formation:"Polytech Paris-Saclay" ~capacite:1;
  ajoute_formation session ~nom_formation:"Université Toulouse III" ~capacite:1;
  ajoute_commission session 
    ~nom_formation:"Polytech Paris-Saclay" 
    ["Valérian";"Elodie"]; 
    ajoute_commission session 
    ~nom_formation:"Université Toulouse III" 
    ["Valérian";"Elodie"]; 
  reunit_commissions session;
  nouveau_jour session;
  affiche_voeux_en_attente session "Valérian";
  affiche_propositions_en_attente session "Valérian";
  affiche_voeux_en_attente session "Elodie";
  affiche_propositions_en_attente session "Elodie";