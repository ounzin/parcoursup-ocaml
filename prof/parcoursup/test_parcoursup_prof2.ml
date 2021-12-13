(*
fonctionnalité testée:
""""
    Si on appelle deux fois ajoute_voeu pour un même candidat et une même formation,
    c'est le rang utilisé au second appel qui est retenu 
""""
*)

open Test_parcoursup_utils
let session = nouvelle_session ()

let () = 
  ajoute_candidat session ~nom_candidat:"a";
  ajoute_formation session ~nom_formation:"A" ~capacite:1;
  ajoute_formation session ~nom_formation:"B" ~capacite:1;
  ajoute_commission session ~nom_formation:"A" ["a"];
  ajoute_commission session ~nom_formation:"B" ["a"];
  ajoute_voeu session ~rang_repondeur:(Some 1) ~nom_candidat:"a" ~nom_formation:"A";
  ajoute_voeu session ~rang_repondeur:(Some 2) ~nom_candidat:"a" ~nom_formation:"B";
  ajoute_voeu session ~rang_repondeur:(Some 3) ~nom_candidat:"a" ~nom_formation:"A";
  reunit_commissions session;
  nouveau_jour session;
  affiche_propositions_en_attente session "a";
  affiche_voeux_en_attente session "a"