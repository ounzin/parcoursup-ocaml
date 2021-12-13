(*
fonctionnalité testée:
""""
    Un voeu pour lequel un rang n'a pas été exprimé est un voeu avec un rang "infini"
    (en quelque sorte, "le répondeur automatique n'a pas été activé").
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
  ajoute_voeu session ~rang_repondeur:None ~nom_candidat:"a" ~nom_formation:"B";
  reunit_commissions session;
  nouveau_jour session;
  affiche_propositions_en_attente session "a";
  affiche_voeux_en_attente session "a"