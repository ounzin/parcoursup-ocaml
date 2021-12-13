(*
fonctionnalité testée:
""""
    Après appel de nouveau_jour, toute formation a soit épuisé sa liste d'appel,
    soit autant de propositions en attente la concernant que son nombre de places.
""""
*)


open Test_parcoursup_utils
let session = nouvelle_session ()

let () = 
  let for_all_candidats f = List.iter f ["a";"b";"c";"d";"e";"f"] in
  for_all_candidats (fun x -> ajoute_candidat session ~nom_candidat:x);
  ajoute_formation session ~nom_formation:"A" ~capacite:2;
  ajoute_commission session ~nom_formation:"A" ["a";"b";"c";"d";"e";"f"];
  for_all_candidats (fun x -> ajoute_voeu session ~rang_repondeur:None ~nom_candidat:x ~nom_formation:"A");
  reunit_commissions session;
  nouveau_jour session;
  let affiche () = for_all_candidats (fun x -> 
    affiche_propositions_en_attente session x;
    affiche_voeux_en_attente session x) in
  affiche();
  renonce session ~nom_candidat:"a" ~nom_formation:"A";
  renonce session ~nom_candidat:"c" ~nom_formation:"A";
  nouveau_jour session;
  affiche();
  renonce session ~nom_candidat:"b" ~nom_formation:"A";
  renonce session ~nom_candidat:"d" ~nom_formation:"A";
  nouveau_jour session;
  affiche();
  renonce session ~nom_candidat:"e" ~nom_formation:"A";
  nouveau_jour session;
  affiche();
