(*
fonctionnalité testée:

"""
    Attention, on doit pouvoir travailler avec plusieurs sessions simultanément
    sans qu'il y ait d'interférences, par exemple une session "parcoursup belge 2030" 
    et une autre session "parcoursup français 2030".
"""

*)

open Test_parcoursup_utils

let ajoute_candidat i session ~nom_candidat =
  Format.printf "[SESSION %d] " i;
  ajoute_candidat session ~nom_candidat

let ajoute_formation i session ~nom_formation ~capacite =
  Format.printf "[SESSION %d] " i;
  ajoute_formation session ~nom_formation ~capacite

let ajoute_commission i session ~nom_formation tab =
  Format.printf "[SESSION %d] " i;
  ajoute_commission session ~nom_formation tab

let ajoute_voeu i session ~rang_repondeur ~nom_candidat ~nom_formation =
  Format.printf "[SESSION %d] " i;
  ajoute_voeu session ~rang_repondeur ~nom_candidat ~nom_formation

let affiche_voeux_en_attente i session s =
  Format.printf "[SESSION %d] " i;
  affiche_voeux_en_attente session s

let affiche_propositions_en_attente i session s =
  Format.printf "[SESSION %d] " i;
  affiche_propositions_en_attente session s
  
let session = Array.init 4 (fun _ -> nouvelle_session ()) 

let () = 
  for i = 0 to 1 do
    ajoute_candidat i session.(i) ~nom_candidat:"a";
    ajoute_formation i session.(i) ~nom_formation:"A" ~capacite:1;
    ajoute_formation i session.(i) ~nom_formation:"B" ~capacite:1;
    ajoute_commission i session.(i) ~nom_formation:"A" ["a"]; 
    ajoute_commission i session.(i) ~nom_formation:"B" ["a"]
  done;
  ajoute_voeu 0 session.(0) ~rang_repondeur:(Some 1) ~nom_candidat:"a" ~nom_formation:"A";
  ajoute_voeu 0 session.(0) ~rang_repondeur:(Some 2) ~nom_candidat:"a" ~nom_formation:"B";
  ajoute_voeu 1 session.(1) ~rang_repondeur:(Some 1) ~nom_candidat:"a" ~nom_formation:"B";
  ajoute_voeu 1 session.(1) ~rang_repondeur:(Some 2) ~nom_candidat:"a" ~nom_formation:"A";
  for i = 0 to 1 do
    reunit_commissions session.(i);
    nouveau_jour session.(i);
    affiche_voeux_en_attente i session.(i) "a";
    affiche_propositions_en_attente i session.(i) "a"
  done;
  for i = 2 to 3 do
    ajoute_candidat i session.(i) ~nom_candidat:"a";
    ajoute_candidat i session.(i) ~nom_candidat:"b";
    ajoute_formation i session.(i) ~nom_formation:"A" ~capacite:1;
    ajoute_voeu i session.(i) ~rang_repondeur:None ~nom_candidat:"a" ~nom_formation:"A";
    ajoute_voeu i session.(i) ~rang_repondeur:None ~nom_candidat:"b" ~nom_formation:"A";
  done;
  ajoute_commission 2 session.(2) ~nom_formation:"A" ["a";"b"]; 
  ajoute_commission 3 session.(3) ~nom_formation:"A" ["b";"a"]; 
  for i = 2 to 3 do
    reunit_commissions session.(i);
    nouveau_jour session.(i);
    affiche_voeux_en_attente i session.(i) "a";
    affiche_voeux_en_attente i session.(i) "b";
  done;
