open Mariages_stables.Definitions

let prefere_of_classement classement =
  let n = Array.length classement in
  let rk = Array.make n 0 in
  for i = 0 to n - 1 do rk.(classement.(i)) <- i done;
  fun h1 h2 -> rk.(h1) < rk.(h2)

let prefere_of tab = Array.map prefere_of_classement tab

let seq_init n f =
  List.init n (fun i -> i) |> List.to_seq |> Seq.map f

let rec all_permutations n =
  if n = 0 then Seq.return [||] else
  let insere_a i tab =
    Array.init n (fun j -> 
      if j < i then tab.(j)
      else if j > i then tab.(j-1)
      else n - 1
    ) in
  all_permutations (n-1)
  |> Seq.flat_map (fun tab -> seq_init n (fun i -> insere_a (n-i-1) tab))

let product_of seq1 seq2 =
  seq1 |> Seq.flat_map (fun x -> 
    seq2 |> Seq.map (fun y -> (x,y))
  )

let append tab v = 
  let n = Array.length tab in
  Array.init (n+1) (fun i -> if i < n then tab.(i) else v)

let rec all_arrays n values = 
  if n = 0 then Seq.return [||] else
  product_of (all_arrays (n-1) values) values
  |> Seq.map (fun (tab, v) -> append tab v)

let toutes_les_entrees_de_taille n = 
  let all_tabs = all_arrays n (all_permutations n) in
  let all_pref = Seq.map prefere_of all_tabs in
  product_of all_tabs all_pref 
  |> Seq.map (fun (tab, pref) -> {
    n=n;
    liste_appel_de = tab;
    prefere = pref;
  })

let c = ref 0

type algo = ?affiche_config:bool -> entree -> sortie

let traite (algo:algo) entree= 
  c := !c + 1;
  print_endline (Format.sprintf "EXEMPLE %d" !c);
  print_entree entree;
  let sortie = algo ~affiche_config:true entree in
  print_sortie sortie

let run algo =
  for n=0 to 3 do
    Seq.iter (traite algo) (toutes_les_entrees_de_taille n)
  done