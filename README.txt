FEATURES
Nous avons dans ce projet implémenté des algoritmes de mariages stables notamment celui de Knuth,
de Gale Shapley et un Algorithme abstrait utilisant une PIOCHE pouvant etre une pile ou une file. 

Grâce aux compétences acquises en implémentant les algoritmes mentionnés ci dessus, nous sommes
parvenus à écrire une version simplifée de l'algoritme de Parcoursup.

Il est interssant de noté que nous avons préféré utilisé des fonctions propres au module Option
notamment Option.get (v 4.8, source = https://ocaml.org/api/Option.html) pour récupéré les valeurs
de nos variables optionnelles contrairement à un modèle classique pattern matching.

L'algoritme de Parcoursup, possède la capacité d'organiser des sessions et d'attribuer à des étudiants 
des formations en prenant en compte le classement des étudiants par rapport à la formation et des formations
par rapport à chaque étudiant.

TESTS
Les séries de test proposées par notre enseignant nous ont permis d'avancer dans le projet en nous aidant
à relever nos erreurs et à corriger notre approche de la programmation fonctionnelle.

Dans le module Parcoursup, nous avons proposé 4 tests.

Un premier test dans lequel chaque étudiant active son répondeur automatique puis renonce a toutes leurs propositions,
Un deuxième test dans lequel aucun étudiant ne formule de voeux,
un troisième dans lequel chaque étudiant désactive son répondeur automatique et
un dernier test similaire au troisième avec comme complément un étudiant qui refuse une formation.

KNOWN ISSUES

Nous avons remarqué qu'en utilisant les anciennes version de OCAML, nous n'avons pas pu utilisé des fonctions du module
Option telle que Option.get qui sont plus recentes. Une alternative aurait été de reprogrammer nous meme une variante d'Option.get
comme fontcion usuelle grâce à un pattern matching.

En utilisant des fonctions telles que List.nth ou Array.get, la moindre erreur sur l'indice lève une exception qui
met fin à notre programme il fallait donc etre vigileant sur les indices ce qui nous a fait perdre du temps.