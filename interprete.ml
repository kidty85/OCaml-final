open Directions;;

type instruction = East | West | North | South | Bloc of instruction list;;

let rec run p aller =
  match p with
  | East -> aller E
  | West -> aller W
  | South -> aller S
  | North -> aller N
  | Bloc (x::xs) -> run x aller; run (Bloc xs) aller
  | Bloc [] -> ();;

let rec runn a sauver =
  match a with
  | East -> sauver E
  | West -> sauver W
  | South -> sauver S
  | North -> sauver N
  | Bloc (x::xs) -> runn x sauver; runn (Bloc xs) sauver
  | Bloc [] -> ()
;;

let rec runnn t disparaitre =
  match t with
  | East -> disparaitre E
  | West -> disparaitre W
  | South -> disparaitre S
  | North -> disparaitre N
  | Bloc (x::xs) -> runnn x disparaitre; runnn (Bloc xs) disparaitre
  | Bloc [] -> ()
;;

let rec runnnn r exploser =
  match r with
  | East -> exploser E
  | West -> exploser W
  | South -> exploser S
  | North -> exploser N
  | Bloc (x::xs) -> runnnn x exploser; runnnn (Bloc xs) exploser
  | Bloc [] -> ()
;;

let aider =
  print_string"Il faut remettre tous les animaux entre les barrières\n";
  print_string"Quelques indices sur les commandes:\n";
  print_string"go dir : pour aller à la direction dir\n";
  print_string"save dir : pour sauver un animal situé à la direction dir\n";
  print_string"vanish dir : pour faire disparaitre un arbre situé à la direction dir\n";
  print_string"explode dir : pour exploser une roche situéé à la direction dir\n";
  print_string"teleport : pour se téléporter\n";
  print_string"check : pour mettre fin au jeu une fois toutes les tâches accomplies\n";
  print_string"help : pour afficher l'aide\n"
;;
