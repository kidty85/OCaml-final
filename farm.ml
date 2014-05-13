open Directions;;
module Images = Imagess;;
let perso = Imagess.pingu;;
(* Graphics.close_graph ();; *)
Graphics.open_graph " 900x900";;
Graphics.set_color (Graphics.rgb 200 200 200);;
Graphics.fill_rect 0 0 900 900;;
Graphics.set_color (Graphics.rgb 0 0 100);;
let f i =
  Graphics.draw_segments [|
      (50*i,0,50*i,900);
      (0,50*i,900,50*i)
     |];
in
List.iter f [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17];;

Graphics.remember_mode false;;

let afficher img (i,j) =
  Dessiner.dessiner_image img (!j * 50) (850 - !i * 50);;

let tilesNW = [(ref 0, ref 0)];;
let tilesNE = [(ref 17, ref 0)];;
let tilesSW = [(ref 0, ref 17)];;
let tilesSE = [(ref 17, ref 17)];;

let tilesN = [(ref 0, ref 1); (ref 0, ref 2); (ref 0, ref 3); (ref 0, ref 4); (ref 0, ref 5);
		(ref 0, ref 6); (ref 0, ref 7); (ref 0, ref 8); (ref 0, ref 9); (ref 0, ref 10);
		(ref 0, ref 11); (ref 0, ref 12); (ref 0, ref 13); (ref 0, ref 14); (ref 0, ref 15);
		(ref 0, ref 16)];;

let tilesS = [(ref 17, ref 1); (ref 17, ref 2); (ref 17, ref 3); (ref 17, ref 4); (ref 17, ref 5);
		(ref 17, ref 6); (ref 17, ref 7); (ref 17, ref 8); (ref 17, ref 9); (ref 17, ref 10);
		(ref 17, ref 11); (ref 17, ref 12); (ref 17, ref 13); (ref 17, ref 14); (ref 17, ref 15);
		(ref 17, ref 16)];;

let tilesW = [(ref 1, ref 0); (ref 2, ref 0); (ref 3, ref 0); (ref 4, ref 0); (ref 5, ref 0);
		(ref 6, ref 0); (ref 7, ref 0); (ref 8, ref 0); (ref 9, ref 0); (ref 10, ref 0);
		(ref 11, ref 0); (ref 12, ref 0); (ref 13, ref 0); (ref 14, ref 0); (ref 15, ref 0);
		(ref 16, ref 0)];;

let tilesE = [(ref 17, ref 1); (ref 17, ref 2); (ref 17, ref 3); (ref 17, ref 4); (ref 17, ref 5);
		(ref 17, ref 6); (ref 17, ref 7); (ref 17, ref 8); (ref 17, ref 9); (ref 17, ref 10);
		(ref 17, ref 11); (ref 17, ref 12); (ref 17, ref 13); (ref 17, ref 14); (ref 17, ref 15);
		(ref 17, ref 16)];;

let rocks = [(ref 6, ref 1);
		(ref 4, ref 2); (ref 5, ref 2); (ref 6, ref 2); (ref 7, ref 2);
		(ref 4, ref 3); (ref 7, ref 3); (ref 9, ref 3); (ref 11, ref 3);
		(ref 2, ref 4); (ref 3, ref 4); (ref 4, ref 4); (ref 6, ref 4); (ref 7, ref 4);
		(ref 9, ref 4); (ref 11, ref 4); (ref 12, ref 4); (ref 13, ref 4);
		(ref 4, ref 5); (ref 9, ref 5); (ref 10, ref 5); (ref 11, ref 5); (ref 13, ref 5);
		(ref 4, ref 6); (ref 12, ref 6); (ref 13, ref 6);
		(ref 3, ref 7); (ref 4, ref 7); (ref 5, ref 7); (ref 6, ref 7); (ref 11, ref 7);
		(ref 2, ref 8); (ref 3, ref 8); (ref 6, ref 8);
		(ref 6, ref 9);
		(ref 4, ref 10); (ref 5, ref 10); (ref 6, ref 10);
		(ref 4, ref 11); (ref 9, ref 11);
		(ref 10, ref 12);
		(ref 3, ref 13); (ref 4, ref 13); (ref 5, ref 13); (ref 6, ref 13);
		(ref 6, ref 14);
		(ref 6, ref 14);
		(ref 6, ref 14)];;

let trees = [(ref 1, ref 1); (ref 2, ref 1); (ref 3, ref 1); (ref 4, ref 1); (ref 5, ref 1);
		(ref 9, ref 1); (ref 10, ref 1);
		(ref 1, ref 2); (ref 9, ref 2);
		(ref 13, ref 3); (ref 14, ref 3);
		(ref 8, ref 4);
		(ref 6, ref 5);
		(ref 11, ref 6);
		(ref 2, ref 7);
		(ref 5, ref 8); (ref 13, ref 8);
		(ref 8, ref 9); (ref 10, ref 9); (ref 11, ref 9);
		(ref 1, ref 10); (ref 12, ref 10);
		(ref 12, ref 11); (ref 13, ref 11);
		(ref 1, ref 12); (ref 8, ref 12);
		(ref 8, ref 13);
		(ref 3, ref 14); (ref 7, ref 14);
		(ref 3, ref 15); (ref 7, ref 15);
		(ref 7, ref 16)];;

let spots = [(ref 2, ref 11); (ref 11, ref 2); (ref 9, ref 14); (ref 11, ref 14)];;


let dinos = [(ref 7, ref 1)];;
let elephants = [(ref 2, ref 2)];;
let cows = [(ref 6,  ref 3)];;
let lions = [(ref 10, ref 4)];;
let panthers = [(ref 12, ref 5)];;
let rabbits = [(ref 3, ref 6)];;
let cats = [(ref 9, ref 7)];;
let dogs = [(ref 5, ref 9)];;
let ducks = [(ref 13, ref 10)];;
let snakes = [(ref 1, ref 11)];;
let fishes = [(ref 6, ref 12)];;
let pigs = [(ref 9, ref 13)];;
let pandas = [(ref 13, ref 14)];;
let hippos = [(ref 4, ref 15)];;
let rhinos = [(ref 14, ref 16)];;

let perso_i, perso_j = ref 1, ref 14;;

let afficher_decor() =
  Graphics.synchronize();;

let afficher_perso() =
  afficher perso (perso_i,perso_j);;

let afficher_obstacles() =
  (*List.iter (afficher Images.tileNO) tilesNO;
  List.iter (afficher Images.tileNE) tilesNE;
  List.iter (afficher Images.tileSO) tilesSO;
  List.iter (afficher Images.tileSE) tilesSE;
  List.iter (afficher Images.tileN) tilesN;
  List.iter (afficher Images.tileS) tilesS;
  List.iter (afficher Images.tileO) tilesO;
  List.iter (afficher Images.tileE) tilesE;*)
  List.iter (afficher Images.spot) spots;;

let afficher_mobiles() =
  List.iter (afficher Images.cat) cats;
  List.iter (afficher Images.cow) cows;
  List.iter (afficher Images.dino) dinos;
  List.iter (afficher Images.dog) dogs;
  List.iter (afficher Images.duck) ducks;
  List.iter (afficher Images.elephant) elephants;
  List.iter (afficher Images.fish) fishes;
  List.iter (afficher Images.hippo) hippos;
  List.iter (afficher Images.lion) lions;
  List.iter (afficher Images.panda) pandas;
  List.iter (afficher Images.panther) panthers;
  List.iter (afficher Images.pig) pigs;
  List.iter (afficher Images.rabbit) rabbits;
  List.iter (afficher Images.rhino) rhinos;
  List.iter (afficher Images.snake) snakes;
  List.iter (afficher Images.rock) rocks;
  List.iter (afficher Images.tree) trees;
  afficher_perso();;

(*let teleporter =
    if (!perso_i = 2 && !perso_j = 11) then begin perso_i := 11; perso_j := 2; end
    if (!perso_i = 11 && !perso_j = 2) then begin perso_i := 2; perso_j := 11; end
    if (!perso_i = 14 && !perso_j = 9) then begin perso_i := 9; perso_j := 14; end
    if (!perso_i = 9 && !perso_j = 14) then begin perso_i := 14; perso_j := 9; end
;;*)

(*let verifier = 
  let check_animals(ri,rj) =
    int cpt = 0;
    if (perso_j
    if (rj = 16) then cpt = cpt+1;
    (*A COMPLETER*)
  in
  List.iter check_animals cats;
  List.iter check_animals cows;
  List.iter check_animals dinos;
  List.iter check_animals dogs;
  List.iter check_animals ducks;
  List.iter check_animals elephants;
  List.iter check_animals fishes;
  List.iter check_animals hippos;
  List.iter check_animals lions;
  List.iter check_animals pandas;
  List.iter check_animals panthers;
  List.iter check_animals pigs;
  List.iter check_animals rabbits;
  List.iter check_animals rhinos;
  List.iter check_animals snakes;
  afficher_decor();
  afficher_mobiles()
;;*)

let exploser direction =
  let explode_rock(ri,rj) = match direction with
    | N -> if ((!ri,!rj) = (!perso_i - 1, !perso_j)) then begin ri := -1; rj := 19; end
    | E -> if ((!ri,!rj) = (!perso_i, !perso_j + 1)) then begin ri := -1; rj := 19; end
    | W -> if ((!ri,!rj) = (!perso_i, !perso_j - 1)) then begin ri := -1; rj := 19; end
    | S -> if ((!ri,!rj) = (!perso_i + 1, !perso_j)) then begin ri := -1; rj := 19; end
  in
  List.iter explode_rock rocks;
  afficher_decor ();
  afficher_mobiles()
;;

let disparaitre direction = 
  let vanish_tree(ri,rj) = match direction with
    | N -> if (!ri = !perso_i - 1 && !rj = !perso_j) then begin ri := -1; rj := 18; end
    | E -> if (!ri = !perso_i && !rj = !perso_j + 1) then begin ri := -1; rj := 18; end
    | W -> if (!ri = !perso_i && !rj = !perso_j - 1) then begin ri := -1; rj := 18; end
    | S -> if (!ri = !perso_i + 1 && !rj = !perso_j) then begin ri := -1; rj := 18; end
  in
  List.iter vanish_tree trees;
  afficher_decor ();
  afficher_mobiles()
;;

let sauver direction = 
  let save_animal(ri,rj) = match direction with
    | N -> if (!ri = !perso_i - 1 && !rj = !perso_j) then begin rj := 16; end
    | E -> if (!ri = !perso_i && !rj = !perso_j + 1) then begin rj := 16; end
    | W -> if (!ri = !perso_i && !rj = !perso_j - 1) then begin rj := 16; end
    | S -> if (!ri = !perso_i + 1 && !rj = !perso_j) then begin rj := 16; end
  in
  List.iter save_animal cats;
  List.iter save_animal cows;
  List.iter save_animal dinos;
  List.iter save_animal dogs;
  List.iter save_animal ducks;
  List.iter save_animal elephants;
  List.iter save_animal fishes;
  List.iter save_animal hippos;
  List.iter save_animal lions;
  List.iter save_animal pandas;
  List.iter save_animal panthers;
  List.iter save_animal pigs;
  List.iter save_animal rabbits;
  List.iter save_animal rhinos;
  List.iter save_animal snakes;
  afficher_decor();
  afficher_mobiles()
;;

let aller direction =
  let () = match direction with
    | E -> perso_j := !perso_j + 1
    | W -> perso_j := !perso_j - 1
    | S -> perso_i := !perso_i + 1
    | N -> perso_i := !perso_i - 1
  in
  let bewareof_stepping(ri,rj) =
    if (!ri,!rj) = (!perso_i,!perso_j) then
      begin
        perso_i := 14;
        perso_j := 1;
      end
  in
  List.iter bewareof_stepping cats;
  List.iter bewareof_stepping cows;
  List.iter bewareof_stepping dinos;
  List.iter bewareof_stepping dogs;
  List.iter bewareof_stepping ducks;
  List.iter bewareof_stepping elephants;
  List.iter bewareof_stepping fishes;
  List.iter bewareof_stepping hippos;
  List.iter bewareof_stepping lions;
  List.iter bewareof_stepping pandas;
  List.iter bewareof_stepping panthers;
  List.iter bewareof_stepping pigs;
  List.iter bewareof_stepping rabbits;
  List.iter bewareof_stepping rhinos;
  List.iter bewareof_stepping snakes;
  List.iter bewareof_stepping rocks;
  List.iter bewareof_stepping trees;
  (*List.iter bewareof_stepping tilesN;
  List.iter bewareof_stepping tilesO;
  List.iter bewareof_stepping tilesS;
  List.iter bewareof_stepping tilesE;
  List.iter bewareof_stepping tilesNE;
  List.iter bewareof_stepping tilesNO;
  List.iter bewareof_stepping tilesSO;
  List.iter bewareof_stepping tilesSE;*)
  afficher_decor();
  afficher_mobiles()
;;

let go p = Interprete.run p aller;;
let save a = Interprete.runn a sauver;;
let vanish t = Interprete.runnn t disparaitre;;
let explode r = Interprete.runnnn r exploser;;
(*let teleport = teleporter;;
let check = verifier;;*)
let help = Interprete.aider;;

print_string " *** JELLY FARM HEROES SAGA, alternate version ***\n";;
print_string " Salut, Pingu le pingouin\nTu dois sauver tes amis animaux, tous sortis du pre!\n";;
print_string "Sauve-les, tous les animaux doivent etre dans le pre\n";;

print_string "\nBy MOUHAMAD Imame 11104625 & NGUYEN Quang Minh 10700214";;
print_string " Cr�dit image : lostgarden.com\tfun-lover.com\n";;
afficher_mobiles();;
afficher_obstacles();;
