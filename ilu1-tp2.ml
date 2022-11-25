let estZero_v1 n = 
  match n with 
  | 0 -> "zero";;

let estZero_v2 n =
  match n with 
  | 0 -> "zero"
  | _-> "nonZero" ;;

let voyelle voy = match voy with
  |'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | 'u'-> true
  | 'y' -> true
  | _ -> false ;;
  
let rang jour = match jour with 
  | "lundi" -> 1
  | "mardi" -> 2
  | "mercredi" -> 3
  | "jeudi" -> 4
  | "vendredi" -> 5
  | "samedi" -> 6
  | "dimanche" -> 7
  | _ -> 0 ;;

let jsem rang = match rang with
  | 1 -> "lundi"
  | 2 -> "mardi"
  | 3 -> "mercredi"
  | 4 -> "jeudi"
  | 5 -> "vendredi"
  | 6 -> "samedi"
  | 7 -> "dimanche"
  | _ -> "jour inconnu" ;;

let inf j1 j2 = match j1,j2 with 
  | "lundi","mardi" -> true
  | "mardi","mercredi" -> true
  | "mercredi","jeudi" -> true
  | "jeudi","vendredi" -> true
  | "vendredi","samedi" -> true
  | "samedi","dimanche" -> true
  | "dimanche","lundi" -> true
  |  _ -> false ;; 

let jourSucc1 jour = match jour with 
  | "lundi" -> "mardi"
  | "mardi" -> "mercredi"
  | "mercredi" -> "jeudi"
  | "jeudi" -> "vendredi"
  | "vendredi" -> "samedi"
  | "samedi" -> "dimanche"
  | "dimanche" -> "lundi"
  | _ -> "jour inconnu" ;;

let jourSucc2 jour =
  if rang jour = 0
  then jsem 0 else jsem ( if rang jour = 7 then 1 else ( rang jour + 1 ) ) ;;

let jourSucc3 jour =
  if rang jour = 0
  then jsem 0 else jsem ( ( rang jour mod 7) + 1 ) ;;

let jourPred1 jour = 
  match jour with
  | "lundi" -> "dimanche"
  | "mardi" -> "lundi"
  | "mercredi" -> "mardi"
  | "jeudi" -> "mercredi"
  | "vendredi" -> "jeudi"
  | "samedi" -> "vendredi"
  | "dimanche" -> "samedi"
  | _-> "jour inconnu" ;;

let jourPred2 = fun jour -> if rang jour = 0
  then jsem 0 else jsem ( if rang jour = 1 then 7 else ( rang jour - 1 ) ) ;; 

let jourPred3 = fun jour -> if rang jour = 0
  then jsem 0 else jsem ( ( rang jour + 5 ) mod 7 + 1 ) ;;


let bissextile = fun annee -> (annee mod 400 = 0)
                              || ( not (annee mod 100 = 0) && (annee mod 4 = 0) ) ;;

let nbjour = fun mois annee -> match mois with
  | 1 -> 31
  | 2 -> if bissextile annee then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31 
  |_-> failwith "Erreur";;
