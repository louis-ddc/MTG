# Charger les bibliothèques nécessaires
library(dplyr)

# Créer une fonction pour compter les occurrences des termes dans un tableau
compter_termes <- function(tableau) {
  tableau %>%
    table() %>%
    as.data.frame() %>%
    arrange(desc(Freq)) %>%
    rename(Termes = ".")
}

# Fonction principale pour créer le tableau final
creer_tableau_occurrences <- function(liste_tableaux) {
  # Combinaison des tableaux en une seule liste
  tous_les_termes <- unlist(liste_tableaux)
  
  # Compter les occurrences de chaque terme
  comptage <- compter_termes(tous_les_termes)
  
  # Filtrer les lignes avec une fréquence supérieure ou égale au nombre de tableaux moins deux
  nb_tableaux <- length(liste_tableaux)
  seuil <- nb_tableaux - 2
  tableau_final <- comptage %>%
    filter(Freq >= seuil)
  
  return(tableau_final)
}

# Charger les tableaux à partir des fichiers texte (.txt)
tableau1 <- readLines("deck1.txt")
tableau2 <- readLines("deck2.txt")
tableau3 <- readLines("deck3.txt")

# Créer le tableau final des occurrences
tableau_final <- creer_tableau_occurrences(list(tableau1, tableau2, tableau3))

# Filtrer le nouveau tableau avec les fréquences supérieures à 2 et les termes présents dans le tableau1
tableau_final_filtre <- tableau_final %>%
  filter(Freq > 2 & Termes %in% tableau1)

# Afficher le nouveau tableau filtré dans la console
print(tableau_final_filtre)
write.csv(tableau_final_filtre, file = "tableau_final_filtre.txt", row.names = FALSE)
