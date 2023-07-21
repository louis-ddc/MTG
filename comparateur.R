# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)

# Créer une fonction pour compter les occurrences des termes dans un tableau
compter_termes <- function(tableau) {
  tableau %>%
    unlist() %>%
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
  
  return(comptage)
}

# Exemple d'utilisation avec 3 tableaux (vous pouvez en ajouter autant que vous le souhaitez)
tableau1 <- read.csv("deck1.csv")
tableau2 <- read.csv("deck2.csv")
tableau3 <- read.csv("deck3.csv")

# Créer le tableau final des occurrences
tableau_final <- creer_tableau_occurrences(list(tableau1, tableau2, tableau3))

# Sauvegarder le résultat au format CSV
write.csv(tableau_final, file = "tableau_occurrences.csv", row.names = FALSE)

# Afficher le tableau final dans la console
print(tableau_final)
