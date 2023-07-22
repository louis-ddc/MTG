library(rvest)
library(httr)
# Charger les bibliothèques nécessaires
library(rvest)
library(httr)

# Fonction pour récupérer le prix moyen d'une carte sur Cardmarket
recuperer_prix_moyen_carte <- function(nom_carte) {
  # Construire l'URL de recherche de la carte
  url_recherche <- paste0("https://www.cardmarket.com/fr/Magic/Cards/Search?searchString=", gsub(" ", "+", nom_carte))
  
  # Envoyer une requête GET pour récupérer la page
  page <- GET(url_recherche)
  
  # Vérifier que la requête a réussi
  if (status_code(page) == 200) {
    # Analyser le contenu HTML de la page
    content_page <- read_html(content(page, as = "text"))
    
    # Extraire le prix moyen de la carte
    prix_moyen <- content_page %>%
      html_nodes(".price-guide-table") %>%
      html_nodes(".avg-price") %>%
      html_text() %>%
      gsub(",", ".", .) %>%
      as.numeric()
    
    return(prix_moyen)
  } else {
    cat("Erreur lors de la récupération des données pour la carte", nom_carte, "\n")
    return(NULL)
  }
}

# Liste des cartes à récupérer les prix
liste_cartes <- c("Black Lotus", "Ancestral Recall", "Time Walk", "Mox Pearl", "Mox Sapphire")

# Créer un tableau pour stocker les résultats
resultats_prix <- data.frame(Nom_Carte = character(), Prix_Moyen_Euro = numeric(), stringsAsFactors = FALSE)

# Récupérer les prix pour chaque carte dans la liste
for (carte in liste_cartes) {
  prix_carte <- recuperer_prix_moyen_carte(carte)
  
  # Ajouter les résultats dans le tableau
  if (!is.null(prix_carte)) {
    resultats_prix <- rbind(resultats_prix, data.frame(Nom_Carte = carte, Prix_Moyen_Euro = prix_carte))
  }
}

# Afficher les résultats
print(resultats_prix)
# Charger les bibliothèques nécessaires
library(rvest)
library(httr)

# Fonction pour récupérer le prix moyen d'une carte sur Cardmarket
recuperer_prix_moyen_carte <- function(nom_carte) {
  # Construire l'URL de recherche de la carte
  url_recherche <- paste0("https://www.cardmarket.com/fr/Magic/Cards/Search?searchString=", gsub(" ", "+", nom_carte))
  
  # Envoyer une requête GET pour récupérer la page
  page <- GET(url_recherche)
  
  # Vérifier que la requête a réussi
  if (status_code(page) == 200) {
    # Analyser le contenu HTML de la page
    content_page <- read_html(content(page, as = "text"))
    
    # Extraire le prix moyen de la carte
    prix_moyen <- content_page %>%
      html_nodes(".price-guide-table") %>%
      html_nodes(".avg-price") %>%
      html_text() %>%
      gsub(",", ".", .) %>%
      as.numeric()
    
    return(prix_moyen)
  } else {
    cat("Erreur lors de la récupération des données pour la carte", nom_carte, "\n")
    return(NULL)
  }
}

# Exemple de tableau_final_filtre
tableau_final_filtre <- data.frame(
  Termes = c("Black Lotus", "Ancestral Recall", "Time Walk", "Mox Pearl", "Mox Sapphire"),
  Freq = c(5, 4, 3, 4, 3)
)

# Créer un tableau pour stocker les résultats
resultats_prix <- data.frame(Nom_Carte = character(), Prix_Moyen_Euro = numeric(), stringsAsFactors = FALSE)

# Récupérer les prix pour chaque carte dans tableau_final_filtre
for (i in seq(nrow(tableau_final_filtre))) {
  carte <- tableau_final_filtre$Termes[i]
  prix_carte <- recuperer_prix_moyen_carte(carte)
  
  # Ajouter les résultats dans le tableau
  if (!is.null(prix_carte)) {
    resultats_prix <- rbind(resultats_prix, data.frame(Nom_Carte = carte, Prix_Moyen_Euro = prix_carte))
  }
}

# Afficher les résultats
print(resultats_prix)

