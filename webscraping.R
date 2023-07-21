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
    cat("Erreur lors de la récupération des données.\n")
    return(NULL)
  }
}

# Exemple d'utilisation pour récupérer le prix moyen du "Black Lotus"
nom_carte <- "Black Lotus"
prix_moyen_carte <- recuperer_prix_moyen_carte(nom_carte)

# Afficher le prix moyen de la carte dans la console
if (!is.null(prix_moyen_carte)) {
  cat("Le prix moyen de la carte", nom_carte, "est de", prix_moyen_carte, "euros.\n")
} else {
  cat("Impossible de récupérer le prix moyen de la carte", nom_carte, ". Vérifiez le nom de la carte ou réessayez plus tard.\n")
}
