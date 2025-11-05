library(readr)
library(dplyr)
library(stringr)

# Daten laden
sales_data <- read_csv("DigitecLive_Cleaned.csv", col_types = cols())

# Alle Kategorien
categories <- unique(sales_data$`infos.Category`)

# Suchbegriffe
search_terms <- c("decke", "kuscheldecke", "reader", "e-reader", "ebook", 
                  "wein", "korkenzieher", "flaschenöffner", "öffner", "blanket")

cat("Searching for relevant product categories:\n\n")

for(term in search_terms) {
  matches <- grep(term, categories, ignore.case = TRUE, value = TRUE)
  if(length(matches) > 0) {
    cat(sprintf("\n'%s' found in:\n", term))
    for(match in matches) {
      count <- sum(sales_data$`infos.Category` == match, na.rm = TRUE)
      cat(sprintf("  - %s: %d sales\n", match, count))
    }
  }
}

# Auch in Produktnamen suchen
cat("\n\n=== SEARCHING IN PRODUCT NAMES ===\n\n")

for(term in search_terms) {
  matches <- sales_data %>%
    filter(str_detect(tolower(fullProductName), tolower(term))) %>%
    group_by(`infos.Category`) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  if(nrow(matches) > 0) {
    cat(sprintf("\n'%s' found in product names:\n", term))
    print(matches)
  }
}
