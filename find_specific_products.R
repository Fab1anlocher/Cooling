library(readr)
library(dplyr)
library(stringr)

sales_data <- read_csv("DigitecLive_Cleaned.csv", col_types = cols())

cat("=== BLANKETS / DECKEN ===\n\n")
blankets <- sales_data %>%
  filter(str_detect(tolower(fullProductName), "decke|blanket|fleece|plaid")) %>%
  filter(!str_detect(tolower(fullProductName), "objektiv|beamer|wand")) %>%
  select(`infos.Category`, fullProductName, canton) %>%
  head(20)
print(blankets)
cat(sprintf("\nTotal blanket-related products: %d\n", nrow(blankets)))

cat("\n\n=== E-READERS ===\n\n")
ereaders <- sales_data %>%
  filter(`infos.Category` == "ereader-1138") %>%
  select(fullProductName, canton) %>%
  head(10)
print(ereaders)
total_ereaders <- sales_data %>% filter(`infos.Category` == "ereader-1138") %>% nrow()
cat(sprintf("\nTotal e-readers: %d\n", total_ereaders))

cat("\n\n=== WINE OPENERS / KORKENZIEHER ===\n\n")
wine_openers <- sales_data %>%
  filter(str_detect(tolower(fullProductName), "korkenzieher|weinöffner|wine.*opener|flaschenöffner.*wein")) %>%
  select(`infos.Category`, fullProductName, canton) %>%
  head(20)
print(wine_openers)
cat(sprintf("\nTotal wine openers: %d\n", nrow(wine_openers)))

# Check all product categories
cat("\n\n=== CHECKING ALL CATEGORIES FOR BETTER MATCHES ===\n")
all_cats <- unique(sales_data$`infos.Category`)
interesting <- grep("kuechenhelfer|haushalts|wohnen|textil|geschenk|accessoire", all_cats, ignore.case = TRUE, value = TRUE)
if(length(interesting) > 0) {
  cat("\nPotentially relevant categories:\n")
  for(cat in interesting) {
    count <- sum(sales_data$`infos.Category` == cat, na.rm = TRUE)
    cat(sprintf("  - %s: %d sales\n", cat, count))
  }
}
