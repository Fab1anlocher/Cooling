library(readr)
library(dplyr)

sales_data <- read_csv("DigitecLive_Cleaned.csv", col_types = cols())

# Kategorien mit mindestens 100 Verkäufen
cat_summary <- sales_data %>%
  filter(!is.na(`infos.Category`)) %>%
  group_by(`infos.Category`) %>%
  summarise(total_sales = n()) %>%
  filter(total_sales >= 100) %>%
  arrange(desc(total_sales))

cat("Top product categories with 100+ sales:\n\n")
print(cat_summary, n = 50)

# Interessante Kategorien für Einzelpersonen
cat("\n\n=== INTERESTING SOLO-LIVING RELATED CATEGORIES ===\n\n")
solo_keywords <- c("kaffee", "mikrowelle", "toaster", "einzeln", "personal", 
                   "wasserkocher", "luftbefeuchter", "aromadiffusor",
                   "kopfhoerer", "headset", "streaming", "fernseher")

for(keyword in solo_keywords) {
  matches <- grep(keyword, cat_summary$`infos.Category`, ignore.case = TRUE, value = TRUE)
  if(length(matches) > 0) {
    cat(sprintf("\n'%s' categories:\n", keyword))
    for(match in matches) {
      count <- cat_summary %>% filter(`infos.Category` == match) %>% pull(total_sales)
      cat(sprintf("  - %s: %d sales\n", match, count))
    }
  }
}
