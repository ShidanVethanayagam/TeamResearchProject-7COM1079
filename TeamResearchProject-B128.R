#Dataset Cleaning code


Fat_Supply_Quantity_Data <- read_csv("Fat_Supply_Quantity_Data.csv", show_col_types = FALSE)
library(tidyverse)

relevant_names <- c("Country", "Animal Products", "Undernourished")
existing_relevant <- intersect(relevant_names, colnames(Fat_Supply_Quantity_Data))

Fat_Supply_Quantity_Data <- Fat_Supply_Quantity_Data %>%
  select(all_of(relevant_names))

Fat_Supply_Quantity_Data$`Animal Products` <- as.character(Fat_Supply_Quantity_Data$`Animal Products`)
Fat_Supply_Quantity_Data$Undernourished   <- as.character(Fat_Supply_Quantity_Data$Undernourished)

#Replace common text placeholders with NA
Fat_Supply_Quantity_Data <- Fat_Supply_Quantity_Data %>%
  mutate(
    Undernourished = if_else(tolower(Undernourished) %in% c("no data", "na", "", "n/a"), NA_character_, Undernourished)
  )
#Parse numeric values safely 
Fat_Supply_Quantity_Data <- Fat_Supply_Quantity_Data %>%
  mutate(
    Animal_Products_num = readr::parse_number(`Animal Products`),
    Undernourished_num  = readr::parse_number(Undernourished)
  )

#Remove rows with missing numeric values in either variable
Fat_Supply_Quantity_Data_clean <- Fat_Supply_Quantity_Data %>%
  filter(!is.na(Animal_Products_num) & !is.na(Undernourished_num))

#final tidy dataframe with clear column names
Fat_Supply_Quantity_Data_final <- Fat_Supply_Quantity_Data_clean %>%
  select(Country, Animal_Products = Animal_Products_num, Undernourished = Undernourished_num)

#View cleaned colums
View(Fat_Supply_Quantity_Data_final)

##Visualisation part

#Histogram for Animal products
df <- Fat_Supply_Quantity_Data_final
png("hist_animal_products.png")
hist(df$Animal_Products,
     main = "Distribution of Animal Product Supply",
     xlab = "Animal Products (kg/capita/year)",
     ylab = "Number of Countries")
dev.off()
#Histogram for Undernourishment column
png("hist_undernourishment.png")
hist(df$Undernourished,
     main = "Distribution of Undernourishment",
     xlab = "Undernourishment (%)",
     ylab = "Number of Countries")
dev.off()

#Scatter Plot
png("scatter_animal_vs_undernourishment.png")
plot(df$Animal_Products,
     df$Undernourished,
     main = "Animal Products Supply vs Undernourishment",
     xlab = "Animal Products (kg/capita/year)",
     ylab = "Undernourishment (%)",
     pch = 19)
dev.off()

