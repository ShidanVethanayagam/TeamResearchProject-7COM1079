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


#Analysis Part

df <- Fat_Supply_Quantity_Data_final
n <- nrow(df)


# Histogram for Animal Products
png("hist_animal_products_analysis.png")
hist(df$Animal_Products,
     main = "Distribution of Animal Product Supply",
     xlab = "Animal Products (kg/capita/year)",
     ylab = "Number of Countries")
dev.off()

# Histogram for Undernourishment
png("hist_undernourishment_analysis.png")
hist(df$Undernourished,
     main = "Distribution of Undernourishment",
     xlab = "Undernourishment (%)",
     ylab = "Number of Countries")
dev.off()

# Scatterplot with linear fit
png("scatter_with_trend.png")
plot(df$Animal_Products, df$Undernourished,
     main = "Animal Products Supply vs Undernourishment",
     xlab = "Animal Products (kg/capita/year)",
     ylab = "Undernourishment (%)",
     pch = 19)
# add simple linear fit line
fit <- lm(Undernourished ~ Animal_Products, data = df)
abline(fit, col = "red")
dev.off()

##Testing code
#Normality checks (Shapiro-Wilk)
shapiro_ap <- shapiro.test(df$Animal_Products)

#Correlation tests
pearson_res <- cor.test(df$Animal_Products, df$Undernourished, method = "pearson")
spearman_res <- cor.test(df$Animal_Products, df$Undernourished, method = "spearman")
shapiro_und <- shapiro.test(df$Undernourished)


#To print the test results
print("=== Analysis summary ===")
print(paste("Sample size (n):", n))

print("Shapiro-Wilk (Animal_Products):")
print(paste("W =", format(shapiro_ap$statistic, digits = 4),
            "p =", format(shapiro_ap$p.value, digits = 4)))

print("Shapiro-Wilk (Undernourished):")
print(paste("W =", format(shapiro_und$statistic, digits = 4),
            "p =", format(shapiro_und$p.value, digits = 4)))

print("Pearson correlation:")
print(paste("r =", format(as.numeric(pearson_res$estimate), digits = 4),
            "p =", format(pearson_res$p.value, digits = 4)))

print("Spearman correlation:")
print(paste("rho =", format(as.numeric(spearman_res$estimate), digits = 4),
            "p =", format(spearman_res$p.value, digits = 4)))


