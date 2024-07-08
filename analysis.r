library(tidyverse)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(RColorBrewer)
library(reshape2)

paired_colors <- brewer.pal(12, "Paired")
paired_colors <- c(brewer.pal(12, "Paired"), "#808080")  # Added an additional gray color

category_colors <- c(
  "Diagnostics" = paired_colors[1],
  "Prognostics" = paired_colors[2],
  "Modelling" = paired_colors[3],
  "Surveillance" = paired_colors[4],
  "Forecasting" = paired_colors[5]
)

# File paths
file_path_disease <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/disease.xlsx"
file_path_country <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/country.xlsx"
file_path_year_category <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/domain_trend.xlsx"
file_path_disease_year <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/disease_trend.xlsx"
file_path_model <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/modeltype.xlsx"
file_path_data <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/datatype.xlsx"
file_path_performance <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/performance_domain.xlsx"
file_path_performance_model <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/modeltype.xlsx"
file_path_performance_data <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/performance_data.xlsx"
file_path_performance_disease <- "/Users/gijsdanoe/Documents/PhD UMCG/Systematische review AI for ID/Tables/disease_trend.xlsx"

#### DISEASE COUNT ####
df_disease <- read_excel(file_path_disease) %>%
  select(Disease, Category)

disease_category_counts <- df_disease %>%
  separate_rows(Disease, sep = ",\\s*") %>%
  filter(Disease != "") %>%
  count(Disease, Category, name = "Count") %>%
  add_count(Disease) %>%
  mutate(Disease = ifelse(n <= 1, 'Other', Disease)) %>%
  group_by(Disease, Category) %>%
  summarise(Count = sum(Count), .groups = 'drop')

disease_totals <- disease_category_counts %>%
  group_by(Disease) %>%
  summarise(TotalCount = sum(Count)) %>%
  arrange(desc(TotalCount))

disease_category_counts$Disease <- factor(disease_category_counts$Disease, levels = disease_totals$Disease)

disease_category_counts <- disease_category_counts %>%
  group_by(Disease) %>%
  mutate(Category = fct_reorder(Category, Count)) %>%
  ungroup()

ggplot(disease_category_counts, aes(x = Disease, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Disease", y = "Count", fill = "Category") + 
  guides(fill = guide_legend(title = "Domain")) +
  scale_fill_manual(values = category_colors) 

#### COUNTRY HEAT MAP ####
df_country <- read_excel(file_path_country) %>%
  select(Country, Category)

country_counts <- df_country %>%
  group_by(Country) %>%
  summarise(Count = n()) %>%
  ungroup()

world <- ne_countries(scale = "medium", returnclass = "sf")
merged_data <- merge(world, country_counts, by.x = "name", by.y = "Country", all.x = TRUE)
color_ramp <- colorRampPalette(c("#FFCCCC", "#800000")) # Custom ramp from light pink to deep maroon

ggplot(data = merged_data %>% filter(name != "Antarctica")) +
  geom_sf(aes(fill = Count), color = "grey70") +
  scale_fill_gradientn(colors = color_ramp(100), name = "Paper Count") + # Use 100 colors for smooth gradient
  scale_fill_viridis_c(direction = -1, na.value = "grey85") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_colorbar(width = 100))

#### PAPERS PER YEAR ####
df_yc <- read_excel(file_path_year_category)

count_table_yc <- df_yc %>%
  count(Year, Category)

count_table_yc <- count_table_yc %>%
  group_by(Category) %>%
  mutate(Sum_n = sum(n)) %>%
  ungroup() %>%
  mutate(Category = reorder(Category, Sum_n)) %>%
  select(-Sum_n) # Removing the temporary Sum_n column

ggplot(count_table_yc, aes(x = Year, y = n, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = min(count_table_yc$Year):max(count_table_yc$Year)) + # Ensure every year is shown
  theme_minimal() +
  labs(y = "Count", x = "Year", fill = "Category") +
  scale_fill_manual(values = category_colors) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill = guide_legend(title = "Domain"))

#### DISEASES PER YEAR ####
df_dc <- read_excel(file_path_disease_year)

df_filtered <- df_dc %>%
  filter(Disease != "Various")

disease_counts <- df_filtered %>%
  count(Disease, name = "total_count")

df_with_other <- df_filtered %>%
  left_join(disease_counts, by = "Disease") %>%
  mutate(Disease = ifelse(total_count < 3, "Other", Disease))

top_diseases <- df_with_other %>%
  count(Year, Disease, Category, sort = TRUE)

color_palette <- c(brewer.pal(12, "Paired"), brewer.pal(3, "Dark2"))

ggplot(top_diseases, aes(x = Year, y = n, fill = Disease)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(breaks = min(top_diseases$Year):max(top_diseases$Year)) +
  theme_minimal() +
  labs(y = "Count", x = "Year", fill = "Disease") + 
  scale_fill_manual(values = color_palette) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x-axis labels for better readability
  facet_wrap("Category", nrow = 5, scales = 'free_y')

#### MODEL CATEGORY TREND ####
df_model <- read_excel(file_path_model, skip = 1)

colors_paired <- brewer.pal(12, "Paired")

extended_colors <- c(colors_paired, "#808080")

all_years <- sort(unique(df_model$Year))
df_model$Year <- factor(df_model$Year, levels = all_years)

ggplot(data = df_model, aes(x = Year, fill = `Model Category`)) + 
  geom_bar(position = "stack") +
  facet_wrap(~Category, nrow = 5, scales = 'free_y') +
  scale_x_discrete(drop = FALSE) +
  theme_minimal() + # Start with the minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x-axis labels 90 degrees
  labs(y = "Count", x = "Year", fill = "Model Category") +
  scale_fill_brewer(palette = 'Paired')

#### MODEL COUNT ####
df_counts <- df_model %>%
  count(`Model Category`, Category)

df_totals <- df_counts %>%
  group_by(`Model Category`) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))

df_counts <- df_counts %>%
  mutate(`Model Category` = factor(`Model Category`, levels = df_totals$`Model Category`)) %>%
  arrange(`Model Category`, n)

df_counts <- df_counts %>%
  group_by(`Model Category`) %>%
  mutate(Category = factor(Category, levels = unique(Category[order(n)]))) %>%
  ungroup()

ggplot(df_counts, aes(x = `Model Category`, y = n, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Model Category",
       y = "Count",
       fill = "Domain") +
  scale_fill_manual(values = category_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### DATA COUNT ####
df_data <- read_excel(file_path_data)

df_count <- df_data %>%
  count(`Data Category`, Category) %>%
  arrange(desc(n))

df_count <- df_count %>%
  group_by(`Data Category`) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(`Data Category` = reorder(`Data Category`, -total_count))

df_count <- df_count %>%
  group_by(`Data Category`) %>%
  mutate(Category = reorder(Category, n)) %>%
  ungroup()

ggplot(df_count, aes(x = `Data Category`, y = n, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Data Category",
       y = "Count") +
  scale_fill_manual(values = category_colors) + 
  guides(fill = guide_legend(title = "Domain")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### DATA TREND ####
all_years <- sort(unique(df_data$Year))
df_data$Year <- factor(df_data$Year, levels = all_years)

ggplot(data = df_data, aes(x = Year, fill = `Data Category`)) + 
  geom_bar(position = "stack") +
  facet_wrap(~Category, nrow = 5, scales = 'free_y') +
  scale_x_discrete(drop = FALSE) +
  theme_minimal() + # Start with the minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x-axis labels 90 degrees
  labs(y = "Count", x = "Year", fill = "Data Category") +
  scale_fill_brewer(palette = 'Paired')

#### PERFORMANCE ####
# DOMAIN
df_pf <- read_excel(file_path_performance)

df_long <- df_pf %>%
  mutate(Year = as.factor(Year)) %>%
  pivot_longer(cols = c(Accuracy, `F1 Score`, AUC, MSE, Specificity, Precision, Recall), 
               names_to = "Metric", 
               values_to = "Score") %>%
  mutate(Score = as.numeric(Score)) %>%
  mutate(Score = ifelse(Metric == "MSE", log10(Score), Score))

ggplot(df_long, aes(x = Year, y = Score, color = Category)) + 
  geom_point() +
  facet_wrap(~ Metric, nrow = 2, scales = 'free_y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Performance Score", 
       x = "Year",
       color = "Domain") +
  scale_color_brewer(palette = "Paired") 

# MODEL
df_pf_mo <- read_excel(file_path_performance_model, skip = 1)

df_long_mo <- df_pf_mo %>%
  mutate(Year = as.factor(Year)) %>%
  pivot_longer(cols = c(Accuracy, `F1 Score`, AUC, MSE, Specificity, Precision, Recall), 
               names_to = "Metric", 
               values_to = "Score") %>%
  mutate(Score = as.numeric(Score)) %>%
  mutate(Score = ifelse(Metric == "MSE", log10(Score), Score))

ggplot(df_long_mo, aes(x = Year, y = Score, color = `Model Category`)) + 
  geom_point() +
  facet_wrap(~ Metric, nrow = 2, scales = 'free_y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Performance Score", 
       x = "Year",
       color = "Model Category") +
  scale_color_brewer(palette = "Paired") 

# DATA
df_pf_da <- read_excel(file_path_performance_data)

df_long_da <- df_pf_da %>%
  mutate(Year = as.factor(Year)) %>%
  pivot_longer(cols = c(Accuracy, `F1 Score`, AUC, MSE, Specificity, Precision, Recall), 
               names_to = "Metric", 
               values_to = "Score") %>%
  mutate(Score = as.numeric(Score)) %>%
  mutate(Score = ifelse(Metric == "MSE", log10(Score), Score))

ggplot(df_long_da, aes(x = Year, y = Score, color = `Data Category`)) + 
  geom_point() +
  facet_wrap(~ Metric, nrow = 2, scales = 'free_y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Performance Score", 
       x = "Year",
       color = "Data Type") +
  scale_color_brewer(palette = "Paired") 

# DISEASE
df_pf_di <- read_excel(file_path_performance_disease)

df_expanded_di <- df_pf_di %>%
  separate_rows(Disease, sep = ", |and ")

disease_counts_di <- df_expanded_di %>%
  count(Disease) %>%
  rename(Count_di = n)

df_expanded_di <- df_expanded_di %>%
  left_join(disease_counts_di, by = "Disease")

df_expanded_di <- df_expanded_di %>%
  mutate(Disease = ifelse(Count_di < 3, "Other", Disease)) %>%
  select(-Count_di)

df_long_di <- df_expanded_di %>%
  mutate(Year = as.factor(Year)) %>%
  pivot_longer(cols = c(Accuracy, `F1 Score`, AUC, MSE, Specificity, Precision, Recall), 
               names_to = "Metric", 
               values_to = "Score") %>%
  mutate(Score = as.numeric(Score)) %>%
  mutate(Score = ifelse(Metric == "MSE", log10(Score), Score))

ggplot(df_long_di, aes(x = Year, y = Score, color = Disease)) + 
  geom_point() +
  facet_wrap(~ Metric, nrow = 2, scales = 'free_y') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Performance Score", 
       x = "Year",
       color = "Disease") +
  scale_color_manual(values = paired_colors)

#### DATA SIZE - PERFORMANCE CORRELATION ####
df_pf_da$`Dataset size` <- as.numeric(df_pf_da$`Dataset size`)
df_pf_da_long <- df_pf_da %>%
  gather(key = "Metric", value = "Score", Accuracy, F1, AUC, MSE, Specificity, Precision, Recall)

ggplot(df_pf_da_long, aes(x = `Dataset size`, y = as.numeric(Score), color = `Data Category`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(x = "Dataset Size",
       y = "Performance Score") +
  scale_x_log10() +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") 

#### FREQUENCY MATRIX ####
freq_mo_da <- read_excel(file_path_data)

heatmap_data <- freq_mo_da %>%
  count(`Data Category`, `Model Category`) %>%
  spread(`Model Category`, n, fill = 0)

heatmap_melt <- melt(heatmap_data, id.vars = 'Data Category')

ggplot(heatmap_melt, aes(x = variable, y = `Data Category`)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = 'Occurrence', x = "Model Category", y = "Data Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
