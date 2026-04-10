# ==============================================================================
# YHC Lithic Analysis - DEMO SCRIPT
# Date: 2026-04-10
# Description: This script reproduces key analyses using a small demo dataset.
# ==============================================================================

# 1. Check & Load Dependencies -------------------------------------------------
required_packages <- c("readxl", "tidyverse", "vegan", "vcd", "car", "MVN", "ggExtra", "FSA")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

suppressPackageStartupMessages(lapply(required_packages, library, character.only = TRUE))

# 2. Set Data Path & Read Demo Data --------------------------------------------
data_path <- "demo_data.xlsx"

# Helper function to format basic metrics
format_lithic_data <- function(df) {
  df %>%
    mutate(across(c(`Cultural phase`, Layer), as.character)) %>%
    mutate(across(any_of(c("L", "B", "Th", "W", "Cortex", "N(Scars)", "X̄(Platform angle)")), as.double))
}

YHDDATA_cores           <- read_excel(data_path, sheet = 1) %>% format_lithic_data()
YHDDATA_complete_flakes  <- read_excel(data_path, sheet = 2) %>% format_lithic_data()
YHDDATA_incomplete_flakes<- read_excel(data_path, sheet = 3) %>% format_lithic_data()
YHDDATA_fragment_flakes  <- read_excel(data_path, sheet = 4) %>% format_lithic_data()
YHDDATA_split_flakes     <- read_excel(data_path, sheet = 5) %>% format_lithic_data()
YHDDATA_tools           <- read_excel(data_path, sheet = 6) %>% format_lithic_data()

# 3. PERMANOVA: Lithic Size by Raw Material (Phase 1) --------------------------
message("Step 1: Running PERMANOVA...")

integration_p1 <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, L, B, Th, W) %>%
    drop_na() %>%
    filter(`Cultural phase` == "1")
}

permanova_p1_data <- bind_rows(
  integration_p1(YHDDATA_cores),
  integration_p1(YHDDATA_complete_flakes),
  integration_p1(YHDDATA_tools)
) %>%
  mutate(Group = case_when(
    `Raw material` %in% c("Sandstone", "Limestone") ~ "Group_1",
    TRUE ~ "Group_2"
  ))

permanova_res <- adonis2(permanova_p1_data[, c("L", "B", "Th", "W")] ~ Group, 
                         data = permanova_p1_data, method = "euclidean")

# 4. Chi-Square: Raw Material & Technique (Phase 1) ----------------------------
message("Step 2: Running Chi-square Test...")

raw_tech_p1 <- bind_rows(YHDDATA_cores, YHDDATA_complete_flakes, YHDDATA_tools) %>%
  select(`Raw material`, `Cultural phase`, Technique) %>%
  drop_na() %>%
  filter(`Cultural phase` == "1") %>%
  mutate(Group = if_else(`Raw material` %in% c("Sandstone", "Limestone"), "Group_1", "Group_2"))

chi_table <- table(raw_tech_p1$Technique, raw_tech_p1$Group)
chi_test_res <- chisq.test(chi_table)

# 5. Kruskal-Wallis: Core Scars across Phases ----------------------------------
message("Step 3: Running Kruskal-Wallis Test...")

core_scars <- YHDDATA_cores %>%
  select(`Cultural phase`, `N(Scars)`) %>%
  drop_na()

kw_res <- kruskal.test(`N(Scars)` ~ `Cultural phase`, data = core_scars)

# 6. PCA: Flake Morphology & Visualization -------------------------------------
message("Step 4: Generating PCA & Saving Plot...")

pca_prep <- YHDDATA_complete_flakes %>%
  select(L, B, Th, W, `Elongation Index`, `Narrowness Index`, `Thinness Index`, `Cultural phase`) %>%
  drop_na()

pca_result <- prcomp(pca_prep %>% select(-`Cultural phase`), center = TRUE, scale. = TRUE)
pca_summary <- summary(pca_result)

# Build visualization
pca_plot_data <- pca_prep %>%
  mutate(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

pca_plot <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = `Cultural phase`)) +
  geom_point(alpha = 0.7) +
  stat_ellipse() +
  theme_classic() +
  labs(title = "PCA of Flake Morphology (Demo)",
       x = paste0("PC1 (", round(pca_summary$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(pca_summary$importance[2,2]*100, 1), "%)"))
pca_plot

# 7. FINAL SUMMARY REPORT ------------------------------------------------------
cat(
  "\n======================================================================\n",
  "                 DEMO ANALYSIS SUMMARY REPORT\n",
  "======================================================================\n\n",
  
  "1. MULTIVARIATE SIZE ANALYSIS (PERMANOVA)\n",
  "   - Phase 1 (Group 1 vs 2) p-value: ", permanova_res$`Pr(>F)`[1], "\n\n",
  
  "2. CATEGORICAL ANALYSIS (Chi-square)\n",
  "   - Technique vs Raw Material p-value: ", chi_test_res$p.value, "\n\n",
  
  "3. ATTRIBUTE ANALYSIS (Kruskal-Wallis)\n",
  "   - Core Scars across Phases p-value: ", kw_res$p.value, "\n\n",
  
  "4. MORPHOLOGICAL SPACE (PCA)\n",
  "   - Total Variance (PC1 + PC2): ", 
  round(sum(pca_summary$importance[2, 1:2]) * 100, 2), "%\n",
  
  "======================================================================\n",
  "                          END OF DEMO\n",
  "======================================================================\n"
)