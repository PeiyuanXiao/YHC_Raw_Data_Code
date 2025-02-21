# library R packages------------------------------------------------------------
library("readxl")
library("writexl")
library("tidyverse")
library("ggpmisc")
library("forcats")
library("vegan")
library("vcd")
library("car")
library("MVN") 
library("ggExtra")

# read raw data-----------------------------------------------------------------
YHDDATA_cores <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 1) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))
YHDDATA_complete_flakes <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 2) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W)) %>%
  mutate(`Dorsal-Cortex` = as.double(`Dorsal-Cortex`)) %>%
  mutate(`Platform-L` = as.double(`Platform-L`))
YHDDATA_incomplete_flakes <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 3) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))
YHDDATA_fragment_flakes <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 4) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))
YHDDATA_split_flakes <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 5) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))
YHDDATA_tools <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 6) %>%  
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))
YHDDATA_hammerstone <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 7)  %>%  
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer))
YHDDATA_chunks <- 
  read_excel("YHC_LITHIC_RAW_DATA.xlsx", sheet = 8) %>%
  mutate(`Cultural phase` = as.character(`Cultural phase`)) %>%  
  mutate(Layer = as.character(Layer)) %>% 
  mutate(L = as.double(L)) %>%
  mutate(B = as.double(B)) %>%
  mutate(Th = as.double(Th)) %>%
  mutate(W = as.double(W))

# PERMANOVA of lithic size in phase 1-------------------------------------------
integration <- function(data) {
  data %>%
    select(`Raw material`, Type, `Cultural phase`, L, B, Th, W)  %>%  
    drop_na() %>% 
    filter(`Cultural phase` == "1")
}

CORE_FLAKE_TOOL_SIZE_P1 <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_tools)
)

permanova_p1 <- CORE_FLAKE_TOOL_SIZE_P1 %>% 
  mutate(`Raw material` = case_when(
      `Raw material` %in% c("Sandstone", 
                            "Limestone") ~ "Group_1", 
      `Raw material` %in% c("Chert", 
                            "Crystal", 
                            "Quartz", 
                            "Tektite") ~ "Group_2",
      TRUE ~ `Raw material`))

# Mardia test
mvn_result_lc_p1 <- mvn(
  data = permanova_p1[, c("L", "B", "Th", "W")], 
  mvnTest = "mardia"
)
print(mvn_result_lc_p1$multivariateNormality)

# PERMANOVA test
permanova_results_lc_p1 <- adonis2(
  permanova_p1[ , c("L", "B", "Th", "W")] ~ `Raw material`, 
  data = permanova_p1, 
  method = "euclidean")
print(permanova_results_lc_p1)

# PERMANOVA of lithic size in phase 2-------------------------------------------
integration <- function(data) {
  data %>%
    select(`Raw material`, Type, `Cultural phase`, L, B, Th, W)  %>%  
    drop_na() %>% 
    filter(`Cultural phase` == "2")
}

CORE_FLAKE_SIZE_TOOL_P2 <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_tools) 
)

permanova_p2 <- CORE_FLAKE_SIZE_TOOL_P2 %>%
  mutate(`Raw material` = case_when(
      `Raw material` %in% c("Sandstone", 
                            "Limestone") ~ "Group_1", 
      `Raw material` %in% c("Chert", 
                            "Crystal", 
                            "Quartz", 
                            "Tektite") ~ "Group_2",
      TRUE ~ `Raw material`))

# Mardia test
mvn_result_lc_p2 <- mvn(
  data = permanova_p2[, c("L", "B", "Th", "W")], 
  mvnTest = "mardia"
)
print(mvn_result_lc_p2$multivariateNormality)

# PERMANOVA test
permanova_results_lc_p2 <- adonis2(
  permanova_p2[ , c("L", "B", "Th", "W")] ~ `Raw material`, 
  data = permanova_p2, 
  method = "euclidean")
print(permanova_results_lc_p2)

# PERMANOVA of lithic size in phase 3-------------------------------------------
integration <- function(data) {
  data %>%
    select(`Raw material`, Type, `Cultural phase`, L, B, Th, W)  %>%  
    drop_na() %>% 
    filter(`Cultural phase` == "3")
}

CORE_FLAKE_SIZE_TOOL_P3 <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_tools) 
)

permanova_p3 <- CORE_FLAKE_SIZE_TOOL_P3 %>%
  mutate(`Raw material` = case_when(
    `Raw material` %in% c("Sandstone", 
                          "Limestone") ~ "Sandstone_Limestone", 
    `Raw material` %in% c("Chert", 
                          "Crystal", 
                          "Quartz", 
                          "Tektite") ~ "Chert_Crystal_Quartz_Tektite",
    TRUE ~ `Raw material`))

# Mardia test
mvn_result_lc_p3 <- mvn(
  data = permanova_p3[, c("L", "B", "Th", "W")], 
  mvnTest = "mardia"
)
print(mvn_result_lc_p3$multivariateNormality)

# PERMANOVA test
permanova_results_lc_p3 <- adonis2(
  permanova_p3[ , c("L", "B", "Th", "W")] ~ `Raw material`, 
  data = permanova_p3, 
  method = "euclidean")
print(permanova_results_lc_p3)

# Correlation between raw material and technique--------------------------------
# Phase 1
integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Sandstone", "Limestone"), 
           `Cultural phase` == "1")
}

Raw_technique_chi_medium <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
  integration(YHDDATA_tools)
)

Raw_technique_chi_medium$`Raw material` <- "Group_1_P1"

integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Chert", "Tektite", "Quartz", "Crystal"), 
           `Cultural phase` == "1")
}

Raw_technique_chi_miniaturized <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes)
)

Raw_technique_chi_miniaturized$`Raw material` <- "Group_2_P1"

Raw_technique_combined <- bind_rows(
  Raw_technique_chi_medium %>% mutate(Group = "Group_1_P1"),
  Raw_technique_chi_miniaturized %>% mutate(Group = "Group_2_P1")
)

contingency_table_P1 <- table(Raw_technique_combined$Technique, 
                              Raw_technique_combined$Group)

cramers_v_P1 <- assocstats(contingency_table_P1)$cramer
print(cramers_v_P1)

chi_test_P1 <- chisq.test(contingency_table_P1)
print(chi_test_P1)

# Phase 2
integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Sandstone", "Limestone"), 
           `Cultural phase` == "2")
}

Raw_technique_chi_medium <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
  integration(YHDDATA_tools)
)

Raw_technique_chi_medium$`Raw material` <- "Group_1_P2"

integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Chert", "Tektite", "Quartz", "Crystal"), 
           `Cultural phase` == "2")
}

Raw_technique_chi_miniaturized <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
)

Raw_technique_chi_miniaturized$`Raw material` <- "Group_2_P2"

Raw_technique_combined <- bind_rows(
  Raw_technique_chi_medium %>% mutate(Group = "Group_1_P2"),
  Raw_technique_chi_miniaturized %>% mutate(Group = "Group_2_P2")
)

contingency_table_P2 <- table(Raw_technique_combined$Technique, 
                              Raw_technique_combined$Group)

cramers_v_P2 <- assocstats(contingency_table_P2)$cramer
print(cramers_v_P2)

chi_test_P2 <- chisq.test(contingency_table_P2)
print(chi_test_P2)

# Phase 3
integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Sandstone", "Limestone"), 
           `Cultural phase` == "3")
}

Raw_technique_chi_medium <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
  integration(YHDDATA_tools)
)

Raw_technique_chi_medium$`Raw material` <- "Group_1_P3"

integration <- function(data) {
  data %>%
    select(`Raw material`, `Cultural phase`, Technique) %>%
    drop_na() %>%
    filter(`Raw material` %in% c("Chert", "Tektite", "Quartz", "Crystal"), 
           `Cultural phase` == "3")
}

Raw_technique_chi_miniaturized <- bind_rows(
  integration(YHDDATA_cores),          
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
)

Raw_technique_chi_miniaturized$`Raw material` <- "Group_2_P3"

Raw_technique_combined <- bind_rows(
  Raw_technique_chi_medium %>% mutate(Group = "Group_1_P3"),
  Raw_technique_chi_miniaturized %>% mutate(Group = "Group_2_P3")
)

contingency_table_P3 <- table(Raw_technique_combined$Technique, 
                              Raw_technique_combined$Group)

cramers_v_P3 <- assocstats(contingency_table_P3)$cramer
print(cramers_v_P3)

chi_test_P3 <- chisq.test(contingency_table_P3)
print(chi_test_P3)

# test for core type------------------------------------------------------------
GROUP_1_CORE_TYPO  <- YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, `de la Torre Typology`)  %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone"| 
           `Raw material` == "Limestone" )

nrow(GROUP_1_CORE_TYPO)

contingency_table_core_type <- 
  table(GROUP_1_CORE_TYPO$`de la Torre Typology`, 
        GROUP_1_CORE_TYPO$"Cultural phase")

# Chi-square test
chisq.test(contingency_table_core_type)

# test for core cortex----------------------------------------------------------
SANDCORE_CORTEX <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, Cortex) %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone"| 
           `Raw material` == "Limestone" )

# Shapiro-Wilk test
grouped_shapiro <- SANDCORE_CORTEX %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(Cortex)$statistic,
    p_value = shapiro.test(Cortex)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(Cortex ~ `Cultural phase`, data = SANDCORE_CORTEX)

KW_result <- kruskal.test(Cortex ~ `Cultural phase`, data = SANDCORE_CORTEX)
print(KW_result)

# test for core butt angle------------------------------------------------------
SANDCORE_ANGLE <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, `X̄(Platform angle)`)  %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone"| 
           `Raw material` == "Limestone")

# Shapiro-Wilk test
grouped_shapiro <- SANDCORE_ANGLE %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`X̄(Platform angle)`)$statistic,
    p_value = shapiro.test(`X̄(Platform angle)`)$p.value
  )

print(grouped_shapiro)

# Bartlett test
bartlett.test(`X̄(Platform angle)` ~ `Cultural phase`, data = SANDCORE_ANGLE)

anov_result <- aov(`X̄(Platform angle)`~ `Cultural phase`, data = SANDCORE_ANGLE)
summary(anov_result)

# test for core scar------------------------------------------------------------
SANDCORE_SCAR <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, `N(Scars)`)  %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone" | 
           `Raw material` == "Limestone" )

# Shapiro-Wilk test
grouped_shapiro <- SANDCORE_SCAR %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`N(Scars)`)$statistic,
    p_value = shapiro.test(`N(Scars)`)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(`N(Scars)` ~ `Cultural phase`, data = SANDCORE_SCAR)

KW_result <- kruskal.test(`N(Scars)`~ `Cultural phase`, data = SANDCORE_SCAR)
print(KW_result)

# test for technique------------------------------------------------------------
integration <- function(data) {
  data %>%
    select(Technique, `Raw material`, `Cultural phase`) %>%
    drop_na() %>%
    filter(`Raw material` == "Tektite" | 
             `Raw material` == "Quartz" | 
             `Raw material` == "Crystal" | 
             `Raw material` == "Chert")
}

Flaking_technique_TQCC <- bind_rows(
  integration(YHDDATA_cores),
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes)
)

nrow(GROUP_1_CORE_TYPO)

contingency_table_technique <- table(Flaking_technique_TQCC$`Cultural phase`, 
                                     Flaking_technique_TQCC$Technique)

# Chi-square test
chisq.test(contingency_table_technique)

# PCA for flake size and shape across three phases------------------------------
flake_size_shape <- 
  select(YHDDATA_complete_flakes, 
         `L`, 
         `B`,
         `Th`, 
         `W`,
         `Elongation Index`,
         `Narrowness Index`,
         `Thinness Index`,
         `Raw material`,
         `Cultural phase`,
         Technique
  ) %>%  
  drop_na()

pca_data_F <- flake_size_shape %>% select(`L`, 
                                          `B`,
                                          `Th`, 
                                          `W`,
                                          `Elongation Index`,
                                          `Narrowness Index`,
                                          `Thinness Index`)

scaled_data_F <- scale(pca_data_F)

# PCA
pca_result_F <- prcomp(scaled_data_F, center = TRUE, scale. = TRUE)
summary(pca_result_F)

# Check loadings
pca_result_F$rotation

# Extract PC1 & PC2
flake_size_shape <- flake_size_shape %>%
  mutate(PC1 = pca_result_F$x[, 1],
         PC2 = pca_result_F$x[, 2],
         PC3 = pca_result_F$x[, 3],
         PC4 = pca_result_F$x[, 4])

# Extract PCA loading matrix
loadings <- as.data.frame(pca_result_F$rotation[, 1:2])  
loadings$Variable <- rownames(loadings)  

loadings <- loadings %>%
  mutate(Variable = case_when(
    Variable == "L" ~ "Length",
    Variable == "B" ~ "Width",
    Variable == "Th" ~ "Thickness",
    Variable == "W" ~ "Mass",
    Variable == "Elongation Index" ~ "Elongation",
    Variable == "Narrowness Index" ~ "Narrowness",
    Variable == "Thinness Index" ~ "Thinness",
    TRUE ~ Variable  
  ))

# Add variable arrow
scaling_factor <- 10
loadings$PC1 <- loadings$PC1 * scaling_factor
loadings$PC2 <- loadings$PC2 * scaling_factor

# PCA scatter plot
scatter_plot <- 
  ggplot(flake_size_shape, 
         aes(x = PC1, y = PC2, color = `Cultural phase`)) + 
  geom_point(shape = 16, size = 2.5, alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.95, size = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) + 
  geom_segment(data = loadings, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "black", size = 0.3) + 
  geom_text(data = loadings, 
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable), 
            size = 3, color = "black", hjust = 0.7) +
  labs(title = "",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)") +
  scale_color_manual(values = c("1" = "#d9e6eb", 
                                "2" = "#8f96bd",  
                                "3" = "#2a347a"),
                     labels = c("1" = "Phase 1", 
                                "2" = "Phase 2", 
                                "3" = "Phase 3")) +
  labs(x = "PC1(53.85%)", y = "PC2(23.89%)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = c(0.2, 0.1), 
        legend.title = element_blank(),  
        legend.key = element_blank(),   
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(margin = margin(t = 5))
    )

scatter_plot_with_margins <- ggMarginal(scatter_plot, 
                                        type = "density", 
                                        margins = "both", 
                                        groupColour = TRUE, 
                                        groupFill = TRUE)

ggsave(scatter_plot_with_margins, 
       filename = "PCA.png", 
       width = 8, height = 8, dpi = 800, bg = "white")

# Scree Plot
explained_variance <- summary(pca_result_F)$importance[2, ] * 100 

scree_data <- data.frame(
  Principal_Component = factor(paste0("PC", seq_along(explained_variance))),
  Variance_Explained = explained_variance
)

scree_plot <- ggplot(scree_data, 
                     aes(x = Principal_Component, 
                         y = Variance_Explained)) +
  geom_bar(stat = "identity", 
           fill = "#8f96bd", 
           color = NA, 
           width = 0.7) +
  geom_line(aes(group = 1),
            size = 0.5, 
            color = "black") +
  geom_point(size = 2, 
             color = "black") +
  labs(x = "", y = "Variance Explained (%)")  +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "None")

scree_plot

flake_size_shape <- flake_size_shape %>%
  drop_na(PC1, PC2, `Cultural phase`)

# Mardia test
mvn_result_pca <- mvn(
  data = flake_size_shape[, c("PC1", "PC2")], 
  mvnTest = "mardia"
)
print(mvn_result_pca$multivariateNormality)

# PERMANOVA
permanova_results_pca <- adonis2(
  flake_size_shape[ , c("PC1", "PC2")] ~ `Cultural phase`, 
  data = flake_size_shape, 
  method = "manhattan")
print(permanova_results_pca)

# Tool compared among 3 phases--------------------------------------------------
# test for tool type------------------------------------------------------------
contingency_table_tool_type <- table(YHDDATA_tools$Typology, 
                                     YHDDATA_tools$"Cultural phase")

nrow(YHDDATA_tools)

chisq_result_1 <- chisq.test(contingency_table_tool_type)
chisq_result_1

# test for blank----------------------------------------------------------------
contingency_table_tool <- table(YHDDATA_tools$Blank, 
                                YHDDATA_tools$"Cultural phase")

nrow(YHDDATA_tools)

chisq_result_1 <- chisq.test(contingency_table_tool)
chisq_result_1

# test for edge number----------------------------------------------------------
# Shapiro-Wilk test
grouped_shapiro <- YHDDATA_tools %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`N(Edge)`)$statistic,
    p_value = shapiro.test(`N(Edge)`)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(`N(Edge)` ~ `Cultural phase`, data = YHDDATA_tools)

KW_result <- kruskal.test(`N(Edge)`~ `Cultural phase`, 
                          data = YHDDATA_tools)
print(KW_result)

# test for edge angle-----------------------------------------------------------
# Shapiro-Wilk test
grouped_shapiro <- YHDDATA_tools %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`X̄(Edge angle)`)$statistic,
    p_value = shapiro.test(`X̄(Edge angle)`)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(`X̄(Edge angle)` ~ `Cultural phase`, data = YHDDATA_tools)

KW_result <- kruskal.test(`X̄(Edge angle)`~ `Cultural phase`,
                          data = YHDDATA_tools)
print(KW_result)

# test for Retouch generation---------------------------------------------------
# Shapiro-Wilk test
grouped_shapiro <- YHDDATA_tools %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`Retouch generation`)$statistic,
    p_value = shapiro.test(`Retouch generation`)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(`Retouch generation` ~ `Cultural phase`, data = YHDDATA_tools)

KW_result <- kruskal.test(`Retouch generation`~ `Cultural phase`, 
                          data = YHDDATA_tools)
print(KW_result)

# test for Retouch length Index-------------------------------------------------
# Shapiro-Wilk test
grouped_shapiro <- YHDDATA_tools %>%
  group_by(`Cultural phase`) %>%
  summarise(
    W = shapiro.test(`Retouch length Index`)$statistic,
    p_value = shapiro.test(`Retouch length Index`)$p.value
  )

print(grouped_shapiro)

# Levene test
leveneTest(`Retouch length Index` ~ `Cultural phase`, data = YHDDATA_tools)

KW_result <- kruskal.test(`Retouch length Index`~ `Cultural phase`, 
                          data = YHDDATA_tools)
print(KW_result)
