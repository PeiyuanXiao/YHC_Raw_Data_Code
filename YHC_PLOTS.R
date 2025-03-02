# Library R packages------------------------------------------------------------
library("readxl")
library("tidyverse")
library("patchwork")
library("hexbin")

# Read raw data-----------------------------------------------------------------
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

# Create plots------------------------------------------------------------------
# define globe variables
# for color 
cultural_phase_colors <- c(
  "1" = "#d9e6eb",
  "2" = "#8f96bd",  
  "3" = "#2a347a"
)

raw_material_colors <- c(
  "Sandstone" = "#d9e6eb", 
  "Limestone" = "#9fc3d5", 
  "Chert" = "#8f96bd", 
  "Quartz" = "#64659c", 
  "Crystal" = "#404f91", 
  "Tektite" = "#2a347a"
)

# for raw order
raw_materials_order <- c(
                         "Sandstone",
                         "Limestone",
                         "Chert", 
                         "Quartz", 
                         "Crystal",
                         "Tektite"
                         )

# for smooth
custom_smooth <- geom_smooth(aes(group = 1), 
                             method = "lm", 
                             color = "black", 
                             fill = "#AFAD8E",
                             linetype = "dashed", 
                             size = 0.5,
                             alpha = 0.3, 
                             se = T)

# for theme
plot_theme_1 <- theme(
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, size = 0.5),
  axis.line = element_blank(), 
  axis.title.x = element_blank(),
  axis.text.x = element_text()
)

# Size plots--------------------------------------------------------------------
core_size <- 
  select(YHDDATA_cores, 
         `L`, `B`, `Th`, `W`,
         `Raw material`,
         `Cultural phase`,
         `de la Torre Typology`,
         Technique
  ) %>%
  drop_na()

# Core size divided by raw materials
SCR1 <- ggplot(core_size, aes(x = `Raw material`, 
                              y = `L`, 
                              fill = `Cultural phase`, 
                              color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) + 
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(title = "Length (mm)", x = "", y = "Core") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1 +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14), 
        axis.title.y = element_text(size = 16))

SCR2 <- ggplot(core_size, aes(x = `Raw material`, 
                              y = `B`, 
                              fill = `Cultural phase`, 
                              color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(title = "Width (mm)", x = "", y = "") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1 +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14))

SCR3 <- ggplot(core_size, aes(x = `Raw material`, 
                              y = `B`, 
                              fill = `Cultural phase`, 
                              color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors, 
                     labels = c("Phase 1", "Phase 2", "Phase 3")) +
  scale_fill_manual(values = cultural_phase_colors, 
                    labels = c("Phase 1", "Phase 2", "Phase 3")) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(title = "Thickness (mm)", x = "", y = "") +
  guides(fill = guide_legend(title = NULL), 
         color = guide_legend(title = NULL)) +
  plot_theme_1 +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),  
        legend.background = element_rect(fill = "white", 
                                         color = "black", 
                                         size = 0.25), 
        legend.box.margin = margin(5, 5, 5, 5))

# Flake size divided by technique
flake_size <- 
  select(YHDDATA_complete_flakes, 
         `L`, `B`, `Th`, `W`,
         `Raw material`,
         `Cultural phase`,
         Technique
  ) %>%  
  drop_na()

SFR1 <- ggplot(flake_size, aes(x = `Raw material`, 
                               y = `L`, 
                               fill = `Cultural phase`, 
                               color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "Flake") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1 +
  theme(axis.title.y = element_text(size = 16))

SFR2 <- ggplot(flake_size, aes(x = `Raw material`, 
                               y = `B`, 
                               fill = `Cultural phase`, 
                               color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1

SFR3 <- ggplot(flake_size, aes(x = `Raw material`, 
                               y = `Th`, 
                               fill = `Cultural phase`, 
                               color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1

# Tool size divided by technique
size_tool <-
  select(YHDDATA_tools, 
         `Cultural phase`, 
         `L`, `B`,`Th`,`W`,
         `Raw material`
) %>%
  drop_na()

ST1 <- ggplot(size_tool, aes(x = `Raw material`, 
                                       y = `L`, 
                                       fill = `Cultural phase`,
                             color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "Tool") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1 +
  theme(axis.title.y = element_text(size = 16))

ST2 <- ggplot(size_tool, aes(x = `Raw material`, 
                                       y = `B`, 
                                       fill = `Cultural phase`,
                             color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1

ST3 <- ggplot(size_tool, aes(x = `Raw material`, 
                                       y = `Th`, 
                                       fill = `Cultural phase`,
                             color = `Cultural phase`)) + 
  geom_boxplot(position = position_dodge(width = 0.85, preserve = "single"),
               outlier.color = "darkgray",
               outlier.size = 1) +
  custom_smooth +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", y = "") +
  guides(fill = FALSE, color = FALSE) +
  plot_theme_1

# Patch plots
y_axis_limits <- c(0, 100)

SCR1 <- SCR1 + scale_y_continuous(limits = y_axis_limits)
SCR2 <- SCR2 + scale_y_continuous(limits = y_axis_limits)
SCR3 <- SCR3 + scale_y_continuous(limits = y_axis_limits)

SFR1 <- SFR1 + scale_y_continuous(limits = y_axis_limits)
SFR2 <- SFR2 + scale_y_continuous(limits = y_axis_limits)
SFR3 <- SFR3 + scale_y_continuous(limits = y_axis_limits)

ST1 <- ST1 + scale_y_continuous(limits = y_axis_limits)
ST2 <- ST2 + scale_y_continuous(limits = y_axis_limits)
ST3 <- ST3 + scale_y_continuous(limits = y_axis_limits)

(SCR1 | SCR2 | SCR3) / (SFR1 | SFR2 | SFR3) / (ST1 | ST2 | ST3)

# Save file
ggsave(filename = "Size Raw material divided by Phase.png", 
       width = 13, height = 15, dpi = 800, bg = "white")

# Size hexbin plot-------------------------------------------------------------
integration <- function(data) {
  data %>%
    select(`Cultural phase`, `Raw material`, Type, 
           `L`, `B`)  %>%
    drop_na()
}

size_scatter <- bind_rows(
  integration(YHDDATA_cores),
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes),
  integration(YHDDATA_tools),
  integration(YHDDATA_hammerstone),
  integration(YHDDATA_chunks)
)

# Hexbin plot 1
HP1 <-
ggplot() + 
  geom_hex(data = size_scatter, aes(x = `B`, y = `L`), 
           bins = 50, alpha = 1) + 
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 9)  
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 9)  
  ) +
  xlab("Width (mm)") +
  ylab("Length (mm)") + 
  scale_fill_gradientn(
    colors = c("#ca3d73", "#FFc300", "#ffe18a", "white"),  
    values = scales::rescale(c(0, 25, 75, 100)),            
    limits = c(0, 100),                                    
    oob = scales::squish
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_blank(), 
    axis.text.x = element_text(),
    legend.position = c(0.95, 0.05),  
    legend.justification = c(1, 0), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.25)
  )

# Hexbin plot 2
sandstone_data <- size_scatter %>% 
  filter(`Raw material` == "Sandstone")

other_data <- size_scatter %>% 
  filter(`Raw material` != "Sandstone")

HP2 <-
ggplot() + 
  geom_hex(data = sandstone_data, aes(x = `B`, y = `L`, fill = `Raw material`), 
           bins = 50, alpha = 1) +
  geom_hex(data = other_data, aes(x = `B`, y = `L`, fill = `Raw material`), 
           bins = 50, alpha = 1) +
  scale_fill_manual(values = raw_material_colors, 
                    limits = raw_materials_order) + 
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 9)  
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 9)  
  ) +
  xlab("Width (mm)") +
  ylab("Length (mm)") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_blank(), 
    axis.text.x = element_text(),
    legend.position = c(0.95, 0.05),  
    legend.justification = c(1, 0), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.25)
  )

(HP1 / HP2) + plot_annotation(tag_levels = "a")

# Save file
ggsave(filename = "Size hexbin plot.png", 
       width = 6, height = 12, dpi = 600, bg = "white")

# Core typo de la Torre---------------------------------------------------------
de_la_Torre_type <- 
  select(YHDDATA_cores, 
         `de la Torre Typology`, 
         `Cultural phase`, 
         `Raw material`
  ) %>%
  drop_na()

de_la_Torre_type <- de_la_Torre_type %>%
  count(`de la Torre Typology`, `Raw material`, `Cultural phase`, name = "Count")

# bubble chart
CT <-
ggplot(de_la_Torre_type, aes(x = `Raw material`, 
                             y = `de la Torre Typology`,
                             size = Count, 
                             fill = `Cultural phase`,
                             color = `Cultural phase`)) +
  geom_point(shape = 21) + 
  scale_size_continuous(range = c(2, 10)) +
  scale_y_discrete(limits = c("Multifacial exploitation",
                              "Bifacial multidirectional exploitation",
                              "Bifacial alternated exploitation",
                              "Bifacial opposed exploitation",
                              "Bifacial orthogonal exploitation",
                              "Unifacial Centripetal", 
                              "Unifacial opposed exploitation", 
                              "Unifacial orthogonal exploitation",
                              "Unifacial unidirectional exploitation", 
                              "Bipolar core", 
                              "Test core")) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_color_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", 
       y = "",
       size = "Count") +
  theme_minimal() +
  plot_theme_1 + 
  theme(strip.text = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
  facet_wrap(~ `Cultural phase`, ncol = 3)

# Flake dorsal scar pattern-----------------------------------------------------
scar_pattern <- 
  select(YHDDATA_complete_flakes, 
         `Direction of scars`, 
         `Cultural phase`,
         `Raw material`
  ) %>%
  drop_na()

scar_pattern %>% count(`Direction of scars`)

scar_pattern <- scar_pattern %>%
  count(`Direction of scars`, `Raw material`, `Cultural phase`, name = "Count")

# bubble chart
DSPT <-
ggplot(scar_pattern, aes(y = `Direction of scars`, 
                         x = `Raw material`, 
                         size = Count, 
                         fill = `Cultural phase`,
                         color = `Cultural phase`)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(2, 10)) +
  scale_x_discrete(limits = c("Unidirection-proximal", 
                              "Lateral", 
                              "Unidirection-distal", 
                              "Orthogonal", 
                              "Opposed", 
                              "Centripetal")) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_color_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", 
       y = "", 
       size = "Count") +
  theme_minimal() +
  plot_theme_1 + 
  theme(strip.text = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
  ) +
  facet_wrap(~ `Cultural phase`, ncol = 3) 

# combine bubble charts 
(CT/DSPT) + plot_annotation(tag_levels = "a")

# save file
ggsave(filename = "CT & DSP divided by phase.png", width = 10, height = 12, 
       dpi = 600, bg = "white")

# Tool type---------------------------------------------------------------------
tool_types_raw_phase <- 
  select(YHDDATA_tools, 
         Typology, 
         `Cultural phase`,
         `Raw material`
  ) %>%
  drop_na()

tool_types_raw_phase %>% count(Typology)

tool_types_raw_phase <- tool_types_raw_phase %>%
  count(Typology, `Raw material`, `Cultural phase`, name = "Count")

# bubble chart
ggplot(tool_types_raw_phase, aes(y = Typology, 
                                 x = `Raw material`, 
                                 size = Count, 
                                 fill = `Cultural phase`,
                                 color = `Cultural phase`)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(2, 10)) +
  scale_x_discrete(limits = c("Backed tool", 
                              "Scraper", 
                              "Notch", 
                              "Denticulate",
                              "Borer", 
                              "Chopper")) +
  scale_fill_manual(values = cultural_phase_colors) +
  scale_color_manual(values = cultural_phase_colors) +
  scale_x_discrete(limits = raw_materials_order) +
  labs(x = "", 
       y = "", 
       size = "Count") +
  theme_minimal() +
  plot_theme_1 + 
  facet_wrap(~ `Cultural phase`, ncol = 3) + 
  theme(
    strip.text = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",          
    legend.box = "horizontal",           
    legend.spacing.x = unit(0.5, "cm"),  
    legend.box.just = "center",          
    legend.margin = margin(t = 0, b = 0)
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(shape = 21, size = 4) 
    ),
    size = guide_legend(
      nrow = 1,
      override.aes = list(shape = 21, fill = "white")  
    )
  )

# save file
ggsave(filename = "Tool types divided by phase.png", width = 7, height = 6, 
       dpi = 600, bg = "white")

# Proportion of technique-------------------------------------------------------
integration <- function(data) {
  data %>%
    select(Technique, `Raw material`, `Cultural phase`) %>%
    drop_na()
}

Flaking_technique <- bind_rows(
  integration(YHDDATA_cores),
  integration(YHDDATA_complete_flakes),
  integration(YHDDATA_incomplete_flakes),
  integration(YHDDATA_fragment_flakes),
  integration(YHDDATA_split_flakes)
)

costom_y_proportion <- scale_y_continuous(labels = scales::percent, 
                                          breaks = c(0, 0.25, 0.5, 0.75, 1)) 
  
# Phase 1
Flaking_technique_1 <- Flaking_technique %>%
  filter(`Cultural phase` == "1") 

tp1 <-
ggplot(Flaking_technique_1, aes(x = `Raw material`, fill = Technique)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = raw_materials_order) +
  scale_fill_manual(values = c(
    "Bipolar" = "#d9e6eb",
    "Percussion" = "#8f96bd")) +
  costom_y_proportion +
  labs(y = "Proportion") +
  plot_theme_1

# Phase 2
Flaking_technique_2 <- Flaking_technique %>%
  filter(`Cultural phase` == "2") 

tp2 <-
ggplot(Flaking_technique_2, aes(x = `Raw material`, fill = Technique)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = raw_materials_order) +
  scale_fill_manual(values = c(
    "Bipolar" = "#d9e6eb",
    "Percussion" = "#8f96bd")) +
  costom_y_proportion +
  labs(y = "Proportion") +
  plot_theme_1

# Phase 3
Flaking_technique_3 <- Flaking_technique %>%
  filter(`Cultural phase` == "2") 

tp3 <-
ggplot(Flaking_technique_3, aes(x = `Raw material`, fill = Technique)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = raw_materials_order) +
  scale_fill_manual(values = c(
    "Bipolar" = "#d9e6eb",
    "Percussion" = "#8f96bd")) +
  costom_y_proportion +
  labs(y = "Proportion") +
  plot_theme_1

(tp1 / tp2 / tp3) + plot_annotation(tag_levels = "a")

# save file
ggsave(filename = "Proportion of technique.png", width = 6, height = 8, 
       dpi = 800, bg = "white")

# Technological attributes plots of core----------------------------------------
# Cortical proportion
SANDCORE_CORTEX <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, Cortex) %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone" | 
         `Raw material` == "Limestone" )

CP <-
ggplot(SANDCORE_CORTEX, aes(x = `Cultural phase`, 
                            y = Cortex, 
                            fill = `Cultural phase`, 
                            color = `Cultural phase`)) +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Cortical proportion", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

# Core flaking angle
SANDCORE_ANGLE <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, `X̄(Platform angle)`)  %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone" | 
         `Raw material` == "Limestone" )

CFA <-
ggplot(SANDCORE_ANGLE, aes(x = `Cultural phase`, 
                           y = `X̄(Platform angle)`, 
                           fill = `Cultural phase`, 
                           color = `Cultural phase`))  +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Core flaking angle", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

# Core scar number
SANDCORE_SCAR <-
  YHDDATA_cores %>%
  select(`Raw material`, `Cultural phase`, `N(Scars)`)  %>%  
  drop_na() %>% 
  filter(`Raw material` == "Sandstone" | 
         `Raw material` == "Limestone" )

CSN <-
ggplot(SANDCORE_SCAR, aes(x = `Cultural phase`, 
                          y = `N(Scars)`, 
                          fill = `Cultural phase`, 
                          color = `Cultural phase`))  +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Number of flaking scars", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

(CP | CFA | CSN) + plot_annotation(tag_levels = "a")

# save file
ggsave(filename = "Technological attributes plots of core.png", 
       width = 8, height = 8, 
       dpi = 800, bg = "white")

# Technological attributes plots of flake---------------------------------------
# Edge number
TEN <-
  ggplot(YHDDATA_tools, aes(x = `Cultural phase`, 
                            y = `N(Edge)`, 
                            fill = `Cultural phase`, 
                            color = `Cultural phase`)) +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Number of retouched edge", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

# Edge angle
TEA <-
  ggplot(YHDDATA_tools, aes(x = `Cultural phase`, 
                            y = `X̄(Edge angle)`, 
                            fill = `Cultural phase`, 
                            color = `Cultural phase`)) +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Edge angle", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

# Edge generation
TEG <-
  ggplot(YHDDATA_tools, aes(x = `Cultural phase`, 
                            y = `Retouch generation`, 
                            fill = `Cultural phase`, 
                            color = `Cultural phase`)) +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Number of retouched generation", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

# Retouch length Index
RIL <-
  ggplot(YHDDATA_tools, aes(x = `Cultural phase`, 
                            y = `Retouch length Index`, 
                            fill = `Cultural phase`, 
                            color = `Cultural phase`)) +
  geom_jitter(shape = 16, size = 3, alpha = 0.7, width = 0.25) +
  geom_boxplot(fill = NA, color = "black", size = 0.3, outliers = F) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, 
               fill = "black", color = "black") +
  scale_color_manual(values = cultural_phase_colors) +
  scale_fill_manual(values = cultural_phase_colors)  +
  scale_x_discrete(labels = c("Phase 1", "Phase 2", "Phase 3")) + 
  labs(y = "Retouch length Index", x = "") +
  plot_theme_1 +
  theme(legend.position = "none")

(TEN | TEA) / (TEG | RIL) + plot_annotation(tag_levels = "a")

# save file
ggsave(filename = "Technological attributes plots of flake.png", 
       width = 8, height = 10, 
       dpi = 800, bg = "white")

