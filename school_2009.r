library(tidyverse)
library(readxl)

# Define paths
school_2025_src <- "data/School level - primary schools-2025.xlsx"
school_2009_src <- "data/School level - primary schools-2009.xlsx"
mckeown_src <- "data/McKeown_Spreadsheet.xls"
census_src <- "data/ni-census21-religion.xlsx"

# =========================================================================
# SECTION 1: 2025 DATA ANALYSIS 
# =========================================================================
cat("=========================================================================\n")
cat("=== RUNNING ANALYSIS FOR 2025 SCHOOL DATA ===\n")
cat("=========================================================================\n")

ref_data_2025 <- read_excel(school_2025_src, sheet = "reference data", skip = 3) %>% mutate(De_ref = as.character(`De ref`))
fsm_data_2025 <- read_excel(school_2025_src, sheet = "Free School Meals", skip = 3) %>% mutate(De_ref = as.character(`DE ref`))
fat_data <- read_excel(mckeown_src, sheet = "Fatalities")
census_data <- read_excel(census_src, sheet = "Table", skip = 4)

school_merged_2025 <- inner_join(ref_data_2025, fsm_data_2025, by = "De_ref")

school_summary_2025 <- school_merged_2025 %>%
    mutate(
        Assembly_Area = toupper(trimws(`Assembly Area (2008)`)),
        enrolment_num = as.numeric(`total enrolment`),
        fsme_num = as.numeric(fsme)
    ) %>%
    group_by(Assembly_Area) %>%
    summarize(
        total_schools = n(),
        integrated_schools = sum(`management type` %in% c("Controlled Integrated", "GMI"), na.rm = TRUE),
        pct_integrated = (integrated_schools / total_schools) * 100,
        pct_urban = sum(`Urban/ Rural` == "URBAN", na.rm = TRUE) / total_schools * 100,
        avg_pct_fsme = sum(fsme_num, na.rm = TRUE) / sum(enrolment_num[!is.na(fsme_num)], na.rm = TRUE) * 100,
        .groups = "drop"
    )

# Fatality data summary: Count deaths in each corresponding assembly area
fatality_summary_2025 <- fat_data %>%
    mutate(
        Assembly_Area = toupper(trimws(`Location Name`)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area)
    ) %>%
    filter(Assembly_Area %in% school_summary_2025$Assembly_Area) %>%
    group_by(Assembly_Area) %>%
    summarize(
        deaths = n(),
        .groups = "drop"
    )

# Census religion data
census_summary <- census_data %>%
    rename(
        Constituency = `Parliamentary Constituency 2008 Label`,
        protestant = `Protestant and Other Christian (including Christian related)`,
        none = `No religion/religion not stated`,
        other = `Other religions`
    ) %>%
    mutate(
        Assembly_Area = toupper(trimws(Constituency)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area),
        total_pop = Catholic + protestant + other + none,
        pct_catholic = (Catholic / total_pop) * 100,
        pct_protestant = (protestant / total_pop) * 100,
        rel_imbalance = abs(pct_catholic - pct_protestant)
    ) %>%
    select(Assembly_Area, rel_imbalance)

# Merge 2025 data
analysis_data_2025 <- school_summary_2025 %>%
    left_join(fatality_summary_2025, by = "Assembly_Area") %>%
    left_join(census_summary, by = "Assembly_Area") %>%
    mutate(deaths = replace_na(deaths, 0))

# 2025 OLS Regressions
model_bivariate_2025 <- lm(pct_integrated ~ deaths, data = analysis_data_2025)
cat("\n=== 2025 MODEL 1: BIVARIATE OLS REGRESSION (pct_integrated ~ deaths) ===\n")
print(summary(model_bivariate_2025))

model_multivariate_2025 <- lm(pct_integrated ~ deaths + pct_urban + avg_pct_fsme + rel_imbalance, data = analysis_data_2025)
cat("\n=== 2025 MODEL 2: MULTIVARIATE OLS REGRESSION (with controls) ===\n")
print(summary(model_multivariate_2025))

model_multivariate_no_fsme_2025 <- lm(pct_integrated ~ deaths + pct_urban + rel_imbalance, data = analysis_data_2025)
cat("\n=== 2025 MODEL 3: MULTIVARIATE OLS REGRESSION (excluding FSME control) ===\n")
print(summary(model_multivariate_no_fsme_2025))

model_deaths_2025 <- lm(deaths ~ avg_pct_fsme + pct_urban + rel_imbalance, data = analysis_data_2025)
cat("\n=== 2025 MODEL 4: MULTIVARIATE OLS REGRESSION (predicting Deaths) ===\n")
print(summary(model_deaths_2025))




# =========================================================================
# SECTION 2: 2009 DATA ANALYSIS 
# =========================================================================
cat("\n\n=========================================================================\n")
cat("=== RUNNING ANALYSIS FOR 2009 SCHOOL DATA ===\n")
cat("=========================================================================\n")

ref_data_2009 <- read_excel(school_2009_src, sheet = "reference data", skip = 2) %>% mutate(De_ref = as.character(inst_ref_no))
fsm_data_2009 <- read_excel(school_2009_src, sheet = "free school meals", skip = 3) %>% mutate(De_ref = as.character(`DE ref`))

school_merged_2009 <- inner_join(ref_data_2009, fsm_data_2009, by = "De_ref")

school_summary_2009 <- school_merged_2009 %>%
    mutate(
        Assembly_Area = toupper(trimws(`Parliamentary Constituency`)),
        enrolment_num = as.numeric(`total enrolment`),
        fsme_num = as.numeric(fsme)
    ) %>%
    group_by(Assembly_Area) %>%
    summarize(
        total_schools = n(),
        integrated_schools = sum(Management %in% c("Controlled Integrated", "GMI"), na.rm = TRUE),
        pct_integrated = (integrated_schools / total_schools) * 100,
        pct_urban = sum(`Urban/Rural marker` == "URBAN", na.rm = TRUE) / total_schools * 100,
        avg_pct_fsme = sum(fsme_num, na.rm = TRUE) / sum(enrolment_num[!is.na(fsme_num)], na.rm = TRUE) * 100,
        .groups = "drop"
    )

# Fatality data summary: Count deaths in each corresponding assembly area (using same 18 assembly areas)
fatality_summary_2009 <- fat_data %>%
    mutate(
        Assembly_Area = toupper(trimws(`Location Name`)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area)
    ) %>%
    filter(Assembly_Area %in% school_summary_2009$Assembly_Area) %>%
    group_by(Assembly_Area) %>%
    summarize(
        deaths = n(),
        .groups = "drop"
    )

# Merge 2009 data
analysis_data_2009 <- school_summary_2009 %>%
    left_join(fatality_summary_2009, by = "Assembly_Area") %>%
    left_join(census_summary, by = "Assembly_Area") %>%
    mutate(deaths = replace_na(deaths, 0))

# 2009 OLS Regressions
model_bivariate_2009 <- lm(pct_integrated ~ deaths, data = analysis_data_2009)
cat("\n=== 2009 MODEL 1: BIVARIATE OLS REGRESSION (pct_integrated ~ deaths) ===\n")
print(summary(model_bivariate_2009))

model_multivariate_2009 <- lm(pct_integrated ~ deaths + pct_urban + avg_pct_fsme + rel_imbalance, data = analysis_data_2009)
cat("\n=== 2009 MODEL 2: MULTIVARIATE OLS REGRESSION (with controls) ===\n")
print(summary(model_multivariate_2009))

model_multivariate_no_fsme_2009 <- lm(pct_integrated ~ deaths + pct_urban + rel_imbalance, data = analysis_data_2009)
cat("\n=== 2009 MODEL 3: MULTIVARIATE OLS REGRESSION (excluding FSME control) ===\n")
print(summary(model_multivariate_no_fsme_2009))

model_deaths_2009 <- lm(deaths ~ avg_pct_fsme + pct_urban + rel_imbalance, data = analysis_data_2009)
cat("\n=== 2009 MODEL 4: MULTIVARIATE OLS REGRESSION (predicting Deaths) ===\n")
print(summary(model_deaths_2009))


# Create and save the combined bivariate relationship plot for 2009/10 vs 2024/25
plot_data_combined <- bind_rows(
    analysis_data_2025 %>% select(Assembly_Area, deaths, pct_integrated) %>% mutate(Year = "2024/25"),
    analysis_data_2009 %>% select(Assembly_Area, deaths, pct_integrated) %>% mutate(Year = "2009/10")
)

plot_combined_years <- ggplot(plot_data_combined, aes(x = deaths, y = pct_integrated, color = Year)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) +
    geom_text(aes(label = Assembly_Area), vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE, fontface = "bold", show.legend = FALSE) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_color_manual(values = c("2024/25" = "#1f77b4", "2009/10" = "#2ca02c")) +
    labs(
        x = "Troubles-Related Deaths",
        y = "% of Primary Schools Integrated",
        color = "Academic Year"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        plot.title = element_text(face = "bold", size = 13, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, color = "gray40", margin = margin(b = 15)),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

ggsave("integrated_vs_deaths_2009_2025_combined.png", plot = plot_combined_years, width = 10, height = 7, dpi = 300)
cat("\nCombined 2009 vs 2025 bivariate plot saved successfully as 'integrated_vs_deaths_2009_2025_combined.png'\n")

