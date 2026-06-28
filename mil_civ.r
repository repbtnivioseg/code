library(tidyverse)
library(readxl)

# Define paths
school_src <- "data/School level - primary schools-2025.xlsx"
mckeown_src <- "data/McKeown_Spreadsheet.xls"
census_src <- "data/ni-census21-religion.xlsx"

# Read sheets 
ref_data <- read_excel(school_src, sheet = "reference data", skip = 3)
fsm_data <- read_excel(school_src, sheet = "Free School Meals", skip = 3)
fat_data <- read_excel(mckeown_src, sheet = "Fatalities")
census_data <- read_excel(census_src, sheet = "Table", skip = 4)

# Normalize school keys to character for joining
ref_data <- ref_data %>% mutate(De_ref = as.character(`De ref`))
fsm_data <- fsm_data %>% mutate(De_ref = as.character(`DE ref`))

# Merge school levels
school_merged <- inner_join(ref_data, fsm_data, by = "De_ref")

# 1. School data summary: Calculate total schools, integrated/gmi %, % urban, and avg % fsme per assembly area (weighted by student enrolment)
school_summary <- school_merged %>%
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

# 2. Fatality data summary: Count deaths in each corresponding assembly area
# EXCLUDING: Counter Insurgency (ID 2) and Security Force Target (ID 4) rationales
fatality_summary <- fat_data %>%
    filter(!Rationale %in% c("Counter Insurgency", "Security Force Target")) %>%
    mutate(
        Assembly_Area = toupper(trimws(`Location Name`)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area)
    ) %>%
    filter(Assembly_Area %in% school_summary$Assembly_Area) %>%
    group_by(Assembly_Area) %>%
    summarize(
        deaths = n(),
        .groups = "drop"
    )

# 3. Census religion data: Calculate absolute difference between Catholic and Protestant/Other Christian %
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

# 4. Merge school summary, fatality data, and census demographic data
analysis_data <- school_summary %>%
    left_join(fatality_summary, by = "Assembly_Area") %>%
    left_join(census_summary, by = "Assembly_Area") %>%
    mutate(deaths = replace_na(deaths, 0))

# 5. Perform OLS Regressions
# Model 1: Bivariate Regression (Deaths only)
model_bivariate <- lm(pct_integrated ~ deaths, data = analysis_data)
cat("\n=== MODEL 1: BIVARIATE OLS REGRESSION (pct_integrated ~ deaths) ===\n")
print(summary(model_bivariate))

# Model 2: Multivariate Regression (with controls: Urban, FSME, and Demographic Imbalance)
model_multivariate <- lm(pct_integrated ~ deaths + pct_urban + avg_pct_fsme + rel_imbalance, data = analysis_data)
cat("\n=== MODEL 2: MULTIVARIATE OLS REGRESSION (with controls: Urban, FSME, and Demographic Imbalance) ===\n")
print(summary(model_multivariate))

# Model 3: Multivariate Regression (excluding FSME control)
model_multivariate_no_fsme <- lm(pct_integrated ~ deaths + pct_urban + rel_imbalance, data = analysis_data)
cat("\n=== MODEL 3: MULTIVARIATE OLS REGRESSION (excluding FSME control) ===\n")
print(summary(model_multivariate_no_fsme))

# Model 4: Multivariate Regression predicting Deaths (checking FSME effect on Deaths)
model_deaths <- lm(deaths ~ avg_pct_fsme + pct_urban + rel_imbalance, data = analysis_data)
cat("\n=== MODEL 4: MULTIVARIATE OLS REGRESSION (predicting Deaths: deaths ~ avg_pct_fsme + pct_urban + rel_imbalance) ===\n")
print(summary(model_deaths))

# 5. Create and save the combined bivariate relationship plot
# Fatality summary for Military Targets (Security Forces) for the combined plot
fatality_summary_mil <- fat_data %>%
    filter(Rationale %in% c("Counter Insurgency", "Security Force Target")) %>%
    mutate(
        Assembly_Area = toupper(trimws(`Location Name`)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area)
    ) %>%
    filter(Assembly_Area %in% school_summary$Assembly_Area) %>%
    group_by(Assembly_Area) %>%
    summarize(
        deaths = n(),
        .groups = "drop"
    )

# Prepare combined plotting dataset
civ_plot <- analysis_data %>%
    select(Assembly_Area, deaths, pct_integrated) %>%
    mutate(Category = "Civilian Targets")

mil_plot <- analysis_data %>%
    select(Assembly_Area, pct_integrated) %>%
    left_join(fatality_summary_mil, by = "Assembly_Area") %>%
    mutate(deaths = replace_na(deaths, 0), Category = "Military Targets")

plot_data <- bind_rows(civ_plot, mil_plot)

plot_combined <- ggplot(plot_data, aes(x = deaths, y = pct_integrated, color = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) +
    geom_text(aes(label = Assembly_Area), vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE, fontface = "bold", show.legend = FALSE) +
    coord_cartesian(ylim = c(0, NA)) +
    scale_color_manual(values = c("Civilian Targets" = "#1f77b4", "Military Targets" = "#ff7f0e")) +
    labs(

        x = "Troubles-Related Deaths",
        y = "% of Primary Schools Integrated"

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

ggsave("integrated_vs_deaths_combined.png", plot = plot_combined, width = 10, height = 7, dpi = 300)
cat("\nCombined bivariate plot saved successfully as 'integrated_vs_deaths_combined.png'\n")

