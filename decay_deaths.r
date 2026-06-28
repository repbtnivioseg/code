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

# 2. Fatality data summary: Count deaths in each corresponding assembly area (WITH GENERATIONAL TEMPORAL DECAY)
# Half-life = 25 years (lambda = ln(2)/25 = 0.02772589). Reference year = 2025 (school data year)
lambda <- log(2) / 25
fatality_summary <- fat_data %>%
    mutate(
        Assembly_Area = toupper(trimws(`Location Name`)),
        Assembly_Area = gsub("&", "AND", Assembly_Area),
        Assembly_Area = gsub("^(EAST|NORTH|SOUTH|WEST) BELFAST$", "BELFAST \\1", Assembly_Area),
        weight = exp(-lambda * (2025 - Year))
    ) %>%
    filter(Assembly_Area %in% school_summary$Assembly_Area) %>%
    group_by(Assembly_Area) %>%
    summarize(
        deaths = sum(weight),
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
# Model 1: Bivariate Regression (Deaths only, decayed)
model_bivariate <- lm(pct_integrated ~ deaths, data = analysis_data)
cat("\n=== MODEL 1: BIVARIATE OLS REGRESSION (pct_integrated ~ deaths) ===\n")
print(summary(model_bivariate))

# Model 3: Multivariate Regression (excluding FSME control, decayed deaths)
model_multivariate_no_fsme <- lm(pct_integrated ~ deaths + pct_urban + rel_imbalance, data = analysis_data)
cat("\n=== MODEL 3: MULTIVARIATE OLS REGRESSION (excluding FSME control) ===\n")
print(summary(model_multivariate_no_fsme))

# 5. Create and save the bivariate relationship plot
plot <- ggplot(analysis_data, aes(x = deaths, y = pct_integrated)) +
    geom_point(color = "#1f77b4", size = 4, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, color = "#d62728", fill = "#ff9896", se = TRUE, linewidth = 1.2) +
    geom_text(aes(label = Assembly_Area), vjust = -1, hjust = 0.5, size = 3, check_overlap = FALSE, fontface = "bold") +
    coord_cartesian(ylim = c(0, NA)) +
    labs(
        title = "Percentage of Integrated Schools vs. Troubles-Related Deaths (Temporally Decayed)",
        subtitle = "Exponential Decay Model (25-Year Generational Half-Life to 2025)",
        x = "Weighted Deaths (Independent Variable)",
        y = "% of Primary Schools (Controlled Integrated or GMI)",
        caption = "Data Source: School level primary schools 2024/25 & McKeown Spreadsheet"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, color = "gray40", margin = margin(b = 15)),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

ggsave("integrated_vs_deaths_decay.png", plot = plot, width = 10, height = 7, dpi = 300)
cat("\nBivariate plot saved successfully as 'integrated_vs_deaths_decay.png'\n")
