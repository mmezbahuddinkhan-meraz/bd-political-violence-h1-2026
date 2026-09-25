# =============================================================================
# Project: Political Violence in Bangladesh, January-June 2026
# All incidents/deaths/injuries figures below are drawn from Human Rights
# Support Society (HRSS) monthly/quarterly/half-yearly reports as covered by
# The Daily Star, The Business Standard, and Bangla Tribune, or fetched
# directly from hrssbd.org. January 2026 is CALCULATED (Q1 total minus the
# directly-reported February and March figures) because no standalone HRSS
# January 2026 monthly report could be located; this is flagged explicitly
# and the six months reconcile exactly to the independently reported
# half-year total (830 / 56 / 5,246), which cross-validates the calculation.
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------
months_data <- tibble::tribble(
  ~month_label, ~incidents, ~deaths, ~injuries, ~source_type,
  "Jan 2026", 151,  8, 1233, "Calculated (Q1 total minus Feb & Mar)",
  "Feb 2026", 346, 10, 1933, "Directly reported (HRSS via Daily Star, 5 Mar 2026)",
  "Mar 2026", 113, 18,  912, "Directly reported (HRSS primary report, 3 Apr 2026)",
  "Apr 2026",  98,  6,  533, "Directly reported (HRSS monthly report, 5 May 2026)",
  "May 2026",  64,  5,  289, "Directly reported (HRSS via TBS, 5 Jun 2026)",
  "Jun 2026",  58,  9,  346, "Directly reported (HRSS via TBS, 2 Jul 2026)"
)

df <- months_data %>%
  mutate(
    date_obj    = my(month_label),
    death_rate  = (deaths / incidents) * 100,
    injury_rate = injuries / incidents
  ) %>%
  arrange(date_obj)

write_csv(df, "/home/claude/bd_violence_v2/output/monthly_dataset_verified.csv")

cat("\n== Reconciliation check against HRSS half-yearly report ==\n")
cat("Sum incidents:", sum(df$incidents), "(HRSS half-yearly report: 830)\n")
cat("Sum deaths:   ", sum(df$deaths),    "(HRSS half-yearly report: 56)\n")
cat("Sum injuries: ", sum(df$injuries),  "(HRSS half-yearly report: 5,246)\n")

# -----------------------------------------------------------------------------
# 2. Party-conflict configuration (H1 2026 aggregate, directly reported)
# -----------------------------------------------------------------------------
composition_h1 <- tibble::tribble(
  ~category,          ~incidents,
  "BNP internal",      273,
  "BNP-Jamaat",        290,
  "BNP-Awami League",   77,
  "BNP-NCP",            33,
  "Awami League-NCP",    3,
  "BNP-other party",    97,
  "Other",              57
) %>%
  mutate(share_pct = round(incidents / sum(incidents) * 100, 1))

write_csv(composition_h1, "/home/claude/bd_violence_v2/output/table_composition_h1.csv")
cat("\n== H1 2026 conflict configuration (directly reported) ==\n")
print(composition_h1)
cat("Sum check:", sum(composition_h1$incidents), "(expect 830)\n")

# Monthly party breakdown - ONLY for months where HRSS reported it (Feb, Mar, May, Jun)
# Jan and Apr breakdowns were not available in any located source and are
# left out rather than estimated.
monthly_composition <- tibble::tribble(
  ~month_label, ~category,           ~incidents, ~deaths, ~injuries,
  "Feb 2026", "BNP internal",  95, 8, 599,
  "Feb 2026", "BNP-Jamaat",   191, 1, 998,
  "Mar 2026", "BNP internal",  45, 9, 501,
  "Mar 2026", "BNP-Jamaat",    16, 5, 109,
  "Mar 2026", "BNP-Awami League", 22, 2, 156,
  "Mar 2026", "BNP-NCP",        2, 0,  11,
  "Mar 2026", "BNP-other party",21, 0,  39,
  "Mar 2026", "Other",          7, 2,  96,
  "May 2026", "BNP internal",  18, 0, 114,
  "May 2026", "BNP-Jamaat",    10, 1,  49,
  "May 2026", "BNP-Awami League", 14, 2, 72,
  "Jun 2026", "BNP internal",  21, 3, 146,
  "Jun 2026", "BNP-Jamaat",     8, 2,  36,
  "Jun 2026", "BNP-Awami League", 14, 2, 115,
  "Jun 2026", "BNP-NCP",        5, 0,  18,
  "Jun 2026", "BNP-other party",5, 0,   9,
  "Jun 2026", "Other",          5, 2,  22
)
write_csv(monthly_composition, "/home/claude/bd_violence_v2/output/monthly_composition_partial.csv")

# -----------------------------------------------------------------------------
# 3. Descriptive statistics (N = 6 -- explicitly noted as a small sample)
# -----------------------------------------------------------------------------
desc_vars <- c("incidents", "deaths", "injuries", "death_rate", "injury_rate")
desc_table <- df %>%
  select(all_of(desc_vars)) %>%
  summarise(across(everything(), list(
    Mean = ~mean(.x), Median = ~median(.x), Min = ~min(.x), Max = ~max(.x), SD = ~sd(.x)
  ))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_(?=[^_]+$)") %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

cat("\n== Table 1: Descriptive statistics (N = 6 months) ==\n")
print(desc_table, n = Inf)
write_csv(desc_table, "/home/claude/bd_violence_v2/output/table1_descriptive_stats.csv")

# -----------------------------------------------------------------------------
# 4. Year-on-year comparison (H1 2026 vs H1 2025, directly reported by HRSS)
# -----------------------------------------------------------------------------
yoy <- tibble::tribble(
  ~Indicator, ~`H1 2025`, ~`H1 2026`, ~`Pct change`,
  "Incidents", 529, 830, round((830-529)/529*100,1),
  "Deaths",     79,  56, round((56-79)/79*100,1),
  "Injuries", 4124,5246, round((5246-4124)/4124*100,1)
)
cat("\n== Table 2: Year-on-year comparison ==\n")
print(yoy)
write_csv(yoy, "/home/claude/bd_violence_v2/output/table2_yoy_comparison.csv")

# -----------------------------------------------------------------------------
# 5. Exploratory correlation (association only, N=6 -- indicative, not robust)
# -----------------------------------------------------------------------------
cor_inc_deaths   <- cor(df$incidents, df$deaths)
cor_inc_injuries <- cor(df$incidents, df$injuries)
cat(sprintf("\nIncidents vs deaths:   r = %.3f (N=6, indicative only)\n", cor_inc_deaths))
cat(sprintf("Incidents vs injuries: r = %.3f (N=6, indicative only)\n", cor_inc_injuries))

# -----------------------------------------------------------------------------
# 6. Figures
# -----------------------------------------------------------------------------
theme_brief <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

src_caption <- "Source: HRSS monthly reports, Jan 2026 calculated (see notes)"

# FIGURE 1: monthly incidents
fig1 <- ggplot(df, aes(x = date_obj, y = incidents)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = incidents), vjust = -0.5, size = 3.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(title = "Figure 1: Monthly Political-Violence Incidents, Jan-Jun 2026",
       x = "Month", y = "Number of Incidents", caption = src_caption) +
  theme_brief
ggsave("/home/claude/bd_violence_v2/figures/figure1_monthly_incidents.png", fig1, width = 8, height = 5, dpi = 150)

# FIGURE 2: deaths and injuries by month (separate panels)
df_long <- df %>% select(date_obj, deaths, injuries) %>%
  pivot_longer(c(deaths, injuries), names_to = "metric", values_to = "count") %>%
  mutate(metric = factor(metric, levels = c("deaths","injuries")))
fig2 <- ggplot(df_long, aes(x = date_obj, y = count, fill = metric)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  scale_fill_manual(values = c(deaths = "firebrick", injuries = "darkorange")) +
  labs(title = "Figure 2: Deaths and Injuries from Political Violence by Month",
       x = "Month", y = "Count", caption = src_caption) +
  theme_brief + theme(legend.position = "none", strip.text = element_text(face="bold"))
ggsave("/home/claude/bd_violence_v2/figures/figure2_deaths_injuries.png", fig2, width = 8, height = 6.5, dpi = 150)

# FIGURE 3: severity rates
df_rates <- df %>% select(date_obj, death_rate, injury_rate) %>%
  pivot_longer(c(death_rate, injury_rate), names_to = "rate_type", values_to = "value")
fig3 <- ggplot(df_rates, aes(x = date_obj, y = value, color = rate_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~rate_type, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(death_rate="Deaths per 100 Incidents", injury_rate="Injuries per Incident"))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  scale_color_manual(values = c(death_rate="darkred", injury_rate="darkorange")) +
  labs(title = "Figure 3: Violence Intensity - Severity per Incident",
       x = "Month", y = "Rate", caption = src_caption) +
  theme_brief + theme(legend.position="none", strip.text = element_text(face="bold"))
ggsave("/home/claude/bd_violence_v2/figures/figure3_severity_rates.png", fig3, width = 8, height = 6, dpi = 150)

# FIGURE 4: H1 2026 conflict configuration (directly reported aggregate)
comp_plot <- composition_h1 %>% mutate(category = fct_reorder(category, incidents))
fig4 <- ggplot(comp_plot, aes(x = category, y = incidents)) +
  geom_col(fill = "#2c7bb6") +
  geom_text(aes(label = paste0(incidents, " (", share_pct, "%)")), hjust = -0.05, size = 3.3) +
  coord_flip(clip = "off") +
  expand_limits(y = max(comp_plot$incidents) * 1.25) +
  labs(title = "Figure 4: Political Violence by Conflict Category, H1 2026",
       x = NULL, y = "Number of Incidents",
       caption = "Source: HRSS half-yearly report (Jan-Jun 2026), via The Daily Star") +
  theme_brief + theme(legend.position = "none")
ggsave("/home/claude/bd_violence_v2/figures/figure4_composition.png", fig4, width = 8, height = 5, dpi = 150)

cat("\nAll figures and tables written.\n")
