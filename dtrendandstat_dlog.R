library(tidyverse)
library(countrycode)

# =========================
# Settings
# =========================
MIN_T <- 30

# =========================
# 0) Read + standardize variables + long
# =========================
read_wdi_block <- function(path){
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Variable = case_when(
      `Series Code` == "NE.EXP.GNFS.ZS" ~ "Xper",
      `Series Code` == "NY.GDP.PCAP.KN" ~ "Y",
      `Series Code` == "NE.GDI.TOTL.ZS" ~ "Iper",
      `Series Code` == "NE.CON.PRVT.ZS" ~ "Cper",
      `Series Code` == "NE.IMP.GNFS.ZS" ~ "Mper",
      TRUE ~ NA_character_
    )) %>%
    pivot_longer(
      cols = starts_with("1960"):starts_with("2019"),
      names_to = "Year",
      values_to = "Value"
    ) %>%
    mutate(Year = as.integer(Year)) %>%
    select(`Country Code`, Variable, Year, Value)
}

WDI_l <- list(
  read_wdi_block("GDP_SSA_WDI.csv"),
  read_wdi_block("GDP_ASIA_WDI.csv"),
  read_wdi_block("GDP_LA_WDI.csv")
) %>% bind_rows() %>%
  filter(Variable %in% c("Y","Cper","Iper","Xper","Mper"))

# =========================
# 1) Keep countries with >= MIN_T consecutive valid obs for each variable
#    and keep only countries that have all 5 variables.
# =========================
WDI_cleaned <- WDI_l %>%
  arrange(`Country Code`, Variable, Year) %>%
  group_by(`Country Code`, Variable) %>%
  mutate(
    valid = ifelse(!is.na(Value) & Value > 0, 1, 0),
    grp = cumsum(lag(valid, default = 0) == 0 & valid == 1),
    valid_grp = ifelse(valid == 1, grp, NA_integer_)
  ) %>%
  group_by(`Country Code`, Variable, valid_grp) %>%
  mutate(run_length = n()) %>%
  ungroup() %>%
  filter(!is.na(valid_grp) & run_length >= MIN_T) %>%
  select(`Country Code`, Variable, Year, Value)

countries_with_all_vars <- WDI_cleaned %>%
  distinct(`Country Code`, Variable) %>%
  count(`Country Code`) %>%
  filter(n == 5) %>%
  pull(`Country Code`)

WDI_cleaned_allvars <- WDI_cleaned %>%
  filter(`Country Code` %in% countries_with_all_vars)

# =========================
# 2) Wide + construct C,I,X,M and TB
# =========================
WDI_wide <- WDI_cleaned_allvars %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(
    C = Y * Cper / 100,
    I = Y * Iper / 100,
    X = Y * Xper / 100,
    M = Y * Mper / 100,
    TB = (X - M) / Y,
    Region = countrycode(`Country Code`, origin = "iso3c", destination = "region")
  )

# =========================
# 3) Build dlog growth rates (Y,C,I) and TB level
#    dlog(x_t) = log(x_t) - log(x_{t-1})
# =========================
WDI_dlog <- WDI_wide %>%
  arrange(`Country Code`, Year) %>%
  group_by(`Country Code`) %>%
  mutate(
    dlogY = ifelse(is.finite(Y) & Y > 0, log(Y) - log(lag(Y)), NA_real_),
    dlogC = ifelse(is.finite(C) & C > 0, log(C) - log(lag(C)), NA_real_),
    dlogI = ifelse(is.finite(I) & I > 0, log(I) - log(lag(I)), NA_real_),
    TB_lv = TB
  ) %>%
  ungroup()

# =========================
# 4) sd by country/region + ratios
# =========================
sd_by_country_dlog <- WDI_dlog %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    sd_dlogY = sd(dlogY, na.rm = TRUE) * 100,
    sd_dlogC = sd(dlogC, na.rm = TRUE) * 100,
    sd_dlogI = sd(dlogI, na.rm = TRUE) * 100,
    sd_TB    = sd(TB_lv, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    sdC_over_sdY = sd_dlogC / sd_dlogY,
    sdI_over_sdY = sd_dlogI / sd_dlogY
  )

sd_by_region_dlog <- sd_by_country_dlog %>%
  group_by(Region) %>%
  summarise(
    sd_dlogY_mean = mean(sd_dlogY, na.rm = TRUE),
    sd_dlogY_sd   = sd(sd_dlogY,   na.rm = TRUE),

    sd_dlogC_mean = mean(sd_dlogC, na.rm = TRUE),
    sd_dlogC_sd   = sd(sd_dlogC,   na.rm = TRUE),

    sd_dlogI_mean = mean(sd_dlogI, na.rm = TRUE),
    sd_dlogI_sd   = sd(sd_dlogI,   na.rm = TRUE),

    sd_TB_mean    = mean(sd_TB,    na.rm = TRUE),
    sd_TB_sd      = sd(sd_TB,      na.rm = TRUE),
    .groups = "drop"
  )

sd_ratio_by_region_dlog <- sd_by_country_dlog %>%
  group_by(Region) %>%
  summarise(
    sdC_over_sdY_mean = mean(sdC_over_sdY, na.rm = TRUE),
    sdC_over_sdY_sd   = sd(sdC_over_sdY,   na.rm = TRUE),

    sdI_over_sdY_mean = mean(sdI_over_sdY, na.rm = TRUE),
    sdI_over_sdY_sd   = sd(sdI_over_sdY,   na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 5) correlations by country/region
# =========================
corr_by_country_dlog <- WDI_dlog %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    corr_Y_C  = cor(dlogY, dlogC, use = "complete.obs"),
    corr_Y_I  = cor(dlogY, dlogI, use = "complete.obs"),
    corr_Y_TB = cor(dlogY, TB_lv, use = "complete.obs"),
    corr_C_I  = cor(dlogC, dlogI, use = "complete.obs"),
    corr_C_TB = cor(dlogC, TB_lv, use = "complete.obs"),
    corr_I_TB = cor(dlogI, TB_lv, use = "complete.obs"),
    .groups = "drop"
  )

corr_by_region_dlog <- corr_by_country_dlog %>%
  group_by(Region) %>%
  summarise(
    corr_Y_C_mean  = mean(corr_Y_C,  na.rm = TRUE),
    corr_Y_C_sd    = sd(corr_Y_C,    na.rm = TRUE),

    corr_Y_I_mean  = mean(corr_Y_I,  na.rm = TRUE),
    corr_Y_I_sd    = sd(corr_Y_I,    na.rm = TRUE),

    corr_Y_TB_mean = mean(corr_Y_TB, na.rm = TRUE),
    corr_Y_TB_sd   = sd(corr_Y_TB,   na.rm = TRUE),

    corr_C_I_mean  = mean(corr_C_I,  na.rm = TRUE),
    corr_C_I_sd    = sd(corr_C_I,    na.rm = TRUE),

    corr_C_TB_mean = mean(corr_C_TB, na.rm = TRUE),
    corr_C_TB_sd   = sd(corr_C_TB,   na.rm = TRUE),

    corr_I_TB_mean = mean(corr_I_TB, na.rm = TRUE),
    corr_I_TB_sd   = sd(corr_I_TB,   na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 6) 1st-order autocorr by country/region (on dlog series + TB level)
# =========================
acf_by_country_dlog <- WDI_dlog %>%
  arrange(`Country Code`, Year) %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    acf_Y  = cor(dlogY, lag(dlogY), use = "complete.obs"),
    acf_C  = cor(dlogC, lag(dlogC), use = "complete.obs"),
    acf_I  = cor(dlogI, lag(dlogI), use = "complete.obs"),
    acf_TB = cor(TB_lv, lag(TB_lv), use = "complete.obs"),
    .groups = "drop"
  )

acf_by_region_dlog <- acf_by_country_dlog %>%
  group_by(Region) %>%
  summarise(
    acf_Y_mean  = mean(acf_Y,  na.rm = TRUE),
    acf_Y_sd    = sd(acf_Y,    na.rm = TRUE),

    acf_C_mean  = mean(acf_C,  na.rm = TRUE),
    acf_C_sd    = sd(acf_C,    na.rm = TRUE),

    acf_I_mean  = mean(acf_I,  na.rm = TRUE),
    acf_I_sd    = sd(acf_I,    na.rm = TRUE),

    acf_TB_mean = mean(acf_TB, na.rm = TRUE),
    acf_TB_sd   = sd(acf_TB,   na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 7) Export
# =========================
write_csv(sd_by_country_dlog, "sd_by_country_dlog.csv")
write_csv(sd_by_region_dlog,  "sd_by_region_dlog.csv")
write_csv(sd_ratio_by_region_dlog, "sd_ratio_by_region_dlog.csv")

write_csv(corr_by_country_dlog, "corr_by_country_dlog.csv")
write_csv(corr_by_region_dlog,  "corr_by_region_dlog.csv")

write_csv(acf_by_country_dlog, "acf_by_country_dlog.csv")
write_csv(acf_by_region_dlog,  "acf_by_region_dlog.csv")
