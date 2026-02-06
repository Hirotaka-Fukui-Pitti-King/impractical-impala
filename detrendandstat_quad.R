library(tidyverse)
library(kableExtra)

SSA<-read_csv(file="GDP_SSA_WDI.csv")
ASIA<-read_csv(file="GDP_ASIA_WDI.csv")
LA<-read_csv(file="GDP_LA_WDI.csv")

SSA <- SSA %>%
  mutate(Variable = case_when(
    `Series Code` == "NE.EXP.GNFS.ZS" ~ "Xper",  # Exports of goods and services (% of GDP)
    `Series Code` == "NY.GDP.PCAP.KN" ~ "Y",  # GDP per capita (constant LCU)
    `Series Code` == "NE.GDI.TOTL.ZS" ~ "Iper",  # Gross capital formation (% of GDP)
    `Series Code` == "NE.CON.PRVT.ZS" ~ "Cper",  # Households and NPISHs final consumption expenditure (% of GDP)	
    `Series Code` == "NE.IMP.GNFS.ZS" ~ "Mper",  # Imports of goods and services (% of GDP)	
    TRUE ~ NA_character_  # それ以外はNA
  ))
ASIA <- ASIA %>%
  mutate(Variable = case_when(
    `Series Code` == "NE.EXP.GNFS.ZS" ~ "Xper",  # Exports of goods and services (% of GDP)
    `Series Code` == "NY.GDP.PCAP.KN" ~ "Y",  # GDP per capita (constant LCU)
    `Series Code` == "NE.GDI.TOTL.ZS" ~ "Iper",  # Gross capital formation (% of GDP)
    `Series Code` == "NE.CON.PRVT.ZS" ~ "Cper",  # Households and NPISHs final consumption expenditure (% of GDP)	
    `Series Code` == "NE.IMP.GNFS.ZS" ~ "Mper",  # Imports of goods and services (% of GDP)	
    TRUE ~ NA_character_  # それ以外はNA
  ))
LA <- LA %>%
  mutate(Variable = case_when(
    `Series Code` == "NE.EXP.GNFS.ZS" ~ "Xper",  # Exports of goods and services (% of GDP)
    `Series Code` == "NY.GDP.PCAP.KN" ~ "Y",  # GDP per capita (constant LCU)
    `Series Code` == "NE.GDI.TOTL.ZS" ~ "Iper",  # Gross capital formation (% of GDP)
    `Series Code` == "NE.CON.PRVT.ZS" ~ "Cper",  # Households and NPISHs final consumption expenditure (% of GDP)	
    `Series Code` == "NE.IMP.GNFS.ZS" ~ "Mper",  # Imports of goods and services (% of GDP)	
    TRUE ~ NA_character_  # それ以外はNA
  ))

SSA_l <- SSA %>%
  pivot_longer(
    cols = starts_with("1960"):starts_with("2019"),  # 年度列を選択
    names_to = "Year",                            # 縦持ちにする列名
    values_to = "Value"                           # 値を格納する列名
  ) %>%
  mutate(Year = as.integer(Year))
ASIA_l <- ASIA %>%
  pivot_longer(
    cols = starts_with("1960"):starts_with("2019"),  # 年度列を選択
    names_to = "Year",                            # 縦持ちにする列名
    values_to = "Value"                           # 値を格納する列名
  ) %>%
  mutate(Year = as.integer(Year))
LA_l <- LA %>%
  pivot_longer(
    cols = starts_with("1960"):starts_with("2019"),  # 年度列を選択
    names_to = "Year",                            # 縦持ちにする列名
    values_to = "Value"                           # 値を格納する列名
  ) %>%
  mutate(Year = as.integer(Year))

SSA_l<-SSA_l |>
select(`Country Code`, Variable, Year, Value)
ASIA_l<-ASIA_l |>
select(`Country Code`, Variable, Year, Value)
LA_l<-LA_l |>
select(`Country Code`, Variable, Year, Value)

WDI_l<-bind_rows(SSA_l, ASIA_l, LA_l)

WDI_valid <- WDI_l %>%
  filter(Variable %in% c("Y", "Cper", "Iper", "Xper", "Mper")) %>%
  arrange(`Country Code`, Variable, Year) %>%
  group_by(`Country Code`, Variable) %>%
  mutate(
    valid = ifelse(!is.na(Value) & Value > 0, 1, 0),
    group = ifelse(valid == 1, cumsum(lag(valid, default = 0) == 0 & valid == 1), NA)
  ) %>%
  filter(valid == 1) %>%  # これが重要！valid==1の行だけに限定
  group_by(`Country Code`, Variable, group) %>%
  summarise(run_length = n(), .groups = "drop") %>%
  group_by(`Country Code`, Variable) %>%
  summarise(max_run = max(run_length), .groups = "drop")

WDI_valid_wide <- WDI_valid %>%
  filter(max_run >= 30) %>%
  pivot_wider(names_from = Variable, values_from = max_run)

WDI_cleaned <- WDI_l %>%
  filter(Variable %in% c("Y", "Cper", "Iper", "Xper", "Mper")) %>%
  arrange(`Country Code`, Variable, Year) %>%
  group_by(`Country Code`, Variable) %>%
  mutate(
    valid = ifelse(!is.na(Value) & Value > 0, 1, 0),
    group = cumsum(lag(valid, default = 0) == 0 & valid == 1),
    valid_group = ifelse(valid == 1, group, NA)
  ) %>%
  group_by(`Country Code`, Variable, valid_group) %>%
  mutate(run_length = n()) %>%
  ungroup() %>%
  filter(!is.na(valid_group) & run_length >= 30) %>%
  select(`Country Code`, Variable, Year, Value)

WDI_cleaned %>%
  distinct(`Country Code`, Variable)

countries_with_all_vars <- WDI_cleaned %>%
  distinct(`Country Code`, Variable) %>%
  count(`Country Code`) %>%
  filter(n == 5) %>%
  pull(`Country Code`)

WDI_cleaned_allvars <- WDI_cleaned %>%
  filter(`Country Code` %in% countries_with_all_vars)

WDI_cleaned_wide <- WDI_cleaned_allvars %>%
  pivot_wider(
    names_from = Variable,     # 各変数名が列名になる
    values_from = Value        # 値は Value 列から取得
  )

WDI_cleaned_wide <- WDI_cleaned_wide %>%
  mutate(
    C = Y * Cper / 100,
    I = Y * Iper / 100,
    X = Y * Xper / 100,
    M = Y * Mper / 100,
    TB = (X - M) / Y
  )

library(countrycode) 

WDI_cleaned_wide <- WDI_cleaned_wide %>% 
  mutate(Region = countrycode(`Country Code`, origin = "iso3c", destination = "region"))

quad_detrend <- function(y, t) {
  ok <- is.finite(y) & is.finite(t)
  out <- rep(NA_real_, length(y))
  if (sum(ok) >= 30) {  # 最低限の観測数
    fit <- lm(y[ok] ~ t[ok] + I(t[ok]^2))
    out[ok] <- resid(fit)
  }
  out
}

# =========================
# 1) Y,C,I は log → 二次トレンド除去（log-quadratic）
# 2) tby(TB) は log を取らずに二次トレンド除去（quadratic）
# =========================
WDI_quadcycle <- WDI_cleaned_wide %>%
  arrange(`Country Code`, Year) %>%
  group_by(`Country Code`) %>%
  mutate(
    # 国ごとの時間トレンド（1,2,3,...）
    t = Year - min(Year, na.rm = TRUE) + 1,

    # log series（対数系列）
    Y_log = ifelse(is.finite(Y) & Y > 0, log(Y), NA_real_),
    C_log = ifelse(is.finite(C) & C > 0, log(C), NA_real_),
    I_log = ifelse(is.finite(I) & I > 0, log(I), NA_real_),

    # log-quadratic detrended cycles（残差）
    Y_cyc_lq = quad_detrend(Y_log, t),
    C_cyc_lq = quad_detrend(C_log, t),
    I_cyc_lq = quad_detrend(I_log, t),

    # tby は「そのまま」quadratic detrend（残差）

    tby_cyc_q = quad_detrend(TB, t)

  ) %>%
  ungroup()

sd_by_country <- WDI_quadcycle %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    sd_Y  = sd(Y_cyc_lq,   na.rm = TRUE) * 100,
    sd_C  = sd(C_cyc_lq,   na.rm = TRUE) * 100,
    sd_I  = sd(I_cyc_lq,   na.rm = TRUE) * 100,
    sd_TB = sd(tby_cyc_q,  na.rm = TRUE) * 100,
    .groups = "drop"
  )

sd_by_region <- sd_by_country %>%
  group_by(Region) %>%
  summarise(
    sd_Y_mean  = mean(sd_Y,  na.rm = TRUE),
    sd_Y_sd    = sd(sd_Y,    na.rm = TRUE),

    sd_C_mean  = mean(sd_C,  na.rm = TRUE),
    sd_C_sd    = sd(sd_C,    na.rm = TRUE),

    sd_I_mean  = mean(sd_I,  na.rm = TRUE),
    sd_I_sd    = sd(sd_I,    na.rm = TRUE),

    sd_TB_mean = mean(sd_TB, na.rm = TRUE),
    sd_TB_sd   = sd(sd_TB,   na.rm = TRUE),
    .groups = "drop"
  )

sd_by_country <- sd_by_country %>%
  mutate(
    sdC_over_sdY = sd_C / sd_Y,
    sdI_over_sdY = sd_I / sd_Y
  )

sd_ratio_by_region <- sd_by_country %>%
  group_by(Region) %>%
  summarise(
    sdC_over_sdY_mean = mean(sdC_over_sdY, na.rm = TRUE),
    sdC_over_sdY_sd   = sd(sdC_over_sdY,   na.rm = TRUE),

    sdI_over_sdY_mean = mean(sdI_over_sdY, na.rm = TRUE),
    sdI_over_sdY_sd   = sd(sdI_over_sdY,   na.rm = TRUE),
    .groups = "drop"
  )

write_csv(sd_by_country, "sd_by_country_logquad.csv")
write_csv(sd_by_region,  "sd_by_region_logquad.csv")
write_csv(sd_ratio_by_region, "sd_ratio_by_region_logquad.csv")

library(broom)

vars <- c("Y_cyc_lq", "C_cyc_lq", "I_cyc_lq", "tby_cyc_q")

corr_by_country <- WDI_quadcycle %>%
  select(`Country Code`, Region, all_of(vars)) %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    corr_Y_C   = cor(Y_cyc_lq, C_cyc_lq, use = "complete.obs"),
    corr_Y_I   = cor(Y_cyc_lq, I_cyc_lq, use = "complete.obs"),
    corr_Y_TB  = cor(Y_cyc_lq, tby_cyc_q, use = "complete.obs"),
    corr_C_I   = cor(C_cyc_lq, I_cyc_lq, use = "complete.obs"),
    corr_C_TB  = cor(C_cyc_lq, tby_cyc_q, use = "complete.obs"),
    corr_I_TB  = cor(I_cyc_lq, tby_cyc_q, use = "complete.obs"),
    .groups = "drop"
  )

corr_by_region <- corr_by_country %>%
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

write_csv(corr_by_country, "corr_by_country_logquad.csv")
write_csv(corr_by_region,  "corr_by_region_logquad.csv")

acf_by_country <- WDI_quadcycle %>%
  arrange(`Country Code`, Year) %>%
  group_by(`Country Code`, Region) %>%
  summarise(
    acf_Y  = cor(Y_cyc_lq, lag(Y_cyc_lq), use = "complete.obs"),
    acf_C  = cor(C_cyc_lq, lag(C_cyc_lq), use = "complete.obs"),
    acf_I  = cor(I_cyc_lq, lag(I_cyc_lq), use = "complete.obs"),
    acf_TB = cor(tby_cyc_q, lag(tby_cyc_q), use = "complete.obs"),
    .groups = "drop"
  )

acf_by_region <- acf_by_country %>%
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

write_csv(acf_by_country, "acf_by_country_logquad.csv")
write_csv(acf_by_region,  "acf_by_region_logquad.csv")


