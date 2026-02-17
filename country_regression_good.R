############################################################
# COUNTRY-LEVEL REPLICATION
############################################################

# ---------------------------
# 1. Packages
# ---------------------------
pkgs <- c("data.table","dplyr","fixest","wbstats")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)>0) install.packages(to_install)

library(data.table)
library(dplyr)
library(fixest)
library(wbstats)

# ---------------------------
# 2. Load data
# ---------------------------
df <- fread("C://Users/dluzzati/Downloads/save_mike_new.csv") %>%
  as_tibble() %>%
  mutate(
    isocode = toupper(as.character(isocode)),
    year = as.integer(year)
  ) %>%
  filter(year >= 1970, year <= 2014)

# ---------------------------
# 4. Build log GDP per capita + growth
# ---------------------------
if("gdp_pc" %in% names(df)){
  df <- df %>% mutate(log_gdppc = log(gdp_pc))
} else if("log_gdp_pc" %in% names(df)){
  df <- df %>% mutate(log_gdppc = as.numeric(log_gdp_pc))
} else if("log_gdppc" %in% names(df)){
  df <- df %>% mutate(log_gdppc = as.numeric(log_gdppc))
} else {
  stop("GDP per capita variable not found.")
}

df <- df %>%
  arrange(isocode, year) %>%
  group_by(isocode) %>%
  mutate(
    # Dep vars
    growth1  = log_gdppc - lag(log_gdppc, 1),
    growth2  = (lead(log_gdppc,  2) - log_gdppc) /  2,
    growth5  = (lead(log_gdppc,  5) - log_gdppc) /  5,
    growth10 = (lead(log_gdppc, 10) - log_gdppc) / 10
  ) %>%
  ungroup()

# ---------------------------
# 5. Initial GDP (1970) as country constant
# ---------------------------
init70 <- df %>%
  filter(year == 1970) %>%
  select(isocode, init_gdp70 = log_gdppc)

df <- df %>% left_join(init70, by = "isocode")

# ---------------------------
# 6. Create lagged RHS variables
# ---------------------------

controls <- c("hc","trade_open","csh_i","csh_c","pl_i","manuf_share","init_gdp70")

# check columns exist before lagging
needed_now <- c(controls, "D","D_nat","D_int")
missing_now <- needed_now[!needed_now %in% names(df)]
if(length(missing_now) > 0){
  stop(paste("Missing variables:", paste(missing_now, collapse = ", ")))
}

# variables to lag
to_lag <- setdiff(c("D","D_nat","D_int","hc","trade_open","csh_i","csh_c","pl_i","manuf_share"), "init_gdp70")

df <- df %>%
  arrange(isocode, year) %>%
  group_by(isocode) %>%
  mutate(across(all_of(to_lag), ~ dplyr::lag(., 1), .names = "{.col}_l1")) %>%
  ungroup()

# build the lagged controls list

controls_l1 <- c(paste0(setdiff(controls, "init_gdp70"), "_l1"), "init_gdp70")

# ---------------------------
# 7. Non-overlapping samples
# ---------------------------
nonoverlap <- function(data, h){
  growth_var <- paste0("growth", h)
  data %>%
    filter(!is.na(.data[[growth_var]])) %>%
    filter((year - 1970) %% h == 0)
}

df2  <- nonoverlap(df, 2)
df5  <- nonoverlap(df, 5)
df10 <- nonoverlap(df, 10)

# ---------------------------
# 8. Regressions (Year FE + clustered SE)
#    growth_h at t uses regressors at t-1
# ---------------------------
f1 <- as.formula(
  paste0("growth1 ~ D_l1 + ", paste(controls_l1, collapse=" + "), " | year")
)

f2 <- as.formula(
  paste0("growth1 ~ D_nat_l1 + D_int_l1 + ", paste(controls_l1, collapse=" + "), " | year")
)

f9 <- as.formula(
  paste0("growth2 ~ D_l1 + ", paste(controls_l1, collapse=" + "), " | year")
)

f10 <- as.formula(
  paste0("growth5 ~ D_l1 + ", paste(controls_l1, collapse=" + "), " | year")
)

f11 <- as.formula(
  paste0("growth10 ~ D_l1 + ", paste(controls_l1, collapse=" + "), " | year")
)

m1  <- feols(f1,  data = df,   cluster = ~isocode)
m2  <- feols(f2,  data = df,   cluster = ~isocode)
m9  <- feols(f9,  data = df2,  cluster = ~isocode)
m10 <- feols(f10, data = df5,  cluster = ~isocode)
m11 <- feols(f11, data = df10, cluster = ~isocode)

etable(m1, m2, m9, m10, m11, se = "cluster")

