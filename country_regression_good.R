############################################################
# COUNTRY-LEVEL REPLICATION — TABLE 6 & 7 (all 11 specs each)
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
df <- fread("C://Users/dluzzati/Downloads/save_mike_GO_share_no23.csv") %>%
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
needed_now <- c("hc","trade_open","csh_i","csh_c","pl_i","manuf_share",
                "D","D_nat","D_int",
                "U","U_nat","U_int",
                "bwd_nat","bwd_int",
                "fwd_nat","fwd_int",
                "D_int_In.","D_int_Agr.","D_int_Serv.",
                "U_int_In.","U_int_Agr.","U_int_Serv.")
missing_now <- needed_now[!needed_now %in% names(df)]
if(length(missing_now) > 0){
  stop(paste("Missing variables:", paste(missing_now, collapse = ", ")))
}
# variables to lag
to_lag <- c("D","D_nat","D_int",
            "U","U_nat","U_int",
            "bwd_nat","bwd_int",
            "fwd_nat","fwd_int",
            "D_int_In.","D_int_Agr.","D_int_Serv.",
            "U_int_In.","U_int_Agr.","U_int_Serv.",
            "hc","trade_open","csh_i","csh_c","pl_i","manuf_share")
df <- df %>%
  arrange(isocode, year) %>%
  group_by(isocode) %>%
  mutate(across(all_of(to_lag), ~ dplyr::lag(., 1), .names = "{.col}_l1")) %>%
  ungroup()
# interaction terms (D and U)
df <- df %>%
  mutate(
    Dnat_x_init = D_nat_l1 * init_gdp70,
    Dint_x_init = D_int_l1 * init_gdp70,
    Unat_x_init = U_nat_l1 * init_gdp70,
    Uint_x_init = U_int_l1 * init_gdp70
  )
# ---------------------------
# 7. Control strings
# ---------------------------
# Base controls: HC, openness, inv share, cons share, price index, initial GDP
# manuf_share is NOT part of the base — it enters from model (2) onward
ctrl      <- "hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 + init_gdp70"
ctrl_mfg  <- paste0(ctrl, " + manuf_share_l1")
ctrl_cfe  <- "hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 + manuf_share_l1"
# ---------------------------
# 8. Non-overlapping samples
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
# 9. Regressions (Year FE + clustered SE)
# ---------------------------


############# TABLE 6 ########################################################
##############################################################################

# (1) D, no manuf_share
f1 <- as.formula(paste0(
  "growth1 ~ D_l1 + ", ctrl, " | year"))

# (2) D + manuf_share
f2 <- as.formula(paste0(
  "growth1 ~ D_l1 + ", ctrl_mfg, " | year"))

# (3) D_nat + D_int + manuf_share
f3 <- as.formula(paste0(
  "growth1 ~ D_nat_l1 + D_int_l1 + ", ctrl_mfg, " | year"))

# (4) bwd_nat + bwd_int + manuf_share
f4 <- as.formula(paste0(
  "growth1 ~ bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (5) D_nat + D_int + bwd centrality + manuf_share
f5 <- as.formula(paste0(
  "growth1 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (6) Sector-specific international D + manuf_share
f6 <- as.formula(paste0(
  "growth1 ~ D_nat_l1 + `D_int_In._l1` + `D_int_Agr._l1` + `D_int_Serv._l1` + ",
  "bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (7) Interactions D x initial GDP + manuf_share
f7 <- as.formula(paste0(
  "growth1 ~ D_nat_l1 + D_int_l1 + Dnat_x_init + Dint_x_init + ",
  "bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (8) Country FE (init_gdp70 absorbed by country FE)
f8 <- as.formula(paste0(
  "growth1 ~ D_nat_l1 + D_int_l1 + Dnat_x_init + Dint_x_init + ",
  "bwd_nat_l1 + bwd_int_l1 + ", ctrl_cfe, " | year + isocode"))

# (9) 2-year non-overlapping
f9 <- as.formula(paste0(
  "growth2 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (10) 5-year non-overlapping
f10 <- as.formula(paste0(
  "growth5 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

# (11) 10-year non-overlapping
f11 <- as.formula(paste0(
  "growth10 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1 + ", ctrl_mfg, " | year"))

m1  <- feols(f1,  data = df,   cluster = ~isocode)
m2  <- feols(f2,  data = df,   cluster = ~isocode)
m3  <- feols(f3,  data = df,   cluster = ~isocode)
m4  <- feols(f4,  data = df,   cluster = ~isocode)
m5  <- feols(f5,  data = df,   cluster = ~isocode)
m6  <- feols(f6,  data = df,   cluster = ~isocode)
m7  <- feols(f7,  data = df,   cluster = ~isocode)
m8  <- feols(f8,  data = df,   cluster = ~isocode)
m9  <- feols(f9,  data = df2,  cluster = ~isocode)
m10 <- feols(f10, data = df5,  cluster = ~isocode)
m11 <- feols(f11, data = df10, cluster = ~isocode)

etable(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
       se = "cluster")

############# TABLE 7 ########################################################
##############################################################################

# (1) U, no manuf_share
g1 <- as.formula(paste0(
  "growth1 ~ U_l1 + ", ctrl, " | year"))

# (2) U + manuf_share
g2 <- as.formula(paste0(
  "growth1 ~ U_l1 + ", ctrl_mfg, " | year"))

# (3) U_nat + U_int + manuf_share
g3 <- as.formula(paste0(
  "growth1 ~ U_nat_l1 + U_int_l1 + ", ctrl_mfg, " | year"))

# (4) fwd_nat + fwd_int + manuf_share
g4 <- as.formula(paste0(
  "growth1 ~ fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (5) U_nat + U_int + fwd centrality + manuf_share
g5 <- as.formula(paste0(
  "growth1 ~ U_nat_l1 + U_int_l1 + fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (6) Sector-specific international U + manuf_share
g6 <- as.formula(paste0(
  "growth1 ~ U_nat_l1 + `U_int_In._l1` + `U_int_Agr._l1` + `U_int_Serv._l1` + ",
  "fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (7) Interactions U x initial GDP + manuf_share
g7 <- as.formula(paste0(
  "growth1 ~ U_nat_l1 + U_int_l1 + Unat_x_init + Uint_x_init + ",
  "fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (8) Country FE (init_gdp70 absorbed by country FE)
g8 <- as.formula(paste0(
  "growth1 ~ U_nat_l1 + U_int_l1 + Unat_x_init + Uint_x_init + ",
  "fwd_nat_l1 + fwd_int_l1 + ", ctrl_cfe, " | year + isocode"))

# (9) 2-year non-overlapping
g9 <- as.formula(paste0(
  "growth2 ~ U_nat_l1 + U_int_l1 + fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (10) 5-year non-overlapping
g10 <- as.formula(paste0(
  "growth5 ~ U_nat_l1 + U_int_l1 + fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

# (11) 10-year non-overlapping
g11 <- as.formula(paste0(
  "growth10 ~ U_nat_l1 + U_int_l1 + fwd_nat_l1 + fwd_int_l1 + ", ctrl_mfg, " | year"))

n1  <- feols(g1,  data = df,   cluster = ~isocode)
n2  <- feols(g2,  data = df,   cluster = ~isocode)
n3  <- feols(g3,  data = df,   cluster = ~isocode)
n4  <- feols(g4,  data = df,   cluster = ~isocode)
n5  <- feols(g5,  data = df,   cluster = ~isocode)
n6  <- feols(g6,  data = df,   cluster = ~isocode)
n7  <- feols(g7,  data = df,   cluster = ~isocode)
n8  <- feols(g8,  data = df,   cluster = ~isocode)
n9  <- feols(g9,  data = df2,  cluster = ~isocode)
n10 <- feols(g10, data = df5,  cluster = ~isocode)
n11 <- feols(g11, data = df10, cluster = ~isocode)

etable(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11,
       se = "cluster")
