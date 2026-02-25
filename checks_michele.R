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
library(wbstats)
# ---------------------------
# 2. Load data
# ---------------------------
df <- fread("C://Users/sebas/Documents/Universita_e_Ricerca/Ricerca/Ricerca_personale/WIOD paper/major_review_WE/data/save_mike_GO_share_no23.csv") %>%
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
    growth2  = (dplyr::lead(log_gdppc,  2) - log_gdppc) /  2,
    growth5  = (dplyr::lead(log_gdppc,  5) - log_gdppc) /  5,
    growth10 = (dplyr::lead(log_gdppc, 10) - log_gdppc) / 10
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
# 9. Regressions PLM
# ---------------------------


############# TABLE 6 ########################################################
##############################################################################
library(plm)


fe_wg <- plm(
  growth2 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1  + hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + init_gdp70 + Dnat_x_init + Dint_x_init,
  data = df,
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)


summary(fe_wg,vcov = function(x) vcovHC(x,
                                  method = "arellano",
                                  type = "HC1",
                                  cluster = "group"))











