# import packages
library(dplyr)
library(plm)

# import data
# df = read.csv("INSERISCI PATH QUI AL DATASET")




# -----------------------------------------
# Econometric specification for sub-periods
# Use 'start' and 'end' to pick a period
# -----------------------------------------
start <- 1995
end <- 2014

df <- df %>% 
  select(-any_of("init_gdp"))

init_gdp <- df %>%
  filter(year == start) %>%
  select(isocode, init_gdp = log_gdppc)
df <- df %>% left_join(init_gdp, by = "isocode")
df_t = df[df$year >= start & df$year <= end, ]


# All controls
fe_t <- plm(
  growth1 ~ D_nat_l1 + D_int_l1 + bwd_nat_l1 + bwd_int_l1 + hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + D_nat_l1*init_gdp + D_int_l1*init_gdp,
  data = df_t,
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)
summary(fe_t,vcov = function(x) vcovHC(x,method = "arellano", type = "HC1", cluster = "group"))


# Without Bwd linkages
fe_t <- plm(
  growth1 ~ D_nat_l1 + D_int_l1 + hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + D_nat_l1*init_gdp + D_int_l1*init_gdp,
  data = df_t,
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)
summary(fe_t,vcov = function(x) vcovHC(x,method = "arellano", type = "HC1", cluster = "group"))




# -----------------------------------------------------------------------------------
# Econometric specification for geographical regions (in development, NOT NEEDED NOW)
# -----------------------------------------------------------------------------------
countries = unique(df_complete$isocode)

# G7 countries
g7 <- c("USA","CAN","DEU","FRA","GBR","ITA","JPN")
# High-income / Advanced economies
high_income <- c("AUS","AUT","BEL","CAN","DEU","DNK","ESP","FIN","FRA","GBR",
                 "GRC","IRL","ITA","JPN","KOR","NLD","PRT","SWE","TWN","USA")
# Emerging / Non-high-income economies
emerging <- c("BRA","CHN","IND","MEX")

# high-income economies
df_hi = df_complete[which(df_complete$isocode %in% high_income),]
df_g7 = df_complete[which(df_complete$isocode %in% g7),]
df_em = df_complete[which(df_complete$isocode %in% emerging),]



fe_hi <- plm(
  growth1 ~ D_nat_l1 + D_int_l1 +hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + init_gdp70 + Dnat_x_init + Dint_x_init,
  data = df_hi[df_hi$year >= start & df_hi$year <= end, ],
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)
summary(fe_hi,vcov = function(x) vcovHC(x,method = "arellano", type = "HC1", cluster = "time"))


# g7 economies
df_g7 = df_complete[which(df_complete$isocode %in% g7),]

fe_g7 <- plm(
  growth1 ~ D_nat_l1 + D_int_l1 + hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + Dnat_x_init + Dint_x_init,
  data = df_g7[df_g7$year >= start & df_g7$year <= end, ],
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)
summary(fe_g7,vcov = function(x) vcovHC(x,method = "arellano", type = "HC1",cluster = "time"))


# Emerging economies
df_em = df_complete[which(df_complete$isocode %in% emerging),]

fe_em <- plm(
  growth1 ~ D_nat_l1 + D_int_l1 +  hc_l1 + trade_open_l1 + csh_i_l1 + csh_c_l1 + pl_i_l1 
  + manuf_share_l1 + Dnat_x_init + Dint_x_init,
  data = df_em[df_em$year >= start & df_em$year <= end, ],,
  index = c('isocode', 'year'),
  model = 'within',
  effect = 'twoways'
)
summary(fe_em,vcov = function(x) vcovHC(x,method = "arellano", type = "HC1",cluster = "time"))




