###############################################################################
# Upstreamness & Downstreamness from MRIO data
# With national / international decomposition
###############################################################################

library(data.table)

# ---- 1. READ DATA -----------------------------------------------------------
# Adjust path to your file (csv or xlsx)
dt <- fread("C://Users/dluzzati/Downloads/io_full.csv")

if (names(dt)[1] %in% c("V1", "")) {
  dt[[1]] <- NULL
}

# ---- 2. IDENTIFY COLUMNS ---------------------------------------------------

all_cols <- names(dt)
meta_cols <- c("year", "row_country", "row_isic3")
non_meta  <- setdiff(all_cols, meta_cols)

GO_col  <- "xTOT_xGO"
FD_cols <- grep("_FD$", non_meta, value = TRUE)
Z_cols  <- setdiff(non_meta, c(FD_cols, GO_col))

fd_countries <- sub("_FD$", "", FD_cols)

get_country <- function(col, countries) {
  for (cc in countries) {
    if (startsWith(col, paste0(cc, "_"))) return(cc)
  }
  return(NA)
}

col_country <- sapply(Z_cols, get_country, countries = fd_countries)
col_sector  <- mapply(function(col, cc) sub(paste0("^", cc, "_"), "", col), Z_cols, col_country)

col_info <- data.table(col = Z_cols, col_country = col_country, col_sector = col_sector)

N <- length(Z_cols)
countries <- unique(col_info$col_country)
n_countries <- length(countries)
n_sectors <- N / n_countries
years <- sort(unique(dt$year))

cat("N =", N, "| Countries:", n_countries, "| Sectors per country:", n_sectors, "\n")

# ---- 3. BUILD DOMESTIC MASK (same for all years) ----------------------------
# dom_mask[i,j] = 1 if row i and col j belong to same country, 0 otherwise

sector_country <- col_info$col_country
dom_mask <- outer(sector_country, sector_country, "==") * 1

cat("Domestic mask: ", sum(dom_mask), "domestic entries,",
    sum(1 - dom_mask), "foreign entries out of", N*N, "total\n")

# ---- 4. MAIN LOOP OVER YEARS -----------------------------------------------

results_list <- list()

for (yr in years) {
  
  cat("\n=== Processing year:", yr, "===\n")
  
  dty <- dt[year == yr]
  
  ind_rows <- dty[row_country %in% fd_countries]
  ind_rows[, row_id := paste0(row_country, "_", row_isic3)]
  ind_rows <- ind_rows[match(Z_cols, row_id)]
  
  if (any(is.na(ind_rows$row_id))) {
    warning(paste("Year", yr, ": row mismatch - skipping"))
    next
  }
  
  va_row <- dty[row_isic3 == "xVA"]
  if (nrow(va_row) == 0) {
    warning(paste("No VA row found for year", yr, "- skipping"))
    next
  }
  
  # --- Z matrix ---
  Z <- as.matrix(ind_rows[, ..Z_cols])
  
  # --- VA ---
  VA <- as.numeric(va_row[1, ..Z_cols])
  names(VA) <- Z_cols
  
  # --- FD ---
  FD_mat <- as.matrix(ind_rows[, ..FD_cols])
  FD <- rowSums(FD_mat)
  names(FD) <- Z_cols
  
  # --- GO from row identity ---
  II_sold <- rowSums(Z)
  II_used <- colSums(Z)
  GO <- II_sold + FD
  names(GO) <- Z_cols
  
  GO_safe <- ifelse(GO == 0, 1, GO)
  
  # --- Full A and B ---
  A <- t(t(Z) / GO_safe)
  B <- Z / GO_safe
  
  # --- Domestic A and B (zero out cross-country flows) ---
  A_dom <- A * dom_mask
  B_dom <- B * dom_mask
  
  I_mat <- diag(N)
  ones  <- rep(1, N)
  
  # ---- FULL UPSTREAMNESS & DOWNSTREAMNESS -----------------------------------
  G_inv_full <- solve(I_mat - B)
  U_full <- as.numeric(G_inv_full %*% ones)
  
  inv_IminAt_full <- solve(I_mat - t(A))
  D_full <- as.numeric(inv_IminAt_full %*% ones)
  
  # ---- AUTARKY UPSTREAMNESS & DOWNSTREAMNESS --------------------------------
  G_inv_dom <- solve(I_mat - B_dom)
  U_aut <- as.numeric(G_inv_dom %*% ones)
  
  inv_IminAt_dom <- solve(I_mat - t(A_dom))
  D_aut <- as.numeric(inv_IminAt_dom %*% ones)
  
  # ---- GVC COMPONENT --------------------------------------------------------
  U_gvc <- U_full - U_aut
  D_gvc <- D_full - D_aut
  
  names(U_full) <- names(U_aut) <- names(U_gvc) <- Z_cols
  names(D_full) <- names(D_aut) <- names(D_gvc) <- Z_cols
  
  cat("  U_full range:", round(min(U_full), 3), "-", round(max(U_full), 3), "\n")
  cat("  U_aut  range:", round(min(U_aut), 3), "-", round(max(U_aut), 3), "(should be >= 1)\n")
  cat("  U_gvc  range:", round(min(U_gvc), 3), "-", round(max(U_gvc), 3), "(should be >= 0)\n")
  cat("  D_full range:", round(min(D_full), 3), "-", round(max(D_full), 3), "\n")
  cat("  D_aut  range:", round(min(D_aut), 3), "-", round(max(D_aut), 3), "(should be >= 1)\n")
  cat("  D_gvc  range:", round(min(D_gvc), 3), "-", round(max(D_gvc), 3), "(should be >= 0)\n")
  
  # ---- COLLECT RESULTS ------------------------------------------------------
  res <- data.table(
    year    = yr,
    country = sector_country,
    sector  = col_info$col_sector,
    GO      = GO,
    VA      = VA,
    FD      = FD,
    II_sold = II_sold,
    II_used = II_used,
    Upstreamness       = U_full,
    Upstreamness_aut   = U_aut,
    Upstreamness_gvc   = U_gvc,
    Downstreamness     = D_full,
    Downstreamness_aut = D_aut,
    Downstreamness_gvc = D_gvc
  )
  
  results_list[[as.character(yr)]] <- res
}

# ---- 5. COMBINE AND SAVE ---------------------------------------------------
results <- rbindlist(results_list)

# Sanity checks
results[, check_U := abs(Upstreamness - Upstreamness_aut - Upstreamness_gvc)]
results[, check_D := abs(Downstreamness - Downstreamness_aut - Downstreamness_gvc)]
cat("\nMax decomposition error U:", max(results$check_U), "\n")
cat("Max decomposition error D:", max(results$check_D), "\n")
results[, c("check_U", "check_D") := NULL]

cat("Min U_full:", min(results$Upstreamness), "(should be >= 1)\n")
cat("Min U_aut: ", min(results$Upstreamness_aut), "(should be >= 1)\n")
cat("Min U_gvc: ", min(results$Upstreamness_gvc), "(should be >= 0)\n")
cat("Min D_full:", min(results$Downstreamness), "(should be >= 1)\n")
cat("Min D_aut: ", min(results$Downstreamness_aut), "(should be >= 1)\n")
cat("Min D_gvc: ", min(results$Downstreamness_gvc), "(should be >= 0)\n")

# World GO-weighted averages
check <- results[, .(wU = sum(Upstreamness * GO) / sum(GO),
                     wD = sum(Downstreamness * GO) / sum(GO)), by = year]
cat("\nWorld GO-weighted averages (U ≈ D):\n")
print(check)
