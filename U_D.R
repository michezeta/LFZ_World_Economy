###############################################################################
# Upstreamness & Downstreamness from MRIO data
# With national / international decomposition
###############################################################################

library(data.table)

# ---- 1. READ DATA -----------------------------------------------------------
# Adjust path to your file (csv or xlsx)
dt <- fread("C://Users/dluzzati/Downloads/io_full.csv")

# Drop first column if it's just a row index
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

# Get country list from FD columns
fd_countries <- sub("_FD$", "", FD_cols)

# Parse country and sector from Z column names
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

# ---- 3. MAIN LOOP OVER YEARS -----------------------------------------------

results_list <- list()

for (yr in years) {
  
  cat("\n=== Processing year:", yr, "===\n")
  
  dty <- dt[year == yr]
  
  # Industry rows
  ind_rows <- dty[row_country %in% fd_countries]
  
  # Match and reorder rows to Z_cols
  ind_rows[, row_id := paste0(row_country, "_", row_isic3)]
  
  missing_in_rows <- setdiff(Z_cols, ind_rows$row_id)
  missing_in_cols <- setdiff(ind_rows$row_id, Z_cols)
  
  if (length(missing_in_rows) > 0) {
    cat("  WARNING: Z_cols not found in rows:", head(missing_in_rows, 5), "\n")
  }
  if (length(missing_in_cols) > 0) {
    cat("  WARNING: row IDs not found in Z_cols:", head(missing_in_cols, 5), "\n")
  }
  
  ind_rows <- ind_rows[match(Z_cols, row_id)]
  
  if (any(is.na(ind_rows$row_id))) {
    warning(paste("Year", yr, ": could not match all rows to columns - skipping"))
    next
  }
  
  # VA row
  va_row <- dty[row_isic3 == "xVA"]
  if (nrow(va_row) == 0) {
    warning(paste("No VA row found for year", yr, "- skipping"))
    next
  }
  
  # --- Z matrix (N x N): Z[i,j] = flow from i to j ---
  Z <- as.matrix(ind_rows[, ..Z_cols])
  
  # --- Value added (from column perspective) ---
  VA <- as.numeric(va_row[1, ..Z_cols])
  names(VA) <- Z_cols
  
  # --- Final demand from FD columns ---
  FD_mat <- as.matrix(ind_rows[, ..FD_cols])
  FD <- rowSums(FD_mat)
  names(FD) <- Z_cols
  
  # --- Intermediate inputs sold (row sums of Z) ---
  II_sold <- rowSums(Z)
  names(II_sold) <- Z_cols
  
  # --- Intermediate inputs used/purchased (col sums of Z) ---
  II_used <- colSums(Z)
  names(II_used) <- Z_cols
  
  # --- Gross output: computed from row identity GO = II_sold + FD ---
  GO <- II_sold + FD
  names(GO) <- Z_cols
  
  # Cross-check with column identity: GO should also ≈ II_used + VA
  GO_col_check <- II_used + VA
  max_diff <- max(abs(GO - GO_col_check) / pmax(GO, 1))
  cat("  Max relative GO discrepancy (row vs col identity):", round(max_diff, 6), "\n")
  
  GO_safe <- ifelse(GO == 0, 1, GO)
  
  # --- A matrix: A[i,j] = Z[i,j] / GO[j] ---
  A <- t(t(Z) / GO_safe)
  
  # --- B matrix: B[i,j] = Z[i,j] / GO[i] ---
  B <- Z / GO_safe
  
  cat("  Max colSum(A):", round(max(colSums(A)), 4), "(should be < 1)\n")
  cat("  Max rowSum(B):", round(max(rowSums(B)), 4), "(should be < 1)\n")
  
  I_mat <- diag(N)
  ones  <- rep(1, N)
  
  # ---- UPSTREAMNESS & DOWNSTREAMNESS ----------------------------------------
  G_inv <- solve(I_mat - B)
  U_vec <- as.numeric(G_inv %*% ones)
  
  inv_IminAt <- solve(I_mat - t(A))
  D_vec <- as.numeric(inv_IminAt %*% ones)
  
  names(U_vec) <- Z_cols
  names(D_vec) <- Z_cols
  
  cat("  U range:", round(min(U_vec), 3), "-", round(max(U_vec), 3), "\n")
  cat("  D range:", round(min(D_vec), 3), "-", round(max(D_vec), 3), "\n")
  
  # ---- NATIONAL / INTERNATIONAL DECOMPOSITION -------------------------------
  sector_country <- col_info$col_country
  
  U_nat <- numeric(N)
  U_int <- numeric(N)
  D_nat <- numeric(N)
  D_int <- numeric(N)
  
  for (cc in countries) {
    idx_c <- which(sector_country == cc)
    
    e_nat <- rep(0, N)
    e_nat[idx_c] <- 1
    e_int <- 1 - e_nat
    
    U_nat[idx_c] <- as.numeric((G_inv %*% e_nat)[idx_c])
    U_int[idx_c] <- as.numeric((G_inv %*% e_int)[idx_c])
    
    D_nat[idx_c] <- as.numeric((inv_IminAt %*% e_nat)[idx_c])
    D_int[idx_c] <- as.numeric((inv_IminAt %*% e_int)[idx_c])
  }
  
  # ---- COLLECT RESULTS ------------------------------------------------------
  res <- data.table(
    year        = yr,
    country     = sector_country,
    sector      = col_info$col_sector,
    GO          = GO,
    VA          = VA,
    FD          = FD,
    II_sold     = II_sold,
    II_used     = II_used,
    Upstreamness      = U_vec,
    Upstreamness_nat  = U_nat,
    Upstreamness_int  = U_int,
    Downstreamness      = D_vec,
    Downstreamness_nat  = D_nat,
    Downstreamness_int  = D_int
  )
  
  results_list[[as.character(yr)]] <- res
}

# ---- 5. COMBINE AND SAVE ---------------------------------------------------
results <- rbindlist(results_list)

# Sanity checks
results[, check_U := abs(Upstreamness - Upstreamness_nat - Upstreamness_int)]
results[, check_D := abs(Downstreamness - Downstreamness_nat - Downstreamness_int)]
cat("\nMax decomposition error U:", max(results$check_U), "\n")
cat("Max decomposition error D:", max(results$check_D), "\n")
results[, c("check_U", "check_D") := NULL]

cat("Min U:", min(results$Upstreamness), "(should be >= 1)\n")
cat("Min D:", min(results$Downstreamness), "(should be >= 1)\n")

# World GO-weighted averages should be equal
check <- results[, .(wU = sum(Upstreamness * GO) / sum(GO),
                     wD = sum(Downstreamness * GO) / sum(GO)), by = year]
cat("\nWorld GO-weighted averages (U ≈ D):\n")
print(check)