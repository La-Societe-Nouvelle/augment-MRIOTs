x = c("tidyverse","curl","DBI","arrow","bir","icarus","readxl","melodi","countrycode","httr2",'rvest','countrycode','httr','fixest','here','eurostat')

lapply(x,library,character.only = T)

files <- c(
  "computations.R",
  "data.R",
  "mapping.R",
  "utils.R"
)

invisible(lapply(files, function(f) source(here("R", f))))

year = 2021

K = format_domestic_capital_tables(year)

FIGARO = import_figaro_io_table(year)

GHG = import_ghg_data(year,FIGARO)

X = FIGARO %>%
  colSums() %>%
  subset(!grepl('P3|P5',names(.))) %>%
  data.frame(production = .) %>%
  as.matrix()

B = case_when(X == 0 ~ 0,
              T ~ GHG$value/X) %>%
  `rownames<-`(rownames(X)) %>%
  as.matrix()*1000 #THS_T


Ds = FIGARO[, grepl('P3|P5', colnames(FIGARO))] %>% as.matrix() # Distributed Final Demand matrix
D = rowSums(FIGARO, na.rm = TRUE) # Aggregated Final Demand vector
Z = FIGARO %>% select(any_of(rownames(.))) %>% filter(rownames(.) %in% colnames(.)) %>% as.matrix() # Intermediate Transactions Matrix

# In IO models, a sector cannot consume its own entire output for intermediate use.
diag(Z)[diag(Z) == colSums(Z, na.rm = TRUE)] <- 0

# Technical Coefficients Matrix (A = Z * (X_hat)^-1)
A <- sweep(Z, 2, as.numeric(X), `/`)
A[is.nan(A) | is.infinite(A)] <- 0 # Handle potential division by zero

if (verbose) message("    Inverting Leontief matrix (I-A)...")
L <- solve(diag(nrow = nrow(A)) - A)

selected_industry_num <- grep('26|61|62|63', colnames(x = L))

sub_invL <- solve(L[selected_industry_num, selected_industry_num])
prescaling_sub_prod <- sub_invL %*% X[selected_industry_num, drop = FALSE]
full_prod <- L[, selected_industry_num] %*% diag(x = as.numeric(prescaling_sub_prod))
distributed_fpt <- diag(x = as.numeric(B)) %*% full_prod
total_fpt <- colSums(distributed_fpt, na.rm = TRUE)

K_x = sweep(K, 2, as.numeric(X), `/`)
K_x[is.nan(K_x) | is.infinite(K_x)] <- 0 # Handle potential division by zero

Lk <- solve(diag(nrow = nrow(A)) - (A+K_x))

sub_invL <- solve(Lk[selected_industry_num, selected_industry_num])
prescaling_sub_prod <- sub_invL %*% X[selected_industry_num, drop = FALSE]
full_prod <- Lk[, selected_industry_num] %*% diag(x = as.numeric(prescaling_sub_prod))
distributed_fpt <- diag(x = as.numeric(B)) %*% full_prod
total_fpt_k <- colSums(distributed_fpt, na.rm = TRUE)

sum(total_fpt) / 1000000 ; sum(total_fpt_k) / 1000000


