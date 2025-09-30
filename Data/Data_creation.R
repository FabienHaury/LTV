set.seed(123)

n <- 500000

CODETICKET <- paste0("T", sprintf("%05d", sample(1:999999, n, replace = TRUE)))
ID_client <- sample(paste0("C", sprintf("%05d", 1:50000)), n, replace = TRUE)
entete_dt_year <- sample(2018:2023, n, replace = TRUE)

# Types de produits simulés en interne (non gardés dans df)
product_type <- sample(
  c("cafe", "the", "accessoire", "cafe_grains"),
  n,
  replace = TRUE,
  prob = c(0.6, 0.15, 0.1, 0.15)
)

mt_net_ttc <- numeric(n)

mt_net_ttc[product_type == "cafe"] <- round(rgamma(sum(product_type == "cafe"), shape = 2, scale = 2.5), 2)
mt_net_ttc[product_type == "the"] <- round(rgamma(sum(product_type == "the"), shape = 2, scale = 3), 2)
mt_net_ttc[product_type == "accessoire"] <- round(rlnorm(sum(product_type == "accessoire"), meanlog = 3, sdlog = 1), 2)
mt_net_ttc[product_type == "cafe_grains"] <- round(rgamma(sum(product_type == "cafe_grains"), shape = 5, scale = 3), 2)

# Remises/retours négatifs, 1% des transactions
neg_idx <- sample(1:n, size = round(0.01 * n))
mt_net_ttc[neg_idx] <- round(runif(length(neg_idx), min = -20, max = 0), 2)

# Assemblage data.frame avec uniquement les colonnes originelles
df <- data.frame(
  CODETICKET,
  ID_client,
  mt_net_ttc,
  entete_dt_year,
  stringsAsFactors = FALSE
)

summary(df$mt_net_ttc)
head(df, 10)


write.csv(df, "data_raw.csv", row.names = FALSE)
