
set.seed(123)
# Parámetros
i <- seq_len(n)
z <- qnorm(0.975)

# Contenedor de resultados
N <- R * 2
df <- data.frame(
  metodo   = character(N),
  replica  = integer(N),
  p_hat    = numeric(N),
  lo       = numeric(N),
  hi       = numeric(N),
  contiene = logical(N),
  stringsAsFactors = FALSE
)
idx <- 1

for (r in seq_len(R)) {
  # Muestreo aleatorio simple
  y      <- rbinom(n, 1, p)
  p_hat  <- mean(y)
  se     <- sqrt(p_hat*(1-p_hat)/n)
  lo     <- p_hat - z*se
  hi     <- p_hat + z*se
  df[idx, ] <- list("MAS", r, p_hat, lo, hi, (lo <= p && p <= hi))
  idx <- idx + 1

  # Muestreo determinista
  m      <- runif(1, -0.05, 0.05)
  b      <- runif(1, -2.5, 2.5)
  y_det  <- as.numeric(m*i + b > 0)
  p_hat  <- mean(y_det)
  se     <- sqrt(p_hat*(1-p_hat)/n)
  lo     <- p_hat - z*se
  hi     <- p_hat + z*se
  df[idx, ] <- list("Determinista", r, p_hat, lo, hi, (lo <= p && p <= hi))
  idx <- idx + 1
}

# Aseguramos tipos
for (col in c("replica")) df[[col]] <- as.integer(df[[col]])

res <- df