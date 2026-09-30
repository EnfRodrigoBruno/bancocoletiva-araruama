# ============================================================
# 05_testar_sinan_select.R — SINAN com select (fix)
# Versão 1.0.0
# ============================================================
# Testa leitura do DENGBR 2023 passando select= para reduzir
# colunas ANTES de materializar. Espera-se pico < 500 MB.
# ============================================================

suppressPackageStartupMessages(library(here))

borda <- function() strrep("─", 66)
cat("\n", borda(), "\n  ▶ SINAN — DENGBR 2023 com select=\n",
    borda(), "\n\n", sep = "")

callr::r(function() {
  if (!requireNamespace("datasusr", quietly = TRUE))
    stop("datasusr ausente")

  # Colunas essenciais — apenas o que a análise usa
  cols <- toupper(c(
    # Município (varia entre agravos, tenta todas)
    "ID_MN_RESI", "ID_MUNICIP", "MUNIC_RES", "CODMUNRES",
    "MUNICIPIO", "COD_MUNIC",
    # Data (varia entre agravos)
    "DT_NOTIFIC", "DT_SIN_PRI", "DT_DIAG",
    # Demográficos
    "CS_SEXO", "NU_IDADE_N", "EVOLUCAO"
  ))

  url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/DENGBR23.dbc"
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp), add = TRUE)

  old <- getOption("timeout"); options(timeout = 300)
  on.exit(options(timeout = old), add = TRUE)

  cat("  Baixando (", "59 MB", ") ...\n")
  curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
  cat("  Tamanho: ", round(file.size(tmp)/1024^2, 1), " MB\n", sep = "")

  cat("  Lendo com select (", length(cols), " colunas) ...\n", sep = "")
  d <- datasusr::read_datasus_dbc(tmp, select = cols, verbose = FALSE)

  cat("  Linhas:", format(nrow(d), big.mark = "."), "\n")
  cat("  Colunas:", ncol(d), "\n")
  cat("  Nomes:", paste(names(d), collapse = ", "), "\n")

  # Filtra Araruama
  col_mun <- intersect(tolower(cols), tolower(names(d)))[1]
  # Normaliza 7→6 dígitos
  cod <- trimws(as.character(d[[col_mun]]))
  cod[nchar(cod) == 7] <- substr(cod[nchar(cod) == 7], 1, 6)
  d_ara <- d[cod == "330020", , drop = FALSE]

  cat("  Linhas em Araruama:", nrow(d_ara), "\n")

  # Pico de RSS
  rss <- tryCatch({
    l <- readLines("/proc/self/status", warn = FALSE)
    m <- grep("^VmRSS:", l, value = TRUE)
    round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
  }, error = function(e) NA_real_)
  cat("  RSS no fim:", rss, "MB\n")

  rm(d, d_ara); gc(verbose = FALSE)
  list(ok = TRUE, n_ara = nrow(d_ara))
}, timeout = 300, show = TRUE, spinner = FALSE)

cat("\n", borda(), "\n  Concluído.\n", borda(), "\n\n", sep = "")