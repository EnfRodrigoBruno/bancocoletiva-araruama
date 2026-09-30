# ============================================================
# 06_testar_sinasc_prelim_e_sipni.R — Fecha os últimos gaps
# Versão 1.0.0
# ============================================================
# Testa:
#   1. SINASC PRELIM (2023+) — descobrir URL correta
#   2. SI-PNI month-only (reduzir escopo para caber na RAM)
# ============================================================

suppressPackageStartupMessages(library(here))

borda <- function() strrep("─", 66)

# ============================================================
# 1. SINASC PRELIM — descobrir nome do arquivo para 2023
# ============================================================
cat("\n", borda(), "\n  [1] SINASC PRELIM — procurando DNRJ2023\n",
    borda(), "\n\n", sep = "")

# Primeiro: LISTAR o que tem em PRELIM
cat("  Listando PRELIM...\n")
prelim_listing <- tryCatch(
  system2("curl", c("-s",
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/"),
    stdout = TRUE, stderr = TRUE),
  error = function(e) character()
)

if (length(prelim_listing) > 0) {
  cat("  Conteúdo (primeiras 30 linhas):\n")
  cat(paste("    ", head(prelim_listing, 30), collapse = "\n"), "\n\n")
} else {
  cat("  ⚠ Não foi possível listar. Tentando direto.\n\n")
}

# Testa candidatos
candidatos <- c(
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRJ2023.dbc",
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRJ23.dbc",
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/NOV/DNRES/DNRJ2023.dbc",
  "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES/DNRJ2023.dbc"
)

callr::r(function(urls) {
  for (url in urls) {
    cat(sprintf("\n  Testando: %s\n", url))
    tmp <- tempfile(fileext = ".dbc")
    ok <- tryCatch({
      old <- getOption("timeout"); options(timeout = 30)
      on.exit(options(timeout = old), add = TRUE)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) {
      cat(sprintf("    ✗ %s\n", conditionMessage(e)))
      FALSE
    })
    if (ok && file.exists(tmp) && file.size(tmp) > 1000) {
      cat(sprintf("    ✓ ENCONTRADO! Tamanho: %.1f MB\n",
                  file.size(tmp) / 1024^2))
      unlink(tmp); rm(tmp)
      return(list(ok = TRUE, url = url))
    }
    unlink(tmp); rm(tmp)
  }
  list(ok = FALSE, msg = "nenhuma URL funcionou")
}, args = list(urls = candidatos), timeout = 60, show = TRUE, spinner = FALSE)

# ============================================================
# 2. SI-PNI — testar mês único isolado
# ============================================================
cat("\n", borda(), "\n  [2] SI-PNI — escopo mínimo (1 mês)\n",
    borda(), "\n\n", sep = "")

callr::r(function() {
  if (!requireNamespace("healthbR", quietly = TRUE))
    stop("healthbR ausente")

  # 2.1 — inspecionar assinatura e objeto
  cat("  Argumentos de sipni_data():\n")
  print(names(formals(healthbR::sipni_data)))

  cat("\n  2.1 — Abrindo dataset (year=2024 apenas)...\n")
  ds <- healthbR::sipni_data(year = 2024, uf = "RJ",
                              source = "r2", lazy = TRUE)
  cat("  Classe:", class(ds)[1], "\n")
  cat("  Colunas disponíveis:",
      paste(head(names(ds), 20), collapse = ", "), "\n")

  # 2.2 — aplicar filtro ANTES de qualquer collect
  cat("\n  2.2 — Filtrando 2024/01 + Araruama (ANTES de collect)...\n")
  cols <- names(ds)
  ano_col  <- intersect(c("ano", "year"), cols)[1]
  mes_col  <- intersect(c("mes", "month"), cols)[1]
  mun_col  <- intersect(c("co_municipio_paciente", "co_mun_pac",
                          "co_municipio"), cols)[1]

  cat("  Coluna ano:", ano_col, "| mês:", mes_col, "| mun:", mun_col, "\n")

  query <- ds |>
    dplyr::filter(
      .data[[ano_col]] == "2024",
      .data[[mes_col]] == "01",
      .data[[mun_col]] == "330020"
    )

  cat("  Query montada (ainda lazy).\n")

  # 2.3 — collect
  cat("\n  2.3 — Collecting...\n")
  t0 <- Sys.time()
  resultado <- query |> dplyr::collect()
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  cat("  Linhas:", format(nrow(resultado), big.mark = "."), "\n")
  cat("  Colunas:", ncol(resultado), "\n")
  cat("  Duração:", dt, "s\n")

  rss <- tryCatch({
    l <- readLines("/proc/self/status", warn = FALSE)
    m <- grep("^VmRSS:", l, value = TRUE)
    round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
  }, error = function(e) NA_real_)
  cat("  RSS no fim:", rss, "MB\n")

  rm(resultado, ds, query); gc(verbose = FALSE)
  list(ok = TRUE)
}, timeout = 180, show = TRUE, spinner = FALSE)

cat("\n", borda(), "\n  Concluído.\n", borda(), "\n\n", sep = "")