# ============================================================
# 03_testar_fonte.R — Testa UMA fonte por vez, isolada
# Versão 1.1.0
# ============================================================
# Roda cada teste em SUBPROCESSO isolado com CAP DE RAM.
# O parent (RStudio) nunca acumula memória e nunca vê o pico.
#
# Uso:
#   1. Edite FONTE_ALVO abaixo
#   2. source(here::here("codigo", "03_testar_fonte.R"))
#
# Se o subprocesso estourar o cap, você verá "cannot allocate"
# e o parent fica em ~300 MB. Nada de susto.
# ============================================================

FONTE_ALVO   <- "POP_POPSVS"     # <<< TROQUE AQUI
ANO          <- 2023
MES          <- 1
CAP_RAM_MB   <- 2000          # subprocesso morre acima disso
TIMEOUT_S    <- 180           # 3 minutos

# ============================================================
# DEFINIÇÕES DE TESTE — TODAS AUTOSSUFICIENTES
# ============================================================
# Cada teste roda em subprocesso limpo. Precisa qualificar tudo
# com pacote::funcao. Constantes literais interpoladas.

.uf   <- "RJ"
.cod6 <- "330020"
.cod7 <- "3300209"

TESTES <- list(

  SIM_FTP = list(
    descr = sprintf("SIM-DO %d via FTP DATASUS + datasusr", ANO),
    expr  = sprintf('{
      if (!requireNamespace("datasusr", quietly = TRUE))
        stop("datasusr ausente")
      url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DO%s%d.dbc"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      old <- getOption("timeout"); options(timeout = 120)
      on.exit(options(timeout = old), add = TRUE)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
      datasusr::read_datasus_dbc(tmp)
    }', .uf, ANO)
  ),

  SIM_HEALTHBR = list(
    descr = sprintf("SIM-DO %d via healthbR (lazy + filter)", ANO),
    expr  = sprintf('{
      if (!requireNamespace("healthbR", quietly = TRUE))
        stop("healthbR ausente")
      if (!requireNamespace("dplyr", quietly = TRUE))
        stop("dplyr ausente")
      ds <- healthbR::datasus_data(
        system = "SIM-DO", year = %d, uf = "%s",
        source = "r2", lazy = TRUE
      )
      ds |>
        dplyr::filter(.data$codmunres == "%s") |>
        dplyr::collect()
    }', ANO, .uf, .cod6)
  ),

  SIM_MICRO = list(
    descr = sprintf("SIM-DO %d via microdatasus", ANO),
    expr  = sprintf('{
      if (!requireNamespace("microdatasus", quietly = TRUE))
        stop("microdatasus ausente")
      microdatasus::fetch_datasus(
        year_start = %d, year_end = %d,
        uf = "%s", information_system = "SIM-DO"
      )
    }', ANO, ANO, .uf)
  ),

  SINASC_FTP = list(
    descr = sprintf("SINASC %d via FTP DATASUS + datasusr", ANO),
    expr  = sprintf('{
      if (!requireNamespace("datasusr", quietly = TRUE))
        stop("datasusr ausente")
      url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DN%s%d.dbc"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      old <- getOption("timeout"); options(timeout = 120)
      on.exit(options(timeout = old), add = TRUE)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
      datasusr::read_datasus_dbc(tmp)
    }', .uf, ANO)
  ),

  SINAN_FTP = list(
    descr = sprintf("SINAN DENGBR %d via FTP + datasusr", ANO),
    expr  = sprintf('{
      if (!requireNamespace("datasusr", quietly = TRUE))
        stop("datasusr ausente")
      url <- sprintf(
        "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/DENGBR%%02d.dbc",
        %d %% 100
      )
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      old <- getOption("timeout"); options(timeout = 120)
      on.exit(options(timeout = old), add = TRUE)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
      datasusr::read_datasus_dbc(tmp)
    }', ANO)
  ),

  SIH_MICRO = list(
    descr = sprintf("SIH-RD %d/%02d via microdatasus", ANO, MES),
    expr  = sprintf('{
      if (!requireNamespace("microdatasus", quietly = TRUE))
        stop("microdatasus ausente")
      microdatasus::fetch_datasus(
        year_start = %d, year_end = %d,
        month_start = %d, month_end = %d,
        uf = "%s", information_system = "SIH-RD"
      )
    }', ANO, ANO, MES, MES, .uf)
  ),

  CNES_DATASUSR = list(
    descr = sprintf("CNES-ST %d/%02d via datasusr", ANO, MES),
    expr  = sprintf('{
      if (!requireNamespace("datasusr", quietly = TRUE))
        stop("datasusr ausente")
      datasusr::datasus_fetch(
        source = "CNES", file_type = "ST",
        year = %d, month = %d, uf = "%s", verbose = FALSE
      )
    }', ANO, MES, .uf)
  ),

  SIPNI_LAZY = list(
    descr = sprintf("SI-PNI %d/%02d via healthbR (lazy + filter)", ANO, MES),
    expr  = sprintf('{
      if (!requireNamespace("healthbR", quietly = TRUE))
        stop("healthbR ausente")
      if (!requireNamespace("dplyr", quietly = TRUE))
        stop("dplyr ausente")
      ds <- healthbR::sipni_data(
        year = %d, uf = "%s", source = "r2", lazy = TRUE
      )
      ds |>
        dplyr::filter(
          .data$ano == "%s",
          .data$mes == "%s",
          .data$co_municipio_paciente == "%s"
        ) |>
        dplyr::collect()
    }', ANO, .uf, as.character(ANO), sprintf("%02d", MES), .cod6)
  ),

  POP_BRPOP = list(
    descr = sprintf("População IBGE %d via brpop", ANO),
    expr  = sprintf('{
      if (!requireNamespace("brpop", quietly = TRUE))
        stop("brpop ausente")
      d <- brpop::ibge_pop()
      d[d$year == %d & d$code_muni == %s, ]
    }', ANO, .cod7)
  ),

  POP_SIDRA = list(
    descr = sprintf("População SIDRA 6579 %d", ANO),
    expr  = sprintf('{
      if (!requireNamespace("sidrar", quietly = TRUE))
        stop("sidrar ausente")
      sidrar::get_sidra(
        x = 6579, variable = 9324,
        period = "%d", geo = "N6",
        geo.filter = list(`N6` = %s)
      )
    }', ANO, .cod7)
  ),

  POP_POPSVS = list(
    descr = sprintf("POPSVS %d (FTP, filtra Araruama)", ANO),
    expr  = sprintf('{
      url <- sprintf(
        "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/POPSBR%%02d.zip",
        %d - 2000
      )
      tmp     <- tempfile(fileext = ".zip")
      tmp_dir <- tempfile("popsvs_")
      on.exit(unlink(c(tmp, tmp_dir), recursive = TRUE), add = TRUE)
      dir.create(tmp_dir, showWarnings = FALSE)
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      arqs <- unzip(tmp, exdir = tmp_dir)
      dbf  <- arqs[grepl("\\\\.dbf$", arqs, ignore.case = TRUE)][1]
      d <- foreign::read.dbf(dbf, as.is = TRUE)
      d[as.character(d$COD_MUN) == "%s", , drop = FALSE]
    }', ANO, .cod7)
  )
)

# ============================================================
# HELPERS (definidos ANTES de qualquer uso)
# ============================================================

rss_mb <- function() {
  tryCatch({
    l <- readLines("/proc/self/status", warn = FALSE)
    m <- grep("^VmRSS:", l, value = TRUE)
    if (length(m) == 0) NA_real_
    else round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
  }, error = function(e) NA_real_)
}

livre_mb <- function() {
  tryCatch({
    l <- readLines("/proc/meminfo", warn = FALSE)
    m <- grep("^MemAvailable:", l, value = TRUE)
    if (length(m) == 0) NA_real_
    else round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
  }, error = function(e) NA_real_)
}

fmt_br <- function(x) {
  if (is.na(x)) return("—")
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

borda <- function() strrep("─", 66)

# ============================================================
# EXECUÇÃO
# ============================================================
suppressPackageStartupMessages({
  library(here)
})

# --- Validação de entrada ---
if (!FONTE_ALVO %in% names(TESTES)) {
  stop(sprintf(
    "FONTE_ALVO='%s' inválida. Opções:\n  %s",
    FONTE_ALVO, paste(names(TESTES), collapse = "\n  ")
  ), call. = FALSE)
}

if (!requireNamespace("callr", quietly = TRUE)) {
  stop("Pacote 'callr' ausente. Instale com install.packages('callr').",
       call. = FALSE)
}

tem_unix <- requireNamespace("unix", quietly = TRUE)

teste <- TESTES[[FONTE_ALVO]]

# --- Cabeçalho ---
cat("\n", borda(), "\n", sep = "")
cat(sprintf("  ▶ 03_testar_fonte.R — %s\n", FONTE_ALVO))
cat(sprintf("     %s\n", teste$descr))
cat(sprintf("     Cap de RAM no subprocesso: %d MB\n", CAP_RAM_MB))
cat(sprintf("     Timeout: %ds\n", TIMEOUT_S))
cat(sprintf("     rlimit_as disponível: %s\n",
            if (tem_unix) "sim" else "não (sem unix)"))
cat(borda(), "\n\n", sep = "")

cat(sprintf("  [parent] RSS=%.0f MB | Livre=%.0f MB\n",
            rss_mb(), livre_mb()))
cat("  [subproc] iniciando...\n\n")
flush.console()

# --- Chamada ao subprocesso ---
t0 <- Sys.time()

resultado <- tryCatch(
  callr::r(
    func = function(expr_str, cap_mb, timeout_s, tem_unix) {

      # ---- Limite de RAM (hard cap) ----
      if (tem_unix) {
        tryCatch(
          unix::rlimit_as(as.integer(cap_mb) * 1024L * 1024L),
          error = function(e) NULL
        )
      }

      # ---- Timeout ----
      setTimeLimit(elapsed = timeout_s, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

      # ---- Snapshot inicial ----
      rss_ini <- tryCatch({
        l <- readLines("/proc/self/status", warn = FALSE)
        m <- grep("^VmRSS:", l, value = TRUE)
        if (length(m) == 0) NA_real_
        else round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
      }, error = function(e) NA_real_)

      # ---- Avaliação do teste ----
      erro <- NA_character_
      ok   <- FALSE
      d    <- NULL
      tryCatch({
        d <- eval(parse(text = expr_str))
        ok <- is.data.frame(d) && nrow(d) > 0
        if (!is.data.frame(d)) {
          erro <- "retorno não é data.frame"
          ok <- FALSE
        }
      }, error = function(e) {
        erro <<- conditionMessage(e)
        ok <<- FALSE
      })

      # ---- Pico de RSS do subprocesso ----
      rss_fim <- tryCatch({
        l <- readLines("/proc/self/status", warn = FALSE)
        m <- grep("^VmRSS:", l, value = TRUE)
        if (length(m) == 0) NA_real_
        else round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
      }, error = function(e) NA_real_)

      # ---- Resumo (dados descartados) ----
      if (ok && !is.null(d)) {
        info <- list(
          ok        = TRUE,
          n_rows    = nrow(d),
          n_cols    = ncol(d),
          cols      = names(d)[1:min(20, ncol(d))],
          rss_ini   = rss_ini,
          rss_fim   = rss_fim,
          erro      = NA_character_
        )
      } else {
        info <- list(
          ok        = FALSE,
          n_rows    = NA_integer_,
          n_cols    = NA_integer_,
          cols      = character(0),
          rss_ini   = rss_ini,
          rss_fim   = rss_fim,
          erro      = erro
        )
      }

      rm(d); gc(verbose = FALSE)
      info
    },
    args = list(
      expr_str = teste$expr,
      cap_mb   = CAP_RAM_MB,
      timeout_s = TIMEOUT_S,
      tem_unix = tem_unix
    ),
    show    = FALSE,
    spinner = FALSE
  ),
  error = function(e) {
    list(ok = FALSE, erro = paste("subprocesso:", conditionMessage(e)),
         n_rows = NA_integer_, n_cols = NA_integer_,
         cols = character(0), rss_ini = NA_real_, rss_fim = NA_real_)
  }
)

dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

# --- Snapshot após ---
cat(sprintf("  [parent] RSS=%.0f MB | Livre=%.0f MB\n\n",
            rss_mb(), livre_mb()))

# --- Resultado ---
if (isTRUE(resultado$ok)) {
  cat(sprintf("  ✓ SUCESSO em %.1fs\n", dt))
  cat(sprintf("     Linhas no subprocesso : %s\n",
              fmt_br(resultado$n_rows)))
  cat(sprintf("     Colunas               : %d\n",
              resultado$n_cols))
  cat(sprintf("     RSS subproc (ini/fim) : %.0f / %.0f MB\n",
              resultado$rss_ini, resultado$rss_fim))
  cat(sprintf("     Primeiras colunas     : %s\n",
              paste(resultado$cols, collapse = ", ")))
} else {
  cat(sprintf("  ✗ FALHOU em %.1fs\n", dt))
  cat(sprintf("     Motivo: %s\n", resultado$erro))
  if (!is.na(resultado$rss_fim)) {
    cat(sprintf("     RSS subproc no fim: %.0f MB\n", resultado$rss_fim))
  }
}

cat("\n", borda(), "\n\n", sep = "")