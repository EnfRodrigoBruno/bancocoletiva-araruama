# ============================================================
# 04_testar_pendentes.R — Fecha os 4 testes pendentes
# Versão 1.0.0
# ============================================================
# Testa:
#   1. SINAN_FTP (bug de sintaxe corrigido)
#   2. SINASC_FTP (descobrir nome do arquivo)
#   3. SIPNI_LAZY (em 3 etapas para isolar o OOM)
#   4. POP_SIDRA (debug do retorno NA)
#
# Uso:
#   source(here::here("codigo", "04_testar_pendentes.R"))
# ============================================================

suppressPackageStartupMessages(library(here))

ANO <- 2023
MES <- 1

borda <- function() strrep("─", 66)
cat("\n", borda(), "\n", sep = "")
cat("  04_testar_pendentes.R — Fechando lacunas\n")
cat(borda(), "\n\n", sep = "")

# ============================================================
# TESTE 1 — SINAN_FTP (sintaxe corrigida)
# ============================================================
cat("\n[1/4] SINAN_FTP — DENGBR 2023\n")

callr::r(function() {
  if (!requireNamespace("datasusr", quietly = TRUE))
    stop("datasusr ausente")

  # Correção: %02d (não %%02d), 23 (não 2023%%100)
  url <- sprintf(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/DENGBR%02d.dbc",
    2023 %% 100
  )
  message("URL: ", url)
  tmp <- tempfile(fileext = ".dbc")
  on.exit(unlink(tmp), add = TRUE)
  old <- getOption("timeout"); options(timeout = 300)
  on.exit(options(timeout = old), add = TRUE)
  curl::curl_download(url, tmp, quiet = TRUE, mode = "wb")
  message("Tamanho do arquivo: ", round(file.size(tmp) / 1024^2, 1), " MB")
  d <- datasusr::read_datasus_dbc(tmp)
  message("Linhas: ", format(nrow(d), big.mark = "."))
  message("Colunas: ", ncol(d))
  rm(d); gc(verbose = FALSE)
  list(ok = TRUE)
}, show = TRUE, spinner = FALSE)

# ============================================================
# TESTE 2 — SINASC_FTP (explorar nomes possíveis)
# ============================================================
cat("\n[2/4] SINASC_FTP — testando variações de nome\n")

callr::r(function() {
  if (!requireNamespace("datasusr", quietly = TRUE))
    stop("datasusr ausente")

  candidatos <- c(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNRJ2023.dbc",
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNRJ23.dbc",
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/DADOS/DNRJ2023.dbc",
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNBR2023.dbc"
  )

  for (url in candidatos) {
    cat(sprintf("  Testando: %s\n", basename(url)))
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
      d <- datasusr::read_datasus_dbc(tmp)
      cat(sprintf("    Linhas: %s | Colunas: %d\n",
                  format(nrow(d), big.mark = "."), ncol(d)))
      unlink(tmp)
      rm(d); gc(verbose = FALSE)
      return(list(ok = TRUE, url = url))
    }
    unlink(tmp)
  }
  list(ok = FALSE)
}, show = TRUE, spinner = FALSE)

# ============================================================
# TESTE 3 — SIPNI em 3 etapas
# ============================================================
cat("\n[3/4] SIPNI_LAZY — diagnóstico em etapas\n")

# Etapa 3.1: apenas abrir o dataset (sem collect)
cat("  3.1 — Apenas abrir o dataset remoto (sem collect)...\n")
r31 <- tryCatch(
  callr::r(function() {
    if (!requireNamespace("healthbR", quietly = TRUE))
      stop("healthbR ausente")
    # Descobrir assinatura de sipni_data
    args_disponiveis <- names(formals(healthbR::sipni_data))
    cat("Argumentos de sipni_data():",
        paste(args_disponiveis, collapse = ", "), "\n")

    ds <- healthbR::sipni_data(year = 2023, uf = "RJ",
                                source = "r2", lazy = TRUE)
    cat("Classe do objeto retornado:", class(ds)[1], "\n")
    cat("Tipo:", typeof(ds), "\n")
    if (inherits(ds, "Dataset") || inherits(ds, "arrow_dplyr_query")) {
      cat("É um Arrow Dataset/query (lazy) ✓\n")
      cat("Colunas do schema:",
          paste(head(names(ds), 15), collapse = ", "), "\n")
    }
    list(ok = TRUE)
  }, timeout = 60, show = TRUE, spinner = FALSE),
  error = function(e) {
    cat("    ✗ Erro:", conditionMessage(e), "\n")
    list(ok = FALSE)
  }
)

# Etapa 3.2: coletar 1 registro só
cat("\n  3.2 — Coletar apenas 1 registro (head)...\n")
r32 <- tryCatch(
  callr::r(function() {
    if (!requireNamespace("healthbR", quietly = TRUE))
      stop("healthbR ausente")
    ds <- healthbR::sipni_data(year = 2023, uf = "RJ",
                                source = "r2", lazy = TRUE)
    amostra <- ds |> head(1) |> dplyr::collect()
    cat("Linhas na amostra:", nrow(amostra), "\n")
    cat("Colunas:", ncol(amostra), "\n")
    cat("Primeiras 8 colunas:",
        paste(head(names(amostra), 8), collapse = ", "), "\n")
    rm(amostra, ds); gc(verbose = FALSE)
    list(ok = TRUE)
  }, timeout = 90, show = TRUE, spinner = FALSE),
  error = function(e) {
    cat("    ✗ Erro:", conditionMessage(e), "\n")
    list(ok = FALSE)
  }
)

# Etapa 3.3: filtro real com 1 mês só
cat("\n  3.3 — Filtrar 2023/01 + Araruama...\n")
r33 <- tryCatch(
  callr::r(function() {
    if (!requireNamespace("healthbR", quietly = TRUE))
      stop("healthbR ausente")
    ds <- healthbR::sipni_data(year = 2023, uf = "RJ",
                                source = "r2", lazy = TRUE)
    # Detectar nomes de colunas de ano/mes/co_municipio
    cols <- names(ds)
    ano_col  <- intersect(c("ano", "year", "ano_aplic"), cols)[1]
    mes_col  <- intersect(c("mes", "month", "mes_aplic"), cols)[1]
    mun_col  <- intersect(c("co_municipio_paciente", "co_mun_pac",
                            "co_municipio"), cols)[1]
    cat("Coluna de ano:", ano_col, "\n")
    cat("Coluna de mês:", mes_col, "\n")
    cat("Coluna de município:", mun_col, "\n")

    if (is.na(ano_col) || is.na(mes_col) || is.na(mun_col)) {
      stop("Alguma coluna essencial não encontrada")
    }

    resultado <- ds |>
      dplyr::filter(
        .data[[ano_col]] == "2023",
        .data[[mes_col]] == "01",
        .data[[mun_col]] == "330020"
      ) |>
      dplyr::collect()
    cat("Linhas filtradas:", format(nrow(resultado), big.mark = "."), "\n")
    rm(resultado, ds); gc(verbose = FALSE)
    list(ok = TRUE)
  }, timeout = 120, show = TRUE, spinner = FALSE),
  error = function(e) {
    cat("    ✗ Erro:", conditionMessage(e), "\n")
    list(ok = FALSE)
  }
)

# ============================================================
# TESTE 4 — POP_SIDRA (debug do NA)
# ============================================================
cat("\n[4/4] POP_SIDRA — debug detalhado\n")

callr::r(function() {
  if (!requireNamespace("sidrar", quietly = TRUE))
    stop("sidrar ausente")

  # Chamada idêntica à que falhou
  cat("  Chamando sidrar::get_sidra()...\n")
  res <- tryCatch(
    sidrar::get_sidra(
      x = 6579, variable = 9324,
      period = "2023", geo = "N6",
      geo.filter = list(`N6` = 3300209)
    ),
    error = function(e) {
      cat("  ✗ Erro direto:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(res)) {
    # Tentar sem geo.filter
    cat("  Tentando sem geo.filter...\n")
    res <- tryCatch(
      sidrar::get_sidra(x = 6579, variable = 9324, period = "2023"),
      error = function(e) NULL
    )
  }

  if (is.null(res)) {
    cat("  ✗ sidrar retornou NULL em ambas tentativas\n")
    return(list(ok = FALSE))
  }

  cat("  Classe:", class(res)[1], "\n")
  cat("  Dimensões:", paste(dim(res), collapse = " x "), "\n")
  if (is.data.frame(res) && nrow(res) > 0) {
    cat("  Colunas:", paste(head(names(res), 10), collapse = ", "), "\n")
    print(head(res, 3))
  } else if (is.list(res)) {
    cat("  Estrutura:\n")
    str(res, max.level = 2)
  }
  rm(res); gc(verbose = FALSE)
  list(ok = TRUE)
}, timeout = 60, show = TRUE, spinner = FALSE)

cat("\n", borda(), "\n")
cat("  Diagnóstico concluído. Cole o output no chat.\n")
cat(borda(), "\n\n", sep = "")