# ============================================================
# 17_extrair_populacao_ibge.R — População total de Araruama
# Versão 3.0.1
# ============================================================
# Fontes: brpop::ibge_pop() → POPTCU (FTP DATASUS)
# Cobertura: 2000–2025
#
# Entrada : (nada)
# Saída   : dados_brutos/populacao/populacao_araruama.parquet
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 17_extrair_populacao_ibge.R v3.0.1 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

caminho_alvo <- file.path(dir_brutos_pop, "populacao_araruama.parquet")

if (file.exists(caminho_alvo) && !FORCAR) {
  cat("\n  ⏭  População total já existe, pulando.\n")
  registrar_log("População total já existe.")
} else {

  cabecalho_etapa(
    titulo    = "17_extrair_populacao_ibge.R — População total",
    subtitulo = c(
      sprintf("Anos     : %d–%d", min(ANOS_POP), max(ANOS_POP)),
      sprintf("RAM livre: %.0f MB", mem_livre_mb())
    ),
    icone = "▶"
  )

  t0 <- Sys.time()

  # --- Fonte 1: brpop ---
  cat("  1. brpop::ibge_pop() ...\n")
  pop_brpop <- tryCatch({
    d <- brpop::ibge_pop()
    d <- d[d$year %in% ANOS_POP &
             d$code_muni == as.numeric(COD_IBGE_ARARUAMA_7), ]
    dplyr::transmute(d,
      ano       = as.integer(year),
      populacao = as.numeric(pop),
      fonte     = "IBGE/brpop"
    )
  }, error = function(e) {
    cat("     ✗ falhou:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(pop_brpop) && nrow(pop_brpop) > 0) {
    cat(sprintf("     ✓ %d anos via brpop\n", nrow(pop_brpop)))
    registrar_log(sprintf("brpop: %d anos", nrow(pop_brpop)))
  }

  # --- Fonte 2: POPTCU (para anos faltantes) ---
  anos_falt <- setdiff(ANOS_POP,
                       if (is.null(pop_brpop)) integer(0) else pop_brpop$ano)

  pop_poptcu <- NULL
  if (length(anos_falt) > 0) {
    cat(sprintf("  2. POPTCU para %d anos faltantes ...\n", length(anos_falt)))

    pop_poptcu <- tryCatch({
      purrr::map_dfr(anos_falt, function(ano) {
        tryCatch({
          url <- sprintf(
            "%s/IBGE/POPTCU/POPTBR%02d.zip",
            .URL_BASE_DATASUS, ano - 2000
          )
          tmp     <- tempfile(fileext = ".zip")
          tmp_dir <- tempfile("poptcu_")
          on.exit(unlink(c(tmp, tmp_dir), recursive = TRUE), add = TRUE)
          dir.create(tmp_dir, showWarnings = FALSE)

          h <- curl::new_handle()
          curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
          curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)

          arqs <- unzip(tmp, exdir = tmp_dir)
          dbf  <- arqs[grepl("\\.dbf$", arqs, ignore.case = TRUE)][1]
          d    <- foreign::read.dbf(dbf, as.is = TRUE)
          names(d) <- tolower(names(d))

          col_mun <- intersect(
            c("codmun", "cod_mun", "codigo", "cod_ibge", "municod"),
            names(d)
          )[1]
          col_pop <- intersect(
            c("populacao", "pop", "populacao_residente", "pop_res"),
            names(d)
          )[1]
          if (is.na(col_mun) || is.na(col_pop)) {
            stop("colunas nao reconhecidas")
          }

          x <- trimws(as.character(d[[col_mun]]))
          x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
          d <- d[x == COD_IBGE_ARARUAMA_6, , drop = FALSE]
          if (nrow(d) == 0) stop("Araruama ausente")

          tibble::tibble(
            ano       = as.integer(ano),
            populacao = sum(as.numeric(d[[col_pop]]), na.rm = TRUE),
            fonte     = "DATASUS/POPTCU"
          )
        }, error = function(e) {
          cat(sprintf("     ✗ POPTCU %d: %s\n", ano, conditionMessage(e)))
          tibble::tibble(ano = integer(), populacao = numeric(),
                         fonte = character())
        })
      })
    }, error = function(e) {
      cat("     ✗ POPTCU falhou por completo\n")
      NULL
    })

    if (!is.null(pop_poptcu) && nrow(pop_poptcu) > 0) {
      cat(sprintf("     ✓ %d anos via POPTCU\n", nrow(pop_poptcu)))
    }
  }

  # --- Combinar ---
  pop_final <- dplyr::bind_rows(pop_brpop, pop_poptcu) |>
    dplyr::distinct(ano, .keep_all = TRUE) |>
    dplyr::filter(ano %in% ANOS_POP) |>
    dplyr::arrange(ano)

  # --- Validação ---
  faltantes <- setdiff(ANOS_POP, pop_final$ano)
  if (length(faltantes) > 0) {
    msg <- sprintf("Anos sem população após todas as fontes: %s",
                   paste(faltantes, collapse = ", "))
    registrar_log(msg, nivel = "ERRO")
    stop(msg, call. = FALSE)
  }

  # --- Detecção de descontinuidade IBGE ---
  p21 <- pop_final$populacao[pop_final$ano == 2021]
  p22 <- pop_final$populacao[pop_final$ano == 2022]
  if (length(p21) > 0 && length(p22) > 0 && p22 < p21) {
    pct <- 100 * (p22 / p21 - 1)
    cat(sprintf("\n  ⚠ Descontinuidade IBGE 2021→2022: %.0f → %.0f (%.1f%%)\n",
                p21, p22, pct))
    registrar_log(sprintf("Descontinuidade IBGE: %.0f → %.0f (%.1f%%)",
                          p21, p22, pct), nivel = "AVISO")
  }

  pop_final <- pop_final |>
    dplyr::mutate(
      cod_ibge  = COD_IBGE_ARARUAMA_7,
      municipio = "Araruama",
      uf        = UF_PROJETO
    ) |>
    dplyr::select(ano, populacao, fonte, cod_ibge, municipio, uf)

  salvar_parquet(pop_final, caminho_alvo)

  linhas <- c(
    sprintf("Anos    : %d", nrow(pop_final)),
    sprintf("Fontes  : %s", paste(unique(pop_final$fonte), collapse = ", "))
  )
  rodape_etapa(linhas, t0)
}

registrar_log("=== Fim 17_extrair_populacao_ibge.R v3.0.1 ===")