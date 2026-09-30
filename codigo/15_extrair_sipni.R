# ============================================================
# 15_extrair_sipni.R — SI-PNI: microdados de vacinação
# Versão 3.4.0 — 1 subprocesso/ano, 1 collect/ano
# ============================================================
# Mudanças v3.4.0:
#   - FIX DE PERFORMANCE: abre healthbR::sipni_data() 1× por ano
#     (~266s de metadata R2) e faz UM collect() único para
#     Araruama. Depois divide em 12 meses LOCALMENTE.
#   - Reduz de 12 chamadas R2/ano para 1.
#   - Cast defensivo para string antes de substr() no filtro
#     Arrow (evita erro se a coluna for int64).
#   - timeout 1200s/ano (20 min), cap_mb=3000.
#   - TMPDIR redirecionado para <raiz>/registros/tmp (evita
#     o problema de /tmp tmpfs que travou o SINAN).
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

# --- TMPDIR no disco do projeto (igual ao SINAN v3.1.3) ---
dir_tmp_projeto <- file.path(raiz, "registros", "tmp")
dir.create(dir_tmp_projeto, recursive = TRUE, showWarnings = FALSE)
lixo <- list.files(dir_tmp_projeto, full.names = TRUE,
                   all.files = TRUE, no.. = TRUE)
if (length(lixo) > 0) unlink(lixo, recursive = TRUE, force = TRUE)
Sys.setenv(TMPDIR = dir_tmp_projeto)
Sys.setenv(TMP    = dir_tmp_projeto)
Sys.setenv(TEMP   = dir_tmp_projeto)
options(tmpdir = dir_tmp_projeto)

registrar_log("=== Início 15_extrair_sipni.R v3.4.0 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

# ⚠️ Para o primeiro teste, deixe só 2024. Depois expanda.
ANOS_SIPNI <- 2020:2025      # <- TESTE: depois vira 2020:2025

TIMEOUT_POR_ANO <- 1200    # 20 min por ano
CAP_MB_POR_ANO  <- 3000

dir.create(dir_brutos_sipni, recursive = TRUE, showWarnings = FALSE)

cabecalho_etapa(
  titulo    = "15_extrair_sipni.R — Doses aplicadas",
  subtitulo = c(
    sprintf("Anos     : %d–%d", min(ANOS_SIPNI), max(ANOS_SIPNI)),
    "Estratégia: 1 subprocesso/ano, 1 collect/ano",
    sprintf("RAM livre: %.0f MB", mem_livre_mb())
  ),
  icone = "▶"
)

t_geral <- Sys.time()

for (ano in ANOS_SIPNI) {

  # Já tem os 12 meses? Pula.
  arquivos_ano <- file.path(dir_brutos_sipni,
                            sprintf("sipni_api_araruama_%d_%02d.parquet",
                                    ano, 1:12))
  if (!FORCAR && all(file.exists(arquivos_ano))) {
    cat(sprintf("  [%d] ⏭  todos os 12 meses existem, pulando\n", ano))
    next
  }

  if (!checar_memoria(1500, abortar = FALSE,
                      contexto = sprintf("ano %d", ano))) {
    cat(sprintf("  [%d] ✗ RAM baixa (%.0f MB)\n", ano, mem_livre_mb()))
    next
  }

  cat(sprintf("  [%d] abrindo R2 e coletando ano inteiro...\n", ano))
  t0 <- Sys.time()

  res <- tryCatch(
    callr::r(
      function(ano, uf, cod_mun, dir_saida, cap_mb, timeout_s) {

        if (requireNamespace("unix", quietly = TRUE)) {
          tryCatch(unix::rlimit_as(as.integer(cap_mb) * 1048576L),
                   error = function(e) NULL)
        }
        setTimeLimit(elapsed = timeout_s, transient = TRUE)
        on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)

        # --- 1) Abre dataset R2 (lento, ~266s) ---
        t_abrir <- Sys.time()
        ds <- healthbR::sipni_data(year = ano, uf = uf,
                                    source = "r2", lazy = TRUE)
        dt_abrir <- round(as.numeric(difftime(Sys.time(), t_abrir,
                                              units = "secs")), 1)

        # --- 2) Detecção defensiva de colunas ---
        cols <- names(ds)
        ano_col <- intersect(c("ano", "year", "ano_aplic", "ano_vac"), cols)[1]
        mes_col <- intersect(c("mes", "month", "mes_aplic", "mes_vac"), cols)[1]
        mun_col <- intersect(c(
          "co_municipio_paciente", "co_mun_pac", "co_municipio",
          "co_municipio_estabelecimento", "co_mun_estab",
          "co_municipio_residencia"
        ), cols)[1]
        if (is.na(mes_col) || is.na(mun_col)) {
          stop(sprintf("schema inesperado: mes=%s mun=%s",
                       mes_col, mun_col))
        }

        # --- 3) UMA query: filtra ano (se houver col) + município ---
        # Cast defensivo para string antes do substr (Arrow pode ter int64)
        base <- ds |>
          dplyr::mutate(
            .mun_str = as.character(.data[[mun_col]])
          ) |>
          dplyr::filter(
            substr(.data$.mun_str, 1, 6) == cod_mun
          )

        if (!is.na(ano_col)) {
          base <- base |>
            dplyr::filter(.data[[ano_col]] == as.character(ano))
        }

        # --- 4) UM collect só para o ano ---
        t_collect <- Sys.time()
        df <- base |>
          dplyr::select(-.data$.mun_str) |>
          dplyr::collect() |>
          as.data.frame()
        dt_collect <- round(as.numeric(difftime(Sys.time(), t_collect,
                                                units = "secs")), 1)

        if (nrow(df) == 0) {
          return(list(ok = TRUE, dt_abrir = dt_abrir,
                      dt_collect = dt_collect, n_total = 0L,
                      meses_n = rep(0L, 12)))
        }

        # --- 5) Detecta coluna de mês no data.frame coletado ---
        mes_col_df <- intersect(c("mes", "month", "mes_aplic", "mes_vac"),
                                names(df))[1]
        if (is.na(mes_col_df)) {
          stop("coluna de mes ausente no df coletado")
        }
        meses_str <- sprintf("%02d", 1:12)

        # --- 6) Divide e salva 12 arquivos ---
        resultado_meses <- integer(12)
        for (mes in 1:12) {
          saida <- file.path(dir_saida,
                             sprintf("sipni_api_araruama_%d_%02d.parquet",
                                     ano, mes))
          d_mes <- df[as.character(df[[mes_col_df]]) == meses_str[mes], ,
                      drop = FALSE]
          if (nrow(d_mes) == 0) {
            resultado_meses[mes] <- 0L
            next
          }
          d_mes$ano_arquivo <- ano
          d_mes$mes_arquivo <- mes
          d_mes$uf_source   <- uf
          arrow::write_parquet(d_mes, saida)
          resultado_meses[mes] <- nrow(d_mes)
        }

        rm(df, base, ds); gc(verbose = FALSE)

        list(ok = TRUE, dt_abrir = dt_abrir, dt_collect = dt_collect,
             n_total = sum(resultado_meses),
             meses_n = resultado_meses)
      },
      args = list(
        ano       = ano,
        uf        = UF_PROJETO,
        cod_mun   = COD_IBGE_ARARUAMA_6,
        dir_saida = dir_brutos_sipni,
        cap_mb    = CAP_MB_POR_ANO,
        timeout_s = TIMEOUT_POR_ANO - 60
      ),
      timeout = TIMEOUT_POR_ANO,
      show    = FALSE,
      spinner = FALSE
    ),
    error = function(e) list(ok = FALSE, msg = conditionMessage(e))
  )

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (isTRUE(res$ok)) {
    n_novos  <- sum(res$meses_n > 0)
    n_vazios <- sum(res$meses_n == 0)
    cat(sprintf("    ✓ abriu=%.0fs | collect=%.0fs | %d meses (%d linhas) | %d vazios | %.1fs total\n",
                res$dt_abrir, res$dt_collect,
                n_novos, res$n_total, n_vazios, dt))
    registrar_log(sprintf("SIPNI %d: abrir=%.0fs collect=%.0fs %d linhas",
                          ano, res$dt_abrir, res$dt_collect, res$n_total))
  } else {
    cat(sprintf("    ✗ falhou em %.1fs: %s\n", dt, res$msg))
    registrar_log(sprintf("SIPNI %d falhou: %s", ano, res$msg),
                  nivel = "AVISO")
  }

  limpar_memoria()
  Sys.sleep(2)
}

# --- Consolidação final ---
cat("\n═══ Consolidando ═══\n")
arquivos <- list.files(dir_brutos_sipni,
                       pattern = "^sipni_api_araruama_\\d{4}_\\d{2}\\.parquet$",
                       full.names = TRUE)
if (length(arquivos) > 0) {
  tamanhos <- file.size(arquivos)
  ruins <- arquivos[tamanhos < 8]
  if (length(ruins) > 0) unlink(ruins)
  arquivos <- arquivos[tamanhos >= 8]

  lista <- purrr::map(arquivos, arrow::read_parquet)
  tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
  tc <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
  mistas <- names(tc)[purrr::map_int(tc, length) > 1]
  if (length(mistas) > 0) {
    lista <- purrr::map(lista, function(df) {
      for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      df
    })
  }
  sipni <- dplyr::bind_rows(lista)
  rm(lista); limpar_memoria()
  salvar_parquet(sipni, file.path(dir_brutos_sipni, "sipni_api_araruama.parquet"))
  cat(sprintf("  Consolidado: %s doses | %d colunas\n",
              fmt_num(nrow(sipni)), ncol(sipni)))
}

duracao <- rodape_etapa(
  linhas = c(sprintf("Anos processados: %d", length(ANOS_SIPNI))),
  tempo_inicio = t_geral
)
registrar_log(sprintf("=== Fim 15_extrair_sipni.R v3.4.0 (%.1fs) ===", duracao))