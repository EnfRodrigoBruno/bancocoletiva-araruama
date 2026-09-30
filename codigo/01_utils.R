# ============================================================
# 01_utils.R — Funções auxiliares e padrões de saída
# Versão 3.0.0 — Refatoração estrutural
# ============================================================
# Carregado após 00_setup.R. Define:
#   - Constantes de domínio (faixas, CID, garbage codes)
#   - Decodificadores (SIM, SIH, SINAN)
#   - Classificadores (faixa etária, capítulo CID)
#   - Funções de I/O (parquet, log)
#   - Padrões estéticos (cabeçalho/rodapé de etapa)
#   - Multi-fonte (tentar_fontes)
#
# Mudanças v3.0.0:
#   - Adicionado padrão estético: cabecalho_etapa() / rodape_etapa().
#   - Adicionado helper multi-fonte: tentar_fontes().
#   - Todas as chamadas de pacote qualificadas.
#   - Adicionado formatar_duracao() para relatórios legíveis.
#   - Mantidas todas as funções de decodificação existentes.
# ============================================================

# Garante que o setup está carregado
if (!exists("raiz") || !exists("dir_registros")) {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Pacote 'here' ausente. Instale com renv::install('here').",
         call. = FALSE)
  }
  sys.source(
    getExportedValue("here", "here")("codigo", "00_setup.R"),
    envir = globalenv()
  )
}

# ------------------------------------------------------------
# 1. Operador auxiliar
# ------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# 2. Constantes de domínio
# ------------------------------------------------------------
FAIXAS_ETARIAS_LABELS <- c(
  "<1", "1-4", "5-9", "10-14", "15-19",
  "20-29", "30-39", "40-49", "50-59",
  "60-69", "70-79", "80+"
)

FAIXAS_ETARIAS_BREAKS <- c(
  0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf
)

PONTOS_MEDIOS_FAIXA <- c(
  "<1"    =  0.5,
  "1-4"   =  2.5,
  "5-9"   =  7.0,
  "10-14" = 12.0,
  "15-19" = 17.0,
  "20-29" = 24.5,
  "30-39" = 34.5,
  "40-49" = 44.5,
  "50-59" = 54.5,
  "60-69" = 64.5,
  "70-79" = 74.5,
  "80+"   = 85.0
)

GARBAGE_NATURAIS <- c("R99", "R95", "P95", "B34")
GARBAGE_EXTERNOS <- c("X59", "V89")
GARBAGE_CODES    <- c(GARBAGE_NATURAIS, GARBAGE_EXTERNOS)

LIMITE_APVP <- 70

CAPITULOS_CID_LABELS <- c(
  "I"     = "I - Infecciosas e parasitárias",
  "II"    = "II - Neoplasias",
  "III"   = "III - Sangue e imunológicas",
  "IV"    = "IV - Endócrinas e metabólicas",
  "V"     = "V - Transtornos mentais",
  "VI"    = "VI - Sistema nervoso",
  "VII"   = "VII - Olho e anexos",
  "VIII"  = "VIII - Ouvido",
  "IX"    = "IX - Aparelho circulatório",
  "X"     = "X - Aparelho respiratório",
  "XI"    = "XI - Aparelho digestivo",
  "XII"   = "XII - Pele e tecido subcutâneo",
  "XIII"  = "XIII - Sistema osteomuscular",
  "XIV"   = "XIV - Aparelho geniturinário",
  "XV"    = "XV - Gravidez, parto e puerpério",
  "XVI"   = "XVI - Afecções perinatais",
  "XVII"  = "XVII - Malformações congênitas",
  "XVIII" = "XVIII - Sintomas e sinais",
  "XIX"   = "XIX - Lesões e envenenamentos",
  "XX"    = "XX - Causas externas",
  "XXI"   = "XXI - Fatores de saúde",
  "XXII"  = "XXII - Códigos para propósitos especiais"
)

# ------------------------------------------------------------
# 3. Padrões estéticos de saída
# ------------------------------------------------------------
.LARGURA_PADRAO <- 66L

#' Cabeçalho padronizado de etapa
#'
#' @param titulo    ex.: "10_extrair_sim.R — Microdados do SIM"
#' @param subtitulo vetor de strings (default NULL)
#' @param icone     emoji opcional no início
cabecalho_etapa <- function(titulo, subtitulo = NULL, icone = "▶") {
  borda <- strrep("─", .LARGURA_PADRAO)
  cat("\n", borda, "\n", sep = "")
  cat("  ", icone, " ", titulo, "\n", sep = "")
  if (!is.null(subtitulo)) {
    for (s in subtitulo) cat("     ", s, "\n", sep = "")
  }
  cat(borda, "\n\n", sep = "")
  invisible(NULL)
}

#' Rodapé padronizado de etapa
#'
#' @param linhas        vetor de strings a imprimir (ex.: "OK: 3 arquivos")
#' @param tempo_inicio  POSIXct
#' @return duração em segundos (invisível)
rodape_etapa <- function(linhas, tempo_inicio) {
  borda <- strrep("─", .LARGURA_PADRAO)
  dur   <- as.numeric(difftime(Sys.time(), tempo_inicio, units = "secs"))
  cat("\n", borda, "\n", sep = "")
  cat("  RESUMO\n", sep = "")
  cat(borda, "\n", sep = "")
  if (length(linhas) > 0) {
    for (l in linhas) cat("  ", l, "\n", sep = "")
  }
  cat("  ", sprintf("Duração: %s", formatar_duracao(dur)), "\n", sep = "")
  cat("  ", sprintf("Memória: %s", mem_snapshot()), "\n", sep = "")
  cat(borda, "\n\n", sep = "")
  invisible(dur)
}

#' Formata duração em segundos → string legível
formatar_duracao <- function(segundos) {
  if (is.na(segundos) || segundos < 0) return("—")
  if (segundos < 60) return(sprintf("%.1f s", segundos))
  min <- floor(segundos / 60)
  seg <- round(segundos %% 60, 1)
  if (min < 60) return(sprintf("%d min %.1f s", min, seg))
  h   <- floor(min / 60)
  min <- min %% 60
  sprintf("%dh %dmin", h, min)
}

#' Formata número no padrão pt-BR (sem warnings)
fmt_num <- function(x, decimais = 0) {
  if (is.na(x)) return("—")
  format(
    round(x, decimais),
    big.mark      = ".",
    decimal.mark  = ",",
    scientific    = FALSE,
    nsmall        = decimais
  )
}

# ------------------------------------------------------------
# 4. Decodificadores (SIM, SIH, SINAN)
# ------------------------------------------------------------

#' Normaliza código de município do IBGE (7→6 dígitos)
normalizar_cod_municipio <- function(x) {
  x <- trimws(as.character(x))
  x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
  x
}

#' Normaliza CID para 3 caracteres
normalizar_cid3 <- function(cid) {
  cid <- toupper(trimws(as.character(cid)))
  cid <- gsub("\\.", "", cid)
  substr(cid, 1, 3)
}

#' Decodifica campo IDADE do SIM para anos (numérico)
decodificar_idade_sim <- function(idade_cod) {
  i <- suppressWarnings(as.integer(idade_cod))
  dplyr::case_when(
    is.na(i)              ~ NA_real_,
    i <  100              ~ as.numeric(i),
    i >= 100 & i < 200    ~ (i - 100) / 365,
    i >= 200 & i < 300    ~ (i - 200) / 12,
    i >= 300 & i < 400    ~ (i - 300) / 52,
    i >= 400 & i < 500    ~ as.numeric(i - 400),
    i >= 500              ~ NA_real_,
    TRUE                  ~ NA_real_
  )
}

#' Decodifica IDADE + COD_IDADE do SIH para anos
#' Códigos: 1=horas, 2=dias, 3=meses, 4=anos, 5=ignorado
decodificar_idade_sih <- function(idade_chr, cod_idade_chr) {
  i   <- suppressWarnings(as.integer(idade_chr))
  cod <- suppressWarnings(as.integer(cod_idade_chr))
  dplyr::case_when(
    is.na(i) | is.na(cod) ~ NA_real_,
    cod == 1              ~ i / (365 * 24),
    cod == 2              ~ i / 365,
    cod == 3              ~ i / 12,
    cod == 4              ~ as.numeric(i),
    TRUE                  ~ NA_real_
  )
}

#' Extrai dia, mês, ano de data SIM (ddmmaaaa)
extrair_data_sim <- function(x) {
  s   <- as.character(x)
  dia <- suppressWarnings(as.integer(substr(s, 1, 2)))
  mes <- suppressWarnings(as.integer(substr(s, 3, 4)))
  ano <- suppressWarnings(as.integer(substr(s, 5, 8)))
  dia[dia < 1L | dia > 31L]      <- NA_integer_
  mes[mes < 1L | mes > 12L]      <- NA_integer_
  ano[ano < 1900L | ano > 2100L] <- NA_integer_
  list(dia = dia, mes = mes, ano = ano)
}

#' Decodifica campo SEXO do SIM/SINASC/SINAN
decodificar_sexo_sim <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("1", "M", "m", "Masculino", "masculino") ~ "Masculino",
    x %in% c("2", "F", "f", "Feminino",  "feminino")  ~ "Feminino",
    TRUE                                              ~ "Ignorado"
  )
}

#' Extrai ano de data SINAN em múltiplos formatos
extrair_ano_sinan <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA", "00000000", "0000-00-00")] <- NA_character_
  ano <- rep(NA_integer_, length(x_chr))

  idx_iso <- !is.na(x_chr) & grepl("^\\d{4}-\\d{2}-\\d{2}", x_chr)
  if (any(idx_iso)) {
    ano[idx_iso] <- as.integer(substr(x_chr[idx_iso], 1, 4))
  }

  idx8 <- !is.na(x_chr) & grepl("^\\d{8}$", x_chr)
  if (any(idx8)) {
    prim4 <- suppressWarnings(as.integer(substr(x_chr[idx8], 1, 4)))
    idx_yyyy <- idx8
    idx_yyyy[idx8] <- !is.na(prim4) & prim4 >= 1900 & prim4 <= 2100
    if (any(idx_yyyy)) {
      ano[idx_yyyy] <- as.integer(substr(x_chr[idx_yyyy], 1, 4))
    }
    idx_ddmm <- idx8
    idx_ddmm[idx8] <- is.na(prim4) | prim4 < 1900 | prim4 > 2100
    if (any(idx_ddmm)) {
      ano[idx_ddmm] <- as.integer(substr(x_chr[idx_ddmm], 5, 8))
    }
  }
  ano
}

# ------------------------------------------------------------
# 5. Classificadores
# ------------------------------------------------------------

#' Classifica idade (anos) em faixas DATASUS
classificar_faixa_etaria <- function(idade_anos) {
  cut(
    idade_anos,
    breaks = FAIXAS_ETARIAS_BREAKS,
    labels = FAIXAS_ETARIAS_LABELS,
    right  = FALSE,
    include.lowest = TRUE
  )
}

#' Ponto médio da faixa etária (para APVP)
ponto_medio_faixa <- function(faixa) {
  unname(PONTOS_MEDIOS_FAIXA[as.character(faixa)])
}

#' Atribui capítulo CID-10 (I a XXII)
atribuir_capitulo_cid <- function(cid) {
  cid    <- toupper(as.character(cid))
  letra  <- substr(cid, 1, 1)
  numero <- suppressWarnings(as.integer(substr(cid, 2, 3)))

  cap <- dplyr::case_when(
    letra %in% c("A", "B")                       ~ "I",
    letra == "C"                                 ~ "II",
    letra == "D" & numero <= 49                  ~ "II",
    letra == "D" & numero >= 50 & numero <= 89   ~ "III",
    letra == "E"                                 ~ "IV",
    letra == "F"                                 ~ "V",
    letra == "G"                                 ~ "VI",
    letra == "H" & numero <= 59                  ~ "VII",
    letra == "H" & numero >= 60                  ~ "VIII",
    letra == "I"                                 ~ "IX",
    letra == "J"                                 ~ "X",
    letra == "K"                                 ~ "XI",
    letra == "L"                                 ~ "XII",
    letra == "M"                                 ~ "XIII",
    letra == "N"                                 ~ "XIV",
    letra == "O"                                 ~ "XV",
    letra == "P"                                 ~ "XVI",
    letra == "Q"                                 ~ "XVII",
    letra == "R"                                 ~ "XVIII",
    letra %in% c("S", "T")                       ~ "XIX",
    letra %in% c("V", "W", "X", "Y")             ~ "XX",
    letra == "Z"                                 ~ "XXI",
    letra == "U"                                 ~ "XXII",
    TRUE                                         ~ NA_character_
  )

  rotulo <- unname(CAPITULOS_CID_LABELS[cap])
  rotulo[is.na(rotulo)] <- "Outros"
  rotulo
}

#' Padroniza rótulos de capítulo vindos do TabNet
padronizar_capitulo_tabnet <- function(x) {
  x <- as.character(x)
  romano <- stringr::str_match(
    x, "^\\s*(?:Cap[íi]tulo\\s+)?([IVXLC]+)"
  )[, 2]
  rotulo <- unname(CAPITULOS_CID_LABELS[romano])

  nao_mapeado <- is.na(rotulo) & !is.na(x) & nzchar(x)
  if (any(nao_mapeado)) {
    warning(
      "padronizar_capitulo_tabnet: rótulos não mapeados: ",
      paste(unique(x[nao_mapeado]), collapse = " | "),
      call. = FALSE
    )
  }
  rotulo[nao_mapeado] <- "Outros"
  rotulo
}

# ------------------------------------------------------------
# 6. I/O de parquet
# ------------------------------------------------------------
ler_parquet <- function(caminho, obrigatorio = TRUE) {
  if (!file.exists(caminho)) {
    if (obrigatorio) {
      stop("Arquivo não encontrado: ", caminho, call. = FALSE)
    }
    return(NULL)
  }
  arrow::read_parquet(caminho)
}

salvar_parquet <- function(df, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, caminho)
  invisible(caminho)
}

# ------------------------------------------------------------
# 6b. Consolidação de arquivos anuais
# ------------------------------------------------------------
#' Consolida arquivos anuais em um único parquet
#'
#' Lê todos os arquivos que casam com `padrao` em `dir_entrada`,
#' une com coerção de tipos mistos e salva em `arquivo_saida`.
#'
#' @param dir_entrada      diretório com os parquets anuais
#' @param padrao           regex dos arquivos anuais (default: .parquet)
#' @param arquivo_saida    caminho completo do parquet consolidado
#' @param normalizar_nomes TRUE = tolower() nos nomes das colunas
#' @param verbose          imprime progresso
#' @return invisível, número de linhas do consolidado
consolidar_anuais <- function(dir_entrada,
                              padrao = "\\.parquet$",
                              arquivo_saida,
                              normalizar_nomes = TRUE,
                              verbose = TRUE) {
  
  arquivos <- list.files(dir_entrada, pattern = padrao, full.names = TRUE)
  
  # Não inclui o próprio consolidado na lista
  if (file.exists(arquivo_saida)) {
    arquivos <- arquivos[normalizePath(arquivos) !=
                           normalizePath(arquivo_saida)]
  }
  
  if (length(arquivos) == 0) {
    warning("Nenhum arquivo para consolidar em ", dir_entrada, call. = FALSE)
    return(invisible(0L))
  }
  
  # Valida tamanho mínimo
  tamanhos <- file.size(arquivos)
  ruins <- arquivos[tamanhos < 8]
  if (length(ruins) > 0) {
    if (verbose) cat(sprintf("  Removendo %d arquivos corrompidos (< 8 bytes)\n",
                             length(ruins)))
    unlink(ruins)
    arquivos <- arquivos[tamanhos >= 8]
  }
  
  if (verbose) cat(sprintf("  Consolidando %d arquivos de %s...\n",
                           length(arquivos), basename(dir_entrada)))
  
  lista <- purrr::map(arquivos, function(f) {
    d <- arrow::read_parquet(f)
    if (normalizar_nomes) names(d) <- tolower(names(d))
    d
  })
  
  # Coerção de tipos mistos
  tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
  tc <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
  mistas <- names(tc)[purrr::map_int(tc, length) > 1]
  if (length(mistas) > 0) {
    if (verbose) cat(sprintf("  %d colunas com tipos mistos -> coerção para character\n",
                             length(mistas)))
    lista <- purrr::map(lista, function(df) {
      for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      df
    })
  }
  
  consol <- dplyr::bind_rows(lista)
  rm(lista); gc(verbose = FALSE)
  
  arrow::write_parquet(consol, arquivo_saida)
  
  if (verbose) {
    cat(sprintf("  Consolidado: %s registros | %d colunas -> %s\n",
                format(nrow(consol), big.mark = ".", decimal.mark = ","),
                ncol(consol),
                basename(arquivo_saida)))
  }
  invisible(nrow(consol))
}

# ------------------------------------------------------------
# 7. Log
# ------------------------------------------------------------
registrar_log <- function(mensagem, nivel = "INFO", arquivo = NULL) {
  if (is.null(arquivo)) arquivo <- getOption("pipeline.arquivo_log")
  if (is.null(arquivo)) {
    dir_log <- if (exists("dir_registros")) dir_registros else tempdir()
    dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)
    arquivo <- file.path(dir_log, paste0("pipeline_", Sys.Date(), ".log"))
  }
  carimbo <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  linha   <- sprintf("[%s] [%-5s] %s", carimbo, nivel, mensagem)
  cat(linha, "\n", file = arquivo, append = TRUE)
  invisible(linha)
}

# ------------------------------------------------------------
# 8. Multi-fonte — tenta fontes em ordem
# ------------------------------------------------------------
#' Tenta fontes em ordem até uma retornar dados
#'
#' @param fontes lista de list(nome = "string", fn = function(ano) -> df|NULL|stop)
#' @param ano    parâmetro passado a cada fn
#' @param verboso imprime progresso (default TRUE)
#' @return list(dados = df, fonte = "nome", duracao = segundos) ou NULL
tentar_fontes <- function(fontes, ano, verboso = TRUE) {
  for (fonte in fontes) {
    t0 <- Sys.time()
    resultado <- tryCatch(
      fonte$fn(ano),
      error = function(e) {
        dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
        registrar_log(
          sprintf("[%s/%d] falhou (%.1fs): %s",
                  fonte$nome, ano, dt, conditionMessage(e)),
          nivel = "AVISO"
        )
        NULL
      }
    )
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

    if (!is.null(resultado) && is.data.frame(resultado) && nrow(resultado) > 0) {
      if (verboso) {
        cat(sprintf("      ✓ %s (%s)\n", fonte$nome, formatar_duracao(dt)))
      }
      return(list(dados = resultado, fonte = fonte$nome, duracao = dt))
    }
  }
  NULL
}

# ------------------------------------------------------------
# 8b. Configuração de fontes
# ------------------------------------------------------------

#' Lê config/fontes_prioridade.R e devolve FONTES_PRIORIDADE
#'
#' Se o arquivo não existir, devolve um default sensato.
#' Se existir mas não definir a variável, aborta.
#'
#' @param arquivo caminho alternativo (default: config/fontes_prioridade.R)
#' @return lista nomeada: sistema -> vetor de nomes de fontes em ordem
ler_config_fontes <- function(arquivo = NULL) {
  if (is.null(arquivo)) {
    base_dir <- if (exists("dir_config")) dir_config else
      file.path(getExportedValue("here", "here")(), "config")
    arquivo <- file.path(base_dir, "fontes_prioridade.R")
  }
  
  if (!file.exists(arquivo)) {
    warning(
      "config/fontes_prioridade.R não encontrado; usando prioridade default.",
      call. = FALSE
    )
    return(list(
      SIM          = c("healthbR", "ftp_datasusr", "microdatasus"),
      SINASC_MICRO = c("healthbR", "ftp_datasusr", "microdatasus"),
      SINAN        = c("healthbR", "ftp_datasusr"),
      SIH          = c("healthbR", "microdatasus"),
      CNES         = c("healthbR", "datasusr"),
      SIPNI        = c("healthbR"),
      POPULACAO    = c("brpop", "sidrar", "popsus")
    ))
  }
  
  env <- new.env(parent = emptyenv())
  sys.source(arquivo, envir = env)
  
  if (!exists("FONTES_PRIORIDADE", envir = env, inherits = FALSE)) {
    stop(
      "Arquivo '", arquivo, "' não define FONTES_PRIORIDADE.",
      call. = FALSE
    )
  }
  get("FONTES_PRIORIDADE", envir = env, inherits = FALSE)
}

#' Reordena uma lista de fontes segundo a prioridade configurada
#'
#' @param fontes        lista de list(nome=, fn=)
#' @param prioridade    vetor de nomes na ordem desejada
#' @return lista de fontes reordenada; fontes não listadas na
#'         prioridade vão para o fim, mantendo a ordem original
ordenar_fontes <- function(fontes, prioridade) {
  nomes_atuais <- vapply(fontes, function(f) f$nome, character(1))
  idx <- match(prioridade, nomes_atuais)
  idx <- idx[!is.na(idx)]
  idx <- c(idx, setdiff(seq_along(fontes), idx))
  fontes[idx]
}

# ------------------------------------------------------------
# 9. Verificações de arquivo
# ------------------------------------------------------------
info_arquivo <- function(caminho) {
  if (!file.exists(caminho)) {
    return(list(existe = FALSE, tamanho = NA, mtime = NA))
  }
  info <- file.info(caminho)
  list(
    existe  = TRUE,
    tamanho = format(info$size, units = "auto"),
    mtime   = info$mtime
  )
}

todos_existem <- function(caminhos) {
  all(file.exists(caminhos))
}

# ------------------------------------------------------------
# 11. Gestão de memória
# ------------------------------------------------------------

#' RSS do processo R atual (MB) — Linux via /proc/self/status
mem_rss_mb <- function() {
  if (.Platform$OS.type != "unix") return(NA_real_)
  linhas <- tryCatch(
    readLines("/proc/self/status", warn = FALSE),
    error = function(e) character()
  )
  m <- grep("^VmRSS:", linhas, value = TRUE)
  if (length(m) == 0) return(NA_real_)
  round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
}

#' RAM disponível no sistema (MB) — Linux via /proc/meminfo
mem_livre_mb <- function() {
  if (.Platform$OS.type != "unix") return(NA_real_)
  if (!file.exists("/proc/meminfo")) return(NA_real_)
  linhas <- tryCatch(
    readLines("/proc/meminfo", warn = FALSE),
    error = function(e) character()
  )
  m <- grep("^MemAvailable:", linhas, value = TRUE)
  if (length(m) == 0) return(NA_real_)
  round(as.numeric(gsub("[^0-9]", "", m[1])) / 1024, 1)
}

#' Força coleta de lixo (gc)
limpar_memoria <- function(verboso = FALSE) {
  invisible(gc(verbose = verboso))
}

#' Verifica headroom de RAM; aborta se insuficiente
#'
#' @param min_mb    RAM livre mínima exigida (MB)
#' @param abortar   TRUE → stop(); FALSE → warning()
#' @param contexto  string descritiva para o erro
#' @return TRUE (invisível) se há RAM suficiente
checar_memoria <- function(min_mb = 500, abortar = TRUE, contexto = "") {
  livre <- mem_livre_mb()
  if (is.na(livre)) return(invisible(TRUE))
  
  if (livre < min_mb) {
    msg <- sprintf(
      "RAM disponível: %.0f MB (mínimo exigido: %d MB). %s",
      livre, min_mb, contexto
    )
    if (abortar) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' Snapshot formatado da memória
mem_snapshot <- function() {
  sprintf("R (RSS): %.0f MB | Sistema livre: %.0f MB",
          mem_rss_mb(), mem_livre_mb())
}

#' Bloco processado com limpeza automática de memória
#'
#' Executa `fn_processar(dados)`, salva o resultado com `fn_salvar`
#' e libera `dados` e o resultado da memória IMEDIATAMENTE.
#'
#' @param fn_buscar     função() → data.frame | NULL
#' @param fn_processar  função(df) → df
#' @param fn_salvar     função(df) → invisible
#' @param min_mb_livre  RAM livre exigida antes de começar
#' @param contexto      string descritiva
#' @return list(status, n_linhas, duracao_s)
processar_item <- function(fn_buscar, fn_processar, fn_salvar,
                           min_mb_livre = 800, contexto = "") {
  t0 <- Sys.time()
  
  if (!checar_memoria(min_mb_livre, abortar = FALSE,
                      contexto = contexto)) {
    return(list(status = "sem_ram", n_linhas = NA_integer_,
                duracao_s = 0, contexto = contexto))
  }
  
  dados <- NULL
  processado <- NULL
  status <- "ok"
  n_linhas <- NA_integer_
  erro_msg <- NA_character_
  
  tryCatch({
    dados <- fn_buscar()
    if (is.null(dados) || !is.data.frame(dados) || nrow(dados) == 0) {
      status <- "vazio"
    } else {
      processado <- fn_processar(dados)
      n_linhas <- if (is.data.frame(processado)) nrow(processado) else NA_integer_
      rm(dados); dados <- NULL
      limpar_memoria()
      
      if (is.null(processado) || nrow(processado) == 0) {
        status <- "vazio"
      } else {
        fn_salvar(processado)
      }
    }
  }, error = function(e) {
    status <<- "erro"
    erro_msg <<- conditionMessage(e)
  }, finally = {
    if (exists("dados", inherits = FALSE)) rm(dados)
    if (exists("processado", inherits = FALSE)) rm(processado)
    limpar_memoria()
  })
  
  list(
    status     = status,
    n_linhas   = n_linhas,
    duracao_s  = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    contexto   = contexto,
    erro       = erro_msg
  )
}

# ------------------------------------------------------------
# 10. Mensagem de carregamento
# ------------------------------------------------------------
message("[01_utils] Funções auxiliares carregadas.")
message("           Faixas etárias : ", length(FAIXAS_ETARIAS_LABELS))
message("           Garbage codes  : ",
        length(GARBAGE_NATURAIS), " naturais + ",
        length(GARBAGE_EXTERNOS), " externos")
message("           Capítulos CID  : ", length(CAPITULOS_CID_LABELS))