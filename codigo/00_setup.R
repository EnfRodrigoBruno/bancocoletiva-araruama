# ============================================================
# 00_setup.R — Configuração central do projeto
# Versão 3.0.0 — Refatoração estrutural
# ============================================================
# Carregado por todos os demais scripts. Define:
#   - Raiz do projeto (here)
#   - Estrutura de diretórios
#   - Pacotes (obrigatórios vs recomendados)
#   - Constantes (município, UF, anos, versão)
#   - Opções globais
#
# Uso: source(here::here("codigo", "00_setup.R"))
#
# Mudanças v3.0.0:
#   - Datasusr e healthbR movidos para "recomendados" (verificados
#     em runtime pelos scripts que os usam).
#   - Novo diretório: dados_brutos/sinasc_micro e sinan explícitos.
#   - Adicionado MUNICIPIO_NOME para uso em cabeçalhos.
#   - Nova fase "extração" reflete nova ordem dos scripts.
# ============================================================

# ------------------------------------------------------------
# 1. Raiz do projeto
# ------------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  stop(
    "Pacote 'here' é obrigatório.\n",
    "Instale com: renv::install('here')",
    call. = FALSE
  )
}
raiz <- here::here()

# ------------------------------------------------------------
# 2. Estrutura de diretórios
# ------------------------------------------------------------
dir_codigo              <- file.path(raiz, "codigo")
dir_dados_brutos        <- file.path(raiz, "dados_brutos")
dir_brutos_sim          <- file.path(dir_dados_brutos, "sim")
dir_brutos_sinasc       <- file.path(dir_dados_brutos, "sinasc")
dir_brutos_sinasc_micro <- file.path(dir_dados_brutos, "sinasc_micro")
dir_brutos_sinan        <- file.path(dir_dados_brutos, "sinan")
dir_brutos_sih          <- file.path(dir_dados_brutos, "sih")
dir_brutos_sipni        <- file.path(dir_dados_brutos, "sipni")
dir_brutos_cnes         <- file.path(dir_dados_brutos, "cnes")
dir_brutos_pop          <- file.path(dir_dados_brutos, "populacao")
dir_brutos_previas      <- file.path(dir_dados_brutos, "sim_preliminar")
dir_dados_tratados      <- file.path(raiz, "dados_tratados")
dir_resultados          <- file.path(raiz, "resultados")
dir_registros           <- file.path(raiz, "registros")
dir_config              <- file.path(raiz, "config")

for (d in c(
  dir_codigo, dir_dados_brutos,
  dir_brutos_sim, dir_brutos_sinasc, dir_brutos_sinasc_micro,
  dir_brutos_sinan, dir_brutos_sih, dir_brutos_sipni,
  dir_brutos_cnes, dir_brutos_pop, dir_brutos_previas,
  dir_dados_tratados, dir_resultados, dir_registros,
  dir_config                                    # <- ADICIONE
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------
# 3. Pacotes
# ------------------------------------------------------------
# OBRIGATÓRIOS: se faltar, o setup falha.
pacotes_obrigatorios <- c(
  "here",
  "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "tibble", "forcats",
  "arrow", "ggplot2", "scales",
  "fs", "curl",
  "rmarkdown"
)

# RECOMENDADOS: se faltar, avisa. Scripts que dependem
# verificam em runtime e sugerem instalação.
pacotes_recomendados <- c(
  "callr",         # orquestração em subprocessos
  "cid10",         # dicionário CID-10
  "datasusr",      # leitura de DBC (SIM, SINASC micro, SINAN)
  "healthbR",      # mirror R2 (SIM, SINAN, SI-PNI)
  "microdatasus",  # fetch_datasus (SIH)
  "datasus",       # TabNet (prévias, SINASC agregado)
  "sidrar",        # SIDRA/IBGE
  "brpop",         # populações IBGE
  "janitor",       # limpeza de nomes
  "patchwork",     # painéis de gráficos
  "pagedown"       # PDF via Chrome headless
)

instalados <- rownames(installed.packages())
faltando_obrig <- setdiff(pacotes_obrigatorios, instalados)

if (length(faltando_obrig) > 0) {
  stop(
    "Pacotes obrigatórios ausentes:\n  ",
    paste(faltando_obrig, collapse = ", "),
    "\n\nInstale com:\n  renv::install(c(",
    paste0('"', faltando_obrig, '"', collapse = ", "),
    "))\n",
    call. = FALSE
  )
}

for (p in pacotes_obrigatorios) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE, quietly = TRUE)
  )
}

faltando_rec <- setdiff(pacotes_recomendados, instalados)
if (length(faltando_rec) > 0) {
  message(
    "Aviso: pacotes recomendados ausentes -> ",
    paste(faltando_rec, collapse = ", "),
    "\n  Scripts dependentes verificarão em runtime."
  )
}

# ------------------------------------------------------------
# 4. Constantes do projeto
# ------------------------------------------------------------
COD_IBGE_ARARUAMA_6 <- "330020"     # SIM, SINASC, SINAN, SIH
COD_IBGE_ARARUAMA_7 <- "3300209"    # IBGE, POPSVS, SIDRA
UF_PROJETO          <- "RJ"
MUNICIPIO_NOME      <- "Araruama (RJ)"

ANOS_SIM    <- 1996:2024   # microdados SIM
ANOS_POP    <- 2000:2025   # população (POPSVS/IBGE)
ANOS_PREVIA <- 2025:2026   # prévias via TabNet

VERSAO_PIPELINE <- "3.0.0"

if (min(ANOS_SIM) < min(ANOS_POP)) {
  message(
    "Aviso: ANOS_SIM começa em ", min(ANOS_SIM),
    " mas ANOS_POP começa em ", min(ANOS_POP),
    ".\n  Taxas para ", min(ANOS_SIM), "–", min(ANOS_POP) - 1,
    " ficarão sem denominador."
  )
}

# ------------------------------------------------------------
# 5. Opções globais
# ------------------------------------------------------------
options(
  timeout                = 600,
  stringsAsFactors       = FALSE,
  scipen                 = 999,
  dplyr.summarise.inform = FALSE
)

# ------------------------------------------------------------
# 6. Mensagem final
# ------------------------------------------------------------
message("============================================================")
message("  Setup carregado")
message("  Raiz       : ", raiz)
message("  Versão     : ", VERSAO_PIPELINE)
message("  Município  : ", MUNICIPIO_NOME)
message("  SIM        : ", min(ANOS_SIM), "–", max(ANOS_SIM))
message("  População  : ", min(ANOS_POP), "–", max(ANOS_POP))
message("  Prévias    : ", min(ANOS_PREVIA), "–", max(ANOS_PREVIA))
message("  Config     : ", dir_config)
message("============================================================")