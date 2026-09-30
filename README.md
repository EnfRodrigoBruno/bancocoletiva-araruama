# Banco de Dados de Saúde Coletiva — Araruama (RJ)

Pipeline reprodutível em R para extração, tratamento e análise de dados públicos de saúde do município de Araruama (RJ), a partir do DATASUS, IBGE e outras fontes oficiais.

## Objetivo

Construir uma base municipal integrada que apoie a gestão da saúde pública com evidência epidemiológica local, cobrindo:

- Mortalidade (SIM/DATASUS, 1996–2024)
- Nascidos vivos (SINASC/DATASUS, 1996–2024)
- Internações hospitalares (SIH/SUS, 2008–2024)
- Agravos de notificação (SINAN: dengue, tuberculose, hanseníase, sífilis congênita, chikungunya, zika, violência)
- Imunizações (SI-PNI, 2020–2025)
- Estabelecimentos de saúde (CNES, 2005–2024)
- População (IBGE / POPSVS / POPTCU, 2000–2025)
- Prévias TabNet (2025–2026)

## Estrutura do projeto

bancocoletiva/
├── codigo/                      # scripts numerados de extração e tratamento
│   ├── 00_setup.R               # configuração central
│   ├── 01_utils.R               # funções auxiliares
│   ├── 05_extracoes_comuns.R    # framework de extração
│   ├── 10-19_*.R                # extração por sistema
│   ├── 20_consolidar_base.R     # camada de tratamento
│   └── 21_visualizar_base.R     # relatório exploratório
├── config/                      # prioridades de fontes
├── dados_brutos/                # dados brutos (NÃO versionado)
├── dados_tratados/              # painéis analíticos (NÃO versionado)
├── resultados/                  # figuras e tabelas (NÃO versionado)
├── registros/                   # logs e relatórios de qualidade
├── renv.lock                    # versões exatas dos pacotes
└── README.md

## Como reproduzir

1. Clonar o repositório:

   git clone https://github.com/EnfRodrigoBruno/bancocoletiva-araruama.git
   cd bancocoletiva-araruama

2. Restaurar o ambiente exato (via renv):

   R -e 'renv::restore()'

3. Rodar o pipeline na ordem numérica. Cada script é autossuficiente, com cache local e retry.

   source("codigo/00_setup.R")
   source("codigo/10_extrair_sim.R")
   source("codigo/11_extrair_sinasc_agregado.R")
   # ... e assim por diante

## Fontes dos dados

- DATASUS / FTP: ftp://ftp.datasus.gov.br/dissemin/publicos/
- IBGE / SIDRA: https://sidra.ibge.gov.br/
- HealthBR (mirror R2 para SI-PNI): https://github.com/SidneyBissoli/healthbR

## Metodologia

- Extração em subprocessos (callr) com cap de RAM e timeout por item
- Cache local — reexecuções não rebaixam o que já existe
- Validação pós-write (parquet íntegro) e relatório de qualidade em registros/qualidade_base.csv
- Indicadores calculados com IC95% (Poisson exato para taxas, Wilson score para proporções)
- Tabelas finais em formato .parquet (integração com Arrow/DuckDB)

## Status

| Fase                    | Cobertura              | Status             |
|-------------------------|------------------------|--------------------|
| Extração                | 8 sistemas             | completo           |
| Tratamento              | 6 painéis analíticos   | completo           |
| Visualização            | 5 figuras + PDF        | completo           |
| Indicadores com IC95%   | —                      | em desenvolvimento |

## Dívidas técnicas registradas

Ver registros/pendencias.csv. Principais:

- dengue/2024 (SINAN) — arquivo DENGBR24 travou em read_dbc em hardware com pouca RAM
- TabNet mes — datasus::sim() não expõe "Mês do óbito" como linha; informação já obtida via DTOBITO do SIM microdados
- SINASC 2023-2024 — aguardando publicação DATASUS

## Licença

Dados públicos. Código sob licença MIT.

## Autor

Rodrigo Bruno — Enfermeiro
Secretaria Municipal de Saúde de Araruama (RJ)