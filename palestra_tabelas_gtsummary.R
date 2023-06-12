### R-Ladies Gyn
### Palestra: gtsummary: uma nova maneira de criar tabelas elegantes e flexíveis
### Palestrante: Quemuel Rodrigues (Baruque)

# Conteúdo ---------------------------------------------------------------------------------------------------------------------------------

# 1 - Entendendo o que é o {gt};
# 2 - Aprendendo os verbos do {gt};
# 3 - Construindo tabelas no R utilizando o {gt};
# 4 - Exportando tabelas com o {gt};
# 5 - Compreendendo o que é o {gtExtras};
# 6 - Produzindo lindas tabelas com o {gtExtras};
# 7 - Inserindo plots dentro de tabelas;
# 8 - Personalizando tabelas com imagens e logos.

# Verbos (camadas) das tabelas -------------------------------------------------------------------------------------------------------------

# - Títulos
# - Subtítulos
# - Rótulos das linhas
# - Rótulos das colunas
# - Células

# Instalar pacotes -------------------------------------------------------------------------------------------------------------------------

install.packages("gt") # Conhecido por ser o ggplot das tabelas.
install.packages("gtsummary")
install.packages("devtools")
devtools::install_github("BaruqueRodrigues/renda.brasileirao")

# Fluxo de trabalho no {gt} ----------------------------------------------------------------------------------------------------------------

## Carregar pacotes

library(dplyr)
library(gt)
library(rendas.brasileirao)

## Carregar dados

dados <- rendas.brasileirao::baixa_rendas_brasileirao(1:38, 2022)
# Seleciona rodadas de 1 a 38 para o ano de 2022.
View(dados)

## Criar tabela descritiva

tab_descritiva <- dados %>%
  dplyr::summarise(
    "Mínimo do público pagante" = min(pagante, na.rm = TRUE),
    "Média do público pagante" = mean(pagante, na.rm = TRUE),
    "Mediana do público pagante" = median(pagante, na.rm = TRUE),
    "Desvio padrão do público pagante" = sd(pagante, na.rm = TRUE),
    "Máximo do público pagante" = max(pagante, na.rm = TRUE),
    .by = clubem) # Não utilizar o View com operador pipe

## Utilizando o {gt}

tab_descritiva %>%
  gt()

# Verbos do {gt} ---------------------------------------------------------------------------------------------------------------------------

## Editando títulos

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = "Público Pagante Campeonato Brasileiro",
    subtitle = "Série A - 2022"
  )

### Utilizando o pacote RMarkdown dentro do {gt}

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022"
  )
