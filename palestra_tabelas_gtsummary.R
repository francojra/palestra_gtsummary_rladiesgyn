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
    .by = clubem) %>% # Não utilizar o View com operador pipe 
   rows_delete(tibble(clubem = 1:6))
## Utilizando o {gt}
View(tab_descritiva)
tab_descritiva %>%
  gt()

# Verbos do {gt} ---------------------------------------------------------------------------------------------------------------------------

## Editando títulos

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = "Público Pagante Campeonato Brasileiro",
    subtitle = "Série A - 2022")

## Utilizando o pacote RMarkdown dentro do {gt}

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022")

## Adicionando fontes

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(md("**Fonte: CBF(2022)**"))

## Adicionando notas de rodapé

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br")

## Formatando os valores da tabela

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_number(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE) # Resume os valores indicando k ou m
# Ou podemos utilizar suffixing = "M" para o padrão português

## Colocar os valores em formato de dolar ou reais

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_currency(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE,
             currency = "BRL")

## Substituindo valores dentro de uma célula

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_currency(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE,
             currency = "BRL") %>%
  sub_values(pattern = "Fluminense-RJ",
             replacement = "Fluminense")

## Adicionando highlight em algumas células

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_currency(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE,
             currency = "BRL") %>%
  
  #### Chamando atenção para os clubes de SP
  
  tab_style_body(
    style = cell_fill(color = "#BDB76B"),
    pattern = "SP",

  #### Chamando atenção para os clubes que tenham a renda menor que 
  #### a renda mínima do flamengo

    columns = 3, # Utiliza apenas coluna 3 da média
    fn = function(x) x <= 33780)

## Alterando o estilo da tabela

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_currency(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE,
             currency = "BRL") %>%
  tab_options(
    table.width = pct(100), # Alterar a largura da tabela
    # pct(100) ocupa 100% do tamanho disponível
    table.background.color = "#DCDCDC", # Altera a cor interna da tabela
    table.font.color = "blue", # Altera a cor da fonte
    table.font.size = 12)

## Alterar o estilo da tabela para valores pré-definidos

tab_descritiva %>%
  gt() %>%
  tab_header(
    title = md("**Público Pagante Campeonato Brasileiro**"),
    subtitle = "Série A - 2022") %>%
  tab_source_note(
    source_note = "Fonte: CBF(2022)") %>%
  tab_footnote(
    footnote = "Raspado do site: www.srgoool.com.br") %>%
  fmt_currency(columns = 2:6, # Utiliza as colunas numéricas
             decimals = 2, 
             dec_mark = ",",
             sep_mark = ".",
             suffixing = TRUE,
             currency = "BRL") %>%
  tab_options(table.width = pct(100)) %>%
  cols_label(clubem = "Clubes") %>%
  opt_stylize(style = 2) # Cada número tem um estilo diferente

