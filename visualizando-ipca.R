---
title: "O Fantasma da (Hiper) Inflação e o IPCA"
date: '2025-08-25'
categories: ['ptbr', 'economics', 'tidyverse', 'ggplot2']
description: 'visualizando a história econômica do IPCA'
execute: 
  message: false
  warning: false
format:
  html:
    code-tools: true
    code-fold: true
    code-summary: 'Code'
---

## O que é Inflação?

Todo brasileiro já ouviu falar de **inflação**, seja pelo telejornal ou no supermercado fazendo as compras do mês. Em termos simples, ela é o **aumento generalizado de preços ao longo do tempo**. No Brasil, o índice oficial é o **IPCA** (Índice Nacional de Preços ao Consumidor Amplo), calculado pelo IBGE desde 1979.

O cálculo do IPCA é feito com base em uma cesta fixa de bens baseada na Pesquisa de Orçamentos Familiares (**POF**) e representa o consumo médio de famílias com renda de 1 a 40 salários mínimos. Estatiscamente, é um índice de **Laspeyres**:

$L_t = \frac{\sum p_t q_0}{\sum p_0 q_0}$

Visualizar a inflação brasileira tem suas pegadinhas. O século XX foi um periódo de alta inflação (e hiperinflação) e mudança de câmbio. Por isso, quando olhando para a história da inflação no país, iremos exergá-la em quatro momentos distintos:

-   **Jul/94 (Plano Real)** → estabilização após hiperinflação
-   **Jul/99 (Regime de Metas)** → foco explícito na inflação
-   **Mai/00 (LRF)** → disciplina fiscal como complemento da política monetária
-   **Mai/03 (pós-choque de 2002)** → consolidação institucional e credibilidade

Neste post, vamos percorrer a trajetória da inflação no Brasil desde o fim da hiperinflação até os dias de hoje, **visualizando o IPCA** (Índice de Preços ao Consumidor Amplo) com R e discutindo os principais marcos econômicos.

### Setup

#### Carregarando Dados e Pacotes 

Vamos utilizar a API do Banco Central via o pacote `{GetBCBData}` para baixar a série histórica do IPCA.

```{r}
library(pacman)

pacman::p_load(tidyverse, dplyr, ggplot2, lubridate,  
               ggtext, showtext, ggdist, GetBCBData,
               ragg, forecast, RcppRoll)

# Importar os dados
ipca <- gbcbd_get_series(
  id = 433,
  first.date = as.Date("1998-01-01")
)

ipca <- subset(ipca, ref.date <= as.Date("2022-12-01"))
```

#### Definindo Tema

```{r theme-plot}

# set theme 

theme <- theme_minimal(base_size = 10, base_family = "lato") +
  theme(
    text = element_text(colour = "gray1"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14, colour = "gray20"),
    plot.caption = element_text(size = 8, colour = "gray20"),
    legend.title = element_text(size = 8, colour = "gray1"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.box.margin = margin(0),
    legend.margin = margin(0)
  )

font_add_google("Lato", "lato")
showtext_auto()
```

### Sazonalidade e Inflação 

#### Preparando Séries Temporais 

```{r}
# Preparar séries temporais 
ipca <- ipca %>%
  mutate(
    year = factor(format(ref.date, "%Y")),
    month = factor(format(ref.date, "%m"), levels = sprintf("%02d", 1:12),
                   labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                              "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
    value = value / 100, # transformar de percentual para proporção
    acum12m = RcppRoll::roll_prod(1 + value, n = 12, fill = NA) - 1
  )
```

Considerar as caractéristicas de cada série temporal é essencial para a interpretação e avaliação de qualquer dado econômico, e com o IPCA não é diferente.

Como já mencionado, o índice é calculado com uma cesta com variados tipos de gastos (vestuário, combustível, serviços, para citar alguns) cuja oferta e demanda varia durante o ano. É por isso que, normalmente, os meses comemorativos Novembro, Dezembro e Janeiro costumam apresentar valores mais altos que no meio do ano.

```{r}
ipca_sazonal <- ipca %>%
  filter(ref.date >= as.Date("1998-01-01") & ref.date <= as.Date("2008-12-01"))

ipca_sazonal %>%
  ggplot(aes(x = ref.date, y = value * 100)) +
  
  # Linha principal da inflação
  geom_line(color = "#2c3e50", size = 1.2, alpha = 0.9) +
  
  # Linha de referência no zero
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 0.5) +
  
  # Marcos institucionais
  geom_vline(xintercept = as.Date("1999-07-01"), 
             color = "#e74c3c", linetype = "solid", alpha = 0.8, size = 1.5) +
  geom_vline(xintercept = as.Date("2000-05-01"), 
             color = "#3498db", linetype = "solid", alpha = 0.8, size = 1.5) +
  geom_vline(xintercept = as.Date("2003-05-01"), 
             color = "#f39c12", linetype = "solid", alpha = 0.8, size = 1.5) +
  
  # Primeira eleição do Lula
  geom_vline(xintercept = as.Date("2002-11-01"), 
             color = "#9b59b6", linetype = "longdash", alpha = 0.7, size = 1.2) +
  
  # Anotações dos marcos
  annotate("text", x = as.Date("1999-05-01"), y = 2.0, 
           label = "Regime de Metas", angle = 90, vjust = 0.5, 
           hjust = 0.5, size = 3.5, color = "#e74c3c", fontface = "bold",
           lineheight = 0.8) +
  annotate("text", x = as.Date("2000-02-01"), y = 2.0, 
           label = "LRF", angle = 90, vjust = 0.5, 
           hjust = 0.5, size = 3.5, color = "#3498db", fontface = "bold",
           lineheight = 0.8) +
  annotate("text", x = as.Date("2003-07-01"), y = 1.8, 
           label = "Pós-choque 2002", angle = 90, vjust = 0.5, 
           hjust = 0.5, size = 3.5, color = "#f39c12", fontface = "bold",
           lineheight = 0.8) +
  annotate("text", x = as.Date("2002-09-01"), y = 2.8, 
           label = "Eleição Lula", angle = 90, vjust = 0.5, 
           hjust = 0.5, size = 3.5, color = "#9b59b6", fontface = "bold",
           lineheight = 0.8) +
  
  labs(
    title = "Evolução do IPCA Mensal - Brasil (1998-2009)",
    subtitle = "",
    x = "Anos",
    y = "IPCA Mensal (%)",
    caption = paste("Fonte: Banco Central do Brasil (SGS - Série 433)",
                   sep = "\n")
  ) +
  
  # Escalas
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 0.1),
    breaks = seq(-1, 3, 0.5),
    limits = c(-1.2, 3.2)
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y",
    limits = c(as.Date("1998-01-01"), as.Date("2008-12-01")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  theme +
  theme(
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.major.x = element_line(color = "gray95", size = 0.2)
  )
```

Podemos ver que o maior outlier foi em Novembro de 2002, quando a inflação disparou em resposta da primeira eleição do presidente Lula, seguido por uma queda significativa.

### Inflação Acumulada de 12 Meses

A análise da inflação acumulada em 12 meses oferece uma perspectiva mais estável e relevante para política econômica, mitigando o efeito da sazonalidade.

```{r}
ipca %>%
  filter(!is.na(acum12m)) %>%
  ggplot(aes(x = ref.date, y = acum12m * 100)) +
  geom_line(color = "#8e44ad", size = 1) +
  
  # Marcos institucionais
  geom_vline(xintercept = as.Date("1999-07-01"), 
             color = "#e74c3c", linetype = "dotted", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2000-05-01"), 
             color = "#3498db", linetype = "dotted", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2003-05-01"), 
             color = "#f39c12", linetype = "dotted", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2002-11-01"), 
             color = "#9b59b6", linetype = "dashed", alpha = 0.7) +
  
  # Destacar períodos críticos
  annotate("rect", xmin = as.Date("2002-01-01"), xmax = as.Date("2003-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#e74c3c") +
  annotate("rect", xmin = as.Date("2008-06-01"), xmax = as.Date("2009-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#e67e22") +
  annotate("rect", xmin = as.Date("2014-06-01"), xmax = as.Date("2016-12-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#e74c3c") +
  annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2022-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#f39c12") +
  
  labs(
    title = "IPCA Acumulado em 12 Meses - Brasil (1999-2022)",
    subtitle = "",
    x = "Período",
    y = "IPCA Acumulado 12m (%)",
    caption = paste("Fonte: Banco Central do Brasil",
                   "Críticos: Crise 2002-03, Crise Global 2008-09, Recessão 2014-16, Pandemia 2021-22",
                   sep = "\n")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 12, 2)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme +
  theme(
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.major.x = element_line(color = "gray95", size = 0.2)
  )
```

#### Metas de Inflação

Em 1999, o Conselho Monetário Nacional introduziou o Regime de Metas de Inflação. Este sistema estabelece uma meta numérica específica para a inflação, com bandas de tolerância, permitindo maior transparência e previsibilidade para a política monetária seguida pelo Banco Central.

```{r}
# Definir grade temporal e metas históricas
grid <- tibble(
  ref.date = seq(
    as.Date("1998-01-01"),
    as.Date("2022-12-01"),
    "1 month"
  )
)

# Metas históricas de inflação no Brasil
ipca_meta <- tibble(
  year = 1999:2022,
  meta = c(8, 6, 4, 3.5, 4, 5.5, rep(4.5, 14), 4.25, 4, 3.75, 3.5),
  banda = c(rep(2, 4), rep(2.5, 3), rep(2, 11), rep(1.5, 6)),
  banda_superior = meta + banda,
  banda_inferior = meta - banda
)

# Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

ipca_com_metas <- ipca %>%
  inner_join(grid, by = "ref.date") %>%
  mutate(
    month = lubridate::month(ref.date, label = TRUE, abbr = TRUE),
    year = lubridate::year(ref.date),
    value_pct = value * 100,
    acum12m = RcppRoll::roll_prod(1 + value, n = 12, fill = NA) - 1,
    acum12m_pct = acum12m * 100
  ) %>%
  left_join(ipca_meta, by = "year") %>%
  mutate(
    deviation = acum12m_pct - meta,
    dentro_meta = abs(deviation) <= banda
  )

library(ggtext)

ipca_com_metas %>%
  filter(!is.na(acum12m_pct), !is.na(meta)) %>%
  ggplot(aes(x = ref.date)) +
  
  # Banda de tolerância
  geom_ribbon(aes(ymin = banda_inferior, ymax = banda_superior), 
              alpha = 0.2, fill = "#27ae60") +
  
  # Meta central
  geom_line(aes(y = meta), color = "#27ae60", size = 1, linetype = "dashed") +
  
  # IPCA realizado
  geom_line(aes(y = acum12m_pct), color = "#2c3e50", size = 1.2) +
  
  labs(
    title = "**IPCA Acumulado** versus <span style='color:#27ae60;'>**Metas de Inflação**</span> (1999-2022)",
    x = "Período",
    y = "Inflação Acumulada 12m (%)",
    caption = "Fonte: Banco Central do Brasil"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 12, 2)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme +
  theme(
    plot.title = element_markdown(size = 18),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.major.x = element_line(color = "gray95", size = 0.2)
  )
```
