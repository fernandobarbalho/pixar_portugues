library(magrittr, include.only = "%>%")

pixar_completo <-
  dados::pixar_filmes %>%
  dplyr::inner_join(dados::pixar_generos) %>%
  dplyr::inner_join(dados::pixar_equipe) %>%
  dplyr::inner_join(dados::pixar_bilheteria) %>%
  dplyr::inner_join(dados::pixar_avalicao_publico)

equipe_20 <-
  pixar_completo %>%
  dplyr::mutate(
    nome = dplyr::case_when(
      nome == "Stanton" ~ "Andrew Stanton",
      nome == "Docter" ~ "Pete Docter",
      nome == "Lasseter" ~ "John Lasseter",
      nome == "Unkrich" ~ "Lee Unkrich",
      nome == "Scanlon" ~ "Dan Scanlon",
      TRUE ~ nome
    )
  ) %>%
  dplyr::group_by(nome) %>%
  dplyr::summarise(quant = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::slice_max(order_by = quant, n = 20) %>%
  dplyr::select(nome)

paleta_de_cores <- c(
  "#3170A9", "#F0F6F0", "#F7C026",
  "#D04427", "#EAAB66", "#864F34"
)
# graficos a partir daqui

dados::pixar_oscars %>%
  dplyr::inner_join(dados::pixar_equipe) %>%
  dplyr::inner_join(equipe_20) %>%
  dplyr::group_by(nome, resultado) %>%
  dplyr::summarise(
    premios = dplyr::n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    nome = forcats::fct_reorder(nome, premios, sum)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(y = nome, x = premios, fill = resultado)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values =
      paleta_de_cores[-2]
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = paleta_de_cores[2]),
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(color = paleta_de_cores[1]),
    axis.text.x = ggplot2::element_text(color = paleta_de_cores[1]),
    axis.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

dados::pixar_oscars %>%
  dplyr::inner_join(dados::pixar_equipe) %>%
  dplyr::inner_join(equipe_20) %>%
  dplyr::group_by(nome, resultado) %>%
  dplyr::summarise(
    premios = dplyr::n()
  ) %>%
  dplyr::group_by(resultado) %>%
  dplyr::mutate(
    pct = premios / sum(premios) * 100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    nome = forcats::fct_reorder(nome, premios, sum)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(y = nome, x = premios, fill = resultado)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values =
      paleta_de_cores[-2]
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = paleta_de_cores[2]),
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(color = paleta_de_cores[1]),
    axis.text.x = ggplot2::element_text(color = paleta_de_cores[1]),
    axis.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )
