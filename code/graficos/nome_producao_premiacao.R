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
