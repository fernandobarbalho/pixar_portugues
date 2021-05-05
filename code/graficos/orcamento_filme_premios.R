library(ggplot2)
dados::pixar_oscars %>%
  dplyr::inner_join(dados::pixar_bilheteria) %>%
  dplyr::filter(resultado %in% c("Venceu", "Venceu Prêmio Especial")) %>%
  dplyr::group_by(
    filme, 
    orcamento, 
    resultado
  ) %>%
  dplyr::summarise(premios = as.factor(dplyr::n())) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = filme, 
    y = orcamento
  )) +
  ggplot2::geom_point(aes(size = premios),
    fill = paleta_de_cores[1],
    color = paleta_de_cores[5],
    pch = 21
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = paleta_de_cores[2]),
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(color = paleta_de_cores[1]),
    axis.text.x = ggplot2::element_text(
      color = paleta_de_cores[1],
      angle = 35, 
      hjust=1)
  ) +
  ggplot2::labs(
    y = "Orçamento em milhões de dólares (US$)",
    x = "Filme"
  )
  
