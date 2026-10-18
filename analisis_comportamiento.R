# Este archivo usa los resultados que guarda script_revision_datos.Rmd.
# Antes de ejecutarlo, ejecuta ese script completo una vez.

for (paquete in c("dplyr", "ggplot2")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

ruta <- "data/processed"

archivos <- c(
  "tiques_base", "cliente_periodo",
  "kpi_periodos", "matriz_clientes"
)

stopifnot(file.exists(file.path(ruta, paste0(archivos, ".rds"))))

tiques_base     <- readRDS(file.path(ruta, "tiques_base.rds"))
cliente_periodo <- readRDS(file.path(ruta, "cliente_periodo.rds"))
kpi_periodos    <- readRDS(file.path(ruta, "kpi_periodos.rds"))
matriz_clientes <- readRDS(file.path(ruta, "matriz_clientes.rds"))

compradores_actuales <- cliente_periodo |>
  dplyr::filter(periodo == "12m_mayo_2023", comprador_observado)

compradores_anteriores <- cliente_periodo |>
  dplyr::filter(periodo == "12m_mayo_2022", comprador_observado)

# Control: mismas cifras que el script principal
stopifnot(
  nrow(compradores_actuales) == 136791,
  nrow(compradores_anteriores) == 114951,
  nrow(matriz_clientes) == 136791
)

# Ids con actividad anómala (posibles tarjetas de tienda o de empleado)

# 1. Comprobar datos necesarios
stopifnot(exists("cliente_periodo"))

# 2. Distribución de compras positivas por comprador en los últimos 12 meses
compras_12m <- cliente_periodo |>
  dplyr::filter(periodo == "12m_mayo_2023", comprador_observado)

cat("\nPERCENTILES DE COMPRAS POSITIVAS POR COMPRADOR (12 MESES)\n")
print(stats::quantile(
  compras_12m$n_compras_positivas,
  probs = c(0.99, 0.999, 0.9999)
))

# 3. Umbral: más de 100 compras en 12 meses (unas dos por semana)
umbral_compras <- 100

ids_anomalos <- compras_12m |>
  dplyr::filter(n_compras_positivas > umbral_compras) |>
  dplyr::arrange(dplyr::desc(n_compras_positivas)) |>
  dplyr::select(Id_cliente, n_compras_positivas, gasto_positivo)

cat("\nIDS CON ACTIVIDAD ANÓMALA\n")
print(c(
  ids = nrow(ids_anomalos),
  compras = sum(ids_anomalos$n_compras_positivas),
  gasto_positivo = round(sum(ids_anomalos$gasto_positivo), 2)
))

# 4. Sensibilidad de los KPI con y sin estos ids
kpi_sensibilidad <- function(excluir) {
  datos <- compras_12m |>
    dplyr::filter(!(Id_cliente %in% excluir))
  
  data.frame(
    compradores = nrow(datos),
    frecuencia_media = mean(datos$n_compras_positivas),
    gasto_por_comprador = mean(datos$gasto_positivo),
    pct_dos_o_mas_compras = 100 * mean(datos$n_compras_positivas >= 2)
  )
}

sensibilidad_ids <- dplyr::bind_rows(
  con_todos = kpi_sensibilidad(character(0)),
  sin_anomalos = kpi_sensibilidad(ids_anomalos$Id_cliente),
  .id = "escenario"
)

print(sensibilidad_ids, digits = 4)

# Valor esperado a 12 meses
# Probabilidad de volver a comprar y gasto observados entre ventanas consecutivas:
# compradores de jun 2021-may 2022 y su comportamiento en jun 2022-may 2023.

# 1. Comprobar datos necesarios
stopifnot(
  exists("cliente_periodo"),
  exists("matriz_clientes")
)

# 2. Compradores de la ventana anterior, por frecuencia
anteriores_valor <- cliente_periodo |>
  dplyr::filter(periodo == "12m_mayo_2022", comprador_observado) |>
  dplyr::mutate(
    frecuencia_grupo = dplyr::if_else(
      n_compras_positivas == 1,
      "Una compra",
      "Dos o más compras"
    )
  ) |>
  dplyr::select(Id_cliente, frecuencia_grupo)

# 3. Su gasto en la ventana siguiente
siguientes_valor <- cliente_periodo |>
  dplyr::filter(periodo == "12m_mayo_2023", comprador_observado) |>
  dplyr::select(Id_cliente, gasto_siguiente = gasto_positivo)

# 4. Probabilidad de compra, gasto si compra y valor esperado
tasas_valor <- anteriores_valor |>
  dplyr::left_join(siguientes_valor, by = "Id_cliente") |>
  dplyr::group_by(frecuencia_grupo) |>
  dplyr::summarise(
    clientes_base = dplyr::n(),
    prob_compra_12m = mean(!is.na(gasto_siguiente)),
    gasto_si_compra = mean(gasto_siguiente, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(valor_esperado_12m = prob_compra_12m * gasto_si_compra)

# 5. Aplicar a los compradores actuales, por segmento
valor_segmentos <- matriz_clientes |>
  dplyr::count(segmento, frecuencia_grupo, name = "clientes_actuales") |>
  dplyr::left_join(
    tasas_valor |> dplyr::select(frecuencia_grupo, valor_esperado_12m),
    by = "frecuencia_grupo"
  ) |>
  dplyr::mutate(valor_esperado_total = clientes_actuales * valor_esperado_12m)

# 6. Controles
stopifnot(
  sum(valor_segmentos$clientes_actuales) == nrow(matriz_clientes),
  !anyNA(valor_segmentos$valor_esperado_12m)
)

print(tasas_valor, digits = 4)
print(valor_segmentos, digits = 6)
cat("\nVALOR ESPERADO DE LA CARTERA ACTUAL A 12 MESES:",
    round(sum(valor_segmentos$valor_esperado_total), 0), "€\n")

# Gráficos del informe

# 1. Paquetes y estilo

library(ggplot2)

dir.create("figuras", showWarnings = FALSE)

azul_oscuro <- "#0B3C5D"
azul_medio  <- "#328CC1"
azul_claro  <- "#A9CCE3"
gris        <- "#7F8C8D"

fmt_num <- function(x, d = 0) {
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
}

con_signo <- function(x, d = 1, unidad = " %") {
  paste0(ifelse(x >= 0, "+", "\u2212"), fmt_num(abs(x), d), unidad)
}

# 2. Figura 1: evolución mensual
mensual <- tiques_base |>
  dplyr::filter(!is.na(Id_cliente), Importe_tiquet > 0) |>
  dplyr::mutate(mes_inicio = as.Date(format(Fecha_tiquet, "%Y-%m-01"))) |>
  dplyr::group_by(mes_inicio) |>
  dplyr::summarise(
    tiquets = dplyr::n(),
    compradores = dplyr::n_distinct(Id_cliente),
    .groups = "drop"
  )

mensual_largo <- dplyr::bind_rows(
  data.frame(mes_inicio = mensual$mes_inicio, valor = mensual$tiquets,
             serie = "Tiquets con importe positivo"),
  data.frame(mes_inicio = mensual$mes_inicio, valor = mensual$compradores,
             serie = "Compradores identificados")
)

fig1 <- ggplot(mensual_largo, aes(mes_inicio, valor, colour = serie, linetype = serie)) +
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2022-05-31"),
           ymin = -Inf, ymax = Inf, fill = azul_claro, alpha = 0.35) +
  annotate("rect", xmin = as.Date("2022-06-01"), xmax = as.Date("2023-05-31"),
           ymin = -Inf, ymax = Inf, fill = azul_medio, alpha = 0.18) +
  annotate("text", x = as.Date("2021-11-25"), y = 41000, size = 3, colour = azul_oscuro,
           label = "Ventana anterior\njun 2021-may 2022") +
  annotate("text", x = as.Date("2022-11-25"), y = 41000, size = 3, colour = azul_oscuro,
           label = "Ventana actual\njun 2022-may 2023") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = c("Tiquets con importe positivo" = azul_oscuro,
                                 "Compradores identificados" = azul_medio)) +
  scale_linetype_manual(values = c("Tiquets con importe positivo" = "solid",
                                   "Compradores identificados" = "dashed")) +
  scale_y_continuous(limits = c(0, 44000), labels = function(x) fmt_num(x)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  labs(title = "Tiquets y compradores por mes, enero 2021 a mayo 2023",
       x = NULL, y = NULL, colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", legend.justification = "left",
        plot.title = element_text(colour = azul_oscuro),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("figuras/fig1_evolucion_mensual.png", fig1, width = 9, height = 3.8, dpi = 300)

# 3. Figura 2: cuadro de mando
k22 <- kpi_periodos[kpi_periodos$periodo == "12m_mayo_2022", ]
k23 <- kpi_periodos[kpi_periodos$periodo == "12m_mayo_2023", ]

recurrencia_12m <- mean(
  compradores_anteriores$Id_cliente %in% compradores_actuales$Id_cliente
)

variacion <- function(actual, anterior) 100 * (actual / anterior - 1)

tarjetas <- data.frame(
  indicador = c("Clientes activos", "Frecuencia media (compras por activo)",
                "Gasto por activo", "Tique medio",
                "Recurrencia entre ventanas", "Compradores con 2 o más compras"),
  actual = c(fmt_num(k23$compradores_identificados),
             fmt_num(k23$frecuencia_media, 2),
             paste(fmt_num(k23$gasto_positivo_por_comprador, 2), "\u20ac"),
             paste(fmt_num(k23$ticket_medio_positivo, 2), "\u20ac"),
             paste(fmt_num(100 * recurrencia_12m, 1), "%"),
             paste(fmt_num(k23$porcentaje_compradores_con_repeticion, 1), "%")),
  anterior = c(fmt_num(k22$compradores_identificados),
               fmt_num(k22$frecuencia_media, 2),
               paste(fmt_num(k22$gasto_positivo_por_comprador, 2), "\u20ac"),
               paste(fmt_num(k22$ticket_medio_positivo, 2), "\u20ac"),
               "n.d.",
               paste(fmt_num(k22$porcentaje_compradores_con_repeticion, 1), "%")),
  cambio = c(
    variacion(k23$compradores_identificados, k22$compradores_identificados),
    variacion(k23$frecuencia_media, k22$frecuencia_media),
    variacion(k23$gasto_positivo_por_comprador, k22$gasto_positivo_por_comprador),
    variacion(k23$ticket_medio_positivo, k22$ticket_medio_positivo),
    NA,
    k23$porcentaje_compradores_con_repeticion - k22$porcentaje_compradores_con_repeticion
  ),
  objetivo = c("143.600", "2,14", "74,0 \u20ac", "\u2265 34,6 \u20ac", "47 %", "42 %")
)

tarjetas$texto_cambio <- ifelse(
  is.na(tarjetas$cambio), "",
  ifelse(seq_len(nrow(tarjetas)) == 6,
         con_signo(tarjetas$cambio, 1, " pp"),
         con_signo(tarjetas$cambio, 1))
)
tarjetas$sentido <- ifelse(is.na(tarjetas$cambio), "neutro",
                           ifelse(tarjetas$cambio >= 0, "sube", "baja"))
tarjetas$pie <- paste0("A\u00f1o anterior: ", tarjetas$anterior,
                       "   \u00b7   Objetivo: ", tarjetas$objetivo)
tarjetas$indicador <- factor(tarjetas$indicador, levels = tarjetas$indicador)

fig2 <- ggplot(tarjetas) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
            fill = "#F2F7FB", colour = azul_claro) +
  geom_text(aes(x = 0.06, y = 0.52, label = actual),
            hjust = 0, size = 7, fontface = "bold", colour = azul_oscuro) +
  geom_text(aes(x = 0.94, y = 0.52, label = texto_cambio, colour = sentido),
            hjust = 1, size = 3.8, fontface = "bold") +
  geom_text(aes(x = 0.06, y = 0.15, label = pie),
            hjust = 0, size = 2.6, colour = gris) +
  scale_colour_manual(values = c(sube = "#2E7D32", baja = "#C0392B", neutro = gris),
                      guide = "none") +
  facet_wrap(~ indicador, ncol = 3) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(title = "Cuadro de mando: jun 2022-may 2023 frente a la ventana anterior") +
  theme_void(base_size = 10) +
  theme(strip.text = element_text(hjust = 0.02, colour = gris, size = 9,
                                  margin = margin(b = 3)),
        plot.title = element_text(colour = azul_oscuro, margin = margin(b = 8)),
        panel.spacing = unit(0.8, "lines"))

ggsave("figuras/fig2_cuadro_mando.png", fig2, width = 9, height = 3.9, dpi = 300)

# 4. Figura 3: matriz de comportamiento
etiquetas_segmento <- c(
  "A" = "A. Una compra, sin continuidad",
  "B" = "B. Varias compras, sin continuidad",
  "C" = "C. Una compra, con continuidad",
  "D" = "D. Varias compras, con continuidad"
)

burbujas <- matriz_clientes |>
  dplyr::mutate(letra = substr(segmento, 1, 1)) |>
  dplyr::group_by(letra) |>
  dplyr::summarise(clientes = dplyr::n(),
                   gasto = sum(gasto_positivo),
                   .groups = "drop") |>
  dplyr::mutate(
    pct_clientes = 100 * clientes / sum(clientes),
    pct_gasto = 100 * gasto / sum(gasto),
    gasto_medio = gasto / clientes,
    etiqueta = paste0(etiquetas_segmento[letra], "\n",
                      fmt_num(pct_clientes, 1), " % clientes \u00b7 ",
                      fmt_num(pct_gasto, 1), " % gasto\n",
                      "Gasto medio: ", fmt_num(gasto_medio, 2), " \u20ac"),
    relleno = ifelse(letra == "D", azul_oscuro,
                     ifelse(letra == "B", azul_medio, azul_claro)),
    lx = pct_clientes + c(A = 0, B = 3.5, C = 3, D = 4.5)[letra],
    ly = pct_gasto + c(A = -9, B = 0, C = 0, D = 0)[letra],
    lh = c(A = 0.5, B = 0, C = 0, D = 0)[letra]
  )

fig3 <- ggplot(burbujas, aes(pct_clientes, pct_gasto)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = gris) +
  annotate("text", x = 50, y = 53, label = "gasto proporcional\na clientes",
           size = 2.8, colour = gris, hjust = 1) +
  geom_point(aes(size = gasto_medio, fill = relleno), shape = 21,
             colour = azul_oscuro, alpha = 0.9) +
  geom_label(aes(x = lx, y = ly, label = etiqueta, hjust = lh),
             size = 2.7, colour = azul_oscuro, linewidth = 0,
             fill = "white", lineheight = 0.95) +
  scale_fill_identity() +
  scale_size_area(max_size = 22, guide = "none") +
  scale_x_continuous(limits = c(0, 55), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 58), expand = c(0, 0)) +
  labs(title = "Matriz de comportamiento: frecuencia y continuidad",
       x = "% de compradores de los \u00faltimos 12 meses",
       y = "% del gasto positivo") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(colour = azul_oscuro),
        panel.grid.minor = element_blank())

ggsave("figuras/fig3_matriz_segmentos.png", fig3, width = 7.5, height = 5, dpi = 300)

