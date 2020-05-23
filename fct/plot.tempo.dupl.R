# Funcao para calcular o tempo de duplicacao
plot.tempo.dupl <- function(df.td) {#data.frame com tempo de duplicacao
    plot <- df.td %>%
        mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data, y = estimativa)) +
        geom_ribbon(aes(ymin = ic.inf, ymax = ic.sup), fill = "lightgrey") +
        geom_line(size = 1.25, colour = RColorBrewer::brewer.pal(3, "Dark2")[1]) +
        scale_x_date(date_labels = "%d/%b", name = "") +
        # coloca coordenada cartesiana para resolver o problema do lim
        coord_cartesian(ylim = c(0, 100)) +
        #ylim(c(0, max(df.td$ic.sup)))
        ylab("Tempo de duplicação (dias)") +
        plot.formatos
    plot
}
