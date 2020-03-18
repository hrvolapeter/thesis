library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

read_processed <- function(name) {
  data <- read_csv(name)
  return(preprocess(data))
}

show_graph <- function(data, max_profile) {
  m = 0
  for (colname in colnames(data)) {
    data[[colname]] = normalize(data[[colname]], 0, max(max_profile[[colname]]));
    n = max(data[[colname]])
    if (n > m) {
      m = n
    }
  }

  p <-plot_ly(
    data,
    r = as.vector(t(data)),
    theta = rep.int(colnames(data), nrow(data)),
    frame = sort(rep.int(seq.int(nrow(data)), length(colnames(data)))),
    type = 'scatterpolar',
    mode = 'toself'
  )  %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      showlegend = F
    );
    p
}

preprocess <- function(data) {
  data$ipc <- data$instructions / data$cycles
  data$instructions <- NULL
  data$cycles <- NULL
  data$cache_references <- NULL
  data$cache_misses <- NULL
  data$l1_dcache_load_misses <- NULL
  data$l1_dcache_loads <- NULL
  data$l1_icache_load_misses <- NULL
  data$l1_icache_loads <- NULL
  data$llc_load_misses <- NULL
  data <- uni(data, 'disk', 'vfs_write', 'vfs_read')
  data <- uni(data, 'network', 'tcp_send_bytes', 'tcp_recv_bytes')
  return(data)
}

normalize <- function(col, minx, maxx) {
  return ((col - minx) / (maxx - minx))
}

uni <- function(df,x,y,z) {
  df[x] = df[y] + df[z]
  df <- df[,!(names(df) %in% c(y,z))]
  return(df)

}

colMax <- function(data) sapply(data, max, na.rm = TRUE)


prof <- function(datas) {
  res = NULL;
  for (data in datas) {
    res <- rbind(res, data);
  }
  maximum <- data.frame(as.list(colMax(res)));
  return(maximum);
}

process <- function (csv_name, plot, name, column) {
  data <- read_csv(csv_name);
  u <-
    aggregate(data, list(data$qps_requested), function(x) {
      return(quantile(x, 0.75))
    })
  colnames(u) <- paste(colnames(u), "u", sep = ".")
  m <-
    aggregate(data, list(data$qps_requested), function(x) {
      return(quantile(x, 0.5))
    })
  colnames(m) <- paste(colnames(m), "m", sep = ".")
  l <-
    aggregate(data, list(data$qps_requested), function(x) {
      return(quantile(x, 0.25))
    })
  colnames(l) <- paste(colnames(l), "l", sep = ".")

  res1 <-
    full_join(l, m, by = c("qps_requested.l" = "qps_requested.m"))
  res <-
    full_join(u, res1, by = c("qps_requested.u" = "qps_requested.l"))

  plot <-
    plot +
    layer(
      data = res,
      mapping = aes_string(x = "qps_requested.u", y = paste0(column,".m"), colour=quote(name)),
      stat = "identity",
      geom = "line",
      params=list(size=0.5),
      position = "identity"
    ) +
    geom_errorbar(
      data = res,
      aes_string(x = "qps_requested.u", ymin = paste0(column,".l"), ymax = paste0(column,".u"), colour=quote(name)),
      size = 0.4,
      alpha = .3
    )
    return(plot)
}


