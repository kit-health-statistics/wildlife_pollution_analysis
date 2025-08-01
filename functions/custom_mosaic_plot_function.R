# Direction of splitting:
# 1. split into bars
# 2. split the bars vertically
# 3. further split horizontally

rectangles_for_mosaic_plots <- function(
  bars,
  splitting,
  sec_splitting,
  bar_width_prop = 0.85
) {
  # Reverse the levels of the factor of the primary split to have the same
  # ordering as in a simple barplot
  splitting <- factor(splitting, levels = rev(levels(splitting)))

  num_obs_per_bar <- unlist(table(bars))
  n_bars <- length(num_obs_per_bar)
  bar_width <- bar_width_prop * 1 / n_bars
  space_width <- (1 - bar_width_prop) / (n_bars - 1)

  xmin <- cumsum(c(0, rep(bar_width + space_width, times = n_bars - 1)))
  xmax <- cumsum(c(bar_width, rep(bar_width + space_width, times = n_bars - 1)))

  table_splitting <- table(bars, splitting)
  proportions_splitting <- t(apply(table_splitting, 1, function(x) {
    x / sum(x)
  }))

  x_modify_split <- t(apply(proportions_splitting, 1, cumsum)) * bar_width

  # Concatenates by columns, e.g. all Male first, then all Female
  xmax_split <- c(sweep(x_modify_split, 1, xmin, FUN = "+"))
  xmin_split <- xmax_split - c(proportions_splitting) * bar_width

  df_primary_splitting <- data.frame(
    xmin = xmin_split,
    xmax = xmax_split,
    ymin = 0,
    ymax = 1,
    split = rep(levels(splitting), each = n_bars),
    bar = rep(names(num_obs_per_bar), times = length(levels(splitting)))
  )

  table_sec_splitting <- as.array(table(bars, splitting, sec_splitting))

  proportions_sec_splitting <- apply(table_sec_splitting, 1:2, function(x) {
    x / sum(x)
  }) %>%
    apply(c(1, 3), t) %>%
    apply(3, as.data.frame) %>%
    lapply(function(x) {
      mutate(x, bar = names(num_obs_per_bar))
    }) %>%
    bind_rows(.id = "split") %>%
    pivot_longer(
      names(table_sec_splitting[1, 1, ]),
      names_to = "sec_split",
      values_to = "modify_ymin"
    ) %>%
    group_by(bar, split) %>%
    mutate(modify_ymax = cumsum(modify_ymin)) %>%
    ungroup()

  df_sec_splitting <- merge(
    df_primary_splitting,
    proportions_sec_splitting,
    by = c("split", "bar")
  ) %>%
    mutate(
      ymax = ymin + modify_ymax,
      ymin = ymax - modify_ymin
    ) %>%
    filter(!is.nan(ymin))  # Filter out "empty" rectangles

  return(df_sec_splitting)
}
