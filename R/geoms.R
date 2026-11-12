#' Half-normal QQ points
#'
#' `geom_qqhn()` draws a half-normal QQ plot using absolute sample values.
#' Theoretical quantiles are from the half-normal distribution, i.e.
#' `qnorm((p + 1) / 2)`.
#'
#' @inheritParams ggplot2::geom_qq
#' @param stat The statistical transformation to use on the layer.
#' @param abs_sample Should the sample be converted to absolute values. Defaults
#'   to `TRUE` for half-normal plots.
#' @param label_n Number of largest points to label. These labeled points are
#'   removed from the point layer and drawn as text labels. Label text is taken
#'   from the `labels` aesthetic when mapped, otherwise from sample values.
#' @export
geom_qqhn <- function(
  mapping = NULL,
  data = NULL,
  stat = "qqhn",
  position = "identity",
  ...,
  abs_sample = TRUE,
  label_n = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!is.numeric(label_n) || length(label_n) != 1L || is.na(label_n) || label_n < 0) {
    stop("`label_n` must be a single non-negative number.", call. = FALSE)
  }

  label_n <- as.integer(label_n)
  base_mapping <- mapping %||% ggplot2::aes()
  labels_var <- NULL
  if (!is.null(base_mapping$labels) && is_formula_symbol(base_mapping$labels)) {
    labels_var <- formula_symbol_name(base_mapping$labels)
  } else if (!is.null(base_mapping$label) && is_formula_symbol(base_mapping$label)) {
    labels_var <- formula_symbol_name(base_mapping$label)
  }

  point_mapping <- base_mapping
  point_mapping$label <- NULL
  point_mapping$labels <- NULL

  point_layer <- ggplot2::layer(
    data = data,
    mapping = point_mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      abs_sample = abs_sample,
      label_n = label_n,
      labels_var = labels_var,
      output = if (label_n > 0) "points" else "all",
      na.rm = na.rm,
      ...
    )
  )

  layers <- list(point_layer)
  if (label_n <= 0) {
    return(new_geom_qqhn_bundle(layers, base_mapping$sample))
  }

  label_mapping <- base_mapping
  label_mapping$label <- NULL
  label_mapping$labels <- NULL

  label_layer <- ggplot2::layer(
    data = data,
    mapping = label_mapping,
    stat = stat,
    geom = ggplot2::GeomText,
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      abs_sample = abs_sample,
      label_n = label_n,
      labels_var = labels_var,
      output = "labels",
      na.rm = na.rm,
      ...
    )
  )

  layers <- c(layers, list(label_layer))
  new_geom_qqhn_bundle(layers, base_mapping$sample)
}

#' @rdname geom_qqhn
#' @param geom The geometric object to use display the data.
#' @param labels_var Internal parameter used to forward label-column mappings.
#' @param output Internal parameter controlling whether `stat_qqhn()` returns
#'   all transformed rows, only point rows, or only labeled rows.
#' @export
stat_qqhn <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  abs_sample = TRUE,
  label_n = 0,
  labels_var = NULL,
  output = c("all", "points", "labels"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  output <- match.arg(output)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQqhn,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      abs_sample = abs_sample,
      label_n = label_n,
      labels_var = labels_var,
      output = output,
      na.rm = na.rm,
      ...
    )
  )
}

StatQqhn <- ggplot2::ggproto(
  "StatQqhn",
  ggplot2::Stat,
  required_aes = c("sample"),
  dropped_aes = c("sample"),
  default_aes = ggplot2::aes(label = ggplot2::after_stat(label)),
  compute_panel = function(data, scales, abs_sample = TRUE, label_n = 0, labels_var = NULL, output = c("all", "points", "labels")) {
    output <- match.arg(output)
    groups <- normalize_qq_groups(data)
    pieces <- lapply(groups, function(gdat) {
      sample <- gdat$sample
      labels <- NULL
      if (!is.null(labels_var) && labels_var %in% names(gdat)) {
        labels <- gdat[[labels_var]]
      }
      if (is.null(labels)) {
        labels <- gdat$labels
      }
      if (is.null(labels)) {
        labels <- gdat$label
      }
      if (is.null(labels)) {
        labels <- sample
      }

      if (abs_sample) {
        sample <- abs(sample)
      }

      keep <- is.finite(sample)
      sample <- sample[keep]
      labels <- as.character(labels[keep])
      passthrough <- gdat[keep, , drop = FALSE]

      n <- length(sample)
      if (n == 0) {
        return(data.frame(x = numeric(0), y = numeric(0), label = character(0), group = integer(0), is_label = logical(0)))
      }

      ord <- order(sample)
      sample <- sample[ord]
      labels <- labels[ord]
      passthrough <- passthrough[ord, , drop = FALSE]

      probs <- stats::ppoints(n)
      theoretical <- stats::qnorm((probs + 1) / 2)

      label_n_g <- min(as.integer(label_n), n)
      is_label <- rep(FALSE, n)
      if (label_n_g > 0) {
        is_label[seq.int(n - label_n_g + 1, n)] <- TRUE
      }

      group_id <- if ("group" %in% names(gdat)) gdat$group[1] else 1L
      passthrough$sample <- NULL
      passthrough$labels <- NULL
      passthrough$label <- NULL
      passthrough$group <- group_id

      out <- cbind(
        passthrough,
        data.frame(
          x = theoretical,
          y = sample,
          label = labels,
          is_label = is_label
        )
      )

      if (output == "points") {
        out <- out[!out$is_label, , drop = FALSE]
      } else if (output == "labels") {
        out <- out[out$is_label, , drop = FALSE]
      }

      out
    })

    do.call(rbind, pieces)
  }
)

#' Half-normal QQ reference line
#'
#' `geom_qqhn_line()` adds a reference line for a half-normal QQ plot.
#' It mirrors `geom_qq_line()` but uses half-normal theoretical quantiles.
#'
#' @inheritParams ggplot2::geom_qq_line
#' @param stat The statistical transformation to use on the layer.
#' @param line.p A single probability `p` in `(0, 0.5)` used with its symmetric
#'   counterpart `1 - p` to compute the reference line, or a 2-vector `c(0, p)`
#'   with `p` in `(0, 1)` to force the line through the origin.
#' @param abs_sample Should the sample be converted to absolute values. Defaults
#'   to `TRUE` for half-normal plots.
#' @export
geom_qqhn_line <- function(
  mapping = NULL,
  data = NULL,
  stat = "qqhn_line",
  position = "identity",
  ...,
  line.p = 0.25,
  abs_sample = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  line_mapping <- mapping %||% ggplot2::aes()
  line_mapping$label <- NULL
  line_mapping$labels <- NULL

  ggplot2::layer(
    data = data,
    mapping = line_mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line.p = line.p,
      abs_sample = abs_sample,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_qqhn_line
#' @param geom The geometric object to use display the data.
#' @export
stat_qqhn_line <- function(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  line.p = 0.25,
  abs_sample = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQqhnLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line.p = line.p,
      abs_sample = abs_sample,
      na.rm = na.rm,
      ...
    )
  )
}

#' Half-normal QQ confidence band
#'
#' `geom_qqhn_band()` adds a confidence envelope around the half-normal QQ
#' reference line. The envelope is built from order-statistic probabilities
#' mapped through half-normal quantiles and then transformed by the fitted QQHN
#' line.
#'
#' Use `conf.level` to control envelope coverage (for example `0.95`); `alpha`
#' only controls ribbon transparency.
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param stat The statistical transformation to use on the layer.
#' @param line.p A single probability `p` in `(0, 0.5)` used with its symmetric
#'   counterpart `1 - p` to compute the reference line, or a 2-vector `c(0, p)`
#'   with `p` in `(0, 1)` to force the line through the origin.
#' @param abs_sample Should the sample be converted to absolute values. Defaults
#'   to `TRUE` for half-normal plots.
#' @param conf.level Confidence level for the envelope.
#' @export
geom_qqhn_band <- function(
  mapping = NULL,
  data = NULL,
  stat = "qqhn_band",
  position = "identity",
  ...,
  line.p = 0.25,
  abs_sample = TRUE,
  conf.level = 0.95,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  band_mapping <- mapping %||% ggplot2::aes()
  band_mapping$label <- NULL
  band_mapping$labels <- NULL

  ggplot2::layer(
    data = data,
    mapping = band_mapping,
    stat = stat,
    geom = ggplot2::GeomRibbon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line.p = line.p,
      abs_sample = abs_sample,
      conf.level = conf.level,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_qqhn_band
#' @param geom The geometric object to use display the data.
#' @export
stat_qqhn_band <- function(
  mapping = NULL,
  data = NULL,
  geom = "ribbon",
  position = "identity",
  ...,
  line.p = 0.25,
  abs_sample = TRUE,
  conf.level = 0.95,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQqhnBand,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      line.p = line.p,
      abs_sample = abs_sample,
      conf.level = conf.level,
      na.rm = na.rm,
      ...
    )
  )
}

StatQqhnBand <- ggplot2::ggproto(
  "StatQqhnBand",
  ggplot2::Stat,
  required_aes = c("sample"),
  dropped_aes = c("sample", "labels", "label"),
  compute_panel = function(data, scales, line.p = 0.25, abs_sample = TRUE, conf.level = 0.95) {
    if (!is.numeric(conf.level) || length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
      stop("`conf.level` must be a single number in (0, 1).", call. = FALSE)
    }

    line_ref <- resolve_qqhn_line.p(line.p)

    groups <- normalize_qq_groups(data)
    pieces <- lapply(groups, function(gdat) {
      sample <- gdat$sample
      if (abs_sample) {
        sample <- abs(sample)
      }

      sample <- sample[is.finite(sample)]
      n <- length(sample)
      if (n < 2) {
        return(data.frame(x = numeric(0), ymin = numeric(0), ymax = numeric(0), group = integer(0)))
      }

      sample <- sort(sample)
      probs <- stats::ppoints(n)
      theoretical <- stats::qnorm((probs + 1) / 2)

      if (line_ref$through_origin) {
        sample_q <- as.numeric(stats::quantile(sample, probs = line_ref$probs[2], names = FALSE, type = 7))
        theoretical_q <- stats::qnorm((line_ref$probs[2] + 1) / 2)
        slope <- sample_q / theoretical_q
        intercept <- 0
      } else {
        sample_q <- as.numeric(stats::quantile(sample, probs = line_ref$probs, names = FALSE, type = 7))
        theoretical_q <- stats::qnorm((line_ref$probs + 1) / 2)
        slope <- diff(sample_q) / diff(theoretical_q)
        intercept <- sample_q[1] - slope * theoretical_q[1]
      }

      alpha <- 1 - conf.level
      i <- seq_len(n)
      p_lo <- stats::qbeta(alpha / 2, i, n - i + 1)
      p_hi <- stats::qbeta(1 - alpha / 2, i, n - i + 1)

      x_lo <- stats::qnorm((p_lo + 1) / 2)
      x_hi <- stats::qnorm((p_hi + 1) / 2)

      y_lo <- intercept + slope * x_lo
      y_hi <- intercept + slope * x_hi

      group_id <- if ("group" %in% names(gdat)) gdat$group[1] else 1L
      data.frame(
        x = theoretical,
        ymin = pmin(y_lo, y_hi),
        ymax = pmax(y_lo, y_hi),
        group = group_id
      )
    })

    do.call(rbind, pieces)
  }
)

StatQqhnLine <- ggplot2::ggproto(
  "StatQqhnLine",
  ggplot2::Stat,
  required_aes = c("sample"),
  dropped_aes = c("sample", "labels", "label"),
  compute_panel = function(data, scales, line.p = 0.25, abs_sample = TRUE) {
    line_ref <- resolve_qqhn_line.p(line.p)
    groups <- normalize_qq_groups(data)
    pieces <- lapply(groups, function(gdat) {
      sample <- gdat$sample
      if (abs_sample) {
        sample <- abs(sample)
      }

      sample <- sample[is.finite(sample)]
      n <- length(sample)
      if (n < 2) {
        return(data.frame(x = numeric(0), y = numeric(0), group = integer(0)))
      }

      passthrough <- gdat[is.finite(gdat$sample), , drop = FALSE]

      sample <- sort(sample)

      probs <- stats::ppoints(n)
      theoretical <- stats::qnorm((probs + 1) / 2)

      if (line_ref$through_origin) {
        sample_q <- as.numeric(stats::quantile(sample, probs = line_ref$probs[2], names = FALSE, type = 7))
        theoretical_q <- stats::qnorm((line_ref$probs[2] + 1) / 2)
        slope <- sample_q / theoretical_q
        intercept <- 0
      } else {
        sample_q <- as.numeric(stats::quantile(sample, probs = line_ref$probs, names = FALSE, type = 7))
        theoretical_q <- stats::qnorm((line_ref$probs + 1) / 2)
        slope <- diff(sample_q) / diff(theoretical_q)
        intercept <- sample_q[1] - slope * theoretical_q[1]
      }

      x <- range(theoretical)
      y <- intercept + slope * x

      group_id <- if ("group" %in% names(gdat)) gdat$group[1] else 1L
      passthrough$sample <- NULL
      passthrough$labels <- NULL
      passthrough$label <- NULL
      passthrough <- passthrough[rep(1L, 2L), , drop = FALSE]
      passthrough$group <- group_id

      cbind(passthrough, data.frame(x = x, y = y))
    })

    do.call(rbind, pieces)
  }
)

#' Pareto bars
#'
#' `geom_pareto_bars()` draws a Pareto-style bar layer:
#' values are sorted by decreasing absolute `y`, bars are drawn with absolute
#' heights, and a computed `fill` indicates the original sign (`"positive"` or
#' `"negative"`).
#'
#' @inheritParams ggplot2::geom_col
#' @param stat The statistical transformation to use on the layer.
#' @export
geom_pareto_bars <- function(
  mapping = NULL,
  data = NULL,
  stat = "pareto_bars",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (is.null(mapping) || is.null(mapping$fill)) {
    mapping <- utils::modifyList(
      ggplot2::aes(fill = ggplot2::after_stat(sign)),
      mapping %||% ggplot2::aes()
    )
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomCol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_pareto_bars
#' @param geom The geometric object to use display the data.
#' @export
stat_pareto_bars <- function(
  mapping = NULL,
  data = NULL,
  geom = "col",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatParetoBars,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

StatParetoBars <- ggplot2::ggproto(
  "StatParetoBars",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_panel = function(data, scales) {
    data <- data[is.finite(data$y), , drop = FALSE]
    if (nrow(data) == 0) {
      return(data.frame())
    }

    ord <- order(abs(data$y), decreasing = TRUE)
    sorted <- data[ord, , drop = FALSE]

    x_labels <- as.character(sorted$x)
    x_pos <- seq_len(nrow(sorted))

    data.frame(
      x = x_pos,
      y = abs(sorted$y),
      x_label = x_labels,
      sign = ifelse(sorted$y >= 0, "positive", "negative")
    )
  }
)

#' Pareto cumulative line
#'
#' `geom_pareto_line()` computes cumulative totals from absolute values sorted by
#' decreasing magnitude and draws the resulting path.
#'
#' @inheritParams ggplot2::geom_line
#' @param stat The statistical transformation to use on the layer.
#' @param cumulative `"raw"` for cumulative absolute sums; `"proportion"` for
#'   cumulative percentages in `[0, 1]`.
#' @export
geom_pareto_line <- function(
  mapping = NULL,
  data = NULL,
  stat = "pareto_line",
  position = "identity",
  ...,
  cumulative = c("raw", "proportion"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  cumulative <- match.arg(cumulative)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      cumulative = cumulative,
      na.rm = na.rm,
      ...
    )
  )
}

#' Pareto cumulative points
#'
#' `geom_pareto_point()` computes cumulative totals from absolute values sorted
#' by decreasing magnitude and draws the resulting points.
#'
#' @inheritParams ggplot2::geom_point
#' @param stat The statistical transformation to use on the layer.
#' @param cumulative `"raw"` for cumulative absolute sums; `"proportion"` for
#'   cumulative percentages in `[0, 1]`.
#' @rdname geom_pareto_line
#' @export
geom_pareto_point <- function(
  mapping = NULL,
  data = NULL,
  stat = "pareto_line",
  position = "identity",
  ...,
  cumulative = c("raw", "proportion"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  cumulative <- match.arg(cumulative)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      cumulative = cumulative,
      na.rm = na.rm,
      ...
    )
  )
}



#' Pareto y-scale with cumulative percentage axis
#'
#' `scale_y_pareto()` configures a primary y-axis for absolute values and a
#' secondary right-side axis that maps cumulative totals to percentages.
#'
#' Use this with `geom_pareto_line(cumulative = "raw")` so the cumulative line
#' ends at 100% on the secondary axis.
#'
#' @param sum_abs Total absolute value used for percentage conversion, typically
#'   `sum(abs(y))` from the source data. When `NULL` (default), the secondary
#'   axis is scaled to the current primary-axis maximum.
#' @param name Primary y-axis label.
#' @param percent_name Secondary y-axis label.
#' @param ... Additional arguments passed to [ggplot2::scale_y_continuous()].
#'
#' @export
scale_y_pareto <- function(
  sum_abs = NULL,
  name = ggplot2::waiver(),
  percent_name = "Cumulative (%)",
  ...
) {
  if (!is.null(sum_abs) && (!is.numeric(sum_abs) || length(sum_abs) != 1L || !is.finite(sum_abs) || sum_abs <= 0)) {
    stop("`sum_abs` must be a single positive finite number.", call. = FALSE)
  }

  sec_transform <- if (is.null(sum_abs)) {
    function(x) {
      xmax <- max(x, na.rm = TRUE)
      if (!is.finite(xmax) || xmax <= 0) {
        rep(0, length(x))
      } else {
        x / xmax * 100
      }
    }
  } else {
    function(x) x / sum_abs * 100
  }

  ggplot2::scale_y_continuous(
    name = name,
    sec.axis = ggplot2::sec_axis(sec_transform, name = percent_name),
    ...
  )
}

#' @rdname geom_pareto_line
#' @param geom The geometric object to use display the data.
#' @export
stat_pareto_line <- function(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  cumulative = c("raw", "proportion"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  cumulative <- match.arg(cumulative)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatParetoLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      cumulative = cumulative,
      na.rm = na.rm,
      ...
    )
  )
}

StatParetoLine <- ggplot2::ggproto(
  "StatParetoLine",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_panel = function(data, scales, cumulative = c("raw", "proportion")) {
    cumulative <- match.arg(cumulative)

    data <- data[is.finite(data$y), , drop = FALSE]
    if (nrow(data) == 0) {
      return(data.frame())
    }

    ord <- order(abs(data$y), decreasing = TRUE)
    sorted <- data[ord, , drop = FALSE]

    abs_values <- abs(sorted$y)
    cum_values <- cumsum(abs_values)
    if (cumulative == "proportion") {
      total <- sum(abs_values)
      cum_values <- if (total > 0) cum_values / total else rep(0, length(cum_values))
    }

    x_labels <- as.character(sorted$x)
    x_pos <- seq_len(nrow(sorted))

    data.frame(
      x = x_pos,
      y = cum_values,
      group = 1L,
      x_label = x_labels,
      cumulative_prop = if (sum(abs_values) > 0) cumsum(abs_values) / sum(abs_values) else rep(0, length(abs_values)),
      cumulative_raw = cumsum(abs_values)
    )
  }
)

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

is_formula_symbol <- function(expr) {
  expr <- formula_rhs_expr(expr)
  is.symbol(expr)
}

formula_symbol_name <- function(expr) {
  as.character(formula_rhs_expr(expr))
}

formula_rhs_expr <- function(expr) {
  if (inherits(expr, "quosure")) {
    expr <- rlang::quo_get_expr(expr)
  }
  if (inherits(expr, "formula")) {
    expr <- expr[[2]]
  }
  expr
}

normalize_qq_groups <- function(data) {
  if (!("group" %in% names(data))) {
    data$group <- 1L
  }

  unique_groups <- unique(data$group)
  if (length(unique_groups) == nrow(data) && "labels" %in% names(data)) {
    data$group <- 1L
  }

  split(data, data$group)
}

resolve_qqhn_line.p <- function(line.p) {
  if (!is.numeric(line.p) || length(line.p) < 1L || length(line.p) > 2L || any(!is.finite(line.p))) {
    stop("`line.p` must be a single number in (0, 0.5) or c(0, p) with p in (0, 1).", call. = FALSE)
  }

  if (length(line.p) == 1L) {
    p <- line.p[1]
    if (p <= 0 || p >= 0.5) {
      stop("`line.p` must be in (0, 0.5) when a single value is supplied.", call. = FALSE)
    }
    return(list(probs = c(p, 1 - p), through_origin = FALSE))
  }

  if (line.p[1] != 0) {
    stop("When `line.p` has length 2, the first value must be 0.", call. = FALSE)
  }

  p <- line.p[2]
  if (p <= 0 || p >= 1) {
    stop("For `line.p = c(0, p)`, `p` must be in (0, 1).", call. = FALSE)
  }

  list(probs = c(0, p), through_origin = TRUE)
}

new_geom_qqhn_bundle <- function(layers, sample_expr) {
  structure(
    list(
      layers = layers,
      sample_expr = sample_expr
    ),
    class = "qqhn_bundle"
  )
}

#' @method ggplot_add qqhn_bundle
#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.qqhn_bundle <- function(object, plot, object_name, ...) {
  sample_var <- "sample"
  sample_expr <- object$sample_expr

  if (is.null(sample_expr) && !is.null(plot$mapping$sample)) {
    sample_expr <- plot$mapping$sample
  }

  if (!is.null(sample_expr) && is_formula_symbol(sample_expr)) {
    sample_var <- formula_symbol_name(sample_expr)
  }

  plot <- plot + object$layers

  if (is.null(plot$labels$x)) {
    plot <- plot + ggplot2::labs(x = "Theoretical quantiles")
  }
  if (is.null(plot$labels$y)) {
    plot <- plot + ggplot2::labs(y = paste0("Sample quantiles for ", sample_var))
  }

  plot
}

#' Tukey's HSD intervals
#'
#' `stat_tukey()` fits a one-way `aov(response ~ factor)` per group and
#' computes Tukey's HSD pairwise differences with [stats::TukeyHSD()].
#' `geom_tukey()` draws them as horizontal error bars with points, plus a red
#' reference line at zero, and labels the y axis with the comparisons. Groups
#' (e.g. a `colour` mapping) are dodged, and facets work as usual.
#'
#' The input variables are given through the non-positional `response` and
#' `factor` aesthetics, because ggplot2 maps a discrete `x` to integers before
#' the stat runs. For the same reason the comparisons are placed on a
#' continuous y axis: `y` is the comparison index, `x` = `diff`,
#' `xmin`/`xmax` = `lwr`/`upr`. The stat also returns `comparison` (the label)
#' and `p.adj`. `geom_tukey()` adds the matching y axis labels; with
#' `stat_tukey()` alone the y axis shows the indices.
#'
#' @inheritParams ggplot2::stat_identity
#' @param geom The geometric object to use display the data.
#' @param conf.level Family-wise confidence level, passed to [stats::TukeyHSD()].
#' @param dodge Dodging width between groups.
#' @returns A ggplot2 layer (`stat_tukey()`) or a set of layers (`geom_tukey()`).
#' @seealso [ggTukey()]
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(battery, aes(response = Response, factor = Material, colour = Temperature)) +
#'   geom_tukey(conf.level = 0.99) +
#'   labs(x = "Difference", y = "Material")
geom_tukey <- function(
  mapping = NULL,
  data = NULL,
  ...,
  conf.level = 0.95,
  dodge = 0.5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  pos <- ggplot2::position_dodge(width = dodge, orientation = "y")
  structure(
    list(
      layers = list(
        ggplot2::geom_vline(xintercept = 0, colour = "red"),
        stat_tukey(mapping, data, geom = "errorbar", position = pos,
                   orientation = "y", width = 0.2, ..., conf.level = conf.level,
                   na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes),
        stat_tukey(mapping, data, geom = "point", position = pos,
                   ..., conf.level = conf.level,
                   na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
      ),
      mapping = mapping,
      data = data,
      inherit.aes = inherit.aes
    ),
    class = "adas_tukey"
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.adas_tukey <- function(object, plot, ...) {
  fct <- object$mapping$factor
  if (is.null(fct) && object$inherit.aes) fct <- plot$mapping$factor
  data <- if (is.data.frame(object$data)) object$data else plot$data
  plot <- plot + object$layers
  if (is.null(fct) || !is.data.frame(data)) return(plot)
  labs <- tukey_comparisons(levels(factor(rlang::eval_tidy(fct, data))))
  plot + ggplot2::scale_y_continuous(
    breaks = seq_along(labs), labels = labs, minor_breaks = NULL
  )
}

# All pairwise labels in TukeyHSD() order ("B-A", "C-A", "C-B", ...)
tukey_comparisons <- function(lv) {
  m <- outer(lv, lv, paste, sep = "-")
  m[lower.tri(m)]
}

#' @rdname geom_tukey
#' @export
stat_tukey <- function(
  mapping = NULL,
  data = NULL,
  geom = "pointrange",
  position = "identity",
  ...,
  conf.level = 0.95,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTukey,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(conf.level = conf.level, na.rm = na.rm, ...)
  )
}

StatTukey <- ggplot2::ggproto(
  "StatTukey",
  ggplot2::Stat,
  required_aes = c("response", "factor"),
  dropped_aes = c("response", "factor"),
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(diff),
    y = ggplot2::after_stat(index),
    xmin = ggplot2::after_stat(lwr),
    xmax = ggplot2::after_stat(upr)
  ),
  # ggplot groups by every discrete aesthetic, `factor` included; regroup
  # without it so each group holds all levels to compare. Levels are fixed on
  # the whole layer so comparison indices agree across groups and panels.
  # shortcut: an explicit `group` mapping is overridden, honour it if ever needed
  setup_data = function(data, params) {
    data$factor <- factor(data$factor)
    disc <- vapply(data, \(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))
    disc <- setdiff(names(data)[disc], c("factor", "PANEL", "label"))
    data$group <- if (length(disc) == 0) 1L else
      as.integer(interaction(data[disc], drop = TRUE, lex.order = TRUE))
    data
  },
  compute_group = function(data, scales, conf.level = 0.95) {
    fct <- droplevels(data$factor)
    tk <- stats::TukeyHSD(stats::aov(data$response ~ fct), conf.level = conf.level)[[1]]
    data.frame(
      comparison = rownames(tk),
      index = match(rownames(tk), tukey_comparisons(levels(data$factor))),
      diff = tk[, "diff"],
      lwr = tk[, "lwr"],
      upr = tk[, "upr"],
      p.adj = tk[, "p adj"],
      row.names = NULL
    )
  }
)
