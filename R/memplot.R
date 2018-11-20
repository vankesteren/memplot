#' Marginal effects at the mean plots
#'
#' The better way of interpreting coefficients in a model: take a typical
#' sample, vary the variable of interest, and see how that changes the
#' prediction. Works for categorical and continuous predictors.
#'
#' @param model a model object with a data slot and a predict() method
#' @param variable a character of the variable of interest
#' @param plt_range optional vector of length 2 indicating the range to plot
#' over for the x - axis
#' @param collector "typical" or "average" or an abbreviation thereof. See
#' details
#'
#' @details The `typical` collector calculates the median values for numeric,
#' and the most common class for categorical predictors. The `average` collector
#' returns the mean for numeric variables and the reference category for
#' categorical variables.
#'
#' @return `ggplot` object with the MEM plot
#'
#' @importFrom stats median predict
#'
#' @export
#' @examples
#' mod <- glm(vs ~ drat + qsec, family = "binomial", data = mtcars)
#' mem_plot(mod, "qsec")
#' mem_plot(mod, "drat")
mem_plot <- function(model, variable, plt_range, collector = c("typical", "average")) {
  err <- check_compatibility(model)
  if (!is.null(err)) stop(err, call. = TRUE)

  dataset <- as.data.frame(model$data)
  cn <- colnames(dataset)
  if (!variable %in% cn) stop("Variable not found.")

  clctrFun <- switch(match.arg(collector),
    average = average,
    typical = typical
  )

  var_idx <- which(cn == variable)

  var_eff <- dataset[, var_idx]

  if (is.numeric(var_eff) && missing(plt_range)) {
    rng <- range(var_eff)
  } else if (!missing(plt_range)) {
    rng <- plt_range
  }

  var_rng <- switch(class(var_eff),
                    factor    = levels(var_eff),
                    character = unique(var_eff),
                    seq(rng[1], rng[2], length.out = 1e3)
  )

  df_mean <- lapply(dataset[, -var_idx], clctrFun)

  df_mean <- as.data.frame(lapply(df_mean, rep, length(var_rng)))

  new_dat <- cbind(var_rng, df_mean)
  colnames(new_dat) <- c(cn[var_idx], cn[-var_idx])

  pred_y <- predict(model, newdata = new_dat, type = "response", se.fit = TRUE)

  ggdf <- data.frame(
    x    = var_rng,
    y    = pred_y$fit,
    ymin = pred_y$fit - 1.96*pred_y$se.fit,
    ymax = pred_y$fit + 1.96*pred_y$se.fit
  )

  if (is.numeric(var_eff)) {
    p <-
      ggplot2::ggplot(ggdf, ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_ribbon(fill = firatheme::firaCols[1], alpha = 0.1) +
      ggplot2::geom_line(col = firatheme::firaCols[1], size = 1) +
      firatheme::theme_fira() +
      ggplot2::labs(
        title = paste0("Effect of ", cn[var_idx]),
        x = cn[var_idx],
        y = "pred_y"
      )
  } else {
    p <-
      ggplot2::ggplot(ggdf, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = x, y = ymin, yend = ymax),
                                         col = firatheme::firaCols[1], size = 1) +
      ggplot2::geom_point(col = firatheme::firaCols[1], size = 4) +
      firatheme::theme_fira() +
      ggplot2::labs(
        title = paste0("Effect of ", cn[var_idx]),
        x = cn[var_idx],
        y = "pred_y"
      )
  }

  return(p)
}

#' @keywords internal
check_compatibility <- function(model) {
  cls <- class(model)
  if (is.null(model$data)) return("Model object has no data.")
  pred <- tryCatch(get(paste0("predict.", cls)), error = function(e) e$message)
  if (is.character(pred)) return(pred)
  return()
}

#' @keywords internal
average <- function(col) {
  switch(class(col),
    factor = factor(levels(col)[1], levels = levels(col)),
    character = unique(col)[1],
    numeric = mean(col, na.rm = TRUE),
    mean(col, na.rm = TRUE)
  )
}

#' @keywords internal
typical <- function(col) {
  switch(class(col),
    factor    = factor(names(table(col))[which.max(table(col))],
                       levels = levels(col)),
    character = names(table(col))[which.max(table(col))],
    numeric   = median(col, na.rm = TRUE),
    median(col, na.rm = TRUE)
  )
}

