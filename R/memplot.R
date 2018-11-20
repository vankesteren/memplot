#' Marginal effects at the mean plots
#'
#' An intuitive way of interpreting coefficients in a model: take a typical
#' case/sample, vary the variable of interest, and see how that changes the
#' prediction. Works for categorical and continuous predictors.
#'
#' @param model a model object with a data slot and a predict() method
#' @param variable a character of the variable of interest
#' @param ci confidence interval range (default 95%)
#' @param plt_range optional vector of length 2 indicating the range to plot
#'     over for the x-axis. Defaults to the range of the variable in the data.
#' @param collector "typical" or "average" or an abbreviation thereof. See
#'     details.
#'
#' @details The `typical` collector calculates the median values for numeric,
#' and the most common class for categorical predictors. The `average` collector
#' returns the mean for numeric variables and the reference category for
#' categorical variables.
#'
#' @return `ggplot` object with the MEM plot
#'
#' @importFrom stats median predict formula model.frame qnorm
#'
#' @export
#' @examples
#' mod <- glm(vs ~ drat + qsec, family = "binomial", data = mtcars)
#' mem_plot(mod, "qsec")
#' mem_plot(mod, "drat")
mem_plot <- function(model, variable, ci = .95, plt_range,
                     collector = c("typical", "average")) {

  # First, check whether the model object has a predict method
  check_predict_method(model)

  # Then, try to extract the original dataset
  full_df <- extract_data(model)

  # Then, check whether variable of interest exists
  cn <- colnames(full_df)
  if (!variable %in% cn) stop("Variable not found.")

  # Set collector (default "typical")
  clct_fun <- switch(match.arg(collector),
    average = average,
    typical = typical
  )

  # find variable of interest
  var_idx <- which(match.arg(variable, cn) == cn)
  var_eff <- full_df[, var_idx]


  # create a range of interest
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

  # generate conditional dataset
  ty_case <- lapply(full_df[, -var_idx], clct_fun)
  df_tpcl <- as.data.frame(lapply(ty_case, rep, length(var_rng)))
  new_dat <- cbind(var_rng, df_tpcl)
  colnames(new_dat) <- c(cn[var_idx], cn[-var_idx])

  # create prediction
  pred_y <- compute_prediction(model, new_dat, ci)

  ggdf <- data.frame(
    x    = var_rng,
    y    = pred_y$yhat,
    ymin = pred_y$lb,
    ymax = pred_y$ub
  )

  # Find y name label
  y_name <- find_outcome_name(model)

  if (is.numeric(var_eff)) {
    p <-
      ggplot2::ggplot(ggdf, ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_ribbon(fill = "#00008b", alpha = 0.1) +
      ggplot2::geom_line(col = "#00008b", size = 1) +
      theme_memplot() +
      ggplot2::labs(
        title = paste0("Effect of ", cn[var_idx], " for a typical case"),
        x = cn[var_idx],
        y = y_name
      )
  } else {
    p <-
      ggplot2::ggplot(ggdf, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = x, y = ymin, yend = ymax),
                                         col = "#00008b", size = 1) +
      ggplot2::geom_point(col = "#00008b", size = 4) +
      theme_memplot() +
      ggplot2::labs(
        title = paste0("Effect of ", cn[var_idx], " for a typical case"),
        x = cn[var_idx],
        y = y_name
      )
  }

  return(p)
}

#' Find typical case
#'
#' Function to find the typical value for each variable as used in mem_plot
#'
#' @param data dataset to find the typical values in
#' @param collector "average" or "typical" (default)
#'
#' @return a named list of typical values for each variable
#'
#' @export
typical_case <- function(data, collector = c("typical", "average")) {
  clct_fun <- switch(match.arg(collector), average = average, typical = typical)
  lapply(data, clct_fun)
}
