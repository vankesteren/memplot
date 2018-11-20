# helper functions for the main wrapper

#' @keywords internal
check_predict_method <- function(model) {
  pred_try <- tryCatch(predict(model, type = "response"), error = function(e) e)
  if (inherits(pred_try, "error")) stop(pred_try$message, call. = FALSE)
}

#' @keywords internal
find_outcome_name <- function(model) {
  as.character(formula(model))[2]
}

#' @keywords internal
average <- function(col) {
  switch(class(col),
    "factor"    = factor(levels(col)[1], levels = levels(col)),
    "character" = unique(col)[1],
    "numeric"   = mean(col, na.rm = TRUE),
    mean(col, na.rm = TRUE)
  )
}

#' @keywords internal
typical <- function(col) {
  switch(class(col),
    "factor"    = factor(names(table(col))[which.max(table(col))],
                         levels = levels(col)),
    "character" = names(table(col))[which.max(table(col))],
    "numeric"   = median(col, na.rm = TRUE),
    median(col, na.rm = TRUE)
  )
}

#' @keywords internal
extract_data <- function(model) {
  dat <- switch(class(model)[1],
    "lmerMod"  = model@frame,
    "glmerMod" = model@frame,
    "lm"       = model$model,
    model$data
  )
  if (is.environment(dat)) dat <- model.frame(formula(model), data = dat)
  dat
}

#' @keywords internal
theme_memplot <- function() {
  # see also github.com/vankesteren/firatheme
  list(ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = 11.5),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(rep(0.5, 4), "cm"),

      # remove the plot background and border
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent",colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),

      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                               colour = "#454545",
                                               size = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#454545", size = 0.3),

      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(size = 18, colour = "#454545",
                                         hjust = 0.5,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 12, colour = "#454545",
                                            hjust = 0.5,
                                            margin = ggplot2::margin(b = 10)),

      # add padding to the caption
      plot.caption = ggplot2::element_text(size = 10, colour = "#454545",
                                           hjust = 1,
                                           margin = ggplot2::margin(t = 15)),

      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = 13, colour = "#454545",
                                         hjust = 0.95),
      axis.text = ggplot2::element_text(size = 10, colour = "#212121"),
      legend.title = ggplot2::element_text(size = 12, colour = "#454545"),
      legend.text = ggplot2::element_text(size = 10, colour = "#212121"),
      strip.text = ggplot2::element_text(size = 12, colour = "#212121")
    )
  ))
}

