#' @keywords internal
compute_prediction <- function(model, new_dat, ci) {
  switch(class(model)[1],
    "lmerMod"  = mem_predict_lmer(model, new_dat),
    "glmerMod" = mem_predict_lmer(model, new_dat),
    mem_predict_glm_lm(model, new_dat, ci)

  )
}

#' @keywords internal
mem_predict_glm_lm <- function(model, new_dat, ci) {
  pred <- tryCatch(predict(model, newdata = new_dat, type = "response",
                           se.fit = TRUE),
                   error = function(e) e)

  if (inherits(pred, "error"))
    stop("Predict method did not work. ",
         "Perhaps mem_plot() is not yet implemented for this type of model.")

  return(list(
    yhat = pred$fit,
    lb   = pred$fit - qnorm(1 - (1 - ci) / 2) * pred$se.fit,
    ub   = pred$fit + qnorm(1 - (1 - ci) / 2) * pred$se.fit
  ))
}

#' @keywords internal
mem_predict_lmer <- function(model, new_dat) {
  pred <- tryCatch(predict(model, newdata = new_dat, type = "response"),
                   error = function(e) e)

  if (inherits(pred, "error"))
    stop("Predict method did not work. ",
         "Perhaps mem_plot() is not yet implemented for this type of model.")

  return(list(
    yhat = pred,
    lb   = pred,
    ub   = pred
  ))
}
