#
# Copyright (C) 2024 Utrecht University
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

###------------------------------------------------------------------------------------------------------------------###

#' @export
pooledLmObject <- function(fits, 
                           fType = 1, 
                           include = list(model = FALSE, qr = FALSE, x = FALSE)
                           ) 
{
  .checkInputs(fits, fType)

  ## Much of the lm object doesn't change across imputed datasets, so we can keep one of the original fits and replace
  ## the appropriate parts with MI-based estimates.
  obj        <- fits[[1]]
  class(obj) <- c("pooledlm", class(obj))

  ## We need to extract these things before casting the 'fits' as a mira object
  residuals <- sapply(fits, resid)
  yHats     <- sapply(fits, fitted)
  effects   <- sapply(fits, effects)

  ## The pooled estimates of linear effects are just the arithmetic mean across imputations:
  obj$residuals     <- rowMeans(residuals)
  obj$fitted.values <- rowMeans(yHats)
  obj$effects       <- rowMeans(effects)

  obj$singularityCheckValue <- sapply(fits, function(x) x$qr$qr |> ncol()) |> min()

  ## Do we keep the stuff we can't pool?
  if(include$model) obj$model <- lapply(fits, "[[", x = "model") else obj$model <- NA
  if(include$qr)    obj$qr    <- lapply(fits, "[[", x = "qr")    else obj$qr    <- NA
  if(include$x)     obj$x     <- lapply(fits, "[[", x = "x")     else obj$x     <- NA

  ## Use the mice pooling routines for the more complicated cases:
  fits <- mice::as.mira(fits)

  pooled      <- list()
  pooled$coef <- mice::pool(fits)
  pooled$r2   <- mice::pool.r.squared(fits)
  pooled$r2A  <- mice::pool.r.squared(fits, adjusted = TRUE)

  fFun <- switch(fType, mice::D1, mice::D2, mice::D3)
  pooled$f <- fFun(fits)

  ## Replace pool-able stats with their pooled versions
  obj$coefficients        <- pooled$coef$pooled$estimate
  names(obj$coefficients) <- as.character(pooled$coef$pooled$term)
  obj$df.residual         <- as.numeric(pooled$f$result[ , "df2"])

  ## Calculate the pooled residual variance:
  resVars   <- pooled$coef$glanced$sigma^2
  u         <- mean(resVars)
  b         <- var(resVars)
  pooled$s2 <- u + b + (b / length(fits))
  
  ## Include the extra pooled stats we'll need to downstream
  obj$pooled <- pooled

  obj
}

###------------------------------------------------------------------------------------------------------------------###

.checkInputs <- function(fits, fType) {
  if (!is.list(fits)) stop("The 'fits' argument must be a list of fitted lm models.")

  fitsAreLm <- sapply(fits, inherits, what = "lm")
  if (!all(fitsAreLm)) stop("All entries in the 'fits' list must have class 'lm'.")

  if (!fType %in% 1:3) stop("The 'fType' argument must be 1, 2, or 3.")
}

###------------------------------------------------------------------------------------------------------------------###

#' @export
summary.pooledlm <- function(object) {
  out <- structure(list(), class = c("summary.lm")) # Maybe we can just use the print method for summary.lm ? 

  ## We can pull a few things directly from the input object
  out$call      <- object$call
  out$residuals <- object$residuals
  out$terms     <- object$terms
  out$weights   <- object$weights

  ## Tidy up the fit related stuff
  out$r.squared     <- object$pooled$r2[1, "est"]
  out$adj.r.squared <- object$pooled$r2A[1, "est"]
  out$sigma         <- object$pooled$s2 |> sqrt()

  f <- object$pooled$f$result[1, 1:3]
  names(f) <- c("value", "numdf", "dendf")
  out$fstatistic <- f

  ## The coefficients table needs to formatting
  coefTab <- with(poolCoefSum, 
    cbind("Estimate" = estimate, "Std. Error" = std.error, "t value" = statistic, "Pr(>|t|)" = p.value)
  )
  rownames(coefTab) <- poolCoefSum$term
  out$coefficients  <- coefTab

  ## Some values used for printing
  out$aliased <- is.na(object$coefficients)
  out$df      <- c(object$rank, f[["dendf"]], object$singularityCheckValue)

  invisible(out)
}

###------------------------------------------------------------------------------------------------------------------###

#' @export
coef.pooledlm   <- function(object) object$coefficients
#' @export
resid.pooledlm  <- function(object) object$residuals
#' @export
fitted.pooledlm <- function(object) object$fitted.values

###------------------------------------------------------------------------------------------------------------------###

# test <- lmLikePooled(fits = miFits$fits[[2]])
# test

# ls(test)
# ls(testSum)

# test$rank

# testSum$df
# testSum <- summary(test)

# class(lmSum)
# class(testSum)

# testSum

# coef(test)
# resid(test)

# print(sum)

# print.summary.lm(sum)

# class(test)

# summary(test)
