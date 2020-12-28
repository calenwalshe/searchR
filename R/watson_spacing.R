#' Watson spacing function from Watson (2014) JoV.
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
watson_spacing <- function(x,y, binoc = 1, sp_scal = 1, f0 = 0) {
  # Note. See Watson (2014) for interpretation of the constants.
  r_m <- 41.03 # see table A2
  d_c0 <- 14804.6 # cone density in the center of the fovea

  # see Table 1
  re_k <- function(k) {
    switch (k, 22.14, 16.35, 7.633, 12.13) # temporal, superior, nasal, inferior
  }

  a_k <- function(k) {
    switch(k, .9851, .9935, .9729, .996)
  }

  r2_k <- function(k) {
    switch(k, 1.058, 1.035, 1.084, .9932)
  }

  dmf <- function(r, k) {
    2 * d_c0 * ( 1 + r / r_m)^(-1) *
      (a_k(k) * (1 + r / r2_k(k))^(-2) +
         (1 - a_k(k)) * exp(-r / re_k(k)))
  } # eqn 8

  f_spacing_rad <- function(r,k) {
    sqrt(2) * sqrt(2 / ((sqrt(3) * dmf(r, k))))
  } # eqn 9

  f_monoc <- function(x,y) {
    r_xy <- sqrt(x^2 + y^2)
    if(x >= 0 & y >= 0) {
      x <- abs(x);y <- abs(y)
      spacing <- sp_scal * 1 / r_xy * sqrt(x^2 * f_spacing_rad(r_xy, 1)^2 + y^2 * f_spacing_rad(r_xy, 2)^2)
    }else if(x < 0 & y >= 0) {
      x <- abs(x);y <- abs(y)
      spacing <- sp_scal * 1 / r_xy * sqrt(x^2 * f_spacing_rad(r_xy, 3)^2 + y^2 * f_spacing_rad(r_xy, 2)^2)
    }else if(x >= 0 & y < 0) {
      x <- abs(x);y <- abs(y)
      spacing <- sp_scal * 1 / r_xy * sqrt(x^2 * f_spacing_rad(r_xy, 1)^2 + y^2 * f_spacing_rad(r_xy, 4)^2)
    }else if(x < 0 & y < 0) {
      x <- abs(x);y <- abs(y)
      spacing <- sp_scal * 1 / r_xy * sqrt(x^2 * f_spacing_rad(r_xy, 3)^2 + y^2 * f_spacing_rad(r_xy, 4)^2)
    }
    return(spacing)
  }
  spacing <- purrr::map2(x,y, function(x, y) {
    r_xy <- sqrt(x^2 + y^2)
  if (r_xy == 0) {
    spacing <- sp_scal * sqrt(2)*sqrt(1/(sqrt(3)*d_c0))
  } else if(binoc == 0) {
    return(f_monoc(x,y))
  } else if(binoc == 1) {
    if(y > 0) {
      binoc <- sqrt(2) * sqrt(f_monoc(-x, y)^2 * f_monoc(x, y)^2 / (f_monoc(-x, y)^2 + f_monoc(x, y)^2))
      return(binoc)
    }else if(y <= 0) {
      binoc <- sqrt(2) * sqrt(f_monoc(-x, y)^2 * f_monoc(x, y)^2 / (f_monoc(-x, y)^2 + f_monoc(x, y)^2))
      return(binoc)
    }

  } else {
      spacing <- NA
  }
  })

  spacing <- unlist(spacing)

  if (f0 > 0){
    n <- 2 # hard coded exponent. can be changed.
    sp0 <- sqrt(2)*sqrt(1/(sqrt(3)*d_c0))
    dspacing <- spacing-sp0
    spacing <- dspacing*dspacing^n/(dspacing^n+f0^n) + sp0;
  }


  return(unlist(spacing))
}


