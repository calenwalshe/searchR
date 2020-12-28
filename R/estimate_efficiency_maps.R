#' Main function to estimate the efficiency maps used for search
#'
#' @param map_parameters
#' @param optim_params
#'
#' @return
#' @export
#' @import dplyr tidyr
#' @examples
estimate_efficiency_maps <- function(map_parameters, optim_params = list(n_cores = 1, maxit = 3000, tolerance = 1e-12, NP = NULL)) {
    #max_res <- 492668 # define a maximum resource to total resources of a 9 ring map.
    n.rows       <- nrow(map_parameters)
    model_fits   <- pbmcapply::pbmclapply(1:n.rows, FUN = function(x) {
      efficiency <- map_parameters[[x, "efficiency"]]

      sp_const <- map_parameters[[x, "sp_const"]]

      params_detection <- as.numeric(stringr::str_extract_all(map_parameters[[x, "params"]], "\\..[:number:]+|[:number:]")[[1]])
      n_rings <- map_parameters[[x, "n_rings"]]

      params_retina <- searchR::create_retinal_constants()
      ring_cent        <- params_retina$ring_centers
      patch_rad        <- params_retina$patch_radii
      n_pixels         <- params_retina$pix_per_patch
      g_cell_per_patch <- params_retina$g_cell_patch
      n_patches        <- params_retina$n_patches
      spacing          <- searchR::watson_spacing(ring_cent, 0, binoc = 1, sp_scal = 1, f0 = 0)
      n_entries        <- length(ring_cent)

      prior         <- n_pixels / sum(n_patches * n_pixels)

      dprime <- humandetectiondata::f.dprime.no.uni(8, ring_cent * 120, 0, 1, p_kSp = params_detection[2], p_k0 = params_detection[1]) # compute d' on the rings for the no uncertainty case.
      compute_error <- function(dprime, patch_gain, prior, criterion) {
        u_i <- -patch_gain ^ 2 * (dprime) ^ 2 / 2 + log(prior)
        u_i_plus <- patch_gain ^ 2 * (dprime) ^ 2 / 2 + log(prior)

        u_len <- length(u_i)

        f <- function(z) {
          all_patches <- map_dbl(z, function(z) {
            sum(map_dbl(1:length(u_i), function(x) {
              n_patches[x] * prior[x] * dnorm(z - u_i_plus[x]) *
                (1 - prod(map_dbl(setdiff(1:length(u_i), x), function(y) {
                  pnorm(z - u_i[y])^n_patches[y]
                })) * pnorm(z - u_i[x])^(n_patches[x] - 1))}))
          })

          return(all_patches)
        }

        # False Hit
        f_fh <- function(criterion) {
          integrate(f, criterion, Inf)$value
        }
        fh <- map_dbl(criterion, f_fh)

        # Miss
        f_miss <- function(criterion) {
          ring_sum <- n_patches * map_dbl(1:u_len, function(x) {
            prior[x] * pnorm(criterion - u_i_plus[x]) *
              prod(map_dbl(setdiff(u_len, x),
                           function(y) {
                             pnorm(criterion - u_i[y]) ^ n_patches[y]
                           })) * pnorm(criterion - u_i[x]) ^ (n_patches[x] - 1)
          })

          return(sum(ring_sum))
        }
        miss <- map_dbl(criterion, f_miss)

        # false alarm
        f_fa <- function(criterion) {
          ring_sum <-
            1 - prod(map_dbl(1:u_len, function(x) {
              pnorm(criterion - u_i[x]) ^ n_patches[x]
            }))
        } # make this definition (and the others) vectorized from the start
        fa <- map_dbl(criterion, f_fa)

        return(log((fa + fh + miss)/2))
      }
      f.obj.1 <- function(x) {
        error <- compute_error(dprime, (x[1:n_entries]), prior, x[n_entries+1])
      }

      f.constr <- function(scale_max_res) {
        con <- function(xx) {
          #sum(xx[1:n_entries] * g_cell_per_patch * n_patches) - sum(g_cell_per_patch * n_patches) * max.res
          #G <- sum((xx[1:n_entries]) * g_cell_per_patch * n_patches) - sum(g_cell_per_patch * n_patches) * scale_max_res
          G <- sum((xx[1:n_entries]) * g_cell_per_patch * n_patches) - scale_max_res
          #X <- xx[1:(n_entries -1)] - xx[2:(n_entries)]
          if(sp_const != 0) {
          r <- xx[1:5] - c(.518, .548, .594, .637, .680) * sp_const
          return(c(G,r))
          } else {
            return(c(G))
          }

        }
        return(con)
      }

      # allows a custom number of elements in the initial population of the GA. Defaults to (number of params * 10)
      if(is.null(optim_params$NP)){
        NP <- 10 * n_entries
      } else {
        NP <- optim_params$NP
      }

      if(sp_const == 0) {
        meq <- 1
      } else {
        meq <- 6
      }

      con <- f.constr(efficiency)
      res <- DEoptimR::JDEoptim(
         c(rep(0, n_entries),-10),
         c(rep(1, n_entries), 10),
         fn = f.obj.1,
         meq = meq,
         constr = con,
         trace = T,
         triter = 20,
         tol = optim_params$tolerance,
         maxiter = optim_params$maxit,
         NP = NP,
         details = TRUE
       )

        constr_frame <- tibble(efficiency = efficiency,
                         neural_resource = g_cell_per_patch * n_patches,
                         y.neural_resource = g_cell_per_patch * n_patches * res$par[1:n_entries],
                         x = 1:n_entries,
                         c_deg = ring_cent,
                         num_ring = n_patches,
                         spacing = spacing,
                         y.scale = (res$par[1:n_entries]),
                         y.dprime = (res$par[1:n_entries]) * dprime,
                         criterion = res$par[n_entries+1],
                         prior = prior,
                         pc_flat = 1,
                         estimation_fail = 1,
                         optim_struct = list(res))

      return(constr_frame)
    }, mc.cores = optim_params$n_cores)

    map_parameters$contrast <- as.numeric(unlist(purrr::map(map_parameters$dpVec, function(x) {stringr::str_extract(x, "[[:number:]]+")})))/1000

    map_parameters$data_est <- model_fits

    map_parameters <- map_parameters %>%
      select(-efficiency) %>%
      unnest(data_est)
    #
    return(map_parameters)
}
