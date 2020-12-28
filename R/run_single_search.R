#' Run a single iteration of the search algorithm, in MATLAB
#'
#' @param map_path
#' @param contrast
#' @param n_trials
#' @param priorType
#' @param efficiency
#' @param radius
#' @param search_params
#'
#' @return
#' @export
#'
#' @examples
run_single_search <- function(map_path, efficiency, contrast, n_trials, radius, priorType, search_params, seed_val = 1, single_thread = FALSE) {
  rand_name <- stringi::stri_rand_strings(1, 20)

  if(radius == 8) {
    bgScale <- 1
  } else {
    bgScale <- 8 / radius # the search model takes the reciprocal for scaling. values larger than 1 result in a scaled down search region.
  }

  # Build the line of code to send matlab
  if(!is.null(map_path)) {
    code = c("cd('~/Dropbox/Calen/Work/search/modeling/visual_search/');",
             paste0("load('", map_path, "')"),
             "scaleVal = myMap;",
             paste0('seed_val = ', seed_val),
             paste0('n_trials = ', n_trials, ';'))
  } else {
    code = c("cd('~/Dropbox/Calen/Work/search/modeling/visual_search/');",
             "scaleVal = 1;",
             paste0('seed_val = ', seed_val),
             paste0('n_trials = ', n_trials, ';'))
  }
  code <- c(code,
              paste0('results = covert_search_dp(n_trials,',
                     '\'optimal_contrast_', rand_name,
                     '\'', ',[],', priorType, ',', bgScale, ',',
                     contrast * (8/.175) , ',', search_params, ', scaleVal, seed_val);'))

  code_final <- code

  print(code_final) # print the code

  # connect to matlab and run the code.
  res = matlabr::run_matlab_code(code_final, single_thread = single_thread, verbose = TRUE)

  # import the code from a text file, output from matlab
  search_results <- read.table(paste0('/tmp/optimal_contrast_', rand_name, '.txt'), header = T, sep = ',')

  search_results$file_id  <- map_path
  search_results$contrast <- contrast
  search_results$n_trials <- n_trials
  search_results$radius   <- radius

  return(search_results)
}
