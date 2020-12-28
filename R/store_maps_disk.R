#' Function to store fitted maps on disk.
#'
#' @param maps_2d_lst
#' @param file_id
#'
#' @return
#' @export
#'
#' @examples
store_maps_disk <- function(file_id, maps_2d_lst){
  export_maps <- purrr::map2(file_id, maps_2d_lst, function(x, y) {
    file_id <- x
    R.matlab::writeMat(file_id, myMap = y)
  })
}
