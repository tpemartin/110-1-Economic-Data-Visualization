wdi_merge <- function(...){
  list_dfs <- list(...)
  purrr::reduce(list_dfs,
    function(.x, .y){
      dplyr::inner_join(
        .x, .y,
        by=c("iso2c","country","year")
      )-> .xy
      return(.xy)
    }) |> dplyr::relocate(year)
}
