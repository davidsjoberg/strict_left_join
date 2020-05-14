strict_left_join <- function(x, y, by = NULL, suffix = c("", ".y")) {
  y <- y %>% 
    dplyr::group_by_at(dplyr::vars(tidyselect::all_of(by))) %>% 
    dplyr::mutate(n__ = n()) %>% 
    dplyr::ungroup()
  
  if(any(y %>% dplyr::pull(n__) > 1)) {
    warning(paste0("Duplicates in .y blocked from join. Duplicates in rows c(", 
                   y %>% 
                     dplyr::mutate(r__ = dplyr::row_number()) %>% 
                     dplyr::filter(n__ > 1) %>% 
                     dplyr::pull(r__) %>% 
                     paste(collapse = ", "), 
                   ")"))
  }
  
  y <- y %>% 
    dplyr::filter(dplyr::n() == 1) %>% 
    dplyr::select(-n__)
  
  dplyr::left_join(x, y, by = by, suffix = suffix)
}