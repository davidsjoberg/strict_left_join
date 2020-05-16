library(dplyr, warn.conflicts = F)
library(stringr)

### strict_left_join ###
strict_left_join <- function(x, y, by = NULL, suffix = c("", ".y"), strictness = c("error", "block", "none")) {
  if(!any(c(is.data.frame(x), is.data.frame(y)))) {stop("'x' and 'y' needs to be data frames")}
  strictness <- match.arg(strictness)
  y <- y %>% 
    dplyr::group_by_at(dplyr::vars(tidyselect::all_of(by))) %>% 
    dplyr::mutate(n__ = n()) %>% 
    dplyr::ungroup()
  
  if(any(y %>% dplyr::pull(n__) > 1) & strictness != "none") {
    if(strictness == "error") {
      stop("Duplicates in the RHS .y.")
    }
    warning(paste0("Duplicates in .y blocked from join. Duplicates in rows c(", 
                   y %>% 
                     dplyr::mutate(r__ = dplyr::row_number()) %>% 
                     dplyr::filter(n__ > 1) %>% 
                     dplyr::pull(r__) %>% 
                     paste(collapse = ", "), 
                   ")"))
    y <- y %>% 
      dplyr::filter(n__ == 1)
  }
  
  y <- y %>% 
    dplyr::select(-n__)
  dplyr::left_join(x, y, by = by, suffix = suffix)
}

### Sample data ###
x <- tibble(id = 1:3,
            name = starwars$name[1:3]); x

y <- tibble(id = c(1, 2, 2, 3),
                favorit_fruit = fruit[1:4]); y

### Function behaviour ###
strict_left_join(x, y, by = "id", strictness = "error")
strict_left_join(x, y, by = "id", strictness = "block")
strict_left_join(x, y, by = "id", strictness = "none")
