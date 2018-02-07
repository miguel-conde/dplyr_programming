my_summarise <- function(df, ...) {
  
  # Use quos() to capture all the ... as a list of formulas.
  group_var <- quos(...) 
  
  df %>%
    # Use !!! instead of !! to splice the arguments into group_by().
    group_by(!!!group_var) %>%
    summarise(a = mean(a))
}