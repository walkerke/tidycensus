# Bind a list of sf results without repeatedly copying accumulated rows.
bind_spatial_rows <- function(pieces) {
  dplyr::bind_rows(pieces)
}
