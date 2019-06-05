#' @param to_mesh_size target mesh type. From 80km to 0.125km. If `NULL`,
#' the meshcode of one small scale will be returned.
mesh_convert <-
  function(meshcode = NULL, to_mesh_size = NULL, scale_down = TRUE, ...) { # nolint
  if (is.mesh(meshcode))
    if (rlang::is_null(to_mesh_size))
      if (rlang::is_true(scale_down))
        to_mesh_size <-
          units::drop_units(df_mesh_size_unit$mesh_size[which(mesh_units == mesh_size(meshcode)) + 1]) # nolint
      else
        to_mesh_size <-
          units::drop_units(df_mesh_size_unit$mesh_size[which(mesh_units == mesh_size(meshcode)) - 1]) # nolint
      to_mesh_size <- units::as_units(to_mesh_size, "km")
      if (mesh_size(meshcode) > to_mesh_size) {
        if (to_mesh_size == units::as_units(10, "km"))
          res <- grep(
            pattern = paste0("^(", meshcode, ")"),
            x = meshcode_set_10km,
            value = TRUE)
        if (to_mesh_size == units::as_units(1, "km"))
          res <- grep(
            pattern = paste0("^(", meshcode, ")"),
            x = meshcode_set_1km,
            value = TRUE)
        if (to_mesh_size <= units::as_units(0.500, "km"))
          res <-
            grep(pattern = paste0("^(", substr(meshcode, 1, 8), ")"),
                 x = meshcode_set_1km,
                 value = TRUE) %>%
            purrr::map(~ paste0(.x, seq_len(4))) %>%
            purrr::reduce(c)
        if (to_mesh_size <= units::as_units(0.250, "km"))
          res <- res %>%
            purrr::map(~ paste0(.x, seq_len(4))) %>%
            purrr::reduce(c)
        if (to_mesh_size == units::as_units(0.125, "km"))
          res <- res %>%
            purrr::map(~ paste0(.x, seq_len(4))) %>%
            purrr::reduce(c)
      } else {
        if (to_mesh_size == units::as_units(80, "km"))
          res <- substr(meshcode, 1, 4)
        if (to_mesh_size == units::as_units(10, "km"))
          res <- substr(meshcode, 1, 6)
        if (to_mesh_size == units::as_units(1, "km"))
          res <- substr(meshcode, 1, 8)
        if (to_mesh_size == units::as_units(0.5, "km"))
          res <- substr(meshcode, 1, 9)
        if (to_mesh_size == units::as_units(0.25, "km"))
          res <- substr(meshcode, 1, 10)
        if (to_mesh_size == units::as_units(0.125, "km"))
          res <- substr(meshcode, 1, 11)
      }
      return(res)
}
