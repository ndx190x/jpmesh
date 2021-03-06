#' @title Check include mesh areas
#' @description It roughly judges whether the given coordinates are within
#' the mesh area.
#' @inheritParams coords_to_mesh
#' @param ... other parameters
#' @examples
#' eval_jp_boundary(139.71471056, 35.70128943)
#' @aliases eval_jp_boundary
#' @export
eval_jp_boundary <- function(longitude = NULL, latitude = NULL, ...) {
  ifelse(
    ifelse(latitude >= 20.0 & latitude <= 46.0, TRUE, FALSE) &
      ifelse(longitude >= 120.0 & longitude <= 154.0, TRUE, FALSE),
    TRUE,
    FALSE
  )
}

mesh_to_poly <- function(lng_center, lat_center, lng_error, lat_error, ...) {
  sf::st_polygon(list(rbind(c(lng_center - lng_error,
                              lat_center - lat_error),
                            c(lng_center + lng_error,
                              lat_center - lat_error),
                            c(lng_center +  lng_error,
                              lat_center + lat_error),
                            c(lng_center - lng_error,
                              lat_center + lat_error),
                            c(lng_center - lng_error,
                              lat_center - lat_error)))) %>%
    sf::st_sfc(crs = 4326) %>%
    sf::st_as_text()
}

#' @title Identifer to mesh size
#' @description Returns a unit object of mesh size for the given number.
#' @inheritParams mesh_to_coords
#' @export
mesh_size <- function(meshcode) {
  mesh_length <- as.character(nchar(meshcode))
  res <- switch(mesh_length,
          "4" = mesh_units[1],
          "6" = mesh_units[2],
          "7" = mesh_units[3],
          "8" = mesh_units[4],
          "9" = mesh_units[5],
          "10" = mesh_units[6],
          "11" = mesh_units[7])
  if (rlang::is_null(res))
    res <- units::as_units(NA_integer_, "km")
  return(res)
}

mesh_units <- units::as_units(c(80.000, 10.000, 5.000,
                                1.000, 0.500, 0.250, 0.125), "km") # nolint

df_mesh_size_unit <-
  tibble::tibble(
    mesh_length = c(4L, 6L, 7L, 8L, 9L, 10L, 11L),
    mesh_size = mesh_units)

meshcode_set_80km <- as.character(c(3036,
  3622, 3623, 3624, 3631, 3641, 3653,
  3724, 3725, 3741,
  3823, 3824, 3831, 3841,
  3926, 3927, 3928, 3942,
  4027, 4028, 4040, 4042,
  4128, 4129, 4142,
  4229, 4230,
  4328, 4329,
  4429, 4440,
  4529, 4530, 4531, 4540,
  4629, 4630, 4631,
  4728, 4729, 4730, 4731, 4739, 4740,
  4828, 4829, 4830, 4831, 4839,
  4928, 4929, 4930, 4931, 4932, 4933, 4934, 4939,
  5029, 5030, 5031, 5032, 5033, 5034, 5035, 5036, 5038, 5039,
  5129, 5130, 5131, 5132, 5133, 5134, 5135, 5136, 5137, 5138, 5139,
  5229, 5231, 5232, 5233, 5234, 5235, 5236, 5237, 5238, 5239, 5240,
  5332, 5333, 5334, 5335, 5336, 5337, 5338, 5339, 5340,
  5432, 5433, 5435, 5436, 5437, 5438, 5439, 5440,
  5531, 5536, 5537, 5538, 5539, 5540, 5541,
  5636, 5637, 5638, 5639, 5640, 5641,
  5738, 5739, 5740, 5741,
  5839, 5840, 5841,
  5939, 5940, 5941, 5942,
  6039, 6040, 6041,
  6139, 6140, 6141,
  6239, 6240, 6241, 6243,
  6339, 6340, 6341, 6342, 6343,
  6439, 6440, 6441, 6442, 6443, 6444, 6445,
  6540, 6541, 6542, 6543, 6544, 6545, 6546,
  6641, 6642, 6643, 6644, 6645, 6646, 6647,
  6740, 6741, 6742, 6747, 6748,
  6840, 6841, 6842, 6847, 6848))

meshcode_set_10km <- meshcode_set_80km %>%
  purrr::map(fine_separate) %>%
  purrr::flatten_chr()

meshcode_set_5km <-
  meshcode_set_10km %>%
  purrr::map(~ paste0(.x, seq.int(1, 4))) %>%
  purrr::flatten_chr()

meshcode_set_1km <- meshcode_set_10km %>%
  purrr::map(fine_separate) %>%
  purrr::flatten_chr()

#' @title Export meshcode vectors ranges 80km to 1km.
#' @description Unique 176 meshcodes.
#' The output code may contain values not found in the actual mesh code.
#' @param mesh_size Export mesh size from 80km to 1km.
#' @examples
#' meshcode_set(mesh_size = 80)
#' @export
meshcode_set <- function(mesh_size = c(80, 10, 5, 1)) {
  get(sprintf("meshcode_set_%skm", mesh_size), envir = asNamespace("jpmesh")) # nolint
}

#' @title Cutoff mesh of outside the area
#' @inheritParams mesh_to_coords
cut_off <- function(meshcode) {
  mesh_80km <- substr(meshcode, 1, 4)
  res <- meshcode[mesh_80km %in% c(meshcode_set(80))] # nolint
  if (length(res) < length(meshcode)) {
    rlang::warn("Some neighborhood meshes are outside the area.")
  }
  res <- as.character(sort(res))
  return(res)
}

validate_neighbor_mesh <- function(meshcode) {
  df_bbox <-
    find_neighbor_mesh(meshcode) %>%
    export_meshes()
  df_bbox <-
    df_bbox %>%
    sf::st_sf() %>%
    sf::st_union() %>%
    sf::st_bbox()
  tibble::tibble(
    xlim = as.numeric(df_bbox[3] - df_bbox[1]),
    ylim = as.numeric(df_bbox[4] - df_bbox[2]))
}

bind_meshpolys <- function(meshcode) {
  meshcode %>%
    purrr::map(fine_separate) %>%
    purrr::reduce(c) %>%
    unique() %>%
    export_meshes()
}

code_reform <- function(jis_code) {
  . <- NULL
  checked <-
    jis_code %>%
    purrr::map(nchar) %>%
    purrr::keep(~ .x %in% c(1, 2, 5)) %>%
    length()
  if (length(jis_code) != checked)
    rlang::abort("Input jis-code must to 2 or 5 digits.")
  jis_code %>%
    purrr::map(as.numeric) %>%
    purrr::map_if(.p = nchar(.) %in% c(1, 2), ~ sprintf("%02d", .x)) %>%
    purrr::map_if(.p = nchar(.) %in% c(4, 5), ~ sprintf("%05d", .x)) %>%
    purrr::flatten_chr()
}
