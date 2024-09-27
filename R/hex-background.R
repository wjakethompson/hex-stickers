library(tidyverse)
library(magick)
library(glue)
library(here)
library(fs)

aspect_ratio <- "2:1"
padding_px <- 10
rstudio_only <- TRUE
featured <- c("tidyverse", "tidymodels")

set.seed(76748)

# Read images ------------------------------------------------------------------
rstudio_stickers <- dir_ls(here("PNG"))
new_stickers <- dir_ls(here("other-stickers", "unmerged"))

if (!rstudio_only) {
  other_stickers <- dir_ls(here("other-stickers", "_png"), type = "file")
} else {
  other_stickers <- NULL
}

sticker_files <- c(rstudio_stickers, new_stickers, other_stickers)
sticker_names <- path_file(sticker_files)
stickers <- sticker_files %>%
  map(function(path) {
    image_read(path)
  }) %>%
  map(image_transparent, "white") %>%
  map(image_trim) %>%
  set_names(sticker_names)

featured_stickers <- stickers[paste0(featured, ".png")]
stickers <- stickers[which(!names(stickers) %in% paste0(featured, ".png"))]

# Desired sticker resolution in pixels
sticker_width <- 121

# Scale all stickers to the desired pixel width
stickers <- stickers %>%
  map(image_scale, sticker_width)

# Identify low resolution stickers
stickers %>%
  map_lgl(~ with(image_info(.x),
                 width < (sticker_width - 1) / 2 && format != "svg")
          ) %>%
  .[.]

# Identify incorrect shapes/proportions (tolerance of +/-2 height)
stickers %>%
  map_lgl(~ with(image_info(.x),
                 height < (median(height) - 2) | height > (median(height) + 2))
          ) %>%
  .[.]

# Extract correct sticker height (this could also be calculated directly from width)
sticker_height <- stickers %>%
  map(image_info) %>%
  map_dbl("height") %>%
  median()

# Coerce sticker dimensions
stickers <- stickers %>%
  map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))

canvas <- image_blank(sticker_width + padding_px,
                      sticker_height + padding_px,
                      color = "white")

stickers <- map(stickers,
                function(.x, canvas, padding_px) {
                  image_composite(canvas, .x,
                                  offset = paste0("+", padding_px / 2,
                                                  "+", padding_px / 2)) |>
                    image_transparent(color = "white")
                },
                canvas = canvas, padding_px = padding_px)
sticker_width <- sticker_width + padding_px
sticker_height <- sticker_height + padding_px


# Create sticker wall ----------------------------------------------------------
create_rows <- function(row_lens, row_cuml, row_splt,
                        stickers, width, height, opacity = 0, op_prop = 0) {
  if (!row_splt) {
    row <- stickers[seq(row_cuml - row_lens + 1, by = 1,
                        length.out = row_lens)] %>%
      map(.f = function(.x) {
        if (runif(1, min = 0, max = 1) < op_prop) {
          stick <- image_colorize(.x, opacity, "white")
        } else {
          stick <- .x
        }
        return(stick)
      }) %>%
      invoke(c, .) %>%
      image_append()
  } else {
    row <- stickers[seq(row_cuml - row_lens + 1, by = 1,
                        length.out = row_lens)] %>%
      map(.f = function(.x) {
        if (runif(1, min = 0, max = 1) < op_prop) {
          stick <- image_colorize(.x, opacity, "white")
        } else {
          stick <- .x
        }
        return(stick)
      })
    half_1 <- image_crop(row[[1]],
                         glue("{width / 2}x{height}"))
    half_2 <- image_crop(row[[1]],
                         glue("{width / 2}x{height}+{width / 2}"))
    row <- c(half_2, row[2:length(row)], half_1) %>%
      invoke(c, .) %>%
      image_append()
  }

  return(row)
}
calc_dim <- function(aspect_ratio = "16:9", total_files = length(stickers),
                     allow_resample = TRUE, max_num = length(stickers)) {
  target_width <- str_split(aspect_ratio, ":") %>%
    flatten() %>%
    .[[1]] %>%
    as.integer()
  target_height <- str_split(aspect_ratio, ":") %>%
    flatten() %>%
    .[[2]] %>%
    as.integer()

  multiplier <- 1L
  ideal_dim <- FALSE
  while(!ideal_dim) {
    cur_width <- target_width * multiplier
    cur_height <- target_height * multiplier

    sticker_row_size <- round(cur_width / sticker_width, digits = 0)
    sticker_col_size <- round(((cur_height) / (sticker_height / 1.33526)) + 1)
    total_stickers <- sticker_row_size * sticker_col_size

    if (sticker_col_size %% 2 != 0) {
      multiplier <- multiplier + 1L
    } else if ((total_stickers > total_files) | (total_stickers > max_num)) {
      if (allow_resample) {
        final_mult <- multiplier
      } else {
        final_mult <- multiplier - 1L
      }
      ideal_dim <- TRUE
    } else {
      multiplier <- multiplier + 1L
    }
  }

  ret_list <- list(target_width = target_width * final_mult,
                   target_height = target_height * final_mult)
}
add_featured <- function(img, featured, crop_info, feat_info) {
  total_width <- crop_info$width
  feat_width <- feat_info$width
  extra_width <- total_width - (feat_width * length(featured))
  width_spacing <- extra_width / (length(featured) + 1)

  for (i in seq_along(featured)) {
    img <- image_composite(
      img, featured[[i]],
      offset = glue("+{(width_spacing * i) + (feat_width * (i - 1))}",
                    "+{(crop_info$height - feat_info$height) / 2}")
    )
  }

  return(img)
}

dimensions <- calc_dim(aspect_ratio = aspect_ratio,
                       total_files = length(stickers), allow_resample = TRUE,
                       max_num = ifelse(aspect_ratio == "16:9",
                                        length(stickers), length(stickers)))

target_width <- dimensions$target_width
target_height <- dimensions$target_height

sticker_row_size <- round(target_width / sticker_width, digits = 0)
sticker_col_size <- round(((target_height) /
                             (sticker_height / 1.33526)) + 1,
                          digits = 0)
total_stickers <- sticker_row_size * sticker_col_size

# Randomize stickers
if (total_stickers > length(stickers)) {
  full_sets <- floor(total_stickers / length(stickers))
  extra <- total_stickers %% length(stickers)
  iter <- 0
  good_sample <- FALSE
  while(!good_sample) {
    choose <- sample(c(rep(seq_along(stickers), full_sets),
                       sample(seq_along(stickers), size = extra)),
                     size = total_stickers, replace = FALSE)
    check_stick <- c(choose, choose)

    unique_stick <- sort(unique(choose))
    good_check <- TRUE
    for (i in seq_along(unique_stick)) {
      cur_loc <- which(check_stick == unique_stick[i])
      if (any(diff(cur_loc) %in%
              c(1:5, (sticker_row_size - 5):(sticker_row_size + 5)))) {
        good_check <- FALSE
        break
      }
    }

    if (good_check) {
      good_sample <- TRUE
    } else {
      iter <- iter + 1
    }

    if (iter > 500000) stop("Max iterations reached")
  }

  stickers <- stickers[choose]
} else {
  stickers <- stickers[sample(seq_along(stickers), size = total_stickers,
                              replace = FALSE)]
}


# Calculate row sizes
row_length <- rep(sticker_row_size, sticker_col_size)
row_split <- rep(c(FALSE, TRUE), length.out = sticker_col_size)

color_rows <- pmap(.l = list(row_lens = row_length,
                             row_cuml = cumsum(row_length),
                             row_splt = row_split),
                   .f = create_rows, stickers = stickers,
                   width = sticker_width, height = sticker_height,
                   opacity = 0, op_prop = 0)
color_rows <- c(color_rows, color_rows[[1]])


# Add stickers to canvas
canvas <- image_blank(sticker_row_size * sticker_width,
                      sticker_height + (sticker_col_size - 1) * sticker_height / 1.33526,
                      "white")

color_wall <- reduce2(color_rows, seq_along(color_rows),
                      ~ image_composite(
                        ..1, ..2,
                        offset = paste0("+0",
                                        "+", round((..3 - 1) * sticker_height / 1.33526))
                      ),
                      .init = canvas)


out_type <- switch(aspect_ratio,
                   "2:1" = "website-promo",
                   "16:9" = "zoom",
                   "9.25:7.75" = "mousepad",
                   "27:12" = "mousepad-L",
                   "31:12" = "mousepad-XL",
                   "other")

outdir <- here("hex-wall", out_type)
if (!dir_exists(outdir)) dir_create(outdir)

asp <- str_split(aspect_ratio, ":") |>
  flatten_chr() |>
  as.integer()
asp <- asp[2] / asp[1]

cropped_wall <- color_wall |>
  image_crop(paste0(image_info(color_wall)$width, ":",
                    image_info(color_wall)$width * asp),
             gravity = "center")

featured_width <- floor((image_info(cropped_wall)$height *
                           (1 - (0.1 * length(featured_stickers)))) /
                          image_info(featured_stickers[[1]])$height *
                          image_info(featured_stickers[[1]])$width)
final_featured <- map(featured_stickers, image_scale, featured_width)

final_img <- cropped_wall |>
  image_composite(image_blank(width = image_info(cropped_wall)$width,
                              height = image_info(cropped_wall)$height,
                              color = "#FFFFFF66")) |>
  add_featured(final_featured, crop_info = image_info(cropped_wall),
               feat_info = image_info(final_featured[[1]]))

final_img |>
  image_write(here("hex-wall", out_type,
                   glue("{str_c(featured, collapse = '_')}.png")))
final_img |>
  image_write(here("hex-wall", out_type,
                   glue("{str_c(featured, collapse = '_')}.jpg")))
