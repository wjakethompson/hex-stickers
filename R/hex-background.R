library(tidyverse)
library(magick)
library(glue)
library(here)
library(fs)

# Read images ------------------------------------------------------------------
rstudio_stickers <- c(dir_ls(here("PNG")), dir_ls("~/Desktop/extra-hex"))
other_stickers <- dir_ls(here("other-stickers", "_png")) %>%
  str_subset("recipes", negate = TRUE) %>%
  str_subset("ropensci", negate = TRUE) %>%
  str_subset("ropensci3", negate = TRUE) %>%
  str_subset("ropensci4", negate = TRUE)

sticker_files <- c(rstudio_stickers, other_stickers)
sticker_names <- path_file(sticker_files)
stickers <- sticker_files %>%
  map(function(path) {
    image_read(path)
  }) %>%
  map(image_transparent, "white") %>%
  map(image_trim) %>%
  set_names(sticker_names)


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

# Randomize stickers
stickers <- stickers[sample(seq_along(stickers), size = length(stickers))]

stickers[[32]] %>%
  image_colorize(0, "white")


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

sticker_row_size <- 14

# Calculate row sizes
# sticker_col_size <- ceiling(length(stickers) / (sticker_row_size - 0.5))
row_length <- rep(sticker_row_size, sticker_row_size)
row_split <- rep(c(FALSE, TRUE), length.out = sticker_row_size)

color_rows <- pmap(.l = list(row_lens = row_length,
                             row_cuml = cumsum(row_length),
                             row_splt = row_split),
                   .f = create_rows, stickers = stickers,
                   width = sticker_width, height = sticker_height,
                   opacity = 0, op_prop = 0)
color_rows <- c(color_rows, color_rows[[1]])

trans_rows <- pmap(.l = list(row_lens = row_length,
                             row_cuml = cumsum(row_length),
                             row_splt = row_split),
                   .f = create_rows, stickers = stickers,
                   width = sticker_width, height = sticker_height,
                   opacity = 70, op_prop = 0.7)
trans_rows <- c(trans_rows, trans_rows[[1]])


# Add stickers to canvas
canvas <- image_blank(sticker_row_size * sticker_width,
                      sticker_height + (sticker_row_size - 1) * sticker_height / 1.33526,
                      "white")

color_wall <- reduce2(color_rows, seq_along(color_rows),
                      ~ image_composite(
                        ..1, ..2,
                        offset = paste0("+0",
                                        "+", round((..3 - 1) * sticker_height / 1.33526))
                      ),
                      .init = canvas)

trans_wall <- reduce2(trans_rows, seq_along(trans_rows),
                      ~ image_composite(
                        ..1, ..2,
                        offset = paste0("+0",
                                        "+", round((..3 - 1) * sticker_height / 1.33526))
                      ),
                      .init = canvas)




color_wall %>%
  image_crop(glue("{sticker_row_size * sticker_width}",
                  "x{(sticker_height * .75) + (sticker_row_size - 1) * sticker_height / 1.33526}",
                  "+0+{sticker_height * .25}")) %>%
  image_write(here("hex-wall", "full-color-wall.png"))

trans_wall %>%
  image_crop(glue("{sticker_row_size * sticker_width}",
                  "x{(sticker_height * .75) + (sticker_row_size - 1) * sticker_height / 1.33526}",
                  "+0+{sticker_height * .25}")) %>%
  image_write(here("hex-wall", "opacity-wall.png"))
