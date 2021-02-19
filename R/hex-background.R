library(tidyverse)
library(magick)
library(glue)
library(here)
library(fs)

aspect_ratio <- "27:12"
rstudio_only <- FALSE

# Read images ------------------------------------------------------------------
rstudio_stickers <- dir_ls(here("PNG")) %>%
  str_subset("rmarkdown", negate = TRUE)
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
    sticker_col_size <- round(((cur_height - sticker_height) / (sticker_height / 1.33526)) + 1)
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

dimensions <- calc_dim(aspect_ratio = aspect_ratio,
                       total_files = length(stickers), allow_resample = TRUE,
                       max_num = ifelse(aspect_ratio == "16:9",
                                        length(stickers), length(stickers)))

target_width <- dimensions$target_width
target_height <- dimensions$target_height

sticker_row_size <- round(target_width / sticker_width, digits = 0)
sticker_col_size <- round(((target_height - sticker_height) /
                             (sticker_height / 1.33526)) + 1,
                          digits = 0)
total_stickers <- sticker_row_size * sticker_col_size

# Randomize stickers
if (total_stickers > length(stickers)) {
  extra <- total_stickers - length(stickers)
  iter <- 0
  good_sample <- FALSE
  while(!good_sample) {
    choose <- sample(c(seq_along(stickers),
                       sample(seq_along(stickers), size = extra)),
                     size = total_stickers, replace = FALSE)
    check_stick <- c(choose, choose)

    unique_stick <- sort(unique(choose))
    good_check <- TRUE
    for (i in seq_along(unique_stick)) {
      cur_loc <- which(check_stick == unique_stick[i])
      if (any(diff(cur_loc) <= (sticker_row_size * 2.5))) {
        good_check <- FALSE
        break
      }
    }

    if (good_check) {
      good_sample <- TRUE
    } else {
      iter <- iter + 1
    }

    if (iter > 100000) stop("Max iterations reached")
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

trans_rows <- pmap(.l = list(row_lens = row_length,
                             row_cuml = cumsum(row_length),
                             row_splt = row_split),
                   .f = create_rows, stickers = stickers,
                   width = sticker_width, height = sticker_height,
                   opacity = 70, op_prop = 0.7)
trans_rows <- c(trans_rows, trans_rows[[1]])


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

trans_wall <- reduce2(trans_rows, seq_along(trans_rows),
                      ~ image_composite(
                        ..1, ..2,
                        offset = paste0("+0",
                                        "+", round((..3 - 1) * sticker_height / 1.33526))
                      ),
                      .init = canvas)

out_type <- switch(aspect_ratio,
                   "16:9" = "zoom",
                   "9.25:7.75" = "mousepad",
                   "27:12" = "mousepad-L",
                   "31:12" = "mousepad-XL",
                   "other")

outdir <- here("hex-wall", out_type)
if (!dir_exists(outdir)) dir_create(outdir)

color_wall %>%
  image_crop(glue("{sticker_row_size * sticker_width}",
                  "x{(sticker_height * .75) + (sticker_row_size - 1) * sticker_height / 1.33526}",
                  "+0+{sticker_height * .25}")) %>%
  image_write(here("hex-wall", out_type, "full-color-wall.png"))

trans_wall %>%
  image_crop(glue("{sticker_row_size * sticker_width}",
                  "x{(sticker_height * .75) + (sticker_row_size - 1) * sticker_height / 1.33526}",
                  "+0+{sticker_height * .25}")) %>%
  image_write(here("hex-wall", out_type, "opacity-wall.png"))
