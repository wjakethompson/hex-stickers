library(tidyverse)
library(magick)
library(glue)
library(here)

pkg <- "table-contest"

image_read(glue("~/Desktop/{pkg}.png")) %>%
  image_transparent("white") %>%
  image_trim() %>%
  image_scale("2521x2911!") %>%
  image_write(here("other-stickers", "_png", glue("{pkg}.png")))


# image_read(glue("~/Desktop/new-stickers/{pkg}.png")) %>%
#   image_trim() %>%
#   image_scale("2206x2557!") %>%
#   image_write(glue("~/Desktop/{pkg}.png"))
#
# image_read(glue("~/Desktop/new-stickers/{pkg}.png")) %>%
#   image_trim() %>%
#   # image_scale("2206x2557!") %>%
#   image_write(glue("~/Desktop/{pkg}2.png"))


