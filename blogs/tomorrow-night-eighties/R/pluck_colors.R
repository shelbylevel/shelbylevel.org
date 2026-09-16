# pluck colors from tomorrow night eighties theme files
# adapted from https://github.com/ivelasq/pipedream/blob/main/blog/positron-theme/R/pluck_colors.R

# 1. setup ----
suppressPackageStartupMessages(library(here))
library(jsonlite)
library(tidyverse)

# 2. data ----
# the yeoman-generated import of the original .tmTheme, before customization;
# this is what the microsoft port of tomorrow night eighties ships
tne_original <- fromJSON(
  here(
    "blogs",
    "tomorrow-night-eighties",
    "data",
    "tomorrow-night-eighties-original.json"
  ),
  flatten = TRUE
)

# the r classic theme file is JSONC; strip comment lines and the trailing
# commas left behind by commented-out keys before parsing
rclassic_path <- here(
  "blogs",
  "tomorrow-night-eighties",
  "data",
  "tomorrow-night-eighties-r-classic.json"
)
tne_rclassic <-
  readLines(rclassic_path) |>
  grep(pattern = "^\\s*//", value = TRUE, invert = TRUE) |>
  paste(collapse = "\n") |>
  gsub(pattern = ",(\\s*[}\\]])", replacement = "\\1", perl = TRUE) |>
  fromJSON(flatten = TRUE)

# 3. functions ----
flatten_tokencolors <- function(x) {
  stopifnot(is.list(x))
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    if (length(x[[i]]) == 0) {
      out[i] <- NA_character_
    } else {
      out[i] <- str_flatten_comma(sort(x[[i]]))
    }
  }
  out
}

flatten_colors <- function(x) {
  stopifnot(is.list(x))
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    if (length(x[[i]]) == 1) {
      out[i] <- x[[i]]
    }
    if (length(x[[i]]) > 1) {
      out[i] <- str_flatten_comma(sort(x[[i]]))
    }
  }
  out
}

pluck_tokencolors <- function(theme) {
  theme[["tokenColors"]] |>
    as_tibble() |>
    select(-name, -settings.fontStyle) |>
    rename(
      background = settings.background,
      foreground = settings.foreground
    ) |>
    pivot_longer(
      cols = all_of(c("background", "foreground")),
      names_to = "setting",
      values_to = "color"
    ) |>
    filter(!is.na(color)) |>
    unnest_longer(col = scope, keep_empty = TRUE) |>
    filter(!is.na(scope)) |>
    pivot_wider(names_from = scope, values_from = scope, values_fn = list) |>
    mutate(across(3:last_col(), flatten_tokencolors)) |>
    unite(col = "scope", 3:last_col(), sep = ", ", na.rm = TRUE) |>
    arrange(color, setting) |>
    relocate(color) |>
    mutate(color = str_to_lower(color))
}

pluck_colors <- function(theme) {
  enframe(theme[["colors"]], name = "setting", value = "color") |>
    mutate(color = flatten_colors(color)) |>
    relocate(color) |>
    arrange(color, setting) |>
    mutate(color = str_to_lower(color))
}

# replicate the preview-colour extension's dot glyph inside table cells only
# (the filter itself also matches named colours like "red" in prose, so it is
# disabled in the yaml and applied here instead)
add_colour_dots <- function(colors) {
  paste0(
    colors,
    '<span style="font-size: 0.8lh; font-family: system-ui, sans-serif; color: ',
    colors,
    '; cursor: pointer; user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none; position: relative;" title="Colour preview: ',
    colors,
    ' (click to copy)" aria-label="Colour preview: ',
    colors,
    ' (click to copy)" onclick="navigator.clipboard.writeText(\'',
    colors,
    '\');">&#9673;</span>'
  )
}

# 4. tomorrow night eighties (microsoft port) ----
tne_original_tokencolors3 <- pluck_tokencolors(tne_original)

tne_original_colors <- pluck_colors(tne_original)

# 5. tomorrow night eighties (r classic) ----
tne_rclassic_tokencolors3 <- pluck_tokencolors(tne_rclassic)

# group workbench settings that share a color onto one row
tne_rclassic_colors <-
  pluck_colors(tne_rclassic) |>
  pivot_wider(names_from = setting, values_from = setting) |>
  unite(col = "setting", 2:last_col(), sep = ", ", na.rm = TRUE)
