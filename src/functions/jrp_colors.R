# SOURCE: https://www.1klb.com/posts/2020/04/16/making-a-colour-palette-package-in-r/
# Require basic tidyverse packages ----

# Set up colours ----
.jrp_colours <- c(
  green      = "#18BC9C",
  orange     = "#F39C12",
  red        = "#E74C3C",
  blue       = "#3498DB",
  purple     = "#9b59b6",
  black      = "#2C3E50"
)

# This function takes a character or integer index
jrp_colours <- function(index = NULL, named = FALSE) {
  # Default to everything
  if (is.null(index)) {
    index <- names(.jrp_colours)
  }

  # This works with integer or character values
  return_value <- .jrp_colours[index]

  if (!named) {
    names(return_value) <- NULL
  }

  return(return_value)
}

# Another convenience function
jrp_colour_names <- function() {
  names(.jrp_colours)
}

# Set up a basic palette ----
jrp_palette <- function() {

  jrp_colour_length <- length(jrp_colours())

  function(n) {
    stopifnot(n <= jrp_colour_length)
    return(jrp_colours(1:n))
  }
}

scale_colour_jrp <- function(...) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      scale_name = "jrp",
      palette = jrp_palette(),
      ...
    )
}


# Automatic palette ----
jrp_palette <- function() {
  jrp_colour_length <- length(jrp_colours())

  function(n) {
    stopifnot(n <= jrp_colour_length)
    
    # Shortcut: if n = 1, we can just return the first colour
    if (n == 1) {
      return(jrp_colours(1))
    }

    # Pick additional colours. Make them as spread out as possible
    interval_between_picks <- jrp_colour_length / n

    additional_colour_indices <- 1 + (1:(n-1)) * interval_between_picks

    # Work out which colours to return
    colour_indices <- c(1, round(additional_colour_indices))

    return(jrp_colours(colour_indices))
  }
}


# Manual palette ----
jrp_palette <- function() {

  jrp_colour_length <- length(jrp_colours())

  function(n) {
    stopifnot(n <= jrp_colour_length)

    colour_indices <- 
      if (n == 1) { "green" }
      else if (n == 2) { c("green", "orange") }
      else if (n == 3) { c("green", "orange", "blue") }
      else if (n == 4) { c("green", "orange", "red", "blue") }
      # ... etc. etc.
      else if (n == 6) {
        c(
          "green", "orange", "red", "blue",
          "purple", "black"
        )
      }
    
    return(jrp_colours(colour_indices))
  }
}


scale_fill_jrp <- function(...) {
      ggplot2::discrete_scale(
      aesthetics = "fill",
      scale_name = "jrp",
      palette = jrp_palette(),
      ...
    )
}


# Continuous sequential scales ----
scale_colour_jrp_c <- function(index = 1, colour_range = 0.75, ...) {
  low_colour <- jrp_colours(index)
  high_colour <- colorspace::lighten(low_colour, amount = colour_range)

  ggplot2::scale_colour_gradient(
    low = low_colour,
    high = high_colour,
    ...
  )
}


# Continuous diverging scales ----
scale_colour_jrp_div <- function(high_index = 1, low_index = 5, ...) {
  high_colour <- jrp_colours(high_index)
  low_colour <- jrp_colours(low_index)

  ggplot2::scale_colour_gradient2(
    low = low_colour,
    high = high_colour,
    ...
  )
}