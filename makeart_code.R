make_art <- function(x) {
  # preliminaries -----------------------------------------------------------

  # load packages
  library(tidyverse)
  library(ambient)
  library(scico)
  library(here)

  # parameters
  art_par <- list(
    seed = sample(1:10, replace = TRUE),
    n_paths = 500,
    n_steps = 80,
    sz_step = 200,
    sz_slip = 70
  )

  # canvas set up -------------------------------------------------------

  set.seed(seed = art_par$seed)

  state <- tibble(
    x = runif(n = art_par$n_paths, min = 0, max = 3),
    y = runif(n = art_par$n_paths, min = 0, max = 3),
    z = 0
  )

  # Including path_id and step_id in the state
  state <- state %>%
    mutate(
      path_id = 1:art_par$n_paths,
      step_id = 1
    )

  # Tracking of the series of states
  art_dat <- state


  # create art in a loop ----------------------------------------------------

  stop_painting <- FALSE

  while(stop_painting == FALSE) {
    # make a step
    step <- curl_noise(
      generator = gen_simplex,
      x = state$x,
      y = state$y,
      z = state$z,
      seed = c(1, 1, 1) * art_par$seed
    )
    state <- state %>%
      mutate(
        x = x + (step$x / 10000) * art_par$sz_step,
        y = y + (step$y / 10000) * art_par$sz_step,
        z = z + (step$z / 10000) * art_par$sz_slip,
        step_id = step_id + 1
      )
    # append state to art_dat
    art_dat <- bind_rows(art_dat, state)

    # check if we should stop
    current_step <- last(state$step_id)
    if(current_step >= art_par$n_steps) {
      stop_painting <- TRUE
    }
  }

  # draw the picture --------------------------------------------------------

  pic <- ggplot(
    data = art_dat,
    mapping = aes(
      x = x,
      y = y,
      group = path_id,
      color = step_id
    )
  ) +
    geom_path(
      size = .5,
      alpha = .5,
      show.legend = FALSE
    ) +
    coord_equal() +
    theme_void() +
    scale_color_scico(palette = "berlin")

  x11()
  print(pic)
}
