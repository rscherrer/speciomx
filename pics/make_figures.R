## ---------------------------
##
## Script name: make_figures
##
## Purpose of script:
##
## Generate the figures in pics/
##
## How to use: just run it
##
## Author: Raphael Scherrer
##
## Date Created: 2021-06-18
##
## This script comes with no guarantee whatsoever.
##
## Copyright (c) Raphael Scherrer, 2021
##
## Find me on GitHub at https://github.com/rscherrer
##
## Email:
## r.scherrer@rug.nl
## raphael.scherrer@evobio.eu
## raph.rjfs@hotmail.fr
##
## ---------------------------

rm(list = ls())

library(speciomx)
library(tidyr)
library(magrittr)
library(ggplot2)

theme_set(theme_classic())

# Set parameters
pars <- get_default_pars()

# Simulate trait evolution
data <- simulate(-0.5, 300, pars, init = rep(1000, 4))

# Plot it
plot <- data %>%
  pivot_longer(c(x1, x2), names_to = "morph", values_to = "x") %>%
  ggplot(aes(x = time, y = x, group = morph)) +
  geom_line() +
  xlab("Time (generations)") +
  ylab("Trait value")

# Save it
ggsave("pics/fig_trait_evo.png", plot, width = 5, height = 3, dpi = 300)

# Plot a pairwise invasibility plot
plot <- plot_pip(seq(-1, 1, 0.01), pars, init = rep(1000, 2))

# Save it
ggsave("pics/fig_pip.png", plot, width = 4, height = 3, dpi = 300)

