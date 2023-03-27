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
ggsave("extra/fig_trait_evo.png", plot, width = 5, height = 3, dpi = 300)

# Plot a pairwise invasibility plot
plot <- plot_pip(seq(-1, 1, 0.01), pars, init = rep(1000, 2))

# Save it
ggsave("extra/fig_pip.png", plot, width = 4, height = 3, dpi = 300)

