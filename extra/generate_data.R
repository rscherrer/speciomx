rm(list = ls())

library(speciomx)
library(tidyverse)
library(patchwork)

theme_set(theme_classic())

# Set-up parameters
pars <- get_example_pars()

#### Simulate a few datasets (~ 10min per simulation) ####

# Here we simulate datasets for 1000 generations, but with a mutational step
# size of 0.1, which approximates a simulation of 100,000 generations (as in
# our default stochastic simulation) with a mutational step size of 0.01. This
# is because the rate of evolutionary change is proportional to the square of
# the mutational step size.

# Type I branching: wait until the selection gradient comes close enough to
# zero, then evaluate evolutionary stability to know if the singularity is
# a branching point.

# Type II branching: evaluate the reciprocal invasibility of the resident and
# a mutant located one mutational step size away. If both can invade each other
# a branching point has been reached.

# Type I branching
data1 <- approx_data <- simulate(
  0, ntimes = 1000, pars, init = rep(1000, 4), mu = 0.01, sigma = 0.1,
  burnin = 200, branch = 1, tol = 0.0001, dodge = 0.01
)

# Type I branching with a more loose equilibrium criterion (tolerance = sigma)
data2 <- approx_data <- simulate(
  0, ntimes = 1000, pars, init = rep(1000, 4), mu = 0.01, sigma = 0.1,
  burnin = 200, branch = 1, tol = 0.01, dodge = 0.01
)

# Type II branching
data3 <- approx_data <- simulate(
  0, ntimes = 1000, pars, init = rep(1000, 4), mu = 0.01, sigma = 0.1,
  burnin = 200, branch = 2, dodge = 0.01
)

# Make plots
plots <- map(list(data1, data2, data3), function(data) {

  data %>%
    mutate(time = time * 100) %>% # back to the original time scale
    pivot_longer(c(x1, x2), names_to = "ecotype") %>%
    ggplot(aes(x = time / 1000, y = value, group = ecotype)) +
    geom_line() +
    xlab(parse(text = "'Time ('*10^3~'generations)'")) +
    ylab("Trait value") +
    ylim(c(-5, 5)) +
    geom_vline(xintercept = 0, linetype = 4)

})

# Compare them
plot1 <- plots[[1]] + ggtitle("Type I branching, tolerance 0.0001")
plot2 <- plots[[2]] + ggtitle("Type I branching, tolerance 0.001")
plot3 <- plots[[3]] + ggtitle("Type II branching")

plot1 / plot2 / plot3

# Save the plot to illustrate
ggsave("extra/comparison.png", width = 4, height = 6, dpi = 300)