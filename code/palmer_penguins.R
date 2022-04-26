# ===========================================================
# some intro to R
# Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago
# (Antarctica) penguin data. R package version 0.1.0.
# https://allisonhorst.github.io/palmerpenguins/
# ===========================================================

# load in libraries
library(palmerpenguins)
library(visdat)
library(dplyr)
library(ggplot2)

# preview data
vis_dat(penguins)

# set penguin data to penguin_dat variable 
penguin_dat <- penguins

# count penguins for each species / island
penguin_dat %>%
  count(species, island, .drop = FALSE)

# ===========================================================
# VISUALIZATION 
# ===========================================================

# plot penguins by sex
ggplot(penguins, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = "none") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# scatterplot example 1: penguin flipper length versus body mass
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g,
                            color = species, shape = species)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) 


# jitter plot example: bill length by species
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_jitter(aes(color = species),
              width = 0.1, 
              alpha = 0.7,
              show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))


# histogram example: flipper length by species
ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal()

# citation
citation("palmerpenguins")
