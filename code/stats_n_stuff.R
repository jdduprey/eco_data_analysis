# ===========================================================
# some intro to basic statistical tests
# Joe Duprey
# April 26, 2022
# ===========================================================
library(tidyverse)
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(stargazer)
library(faux)
library(gridExtra)
set.seed(33) 

# set penguin data to penguin_dat variable 
penguin_dat <- penguins

# histogram of flipper length
ggplot(data = penguin_dat, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal()
ggsave("./figures/flipper_length_hist.png")

# boxplot
ggplot(data = penguin_dat, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(fill = species), alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal()

# boxplot with raw data
ggplot(data = penguin_dat, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(fill = species), alpha = 0.5) +
  geom_jitter(aes(color = species),
              width = 0.1, 
              alpha = 0.7) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal() +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

# run the anova
flipper_length_anova <- aov(flipper_length_mm ~ species, penguin_dat)
summary(flipper_length_anova)

# run the post-hoc test 
HSD <- TukeyHSD(flipper_length_anova, conf.level=.95)
HSD

# linear regression 
# ===========================================================
adelie_dat <- penguin_dat %>%
  filter(species %in% "Adelie")

chinstrap_dat <- penguin_dat %>%
  filter(species %in% "Chinstrap")

ggplot(data = penguin_dat, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(color = species)) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))


flipper_lm <- lm(flipper_length_mm ~ body_mass_g, data = penguin_dat)
summary(flipper_lm)  

# generating correlated data
dat <- rnorm_multi(100, 2, mu=c(20, 1000), sd=c(5,100), r = 0.99, varnames = c("temp","mopeds"))
plot(dat$temp, dat$mopeds)

moped_lm1 <- lm(mopeds~temp, dat)
summary(moped_lm1)
stargazer(moped_lm1, type = "html", out = "./figures/moped1.html")

dat2 <- rnorm_multi(100, 2, mu=c(20, 1000), sd=c(5,100), r = 0.8, varnames = c("temp","mopeds"))
plot(dat2$temp, dat2$mopeds)

moped_lm2 <- lm(mopeds~temp, dat2)
summary(moped_lm2)
stargazer(moped_lm2, type = "html", out = "./figures/moped2.html")

dat3 <- rnorm_multi(100, 2, mu=c(20, 1000), sd=c(5,100), r = 0.01, varnames = c("temp","mopeds"))
plot(dat3$temp, dat3$mopeds)

moped_lm3 <- lm(mopeds~temp, dat3)
summary(moped_lm3)
stargazer(moped_lm3, type = "html", out = "./figures/moped3.html")


aplot <- ggplot(data=dat, aes(temp, mopeds)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE)

bplot <- ggplot(data=dat2, aes(temp, mopeds)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE)

cplot <- ggplot(data=dat3, aes(temp, mopeds)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE)

all_plot <- grid.arrange(aplot, bplot, cplot, nrow=1, ncol=3)
ggsave(all_plot, "./figures/moped_multiverse.png")


# lets look at why p value can be misleading 
# ===========================================================
uh_oh_dat <- rnorm_multi(300, 2, mu=c(20, 1000), sd=c(5,1000), r = 0.12, varnames = c("temp","mopeds"))
plot(uh_oh_dat$temp, uh_oh_dat$mopeds)

ggplot(data=uh_oh_dat, aes(temp, mopeds)) +
  geom_point() +
  theme_minimal() 
ggsave("./figures/shotgun.png")

ggplot(data=uh_oh_dat, aes(temp, mopeds)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method="lm", se=FALSE)
ggsave("./figures/lm_shotgun.png")

uhoh_lm <- lm(mopeds~temp, uh_oh_dat)
summary(uhoh_lm)
stargazer(uhoh_lm, type = "html", out = "./figures/uhoh.html")


# generate an anova with high within group variance relative to between group 
test <- rnorm_multi(100, 3, mu=c(20, 20, 20), sd=c(5, 6, 7), r = 0.1, varnames = c("A","B","C"))
test <- test %>%
  pivot_longer(cols=c("A","B","C"), names_to="group")

ggplot(data = test, aes(x=group, y=value)) +
  geom_boxplot() +
  theme_minimal()

test_aov <- aov(value ~ group, data=test)
summary(test_aov)

test_HSD <- TukeyHSD(test_aov, conf.level=.95)
test_HSD



