install.packages("Lahman")
library(Lahman)
install.packages("HistData")
library(HistData)
library(tidyverse)
library(ggplot2)
data()
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# 
# Run a linear model in R predicting the number of runs 
# per game based on the number of bases on balls and the 
# number of home runs. Remember to first limit your data to 1961-2001.
#
lm_model <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G, BB_per_game = BB / G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .)

lm_model
