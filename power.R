library(clusterPower)
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)
library(lme4)


expit <- function(x) 1 / (1 + exp(-x))

interpret_p <- function(p, sigma_b, name, n = 1000) {
    logit_p <- log(p / (1 - p))
    b <- rnorm(n, 0, sqrt(sigma_b))
    data.frame(x = expit(logit_p + b), name = name)
}


#################################################
# Assumptions
#################################################
p1 <- 0.58
p2 <- 0.73


#################################################
# Closed-form estimation w/ ICC estimate
#################################################
cpa.binary(
    alpha = 0.05,
    power = 0.8,
    nclusters = NA,
    nsubjects = 200,
    CV = 0,
    p1 = p1,
    p2 = p2,
    ICC = 0.02
)


#################################################
# Simulation estimation with survey data
#################################################
survey_data <- read_csv("nigeria-prelim-survey.csv") %>%
    mutate(location = as_factor(location))

# Estimate random-effects model on survey data
model <- glmer(vaccinated ~ 1 + (1 | location),
    survey_data,
    family = "binomial"
)
estimated_zone_means <- expit(coef(model)$location)[["(Intercept)"]]
estimated_sd <- attributes(VarCorr(model)$location)$stddev

# Estimate empirical mean per zone in survey data
zone_means <- survey_data %>%
    group_by(location) %>%
    summarize(vaccinated = mean(vaccinated)) %>%
    pull(vaccinated)

# Simulate
s <- cps.binary(
    nsim = 500,
    alpha = 0.05,
    nclusters = 5,
    nsubjects = 150,
    p1 = p1,
    p2 = p2,
    sigma_b_sq = estimated_sd**2
)

s$power


#################################################
# Visualize simulations and empirical means
#################################################
df <- rbind(
    interpret_p(p1, estimated_sd**2, "control", 2000),
    interpret_p(p2, estimated_sd**2, "treated", 2000)
)

ggplot(df, aes(x = x, fill = name)) +
    geom_histogram(alpha = 0.5, position = "identity") +
    geom_vline(xintercept = zone_means, color = "black")

ggsave("nigeria-bernouli-outcome-simulation.png")