library(readr)
library(dplyr)
library(forcats)
library(tidyr)
library(rdist)
library(Matching)

get_closest_pair <- function(d) {
    target <- min(d, na.rm = TRUE)
    pair <- rownames(which(d == target, arr.ind = TRUE))
    pair
}

get_closest_group <- function(d) {
    target <- min(d, na.rm = TRUE)
    pair <- get_closest_pair(d)
    f <- d %>% dplyr::select(dplyr::all_of(pair))

    extra <- names(which(((f < 2 * target) %>% rowSums()) >= 1))
    base <- names(which(((f < 3 * target) %>% rowSums()) >= 2))
    news <- intersect(extra, base)
    c(pair, news)
}

dists <- function(df, cols) {
    df <- df %>%
        filter(political_will >= mean(political_will, na.rm = TRUE) |
            political_will >= 2)

    d <- df %>%
        dplyr::select(cols) %>%
        as.matrix() %>%
        pdist() %>%
        as.data.frame(row.names = df$state) %>%
        na_if(0)

    colnames(d) <- df$state
    d
}

df <- read_csv("nigeria-hcp-state-data.csv")

COLS <- c("vaxed", "fb_penetration")

groups <- df %>%
    filter(fb_reach > 250000) %>%
    mutate(across(COLS, scale)) %>%
    mutate(political_will = replace_na(political_will, 0)) %>%
    filter(state != "Federal Capital Territory" & state != "Lagos") %>%
    group_by(zone) %>%
    group_map(~ dists(.x, COLS))


flip <- function() {
    a <- rbinom(1, 1, 0.5)
    b <- ifelse(a == 0, 1, 0)
    c(a, b)
}

choose <- function(pairs) {
    n <- length(pairs)
    out <- rep(0, n)
    for (i in seq(1, n, by = 2)) {
        f <- flip()
        out[i] <- f[1]
        out[i + 1] <- f[2]
    }
    out
}


set.seed(1)
pairs <- sapply(groups, get_closest_pair)

pdf <- data.frame(state = c(pairs))
pdf <- pdf %>% mutate(treated = choose(pdf$state))

df_full <- pdf %>% left_join(df)

df_full %>% write_csv("nigeria-hcp-state-data-with-treatment.csv")

##########################################################
# CHECK BALANCE



MatchBalance(treated ~ vaxed + poverty_index + fb_penetration,
    data = df_full,
    print.level = 1
)