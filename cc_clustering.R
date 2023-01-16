source("distributions.R")
library(MixtureMissing)

cc <- read.csv("credit_cards.csv")
cc_missing <- ampute(cc, 0.05)
sapply(cc_missing, function(x) sum(is.na(x)))

cc_missing <- cc_missing[-1]
# mtm_model <- MtM(cc_missing, 2)
mnm_model <- MNM(cc_missing, 2, max_iter = 1)
# mcnm_model <- MCNM(cc_missing, 2)

ggplot(cc_missing, aes(x = `BALANCE`, y = `PAYMENTS`)) +
    geom_point(aes(colour = factor(mnm_model$clusters))) +
    scale_color_manual(values = c("green", "blue")) +
    ggtitle("Results")
