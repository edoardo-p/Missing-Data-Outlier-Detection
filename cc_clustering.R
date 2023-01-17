source("distributions.R")
library(MixtureMissing)

cc <- read.csv("credit_cards.csv")
cc_missing <- ampute(cc, 0.05)
sapply(cc_missing, function(x) sum(is.na(x)))

cc_missing <- cc_missing[-1]
# mtm_model <- MtM(cc_missing, 2)+
best_bic <- 9999999999
best_g <- 0
for (G in 2:5) {
    mnm_model <- MNM(cc_missing, G, max_iter = 1)
    if (mnm_model$BIC < best_bic) {
        best_bic <- mnm_model$BIC
        best_g <- G
    }
}
# mcnm_model <- MCNM(cc_missing, 2)

ggplot(cc_missing, aes(x = `BALANCE`, y = `PURCHASES`)) +
    geom_point(aes(colour = factor(mnm_model$clusters))) +
    scale_color_manual(values = c("green", "blue", "red", "yellow", "purple")) +
    ggtitle("Results")
