pdf("Scatter_180719.pdf", width = 20, height = 15)

par(mar = c(5, 5, 2, 2), mfrow = c(3,2))
plot(test_v2$funding_rounds, test_v2$probs,    col = "darkred", xlab = "Funding rounds",       ylab = "", cex.lab = 1.5)
plot(test_v2$company_age, test_v2$probs,       col = "darkred", xlab = "Company age",          ylab = "", cex.lab = 1.5)
plot(test_v2$lastFundingtoDate, test_v2$probs, col = "darkred", xlab = "Last funding to date", ylab = "", cex.lab = 1.5)
plot(test_v2$funding_total_usd, test_v2$probs, col = "darkred", xlab = "Total funding (USD)",  ylab = "P(Success=1)", cex.lab = 1.5)
plot(test_v2$first_funding_lag, test_v2$probs, col = "darkred", xlab = "First funding lag",    ylab = "", cex.lab = 1.5)
plot(test_v2$last_funding_lag, test_v2$probs,  col = "darkred", xlab = "Last funding lag",     ylab = "", cex.lab = 1.5)

dev.off()

