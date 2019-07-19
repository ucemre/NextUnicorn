pdf("AUC_Comparison.pdf", width = 5, height = 5)

par(mar = c(5, 5, 5, 5), mfrow = c(1,1))
plot(perf_obj_full,    col = "indianred1",  lwd = 2.5)
par(new = T)
plot(perf_obj_reduced, col = 'blue',        lwd = 2.5)
par(new = T)
plot(perf_obj_rpart,   col = "green4",      lwd = 2.5)
par(new = T)
plot(perf_obj_inf,     col = "blueviolet",  lwd = 2.5)
par(new = T)
plot(perf_obj_rf,      col = "aquamarine4", lwd = 2.5)
par(new = T)
plot(perf_obj_xgb,     col = "chocolate1",  lwd = 2.5)

abline(a = 0, b = 1,   col = "black")

legend("bottomright", legend = c("M0", "M1", "Rpart", "Cond. Inf. Tree", "Random Forest", "XGB"),
       col = c("indianred1", "blue", "green4","blueviolet" , "aquamarine4", "chocolate1" ), 
       lty = 1, cex = 0.8, plot = FALSE)

dev.off()
