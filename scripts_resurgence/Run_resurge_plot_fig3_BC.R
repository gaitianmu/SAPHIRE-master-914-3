rm(list = ls())
## IMPORTANT: Please set code_root variable properly. 
## code_root should be set to the directory where the repository README file is located. 
## For more information, please read the repository README file
code_root="/Users/gaixin/Desktop/SAPHIRE-master-914-3/"
setwd(paste0(code_root, "scripts_resurgence"))


##
s0_dat1 <- as.matrix(read.table("../output/simu_main_resurgence_date_type_1_lift.txt", header = T))
#s0_dat2 <- as.matrix(read.table("../output/simu_main_resurgence_date_type_2_lift.txt", header = T))
#s8_dat1 <- as.matrix(read.table("../output/simu_s8_resurgence_date_type_1_lift.txt", header = T))
#s8_dat2 <- as.matrix(read.table("../output/simu_s8_resurgence_date_type_2_lift.txt", header = T))

simu_outbreak_date1 <- s0_dat1[, 14]
sum(!is.na(simu_outbreak_date1)) /  length(simu_outbreak_date1)
##
outbreak_prop_mat <- matrix(NA, 60, 1)
outbreak_prop_mat[, 1] <- apply(s0_dat1, 2, function(x) length(na.omit(x)) / length(x))
#outbreak_prop_mat[, 1] <- apply(s0_dat2, 2, function(x) length(na.omit(x)) / length(x))
#outbreak_prop_mat[, 4] <- apply(s8_dat1, 2, function(x) length(na.omit(x)) / length(x))
#outbreak_prop_mat[, 3] <- apply(s8_dat2, 2, function(x) length(na.omit(x)) / length(x))
#outbreak_prop_mat[14, ]
##
cal_date <- function(x) {
  # x <- s0_dat1
  x20 <- apply(x[, 1:60], 2, function(x) mean(na.omit(x)))
  #x21p <- mean(na.omit(as.vector(x[, -c(1:60)])))
  #return(c(x20, x21p))
  return(c(x20))
}
cal_low_date <- function(x) {
  # x <- s0_dat1
  x20 <- apply(x[, 1:60], 2, function(x) quantile(na.omit(x), 0.025))
  #x21p <- quantile(na.omit(as.vector(x[, -c(1:60)])), 0.025)
  #return(c(x20, x21p))
  return(c(x20))
}

cal_up_date <- function(x) {
  # x <- s0_dat1
  x20 <- apply(x[, 1:60], 2, function(x) quantile(na.omit(x), 0.975))
  #x21p <- quantile(na.omit(as.vector(x[, -c(1:60)])), 0.975)
  #return(c(x20, x21p))
  return(c(x20))
}

s0_date1 <- cal_date(s0_dat1)
#s0_date2 <- cal_date(s0_dat2)
#s8_date1 <- cal_date(s8_dat1)
#s8_date2 <- cal_date(s8_dat2)
#date_mat <- cbind(s0_date2, s0_date1, s8_date2, s8_date1)
#date_mat <- cbind(s0_date2, s0_date1)
date_mat <- s0_date1
# date_mat[14, ]
#cal_low_date(s0_dat2)[14]
#cal_up_date(s0_dat2)[14]
cal_low_date(s0_dat1)[14]
cal_up_date(s0_dat1)[14]
#
#cal_low_date(s8_dat1)[14]
#cal_up_date(s8_dat1)[14]
print(s0_date1)
print(cal_low_date(s0_dat1)[14])
print(cal_up_date(s0_dat1)[14])
#print(s0_date2)
#print(cal_low_date(s0_dat2)[14])
#print(cal_up_date(s0_dat2)[14])


#svg("../output/simulate_resurge_fig3_BC-new.svg", width = 11, height = 10)
#par(mai = c(1, 1,0.5,0.1))
#layout(matrix(c(1:4), byrow = T, nrow = 2))
pdf("../output/NEW-simulate_resurge_fig3_BC-NEW.pdf", width = 14.1, height = 7)
par(mfrow = c(1, 2), mar = c(4, 5, 3, 2))
#############fig5##################\
ptime <- 1:68
mydate <- c(paste("Jan", 1:31), paste("Feb", 1:29), paste("Mar", 1:31), paste("Apr", 1:30), paste("May", 1:31), paste("Jun", 1:15))
probbase1<- read.csv("../data/probability-14.csv", row.names = 1)
prob1<-probbase1[-c(1:24),]
qd<-prob1[1:68]
plot(ptime, qd, ylim = c(0.4, qd[68]*1.1), xlab = "", ylab = "", type = "p", col = "white", pch = 16, xaxt="n", cex = 0.5)
mtext("Date of symptom onset", side = 1, line  = 3, cex = 1.9)
mtext("Ascertainment rate in I(fraction)", side = 2, line = 3, cex = 1.9)
axis(1, at = seq(1, 68, 11), labels = mydate[seq(1, 68, 11)])
points( ptime,qd, col = "#0072B5FF", pch = 16, cex = 1.5)
lines( ptime,qd, col = "#0072B5FF",type = "l",lwd =3)
text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.1, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.05, labels = "C", xpd = T, cex = 2)

##################################

plot(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 1], type = "l", ylim = c(0, 1), col = "#BC3C29FF", lwd = 3, xlab = "", ylab = "", xaxt = "n")
#lines(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 2], col = "#BC3C29FF", lwd = 3)
axis(1, at = round(seq(1, 60, length.out = 6), 0), labels = round(seq(1, 60, length.out = 6), 0))
mtext("t (days)", 1, line = 3, cex = 1.9)
mtext("Probability of resurgence", 2, line = 3, cex = 1.9)
#lines(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 2], col = "#BC3C29FF", lwd = 3)
#lines(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 3], col = "#0072B5FF", lwd = 3, lty = 3)
#lines(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 4], col = "#BC3C29FF", lwd = 3, lty = 3)
#points(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 1], pch = 16, col = "#0072B5FF", cex = 1.5)
points(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 1], pch = 16, col = "#BC3C29FF", cex = 1.5)
#points(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 3], pch = 18, col = "#0072B5FF", cex = 1.5)
#points(1:nrow(outbreak_prop_mat), outbreak_prop_mat[, 4], pch = 18, col = "#BC3C29FF", cex = 1.5)
#text(30, 0.9, pos = 4, labels = "Legend in panel C")
####legend("bottomright", title = "When to lift all controls" ,legend = c("t days of zero new ascertainable cases consecutively"), 
 ###      col = "#BC3C29FF", lty = 1, lwd = 2, bty = "o", pch = 16)
text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.1, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.05, labels = "D", xpd = T, cex = 2)
dev.off()


##########################D###################################
#plot(1:60, s0_date2, type = "l", ylim = c(10, 50), col = "#0072B5FF", lwd = 3, xlab = "", ylab = "", xaxt = "n")
###plot(1:60, s0_date1, type = "l", ylim = c(10, 50), col = "#BC3C29FF", lwd = 3, xlab = "", ylab = "", xaxt = "n")
#axis(1, at = round(seq(1, 21, length.out = 6), 0), labels = c("1", "5", "9", "13", "17", "21-30"))
###axis(1, at = round(seq(1, 60, length.out = 6), 0), labels = round(seq(1, 60, length.out = 6), 0))
###mtext("t (days)", 1, line = 2.5, cex = 1.5)
###mtext("Conditional expectation of time to resurgence (days)", 2, line = 2.5, cex = 1.5)
#lines(1:60, s0_date1, col = "#BC3C29FF", lwd = 3)
#lines(1:21, s8_date2, col = "#0072B5FF", lwd = 3, lty = 3)
#lines(1:21, s8_date1, col = "#BC3C29FF", lwd = 3, lty = 3)
#points(1:60, s0_date2, pch = 16, col = "#0072B5FF", cex = 1.5)
####points(1:60, s0_date1, pch = 16, col = "#BC3C29FF", cex = 1.5)
####print(outbreak_prop_mat[, 1])
#print("========================")
#print(outbreak_prop_mat[, 2])
#points(1:21, s8_date2, pch = 18, col = "#0072B5FF", cex = 1.5)
#points(1:21, s8_date1, pch = 18, col = "#BC3C29FF", cex = 1.5)
#legend("bottomright", title = "When to lift all controls" ,legend = c("t days after first I=0 (M)", "t days of I=0 consecutively (M)", "t days after first I=0 (S8)", "t days of I=0 consecutively (S8)"), 
#col = c("#0072B5FF", "#BC3C29FF"), lty = c(1, 1, 3, 3), lwd = 2, bty = "n", pch = c(16, 16, 18, 18))
####legend("bottomright", title = "When to lift all controls" ,legend = c("t days of zero new ascertainable cases consecutively"), 
    ###   col = "#BC3C29FF", lty = 1, lwd = 2, bty = "o", pch = 16)
#legend("bottomright", title = "When to lift all controls" ,legend = c("t days after the first day of zero new ascertainable cases(M)", "t days of zero new ascertainable cases consecutively(M)"), 
       #col = c("#0072B5FF", "#BC3C29FF"), lty = c(1, 1), lwd = 2, bty = "o", pch = c(16, 16))
#legend("bottomright", title = "When to lift all controls" ,legend = c("t days after first I=0 (M)", "t days of I=0 consecutively (M)"), 
 #      col = c("#0072B5FF", "#BC3C29FF"), lty = c(1, 1), lwd = 2, bty = "o", pch = c(16, 16))
###text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.1, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.05, labels = "D", xpd = T, cex = 2)
###dev.off()






















