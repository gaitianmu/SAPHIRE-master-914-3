## SEIR model plots for the six panels
## five periods: Jan 1-9 (index 1-9), Jan 10-22 (index 10-22), Jan 23-Feb 1 (index 23-32), Feb 2-16 (index 33-47), Feb 17- (index 48-60)
#' @param pars_estimate           a vetor of parameters: c(b12, b3, b3, b5, r12, delta3, delta4, delta5)
#' @param init_settings           a list of initial values and known parameters
#' @param Dp                      presymptomatic infectious period
#' @param Di                      symptomatic infectious period
#' @param De                      latent period
#' @param Dq                      duration from illness onset to hospitalization
#' @param Dh                      hospitalization period                   
#' @param alpha                   ratio of the transmission rate of unascertained over ascertained case
#' @param N                       population size
#' @param flowN_vec               daily inbound and outbound size during five periods (n)
#' @param init_states             initial c(S, E, P, Is, A, H, R)
#' @param days_to_fit             the days to fit the model
#' @param b                       transmission rate of ascertained cases
#' @param r                       ascertainment rate 
#' @param num_periods             number of periods to simulate
#################################################################################################
SEIRplot <- function(pars_estimate, file_name, init_settings, panel_B_R_ylim=5) {
  print(length(pars_estimate))
  init_settings$days_to_fit <- 1:68
  library(vioplot)
  #set.seed(28)
  ##
  onset_obs_all <- init_settings$daily_new_case_all
  ptime <- 1:length(onset_obs_all)
  mydate <- c(paste("Jan", 1:31), paste("Feb", 1:29), paste("Mar", 1:31), paste("Apr", 1:30), paste("May", 1:31), paste("Jun", 1:15))
  #
  #pdf(paste0("../output/Figure_", file_name, ".pdf"), width = 9, height = 10)
  #svg(paste0("../output/Figure1_", file_name, ".svg"), width = 13, height = 5)
  #par(mai = c(1, 1, 0.5, 0.1))
  #layout(matrix(c(1:2), byrow = T, nrow = 1))
  pdf("../output/Figure1_.pdf", width = 14, height = 5.5)
  par(mfrow = c(1, 2), mar = c(4, 5, 3, 2))
  probbase1<- read.csv("../data/probability-14.csv", row.names = 1)
  prob1<-probbase1[-c(1:24),]
  qd<-prob1[1:68]
  ##########################################   Panel A  ##########################################################
  estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "Onset_expect"])
  estH <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "expect_H"])
  estA <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "expect_A"])
  estEa <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "expect_Ea"])
  estEi <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 5)[, "expect_Ei"])
  estEa_mean <- round(apply(estEa, 1, mean), 0)
  estEi_mean <- round(apply(estEi, 1, mean), 0)
  print("#########################S->E######")
  print(estEa_mean)
  print(estEi_mean)
  print(sum(estEa_mean))
  print(sum(estEi_mean))
  estAAI_up<- round(apply(as.matrix(apply(estEa,2,sum)/(apply(estEa,2,sum)+apply(estEi,2,sum))), 2, function(x) quantile(x, 0.975)), 5)
  estAAI_me<- round(apply(as.matrix(apply(estEa,2,sum)/(apply(estEa,2,sum)+apply(estEi,2,sum))), 2, mean), 5)
  estAAI_low<- round(apply(as.matrix(apply(estEa,2,sum)/(apply(estEa,2,sum)+apply(estEi,2,sum))), 2, function(x) quantile(x, 0.025)), 5)
  print(estAAI_up)
  print(estAAI_me)
  print(estAAI_low)
  
  
  print("#########################S->E######")
  
  estN_mean <- round(apply(estN_mat, 1, mean), 0)###P->I
  estHmean<-round(apply(estH, 1, mean), 0)###I->H
  estAmean<-round(apply(estA, 1, mean), 0)###P->A
  westN_mean<- estN_mean*qd 
  westN_up <- round(apply(estN_mat, 1, function(x) quantile(x, 0.975)), 0)*qd
  westN_low <- round(apply(estN_mat, 1, function(x) quantile(x, 0.025)), 0)*qd
  
  
  # start A
  plot(ptime, westN_mean, ylim = c(0, max(westN_up, onset_obs_all) * 1.05), xlab = "", ylab = "", type = "p", col = "white", pch = 16, xaxt="n", cex = 0.5)
  mtext("Onset date (2020)", side = 1, line  = 3, cex = 1.01)
  #mtext("No. of ascertained cases", side = 2, line = 3, cex = 1.01)
  mtext("Ascertained cases", side = 2, line = 3, cex = 1.01)
  #axis(1, at = seq(1, 68, 11), labels = mydate[seq(1, 68, 11)])
  axis(1, at = c(1,10, 23, 33, 48, 61), labels = mydate[c(1,10, 23, 33, 48, 61)])
  #
  abline(v = c(1,10, 23, 33, 48, 61), lty = 3, lwd = 3, col = "orange")
 # text(c(10, 23, 33, 48, 61), par()$usr[4], labels = mydate[c(10, 23, 33, 48, 61)], col = "darkgrey", pos = 3, xpd = T)
  #
  polygon(c(ptime[1:61], rev(ptime[1:61])), c(westN_up[1:61], rev(westN_low[1:61])), col = "#CEE6F4FF", border = NA)
  polygon(c(ptime[-c(1:60)], rev(ptime[-c(1:60)])), c(westN_up[-c(1:60)], rev(westN_low[-c(1:60)])), col = "#F39B7FB2", border = NA)
  points(ptime[1:60], westN_mean[1:60], col = "#3742D2FF", pch = 16, cex = 0.8)
  points(ptime[-c(1:60)], westN_mean[-c(1:60)], col = "#BC3C29FF", pch = 17, cex = 0.8)
  points(ptime, onset_obs_all, col = "black", pch = 3, cex = 0.8)
  #points(ptime[1:60], westN_mean[1:60], col = "#BC3C29FF", pch = 16, cex = 0.8)
  #points(ptime[-c(1:60)], westN_mean[-c(1:60)], col = "#0072B5FF", pch = 17, cex = 0.8)
  ##20854EFF
  print(westN_mean)
  print(estN_mean)
  print(estHmean)
  print(estAmean)
  print(sum(westN_mean))
  print(sum(estN_mean))
  print(sum(estHmean))
  print(sum(estAmean))
  print(sum(estAmean)+sum(estN_mean))
  westI_up<- round(apply( as.matrix(apply(qd*estN_mat,2,sum))  , 2, function(x) quantile(x, 0.975)), 5)
  westI_me<- round(apply( as.matrix(apply(qd*estN_mat,2,sum)), 2, mean), 5)
  westI_low<- round(apply( as.matrix(apply(qd*estN_mat,2,sum)), 2, function(x) quantile(x, 0.025)), 5)
  print(westI_up)
  print(westI_me)
  print(westI_low)
  print("##################################ALL-A###########################################")
  All_mean<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, mean), 5)
  All_up<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.975)), 5)
  All_low<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(All_mean)
  print(All_up)
  print(All_low)
  print("===========+I++A===============================")
  estIA_up <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, function(x) quantile(x, 0.975)), 5)
  estIA_me <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, mean), 5)
  estIA_low <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(estIA_me)
  print(estIA_up)
  print(estIA_low)
  print("===========+I/(+I++A)===========================")
  estIIA_up<- round(apply(as.matrix(apply(estN_mat,2,sum)/(apply(estN_mat,2,sum)+apply(estA,2,sum)))  , 2, function(x) quantile(x, 0.975)), 5)
  estIIA_me<- round(apply(as.matrix(apply(estN_mat,2,sum)/(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, mean), 5)
  estIIA_low<- round(apply(as.matrix(apply(estN_mat,2,sum)/(apply(estN_mat,2,sum)+apply(estA,2,sum))), 2, function(x) quantile(x, 0.025)), 5)
  print(estIIA_me)
  print(estIIA_up)
  print(estIIA_low)
  print("===========+H/+I===========================")
  estHI_up<- round(apply( as.matrix(32583/apply(estN_mat,2,sum))  , 2, function(x) quantile(x, 0.975)), 5)
  estHI_me<- round(apply( as.matrix(32583/apply(estN_mat,2,sum)), 2, mean), 5)
  estHI_low<- round(apply( as.matrix(32583/apply(estN_mat,2,sum)), 2, function(x) quantile(x, 0.025)), 5)
  
  print(estHI_me)
  print(estHI_up)
  print(estHI_low)
  
  #
  legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#3742D2FF", "#BC3C29FF"), pch = c(3, 16, 17), bty = "n")
  #legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#BC3C29FF", "#0072B5FF"), pch = c(4, 16, 17), bty = "n")
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "A", xpd = T, cex = 2)
  
  ##########################################   Panel B  ##########################################################
  estRt_mat <- apply(pars_estimate, 1, function(x) estimate_R(pars = x, init_settings = init_settings))
  estRt_mat <- t(estRt_mat)
  ##
  rt_mean <- sprintf("%.2f", round(apply(estRt_mat, 2, function(x) mean(x)), 2))
  rt_low <- sprintf("%.2f", round(apply(estRt_mat, 2, function(x) quantile(x, 0.025)), 2))
  rt_up <- sprintf("%.2f", round(apply(estRt_mat, 2, function(x) quantile(x, 0.975)), 2))
  #
  #vioplot(estRt_mat[, 1], estRt_mat[, 2], estRt_mat[, 3], estRt_mat[, 4], estRt_mat[, 5], names = NA, ylim = c(0, panel_B_R_ylim *1.5), col = c("#BC3C29FF","#0072B5FF", "#E18727FF", "#7876B1FF", "#FFDC91FF"), ylab = "", xlab = "")
  vioplot(estRt_mat[, 1], estRt_mat[, 2], estRt_mat[, 3], estRt_mat[, 4], estRt_mat[, 5], names = NA, ylim = c(0, panel_B_R_ylim *1.5), col = "#E95E1BFF", ylab = "", xlab = "")
  mtext("Outbreak period (2020)", side = 1, line  = 3, cex = 1.01)
  mtext(expression("R"["e"]), side = 2, line = 3, cex = 1.01)
  axis(1, at = c(1, 1.9, 3, 4, 5), tick = F, labels = c("Jan 1-9", "Jan 10-22", "Jan 23-Feb 1", "Feb 2-16", "Feb 17-Mar 8"))
  abline(h = 1, lwd = 2, lty = 2, col = "#BC3C29FF")
  #
  text(1, min(estRt_mat[, 1]) - 0.2, labels = rt_mean[1])
  text(1, min(estRt_mat[, 1]) - 0.45, labels = paste("(", rt_low[1], "-", rt_up[1], ")", sep = ""))
  text(2, min(estRt_mat[, 2]) - 0.2, labels = rt_mean[2])
  text(2, min(estRt_mat[, 2]) - 0.45, labels = paste("(", rt_low[2], "-", rt_up[2], ")", sep = ""))
  text(3, max(estRt_mat[, 3]) + 0.4, labels = rt_mean[3])
  text(3, max(estRt_mat[, 3]) + 0.15, labels = paste("(", rt_low[3], "-", rt_up[3], ")", sep = ""))
  text(4, max(estRt_mat[, 4]) + 0.4, labels = rt_mean[4])
  text(4, max(estRt_mat[, 4]) + 0.15, labels = paste("(", rt_low[4], "-", rt_up[4], ")", sep = ""))
  text(5, max(estRt_mat[, 5]) + 0.4, labels = rt_mean[5])
  text(5, max(estRt_mat[, 5]) + 0.15, labels = paste("(", rt_low[5], "-", rt_up[5], ")", sep = ""))
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "B", xpd = T, cex = 2)
  dev.off()
  ##########################################   Panel C  ##########################################################
  svg(paste0("../output/Figure2_", file_name, ".svg"), width = 11, height = 10)
  par(mai = c(1, 1,0.5,0.1))
  layout(matrix(c(1:4), byrow = T, nrow = 2))
  estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 4)[, "Onset_expect"])
  estA <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 4)[, "expect_A"])
  estN_mean <- round(apply(estN_mat, 1, mean), 0)
  westN_mean <-estN_mean*qd
  westN_up <- round(apply(estN_mat, 1, function(x) quantile(x, 0.975)), 0)*qd
  westN_low <- round(apply(estN_mat, 1, function(x) quantile(x, 0.025)), 0)*qd
  
  
  print("##################################ALL-C###########################################")
  All_mean<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, mean), 5)
  All_up<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.975)), 5)
  All_low<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(All_mean)
  print(All_up)
  print(All_low)
  # start C
  plot(ptime, westN_mean, ylim = c(0, max(westN_up, onset_obs_all) * 1.05), xlab = "", ylab = "", type = "p", col = "white", pch = 16, xaxt="n", cex = 0.5)
  mtext("Onset date (2020)", side = 1, line  = 3, cex = 1.01)
  #mtext("No. of ascertained cases", side = 2, line = 3, cex = 1.01)
  mtext("Ascertained cases", side = 2, line = 3, cex = 1.01)
  #axis(1, at = seq(1, 68, 11), labels = mydate[seq(1, 68, 11)])
  axis(1, at = c(1,10, 23, 33, 48, 61), labels = mydate[c(1,10, 23, 33, 48, 61)])
  #
  abline(v = c(1,10, 23, 33, 48, 61), lty = 3, lwd = 3, col = "orange")
  # text(c(10, 23, 33, 48, 61), par()$usr[4], labels = mydate[c(10, 23, 33, 48, 61)], col = "darkgrey", pos = 3, xpd = T)
  #
  polygon(c(ptime[1:48], rev(ptime[1:48])), c(westN_up[1:48], rev(westN_low[1:48])), col = "#CEE6F4FF", border = NA)
  polygon(c(ptime[-c(1:47)], rev(ptime[-c(1:47)])), c(westN_up[-c(1:47)], rev(westN_low[-c(1:47)])), col = "#F39B7FB2", border = NA)
  points(ptime[1:47], westN_mean[1:47], col = "#3742D2FF", pch = 16, cex = 0.8)
  points(ptime[-c(1:47)], westN_mean[-c(1:47)], col = "#BC3C29FF", pch = 17, cex = 0.8)
  points(ptime, onset_obs_all, col = "black", pch = 3, cex = 0.8)
  #
  legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black", "#3742D2FF", "#BC3C29FF"), pch = c(3, 16, 17), bty = "n")
  #legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#BC3C29FF", "#0072B5FF"), pch = c(4, 16, 17), bty = "n")
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "A", xpd = T, cex = 2)
  
  ##########################################   Panel D  ##########################################################
  estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 3)[, "Onset_expect"])
  estA <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 3)[, "expect_A"])
  estN_mean <- round(apply(estN_mat, 1, mean), 0)
  westN_mean <- estN_mean*qd
  westN_up <- round(apply(estN_mat, 1, function(x) quantile(x, 0.975)), 0)*qd
  westN_low <- round(apply(estN_mat, 1, function(x) quantile(x, 0.025)), 0)*qd
  print("##################################ALL-D###########################################")
  All_mean<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, mean), 5)
  All_up<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.975)), 5)
  All_low<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(All_mean)
  print(All_up)
  print(All_low)
  
  # start D
  plot(ptime, westN_mean, ylim = c(0, max(westN_up, onset_obs_all) * 1.05), xlab = "", ylab = "", type = "p", col = "white", pch = 16, xaxt="n", cex = 0.5)
  mtext("Onset date (2020)", side = 1, line  = 3, cex = 1.01)
  #mtext("No. of ascertained cases", side = 2, line = 3, cex = 1.01)
  mtext("Ascertained cases", side = 2, line = 3, cex = 1.01)
  #axis(1, at = seq(1, 68, 11), labels = mydate[seq(1, 68, 11)])
  axis(1, at = c(1,10, 23, 33, 48, 61), labels = mydate[c(1,10, 23, 33, 48, 61)])
  #
  abline(v = c(1,10, 23, 33, 48, 61), lty = 3, lwd = 3, col = "orange")
  # text(c(10, 23, 33, 48, 61), par()$usr[4], labels = mydate[c(10, 23, 33, 48, 61)], col = "darkgrey", pos = 3, xpd = T)
  #
  polygon(c(ptime[1:33], rev(ptime[1:33])), c(westN_up[1:33], rev(westN_low[1:33])), col ="#CEE6F4FF", border = NA)
  polygon(c(ptime[-c(1:32)], rev(ptime[-c(1:32)])), c(westN_up[-c(1:32)], rev(westN_low[-c(1:32)])), col = "#F39B7FB2", border = NA)
  points(ptime[1:32], westN_mean[1:32], col = "#3742D2FF", pch = 16, cex = 0.8)
  points(ptime[-c(1:32)], westN_mean[-c(1:32)], col = "#BC3C29FF", pch = 17, cex = 0.8)
  points(ptime, onset_obs_all, col = "black", pch = 3, cex = 0.8)
  #
  legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black", "#3742D2FF", "#BC3C29FF"), pch = c(3, 16, 17), bty = "n")
  #legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#BC3C29FF", "#0072B5FF"), pch = c(4, 16, 17), bty = "n")
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "B", xpd = T, cex = 2)
  
  ##########################################   Panel E  ##########################################################
  estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 2)[, "Onset_expect"])
  estA <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 2)[, "expect_A"])
  estS <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings, num_periods = 2)[, "S"])
  estS_mean <- round(apply(estS, 1, mean), 0)
  print("====================S===============")
  print(estS_mean)
  estN_mean <- round(apply(estN_mat, 1, mean), 0)
  westN_mean<-estN_mean*qd
  westN_up <- round(apply(estN_mat, 1, function(x) quantile(x, 0.975)), 0)*qd
  westN_low <- round(apply(estN_mat, 1, function(x) quantile(x, 0.025)), 0)*qd
  print("##################################ALL-E###########################################")
  All_mean<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, mean), 5)
  All_up<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.975)), 5)
  All_low<-round(apply((as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(All_mean)
  print(All_up)
  print(All_low)
  # start E
  plot(ptime, westN_mean, ylim = c(0, max(westN_up, onset_obs_all) * 1.05), xlab = "", ylab = "", type = "p", col = "white", pch = 16, xaxt="n", cex = 0.5)
  mtext("Onset date (2020)", side = 1, line  = 3, cex = 1.01)
  #mtext("No. of ascertained cases", side = 2, line = 3, cex = 1.01)
  mtext("Ascertained cases", side = 2, line = 3, cex = 1.01)
  #axis(1, at = seq(1, 68, 11), labels = mydate[seq(1, 68, 11)])
  axis(1, at = c(1,10, 23, 33, 48, 61), labels = mydate[c(1,10, 23, 33, 48, 61)])
  #
  abline(v = c(1,10, 23, 33, 48, 61), lty = 3, lwd = 3, col = "orange")
  # text(c(10, 23, 33, 48, 61), par()$usr[4], labels = mydate[c(10, 23, 33, 48, 61)], col = "darkgrey", pos = 3, xpd = T)
  #
  polygon(c(ptime[1:23], rev(ptime[1:23])), c(westN_up[1:23], rev(westN_low[1:23])), col = "#CEE6F4FF", border = NA)
  polygon(c(ptime[-c(1:22)], rev(ptime[-c(1:22)])), c(westN_up[-c(1:22)], rev(westN_low[-c(1:22)])), col = "#F39B7FB2", border = NA)
  points(ptime[1:22], westN_mean[1:22], col = "#3742D2FF", pch = 16, cex = 0.8)
  points(ptime[-c(1:22)], westN_mean[-c(1:22)], col = "#BC3C29FF", pch = 17, cex = 0.8)
  points(ptime, onset_obs_all, col = "black", pch = 3, cex = 0.8)
  #
  legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#3742D2FF", "#BC3C29FF"), pch = c(3, 16, 17), bty = "n")
  #legend("topleft", legend = c("Observed", "Fitted",  "Predicted"), col = c("black",  "#BC3C29FF", "#0072B5FF"), pch = c(4, 16, 17), bty = "n")
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "C", xpd = T, cex = 2)
  
  ##########################################   Panel F  ##########################################################
  init_settings1<-init_settings
  init_settings1$days_to_fit <- 1:165
  ptime <- 1:165
  print(init_settings1$days_to_fit)
  estAIP_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings1, num_periods = 5)[, c("Ie","Il", "Ae","Al", "P","E")])
  estN_mat <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings1, num_periods = 5)[, "Onset_expect"])
  estA <- apply(pars_estimate, 1, function(x) SEIRsimu(pars = x, init_settings = init_settings1, num_periods = 5)[, "expect_A"])
  estN_mean <- round(apply(estN_mat, 1, mean), 0)###P->I
  estAmean<-round(apply(estA, 1, mean), 0)###P->A
  estI_mat <- estAIP_mat[ptime, ]+estAIP_mat[ptime + length(ptime), ]
  estA_mat <- estAIP_mat[ptime + length(ptime)*2, ] + estAIP_mat[ptime + length(ptime) * 3, ]
  estP_mat <- estAIP_mat[ptime + length(ptime) * 4, ]
  estE_mat <- estAIP_mat[ptime + length(ptime) * 5, ]
  estI_mean <- apply(estI_mat, 1, mean)
  estA_mean <- apply(estA_mat, 1, mean)
  estP_mean <- apply(estP_mat, 1, mean)
  estAIP_dat <- rbind(estI_mean, estA_mean, estP_mean)
  print("========================ratio=========================")
  print(sum(estN_mean))
  print(sum(estAmean))
  estIA_up <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum)) ), 2, function(x) quantile(x, 0.975)), 5)
  estIA_me <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, mean), 5)
  estIA_low <- round(apply(32583/(as.matrix(apply(estN_mat,2,sum)+apply(estA,2,sum))) , 2, function(x) quantile(x, 0.025)), 5)
  print(estIA_me)
  print(estIA_up)
  print(estIA_low)
  print("========================AIP=========================")
  print(estAIP_dat)
  print("========================A+I+P=========================")
  AIP_mean<-round(apply((estI_mat+estA_mat+estP_mat), 1, mean),5)
  AIP_up<-round(apply((estI_mat+estA_mat+estP_mat), 1, function(x) quantile(x, 0.975)), 5)
  AIP_low<-round(apply((estI_mat+estA_mat+estP_mat), 1, function(x) quantile(x, 0.025)), 5)
  print(AIP_mean)
  print(AIP_up)
  print(AIP_low)
  print(AIP_mean[32])
  print(AIP_up[32])
  print(AIP_low[32])
  print(AIP_mean[68])
  print(AIP_up[68])
  print(AIP_low[68])
  print(AIP_mean[135])
  print(AIP_up[135])
  print(AIP_low[135])
  print("========================5.14=========================")
  A_mean <- round(apply(estA_mat, 1, mean),5)
  A_up <- round(apply(estA_mat, 1, function(x) quantile(x, 0.975)), 5)
  A_low <- round(apply(estA_mat, 1, function(x) quantile(x, 0.025)), 5)
  print(A_mean)
  print(A_up)
  print(A_low)
  print(A_mean[68])
  print(A_up[68])
  print(A_low[68])
  print(A_mean[78])
  print(A_up[78])
  print(A_low[78])
  print(A_mean[135])
  print(A_up[135])
  print(A_low[135])
  AIEP_mean<-round(apply((estI_mat+estA_mat+estP_mat+estE_mat), 1, mean),5)
  AIEP_up<-round(apply((estI_mat+estA_mat+estP_mat+estE_mat), 1, function(x) quantile(x, 0.975)), 5)
  AIEP_low<-round(apply((estI_mat+estA_mat+estP_mat+estE_mat), 1, function(x) quantile(x, 0.025)), 5)
  print(AIEP_mean)
  print(AIEP_up)
  print(AIEP_low)
  print(AIEP_mean[68])
  print(AIEP_up[68])
  print(AIEP_low[68])
  print(AIEP_mean[78])
  print(AIEP_up[78])
  print(AIEP_low[78])
  print(AIEP_mean[135])
  print(AIEP_up[135])
  print(AIEP_low[135])
  barpos <- barplot(estAIP_dat, col = c("#BC3C29FF",  "#FFDC91FF","#0072B5FF"), xlab = "", ylab = "", border = "NA")
  mtext("Date (2020)", side = 1, line  = 3, cex = 1.01)
  mtext("Active infectious cases", side = 2, line = 3, cex = 1.01)
  axis(1, at = barpos[seq(1, 165, 33)], labels = mydate[seq(1, 165, 33)])
 # legend("topright", legend = c("Presymptomatic (P)", "Unascertainable (A)", "Ascertainable (I)"), fill = c("#0072B5FF", "#FFDC91FF", "#BC3C29FF"), bty = "n")
  text(par()$usr[1] - (par()$usr[2] -par()$usr[1]) * 0.12, par()$usr[4] + (par()$usr[4] - par()$usr[3]) * 0.06, labels = "D", xpd = T, cex = 2)
  ##figure_F finished
  dev.off()
}
