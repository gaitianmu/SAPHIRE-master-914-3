## Stochastic SEIR model
## five periods: Jan 1-9 (index 1-9), Jan 10-22 (index 10-22), Jan 23-Feb 1 (index 23-32), Feb 2-16 (index 33-47), Feb 17- (index 48-60)
#' @param pars                    a vetor of parameters: c(b12, b3, b3, b5, r12, delta3, delta4, delta5)
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
#' @param lift_type               lift_type = 1: lifting all control measures at least zero_days days of zero ascertained cases; lift_type = 2: lifting all control measures zero_days days after the first day of zero ascertained cases
#' @param zero_days               number of zero days
#' @param outbreak_sign           the outbreak is defined if I >= outbreak_sign
#' @param return.Mat              whether return the prediction matrix 
#################################################################################################
SEIRresurge <- function(pars, init_settings, lift_type = 1, zero_days = 14, outbreak_sign = 100, return.Mat = F) {
  b_vec <- pars[1:4]
  b_vec <- c(b_vec[1], b_vec[1], b_vec[2:4])
  ##
  r12 <- pars[5]
  r3 <- 1 / (1 + (1 - r12) / (r12 * exp(pars[6])))
  r4 <- 1 / (1 + (1 - r3) / (r3 * exp(pars[7])))
  r5 <- 1 / (1 + (1 - r4) / (r4 * exp(pars[8])))
  r_vec <- c(r12,r12,r3,r4,r5)
  ##shuaijianxishu
  m_vec<-pars[9]
  m_vec<-c(m_vec)
  ###
  Die <- init_settings$Die
  Dil <- init_settings$Dil
  Dp <- init_settings$Dp
  De <- init_settings$De
  Dq_vec <- init_settings$Dq
  alpha <- init_settings$alpha
  Dh <- init_settings$Dh
  N <- init_settings$N
  flowN_vec <- init_settings$flowN
  init_states <- init_settings$init_states
  days_to_fit <- init_settings$days_to_fit
  ## ODE function based on stochastic SEIR model
  update_func <- function(stage_pars, states_old) {
    ## stage pars
    b <- stage_pars[1]
    r <- stage_pars[2]
    Dq <- stage_pars[3]
    #n <- stage_pars[4]
    n <- rpois(1, lambda = stage_pars[4])      ## stochastic, Poisson Distribution
    m<-stage_pars[5]
    ## old states number: c(S, E, P, I, A, H, R)
    S <- states_old[1]
    E <- states_old[2]
    P <- states_old[3]
    Ie <- states_old[4]
    Il <- states_old[5]
    Ae <- states_old[6]
    Al <- states_old[7]
    H <- states_old[8]
    R <- states_old[9]
    ## S
    ## meaning S->E, S->, S->S
    pS_vec <- c(b * (alpha * P +alpha * Ae+alpha *m * Al+ Ie +m*Il ) / N, n /N, 1 - b * (alpha * P +alpha * Ae+alpha *m * Al+ Ie +m*Il ) / N - n / N)
    sample_S <- rmultinom(1, size = S, prob = pS_vec)
    ## E
    ## meaning E->P, E->, E->E
    pE_vec <- c(1 / De, n / N, 1 - 1 / De - n / N)
    sample_E <- rmultinom(1, size = E, prob = pE_vec)
    ## P
    ## meaning P->Ie, P->Ae, P->, P->P
    pP_vec <- c(r / Dp, (1 - r) / Dp, n/N, 1 - 1 / Dp - n/N)
    sample_P <- rmultinom(1, size = P, prob = pP_vec)
    ## Ie
    ## meaning Ie->H, Ie->Il, Ie->Ie
    pIe_vec <- c(1 / Dq, 1 / Die, 1 - 1 / Dq - 1 / Die)
    sample_Ie <- rmultinom(1, size = Ie, prob = pIe_vec)
    ## Il
    ## meaning Il->H, Il->R, Il->Il
    pIl_vec <- c(1 / Dq, 1 / Dil, 1 - 1 / Dq - 1 / Dil)
    sample_Il <- rmultinom(1, size = Il, prob = pIl_vec)
    ## Ae
    ## meaning Ae->Al, Al->, Ae->Ae
    pAe_vec <- c(1 / Die, n /N, 1 - 1 / Die - n /N)
    sample_Ae <- rmultinom(1, size = Ae, prob = pAe_vec)
    ## Al
    ## meaning Al->R, Al->, Al->Al
    pAl_vec <- c(1 / Dil, n /N, 1 - 1 / Dil - n /N)
    sample_Al <- rmultinom(1, size = Al, prob = pAl_vec)
    ## H
    ## meaning H->R, H->H
    pH_vec <- c(1 / Dh, 1 - 1 / Dh)
    sample_H <- rmultinom(1, size = H, prob = pH_vec)
    ## R
    ## meaning R->, R->R
    pR_vec <- c(n /N, 1 - n /N)
    sample_R <- rmultinom(1, size = R, prob = pR_vec)
    ## new values
    S_new <- sample_S[3] + n
    E_new <- sample_E[3] + sample_S[1]
    P_new <- sample_P[4] + sample_E[1]
    Ie_new <- sample_Ie[3] + sample_P[1]
    Il_new <- sample_Ie[2] + sample_Il[3]
    Ae_new <- sample_Ae[3] + sample_P[2]
    Al_new <- sample_Al[3] + sample_Ae[1]
    H_new <- sample_H[2] + sample_Ie[1] + sample_Il[1]
    R_new <- sample_R[2] + sample_Il[2] + sample_Al[1] + sample_H[1]
    Onset_expect <- sample_P[1]
    expect_H<-sample_Ie[1]+sample_Il[1]
    expect_A<-sample_P[2]
    ##
    return(c(S_new, E_new, P_new, Ie_new,Il_new, Ae_new,Al_new, H_new, R_new, Onset_expect,expect_H,expect_A))
  }
  ## matrix for results
  states_mat <- matrix(NA, 700, length(init_states) + 4)
  states_mat[, 1] <- 1:700
  colnames(states_mat) <- c("time", "S", "E", "P", "Ie","Il", "Ae","Al", "H", "R", "Onset_expect","expect_H","expect_A")
  ## evovle the system according to the discretized ODEs
  stage_start <- c(1, 10, 23, 33, 48)               # corresponding to dates Jan1, Jan10, Jan23, Feb2, Feb17
  stage_end <- c(9, 22, 32, 47, 700)                # corresponding to dates Jan9, Jan22, Feb1, Feb16, the last day (we set it as the the epidemic will end if continue the surveillance and interventions )
  ##
  myold_states <- init_states
  for (i_stage in 1:5) {
    stage_pars_setings <-  c(b = b_vec[i_stage], r = r_vec[i_stage],Dq = Dq_vec[i_stage], n = flowN_vec[i_stage],m =m_vec)
    for (d in stage_start[i_stage]:stage_end[i_stage]) {
      states_mat[d, -1] <- update_func(stage_pars = stage_pars_setings, states_old = myold_states)
      myold_states <- states_mat[d, -1]
    }
  }
  ## 
  if(lift_type == 1) { ## lifting all control measures at least zero_days days of zero ascertained cases
    find0Func <- function(freedays, onset_num) {
      for(i in 50:700) {
        tmpN <- onset_num[i:(i + freedays - 1)]
        if(all(tmpN == 0)) {
          break
        }
      }
      return(i + freedays - 1)
    }
    start_index <- find0Func(freedays = zero_days, onset_num = states_mat[, "expect_H"])
    states_mat[-c(1:start_index), -1] <- NA
    #
    myold_states <- states_mat[start_index, -1]
    stage_pars_setings <- c(b = b_vec[1], r = r_vec[1], Dq = Dq_vec[5], n = flowN_vec[1],m =m_vec)
    for(t in 1:731) { ## We assume the ourbreak will happen in one year
      mynew_values <- update_func(stage_pars = stage_pars_setings, states_old = myold_states)
      myold_states <- mynew_values
      states_mat[start_index + t, -1] <- mynew_values
      if((mynew_values[4]+mynew_values[5]) >= outbreak_sign) { # 1:sum(mynew_values[c(3:5)]) >= outbreak_sign; 2:mynew_values[4]
        # cat(t, " Bad news, outbreak resurge !!!", fill = T)
        break
      } else if(sum(mynew_values[c(2:7)]) == 0) {  ## Stationary state, epidemic ends
        t <- NA
        break
      }
    }
  }
  ##
  #if(lift_type == 2) { ## lifting all control measures zero_days days after the first day of zero ascertained cases
   # start_index <- which(states_mat[,"Onset_expect"] == 0)[1]   ## first day of zero onset....
  #  states_mat[-c(1:(start_index + zero_days - 1)), -1] <- NA
   # myold_states <- states_mat[start_index + zero_days - 1, -1]
  #  stage_pars_setings <- c(b = b_vec[1], r = r_vec[1], Dq = Dq_vec[5], n = flowN_vec[1],m =m_vec)
   # for(t in 1:731) { ## We assume the ourbreak will happen in one year
  #    mynew_values <- update_func(stage_pars = stage_pars_setings, states_old = myold_states)
   #   myold_states <- mynew_values
  #    states_mat[start_index + zero_days - 1 + t, -1] <- mynew_values
   #   if((mynew_values[4]+mynew_values[5]) >= outbreak_sign) { # 1:sum(mynew_values[c(3:5)]) >= outbreak_sign; 2:mynew_values[4]
       # # cat(t, " Bad news, outbreak resurge !!!", fill = T)
  #      break
   #   } else if(sum(mynew_values[c(2:7)]) == 0) {  ## Stationary state, epidemic ends
  #      t <- NA
   #    break
  #   }
  #  }
#  }
  if(return.Mat) {
    return(states_mat)
  } else {
    return(t)
  }
}


