# function.markov.S_t.df().r




#@ number.of.states = 3 ======
number.of.states = 3
transition.matrix = matrix(nrow = number.of.states, ncol = number.of.states)
colnames(transition.matrix) = paste0("S", 1:number.of.states, "(t-1)")
rownames(transition.matrix) = paste0("S", 1:number.of.states, "(t)")
transition.matrix
# > number.of.states = 3
# > transition.matrix = matrix(nrow = number.of.states, ncol = number.of.states)
# > transition.matrix
#       S1(t-1) S2(t-1) S3(t-1)
# S1(t)      NA      NA      NA
# S2(t)      NA      NA      NA
# S3(t)      NA      NA      NA

transition.matrix[,1] = c(0.75, 0.2, 0.05)
transition.matrix[,2] = c(0, 0.7, 0.3)
transition.matrix[,3] = c(0, 0, 1)
transition.matrix
# > transition.matrix
#       S1(t-1) S2(t-1) S3(t-1)
# S1(t)    0.75     0.0       0
# S2(t)    0.20     0.7       0
# S3(t)    0.05     0.3       1

for (i in 1:number.of.states) {
  transition.matrix[,i] = transition.matrix[,i] / sum(transition.matrix[,i])
}
transition.matrix
# > for (i in 1:number.of.states) {
# +   transition.matrix[,i] = transition.matrix[,i] / sum(transition.matrix[,i])
# + }
# > transition.matrix
#       S1(t-1) S2(t-1) S3(t-1)
# S1(t)    0.75     0.0       0
# S2(t)    0.20     0.7       0
# S3(t)    0.05     0.3       1

S_t.list = list()
# S_t.list[[1]] = as.matrix(rep(1, number.of.states) / number.of.states)
S_t.list[[1]] = c(1, rep(0, number.of.states-1)) %>% as.matrix
rownames(S_t.list[[1]]) = paste0("S", 0:(number.of.states-1), "(t)")
colnames(S_t.list[[1]]) = "t0"
S_t.list
# > S_t.list = list()
# > S_t.list[[1]] = as.matrix(rep(1, number.of.states) / number.of.states)
# > rownames(S_t.list[[1]]) = paste0("S", 1:number.of.states, "(t)")
# > colnames(S_t.list[[1]]) = "t1"
# > S_t.list
# [[1]]
#       t0
# S0(t)  1
# S1(t)  0
# S2(t)  0

transition.matrix %*% S_t.list[[1]] 
# > transition.matrix %*% S_t.list[[1]]
#         t0
# S1(t) 0.75
# S2(t) 0.20
# S3(t) 0.05


S_t.df = as.data.frame(S_t.list[[1]])
S_t.df
for (i in 2:10) {
  S_t.list[[i]] = transition.matrix %*% S_t.list[[i-1]]
  colnames(S_t.list[[i]]) = paste0("t",i-1)
  S_t.df = cbind(S_t.df, S_t.list[[i]])
}
# S_t.list
S_t.df
# > S_t.df
#       t0   t1     t2       t3        t4        t5        t6        t7        t8         t9
# S0(t)  1 0.75 0.5625 0.421875 0.3164062 0.2373047 0.1779785 0.1334839 0.1001129 0.07508469
# S1(t)  0 0.20 0.2900 0.315500 0.3052250 0.2769387 0.2413181 0.2045183 0.1698596 0.13892432
# S2(t)  0 0.05 0.1475 0.262625 0.3783688 0.4857566 0.5807034 0.6619978 0.7300275 0.78599100
(transition.matrix %^% 9) %*% S_t.list[[1]]
# > (transition.matrix %^% 9) %*% S_t.list[[1]]
#               t0
# S1(t) 0.07508469
# S2(t) 0.13892432
# S3(t) 0.78599100


# S_t.df = as.data.frame(S_t.list[[1]])
# # S_t.df = S_t.df %>% add_row(t0 = 0)
# S_t.df = S_t.df %>% rbind(0)
# row.names(S_t.df)[nrow(S_t.df)] = "Cycle Reward"
# S_t.df = S_t.df %>% rbind(0)
# row.names(S_t.df)[nrow(S_t.df)] = "Total Reward"
# S_t.df
# # > S_t.df
# #              t0
# # G1(t)         1
# # G2(t)         0
# # G3(t)         0
# # Cycle Reward  0
# # Total Reward  0

S_t.df = as.data.frame(S_t.list[[1]])
S_t.df
reward.per.state = c(1, 1, 0)
reward.list = list()
reward.list$CycleReward = list()
reward.list$TotalReward = list()
reward.list$CycleReward$t0 = 0
reward.list$TotalReward$t0 = 0
reward.list
# > reward.list
# $CycleReward
# $CycleReward$t1
# [1] 0
# $TotalReward
# $TotalReward$t1
# [1] 0


for (i in 2:10) {
  S_t.list[[i]] = transition.matrix %*% S_t.list[[i-1]]
  colnames(S_t.list[[i]]) = paste0("t",i-1)
  reward.list$CycleReward[[paste0("t",i-1)]] = sum(S_t.list[[i]] * reward.per.state)
  reward.list$TotalReward[[paste0("t",i-1)]] = reward.list$TotalReward[[paste0("t",i-2)]] + reward.list$CycleReward[[paste0("t",i-1)]]
  S_t.df = cbind(S_t.df, S_t.list[[i]])
}
S_t.df = S_t.df %>% rbind(as.data.frame(reward.list$CycleReward))
row.names(S_t.df)[nrow(S_t.df)] = "CycleReward"
S_t.df = S_t.df %>% rbind(as.data.frame(reward.list$TotalReward))
row.names(S_t.df)[nrow(S_t.df)] = "TotalReward"

# S_t.list
# reward.list
S_t.df
# > S_t.df
#             t0   t1     t2       t3        t4        t5        t6        t7        t8         t9
# S0(t)        1 0.75 0.5625 0.421875 0.3164062 0.2373047 0.1779785 0.1334839 0.1001129 0.07508469
# S1(t)        0 0.20 0.2900 0.315500 0.3052250 0.2769387 0.2413181 0.2045183 0.1698596 0.13892432
# S2(t)        0 0.05 0.1475 0.262625 0.3783688 0.4857566 0.5807034 0.6619978 0.7300275 0.78599100
# CycleReward  0 0.95 0.8525 0.737375 0.6216312 0.5142434 0.4192966 0.3380022 0.2699725 0.21400900
# TotalReward  0 0.95 1.8025 2.539875 3.1615062 3.6757497 4.0950463 4.4330485 4.7030210 4.91703004
S_t.df %>% t
# > S_t.df %>% t
#         S0(t)     S1(t)     S2(t) CycleReward TotalReward
# t0 1.00000000 0.0000000 0.0000000   0.0000000    0.000000
# t1 0.75000000 0.2000000 0.0500000   0.9500000    0.950000
# t2 0.56250000 0.2900000 0.1475000   0.8525000    1.802500
# t3 0.42187500 0.3155000 0.2626250   0.7373750    2.539875
# t4 0.31640625 0.3052250 0.3783688   0.6216312    3.161506
# t5 0.23730469 0.2769387 0.4857566   0.5142434    3.675750
# t6 0.17797852 0.2413181 0.5807034   0.4192966    4.095046
# t7 0.13348389 0.2045183 0.6619978   0.3380022    4.433048
# t8 0.10011292 0.1698596 0.7300275   0.2699725    4.703021
# t9 0.07508469 0.1389243 0.7859910   0.2140090    4.917030


#@ LY vs QALY ====

# reward.per.state
# # > reward.per.state
# # [1] 1 1 0
reward.vector.list = list()
reward.vector.list$LY = c(1,1,0)
reward.vector.list$QALY = c(1,.5,0)
reward.vector.list
# > reward.vector.list
# $LY
# [1] 1 1 0
# 
# $QALY
# [1] 1.0 0.5 0.0


S_t.df = as.data.frame(S_t.list[[1]])
S_t.df
# reward.per.state = c(1, 1, 0)
reward_t.list = list()
for (k in names(reward.vector.list)) {
  reward_t.list[[paste0("Cycle",k)]] = list()
  reward_t.list[[paste0("Cycle",k)]]$t0 = 0
  reward_t.list[[paste0("Total",k)]] = list()
  reward_t.list[[paste0("Total",k)]]$t0 = 0
}
reward_t.list
# > reward_t.list
# $CycleLY
# $CycleLY$t0
# [1] 0
# 
# 
# $TotalLY
# $TotalLY$t0
# [1] 0
# 
# 
# $CycleQALY
# $CycleQALY$t0
# [1] 0
# 
# 
# $TotalQALY
# $TotalQALY$t0
# [1] 0


for (i in 2:10) {
  S_t.list[[i]] = transition.matrix %*% S_t.list[[i-1]]
  colnames(S_t.list[[i]]) = paste0("t",i-1)
  S_t.df = cbind(S_t.df, S_t.list[[i]])
  for (k in names(reward.vector.list)) {
    reward_t.list[[paste0("Cycle",k)]][[paste0("t",i-1)]] = sum(S_t.list[[i]] * reward.vector.list[[k]])
    reward_t.list[[paste0("Total",k)]][[paste0("t",i-1)]] = reward_t.list[[paste0("Total",k)]][[paste0("t",i-2)]] + reward_t.list[[paste0("Cycle",k)]][[paste0("t",i-1)]]
  }
}

for (k in names(reward.vector.list)) {
  S_t.df = S_t.df %>% rbind(as.data.frame(reward_t.list[[paste0("Cycle",k)]]))
  row.names(S_t.df)[nrow(S_t.df)] = paste0("Cycle", k)
  S_t.df = S_t.df %>% rbind(as.data.frame(reward_t.list[[paste0("Total",k)]]))
  row.names(S_t.df)[nrow(S_t.df)] = paste0("Total", k)
}
S_t.df
# > S_t.df
#           t0   t1     t2       t3        t4        t5        t6        t7        t8         t9
# S0(t)      1 0.75 0.5625 0.421875 0.3164062 0.2373047 0.1779785 0.1334839 0.1001129 0.07508469
# S1(t)      0 0.20 0.2900 0.315500 0.3052250 0.2769387 0.2413181 0.2045183 0.1698596 0.13892432
# S2(t)      0 0.05 0.1475 0.262625 0.3783688 0.4857566 0.5807034 0.6619978 0.7300275 0.78599100
# CycleLY    0 0.95 0.8525 0.737375 0.6216312 0.5142434 0.4192966 0.3380022 0.2699725 0.21400900
# TotalLY    0 0.95 1.8025 2.539875 3.1615062 3.6757497 4.0950463 4.4330485 4.7030210 4.91703004
# CycleQALY  0 0.85 0.7075 0.579625 0.4690187 0.3757741 0.2986375 0.2357431 0.1850427 0.14454684
# TotalQALY  0 0.85 1.5575 2.137125 2.6061438 2.9819178 3.2805554 3.5162984 3.7013411 3.84588799

S_t.df %>% t
# > S_t.df %>% t
#         S0(t)     S1(t)     S2(t)   CycleLY  TotalLY CycleQALY TotalQALY
# t0 1.00000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000  0.000000
# t1 0.75000000 0.2000000 0.0500000 0.9500000 0.950000 0.8500000  0.850000
# t2 0.56250000 0.2900000 0.1475000 0.8525000 1.802500 0.7075000  1.557500
# t3 0.42187500 0.3155000 0.2626250 0.7373750 2.539875 0.5796250  2.137125
# t4 0.31640625 0.3052250 0.3783688 0.6216312 3.161506 0.4690187  2.606144
# t5 0.23730469 0.2769387 0.4857566 0.5142434 3.675750 0.3757741  2.981918
# t6 0.17797852 0.2413181 0.5807034 0.4192966 4.095046 0.2986375  3.280555
# t7 0.13348389 0.2045183 0.6619978 0.3380022 4.433048 0.2357431  3.516298
# t8 0.10011292 0.1698596 0.7300275 0.2699725 4.703021 0.1850427  3.701341
# t9 0.07508469 0.1389243 0.7859910 0.2140090 4.917030 0.1445468  3.845888


function.markov.S_t.df = function(transition.matrix, vector.initial.states, reward.vector.list = NA, number.of.cycles = 20) {
  if(dim(transition.matrix)[1] != dim(transition.matrix)[2]) {
    print("error")
    break()
  }
  number.of.states = nrow(transition.matrix)
  if(number.of.states != length(vector.initial.states)) {
    print("error")
    break()
  }
  for (i in 1:number.of.states) {
    if(sum(transition.matrix[,i]) != 1) {
      print("error")
      break()
    }
  }

  # colnames(transition.matrix) = paste0("G", 1:number.of.states, "(t-1)")
  # rownames(transition.matrix) = paste0("G", 1:number.of.states, "(t)")

  S_t.list = list()
  # S_t.list[[1]] = as.matrix(rep(1, number.of.states) / number.of.states)
  S_t.list[[1]] = as.matrix(vector.initial.states)
  rownames(S_t.list[[1]]) = paste0("S", 1:number.of.states, "(t)")
  colnames(S_t.list[[1]]) = "t0"
  
  reward_t.list = list()
  for (k in names(reward.vector.list)) {
    reward_t.list[[paste0("Cycle",k)]] = list()
    reward_t.list[[paste0("Cycle",k)]]$t0 = 0
    reward_t.list[[paste0("Total",k)]] = list()
    reward_t.list[[paste0("Total",k)]]$t0 = 0
  }

  S_t.df = as.data.frame(S_t.list[[1]])
  # S_t.df
  for (i in 2:number.of.cycles) {
    S_t.list[[i]] = transition.matrix %*% S_t.list[[i-1]]
    colnames(S_t.list[[i]]) = paste0("t",i-1)
    S_t.df = cbind(S_t.df, S_t.list[[i]])
    for (k in names(reward.vector.list)) {
      reward_t.list[[paste0("Cycle",k)]][[paste0("t",i-1)]] = sum(S_t.list[[i]] * reward.vector.list[[k]])
      reward_t.list[[paste0("Total",k)]][[paste0("t",i-1)]] = reward_t.list[[paste0("Total",k)]][[paste0("t",i-2)]] + reward_t.list[[paste0("Cycle",k)]][[paste0("t",i-1)]]
    }
  }
  # S_t.list
  
  for (k in names(reward.vector.list)) {
    S_t.df = S_t.df %>% rbind(as.data.frame(reward_t.list[[paste0("Cycle",k)]]))
    row.names(S_t.df)[nrow(S_t.df)] = paste0("Cycle", k)
    S_t.df = S_t.df %>% rbind(as.data.frame(reward_t.list[[paste0("Total",k)]]))
    row.names(S_t.df)[nrow(S_t.df)] = paste0("Total", k)
  }
  S_t.df = S_t.df %>% signif(digits = 3)
  S_t.df
}

function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1/3,1/3,1/3), reward.vector.list = NA, number.of.cycles = 20)
# > function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1/3,1/3,1/3), reward.vector.list = NA, number.of.cycles = 20)
#          t0   t1    t2    t3    t4     t5     t6     t7     t8     t9    t10    t11    t12     t13
# S1(t) 0.333 0.25 0.188 0.141 0.105 0.0791 0.0593 0.0445 0.0334 0.0250 0.0188 0.0141 0.0106 0.00792
# S2(t) 0.333 0.30 0.260 0.220 0.182 0.1480 0.1200 0.0956 0.0758 0.0598 0.0468 0.0365 0.0284 0.02200
# S3(t) 0.333 0.45 0.552 0.640 0.713 0.7730 0.8210 0.8600 0.8910 0.9150 0.9340 0.9490 0.9610 0.97000

function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1,0,0), reward.vector.list = NA, number.of.cycles = 20)
# > function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1,0,0), reward.vector.list = NA, number.of.cycles = 20)
#       t0   t1    t2    t3    t4    t5    t6    t7   t8     t9    t10    t11    t12    t13    t14
# S1(t)  1 0.75 0.562 0.422 0.316 0.237 0.178 0.133 0.10 0.0751 0.0563 0.0422 0.0317 0.0238 0.0178
# S2(t)  0 0.20 0.290 0.316 0.305 0.277 0.241 0.205 0.17 0.1390 0.1120 0.0898 0.0713 0.0563 0.0441
# S3(t)  0 0.05 0.148 0.263 0.378 0.486 0.581 0.662 0.73 0.7860 0.8310 0.8680 0.8970 0.9200 0.9380

function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1,0,0), reward.vector.list = reward.vector.list, number.of.cycles = 20)
# > function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = c(1,0,0), reward.vector.list = reward.vector.list, number.of.cycles = 20)
#           t0   t1    t2    t3    t4    t5    t6    t7    t8     t9    t10    t11    t12    t13
# S1(t)      1 0.75 0.562 0.422 0.316 0.237 0.178 0.133 0.100 0.0751 0.0563 0.0422 0.0317 0.0238
# S2(t)      0 0.20 0.290 0.316 0.305 0.277 0.241 0.205 0.170 0.1390 0.1120 0.0898 0.0713 0.0563
# S3(t)      0 0.05 0.148 0.263 0.378 0.486 0.581 0.662 0.730 0.7860 0.8310 0.8680 0.8970 0.9200
# CycleLY    0 0.95 0.852 0.737 0.622 0.514 0.419 0.338 0.270 0.2140 0.1690 0.1320 0.1030 0.0800
# TotalLY    0 0.95 1.800 2.540 3.160 3.680 4.100 4.430 4.700 4.9200 5.0900 5.2200 5.3200 5.4000
# CycleQALY  0 0.85 0.708 0.580 0.469 0.376 0.299 0.236 0.185 0.1450 0.1120 0.0872 0.0673 0.0519
# TotalQALY  0 0.85 1.560 2.140 2.610 2.980 3.280 3.520 3.700 3.8500 3.9600 4.0500 4.1100 4.1600


#@ end -----
