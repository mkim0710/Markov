# Matrix(ICU) StateList.r


StateList <- read_csv("H:/Dropbox/[[[[Rproject]]]]/Rproject_MH/Rproject_Markov/StateList.csv")
# Parsed with column specification:
# cols(
#   `State Code` = col_character(),
#   `State Description` = col_character(),
#   `Percent estimate` = col_double()
# )
Matrix_ICU_ <- read_csv("H:/Dropbox/[[[[Rproject]]]]/Rproject_MH/Rproject_Markov/Matrix(ICU).csv")
# Parsed with column specification:
# cols(
#   `Sn(t+1) \ Sn(t)` = col_character(),
#   `At home without home care` = col_double(),
#   `At home with home care` = col_double(),
#   `EOL prognosis at home with home care` = col_integer(),
#   `Chronic conditions in nursing home` = col_double(),
#   `EOL prognosis in nursing home` = col_double(),
#   `Acute illness requiring ED care` = col_double(),
#   `EOL prognosis requiring acute care in ED` = col_double(),
#   `Acute illness requiring inpatient care` = col_character(),
#   `EOL prognosis requiring inpatient care` = col_character(),
#   `EOL care in complex continuing care (CCC)` = col_character(),
#   `EOL care in palliative care ward or ALC` = col_character(),
#   `Palliative prognosis in non-home hospice` = col_character(),
#   `EOL prognosis requiring ICU care` = col_character()
# )

StateList[is.na(StateList)] = 0
StateList
# > StateList
# # A tibble: 13 ¡¿ 3
#    `State Code`                       `State Description` `Percent estimate`
#           <chr>                                     <chr>              <dbl>
# 1            S1                 At home without home care              51.07
# 2            S2                    At home with home care              17.34
# 3            S3      EOL prognosis at home with home care               9.41
# 4            S4        Chronic conditions in nursing home              13.91
# 5            S5             EOL prognosis in nursing home               6.66
# 6            S6           Acute illness requiring ED care               0.22
# 7            S7  EOL prognosis requiring acute care in ED               0.04
# 8            S8    Acute illness requiring inpatient care               0.15
# 9            S9    EOL prognosis requiring inpatient care               0.03
# 10          S10 EOL care in complex continuing care (CCC)               1.17
# 11          S11   EOL care in palliative care ward or ALC               0.00
# 12          S12  Palliative prognosis in non-home hospice               0.00
# 13          S13          EOL prognosis requiring ICU care               0.00


library(tidyverse)
Matrix_ICU_[is.na(Matrix_ICU_)] = 0
Matrix_ICU_ %>% as.matrix


vector.initial.states = StateList$`Percent estimate` / 100
vector.initial.states
length(vector.initial.states)
# > vector.initial.states
#  [1] 0.5107 0.1734 0.0941 0.1391 0.0666 0.0022 0.0004 0.0015 0.0003 0.0117 0.0000 0.0000 0.0000
# > length(vector.initial.states)
# [1] 13

transition.matrix = Matrix_ICU_[,-1] %>% as.matrix
dim(transition.matrix)
# > transition.matrix = Matrix_ICU_[,-1] %>% as.matrix
# > dim(transition.matrix)
# [1] 13 13

function.markov.S_t.df = function(transition.matrix, vector.initial.states, reward.vector.list = NA, number.of.cycles = 20) {
  if(dim(transition.matrix)[1] != dim(transition.matrix)[2]) {
    print("error1")
    break()
  }
  number.of.states = nrow(transition.matrix)
  if(number.of.states != length(vector.initial.states)) {
    print("error2")
    break()
  }
  for (i in 1:number.of.states) {
    if(sum(transition.matrix[,i]) != 1) {
      print("error3")
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
  S_t.df = S_t.df %>% signif(digits = 3) %>% t
  # if(nrow(S_t.df > 10)) {
  #   # print(
  #   #   ((1:(floor((nrow(S_t.df)-0)/10)))-1) * 10 + 1
  #   # )
  #   S_t.df = S_t.df[((1:(floor((nrow(S_t.df)-0)/10)))-1) * 10 + 1, ]
  # }
  S_t.df
}


function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = vector.initial.states, reward.vector.list = NA, number.of.cycles = 50)
# > function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = vector.initial.states, reward.vector.list = NA, number.of.cycles = 50)
# [1]  1 11 21 31 41
#     S1(t) S2(t)  S3(t) S4(t)  S5(t)   S6(t)    S7(t)  S8(t)    S9(t)   S10(t)  S11(t)  S12(t)  S13(t)
# t0  0.511 0.173 0.0941 0.139 0.0666 0.00220 0.000400 0.0015 0.000300 0.011700 0.00000 0.00000 0.00000
# t10 0.454 0.205 0.0941 0.137 0.0653 0.00611 0.000294 0.0161 0.000193 0.004630 0.00752 0.00718 0.00296
# t20 0.403 0.231 0.0941 0.135 0.0640 0.00591 0.000287 0.0217 0.000189 0.001870 0.02830 0.01000 0.00451
# t30 0.358 0.252 0.0941 0.133 0.0627 0.00570 0.000282 0.0229 0.000185 0.000791 0.05390 0.01120 0.00487
# t40 0.318 0.269 0.0941 0.131 0.0615 0.00550 0.000276 0.0227 0.000181 0.000370 0.08020 0.01170 0.00486

function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = vector.initial.states, reward.vector.list = NA, number.of.cycles = 365)
# > function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = vector.initial.states, reward.vector.list = NA, number.of.cycles = 365)
#  [1]   1  11  21  31  41  51  61  71  81  91 101 111 121 131 141 151 161 171 181 191 201 211 221 231 241 251
# [27] 261 271 281 291 301 311 321 331 341 351
#        S1(t) S2(t)  S3(t)  S4(t)  S5(t)   S6(t)    S7(t)   S8(t)    S9(t)   S10(t)  S11(t)  S12(t)  S13(t)
# t0   0.51100 0.173 0.0941 0.1390 0.0666 0.00220 0.000400 0.00150 3.00e-04 1.17e-02 0.00000 0.00000 0.00000
# t10  0.45400 0.205 0.0941 0.1370 0.0653 0.00611 0.000294 0.01610 1.93e-04 4.63e-03 0.00752 0.00718 0.00296
# t20  0.40300 0.231 0.0941 0.1350 0.0640 0.00591 0.000287 0.02170 1.89e-04 1.87e-03 0.02830 0.01000 0.00451
# t30  0.35800 0.252 0.0941 0.1330 0.0627 0.00570 0.000282 0.02290 1.85e-04 7.91e-04 0.05390 0.01120 0.00487
# t40  0.31800 0.269 0.0941 0.1310 0.0615 0.00550 0.000276 0.02270 1.81e-04 3.70e-04 0.08020 0.01170 0.00486
# t50  0.28300 0.283 0.0941 0.1290 0.0603 0.00531 0.000271 0.02210 1.78e-04 2.05e-04 0.10600 0.01200 0.00474
# t60  0.25100 0.293 0.0941 0.1270 0.0591 0.00513 0.000265 0.02140 1.74e-04 1.39e-04 0.13100 0.01210 0.00459
# t70  0.22300 0.301 0.0941 0.1250 0.0579 0.00495 0.000260 0.02070 1.71e-04 1.13e-04 0.15600 0.01220 0.00444
# t80  0.19800 0.306 0.0941 0.1230 0.0568 0.00477 0.000255 0.02000 1.67e-04 1.01e-04 0.17900 0.01230 0.00429
# t90  0.17600 0.310 0.0941 0.1210 0.0557 0.00461 0.000250 0.01930 1.64e-04 9.56e-05 0.20200 0.01240 0.00414
# t100 0.15700 0.311 0.0941 0.1190 0.0546 0.00445 0.000245 0.01860 1.61e-04 9.23e-05 0.22400 0.01250 0.00399
# t110 0.13900 0.311 0.0941 0.1170 0.0535 0.00429 0.000240 0.01790 1.58e-04 9.00e-05 0.24500 0.01260 0.00386
# t120 0.12400 0.310 0.0941 0.1150 0.0524 0.00415 0.000236 0.01730 1.55e-04 8.80e-05 0.26600 0.01270 0.00372
# t130 0.11000 0.308 0.0941 0.1140 0.0514 0.00400 0.000231 0.01670 1.52e-04 8.62e-05 0.28600 0.01280 0.00359
# t140 0.09760 0.304 0.0941 0.1120 0.0504 0.00386 0.000226 0.01610 1.49e-04 8.44e-05 0.30500 0.01280 0.00347
# t150 0.08670 0.300 0.0941 0.1100 0.0494 0.00373 0.000222 0.01560 1.46e-04 8.27e-05 0.32400 0.01290 0.00335
# t160 0.07710 0.296 0.0941 0.1080 0.0484 0.00360 0.000218 0.01500 1.43e-04 8.11e-05 0.34100 0.01300 0.00324
# t170 0.06850 0.290 0.0941 0.1060 0.0475 0.00348 0.000213 0.01450 1.40e-04 7.95e-05 0.35900 0.01310 0.00312
# t180 0.06080 0.285 0.0941 0.1050 0.0465 0.00336 0.000209 0.01400 1.37e-04 7.79e-05 0.37500 0.01310 0.00302
# t190 0.05410 0.278 0.0941 0.1030 0.0456 0.00324 0.000205 0.01350 1.34e-04 7.64e-05 0.39200 0.01320 0.00291
# t200 0.04800 0.272 0.0941 0.1010 0.0447 0.00313 0.000201 0.01310 1.32e-04 7.49e-05 0.40700 0.01330 0.00281
# t210 0.04270 0.266 0.0941 0.0994 0.0438 0.00302 0.000197 0.01260 1.29e-04 7.34e-05 0.42200 0.01330 0.00272
# t220 0.03790 0.259 0.0941 0.0978 0.0430 0.00292 0.000193 0.01220 1.27e-04 7.20e-05 0.43700 0.01340 0.00263
# t230 0.03370 0.252 0.0941 0.0961 0.0421 0.00282 0.000189 0.01180 1.24e-04 7.06e-05 0.45100 0.01350 0.00254
# t240 0.02990 0.245 0.0941 0.0945 0.0413 0.00272 0.000185 0.01140 1.22e-04 6.92e-05 0.46500 0.01350 0.00245
# t250 0.02660 0.238 0.0941 0.0929 0.0405 0.00263 0.000182 0.01100 1.19e-04 6.78e-05 0.47800 0.01360 0.00237
# t260 0.02360 0.231 0.0941 0.0914 0.0397 0.00254 0.000178 0.01060 1.17e-04 6.65e-05 0.49100 0.01360 0.00229
# t270 0.02100 0.224 0.0941 0.0898 0.0389 0.00246 0.000175 0.01030 1.15e-04 6.51e-05 0.50300 0.01370 0.00221
# t280 0.01870 0.218 0.0941 0.0883 0.0381 0.00237 0.000171 0.00991 1.12e-04 6.39e-05 0.51500 0.01380 0.00213
# t290 0.01660 0.211 0.0941 0.0868 0.0374 0.00229 0.000168 0.00957 1.10e-04 6.26e-05 0.52600 0.01380 0.00206
# t300 0.01470 0.204 0.0941 0.0853 0.0366 0.00221 0.000165 0.00925 1.08e-04 6.14e-05 0.53700 0.01390 0.00199
# t310 0.01310 0.198 0.0941 0.0838 0.0359 0.00214 0.000161 0.00893 1.06e-04 6.02e-05 0.54800 0.01390 0.00192
# t320 0.01160 0.191 0.0941 0.0824 0.0352 0.00207 0.000158 0.00863 1.04e-04 5.90e-05 0.55900 0.01400 0.00186
# t330 0.01030 0.185 0.0941 0.0809 0.0345 0.00200 0.000155 0.00834 1.02e-04 5.78e-05 0.56900 0.01400 0.00180
# t340 0.00918 0.179 0.0941 0.0795 0.0338 0.00193 0.000152 0.00806 9.97e-05 5.67e-05 0.57800 0.01410 0.00174
# t350 0.00816 0.173 0.0941 0.0781 0.0332 0.00187 0.000149 0.00779 9.78e-05 5.56e-05 0.58800 0.01410 0.00168

result = function.markov.S_t.df(transition.matrix = transition.matrix, vector.initial.states = vector.initial.states, reward.vector.list = NA, number.of.cycles = 365)
result %>% tail
# > result %>% tail
#        S1(t) S2(t)  S3(t)  S4(t)  S5(t)   S6(t)    S7(t)   S8(t)    S9(t)   S10(t) S11(t) S12(t)  S13(t)
# t359 0.00733 0.168 0.0941 0.0769 0.0326 0.00181 0.000146 0.00755 9.60e-05 5.46e-05  0.596 0.0142 0.00163
# t360 0.00725 0.167 0.0941 0.0768 0.0325 0.00180 0.000146 0.00753 9.59e-05 5.45e-05  0.597 0.0142 0.00162
# t361 0.00716 0.167 0.0941 0.0766 0.0324 0.00180 0.000146 0.00750 9.57e-05 5.43e-05  0.598 0.0142 0.00162
# t362 0.00708 0.166 0.0941 0.0765 0.0324 0.00179 0.000145 0.00748 9.55e-05 5.42e-05  0.599 0.0142 0.00161
# t363 0.00700 0.165 0.0941 0.0764 0.0323 0.00178 0.000145 0.00745 9.53e-05 5.41e-05  0.599 0.0142 0.00161
# t364 0.00691 0.165 0.0941 0.0762 0.0323 0.00178 0.000145 0.00742 9.51e-05 5.40e-05  0.600 0.0142 0.00160



#@ end -----
