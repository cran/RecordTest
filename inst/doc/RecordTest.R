## ----loadLibs, include = FALSE-----------------------
library(RecordTest)
library(ggpubr)
data("TX_Zaragoza")
data("ZaragozaSeries")
library(knitr)
knitr::opts_chunk$set(
  cache = TRUE,
  comment = "#>",
  collapse = TRUE,
  digits = 5,
  tidy = FALSE,
  background = "#FFFF00",
  fig.align = 'center',
  warning = FALSE,
  message = FALSE
  )
RNGkind(sample.kind = 'Rounding')
options(width = 55, digits = 5)
theme_set(theme_bw())

## ----install, eval = FALSE---------------------------
#  install.packages("RecordTest")

## ----help, eval = FALSE------------------------------
#  help("RecordTest-package")

## ----records-----------------------------------------
library(RecordTest)
library(ggpubr) # To join plots
data(TX_Zaragoza)

records(TX_Zaragoza$TX, alpha = c(1, 1, 0.05, 1))

## ----pre-process-------------------------------------
TxZ <- series_split(TX_Zaragoza$TX, Mcols = 365)

TxZ <- TxZ[, series_uncor(TxZ)]

## ----------------------------------------------------
TxZ <- series_untie(TxZ, a = -0.5, b = 0.5, seed = 23)

## ----------------------------------------------------
L.plot(TxZ)

## ----------------------------------------------------
ggpubr::ggarrange(N.plot(TxZ) + ggplot2::ylim(1, 5.7),
        N.plot(series_rev(TxZ)) + ggplot2::ylim(1, 5.7),
        ncol = 2, nrow = 1, common.legend = TRUE, legend='bottom')

## ----------------------------------------------------
foster.plot(TxZ) + ggplot2::ylim(-1.80, 1.80)

## ----------------------------------------------------
foster.plot(TxZ, weights = function(t) t-1) + 
  ggplot2::ylim(-80, 80)

## ----------------------------------------------------
foster.test(TxZ, distribution = 'normal', weights = function(t) t-1)
foster.test(TxZ, distribution = 't', weights = function(t) t-1)

## ----------------------------------------------------
ggpubr::ggarrange(P_regression.plot(TxZ, record = 'upper') + 
            ggplot2::ylim(0, 6),
          P_regression.plot(TxZ, record = 'lower') +
            ggplot2::ylim(0, 6),
          P_regression.plot(series_rev(TxZ), record = 'upper') +
            ggplot2::ylim(0, 6),
          P_regression.plot(series_rev(TxZ), record = 'lower') +
            ggplot2::ylim(0, 6),
          ncol = 2, nrow = 2, common.legend = TRUE, legend='bottom')

## ----------------------------------------------------
P_regression.test(TxZ, record = 'upper')
P_regression.test(TxZ, record = 'lower')
P_regression.test(series_rev(TxZ), record = 'upper')
P_regression.test(series_rev(TxZ), record = 'lower')

## ----------------------------------------------------
L_global.test(TxZ, test = 'LM', statistic = 'G1', B = 10000, seed = 1:10000)

## ----------------------------------------------------
N_normal.test(TxZ, weights = function(t) t-1)
L_lm.test(TxZ, B = 10000, seed = 1:10000)
L_lr.test(TxZ, B = 10000, seed = 1:10000)
P_exactPB.test(TxZ)

## ----------------------------------------------------
ggpubr::ggarrange(
  P_regression.plot(TxZ, plot = 2),
  P_regression.plot(TxZ, plot = 3),
  ncol = 2, nrow = 1)

