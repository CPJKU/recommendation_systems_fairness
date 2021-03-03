library(haven)
library(here)
library(readr)
library(car)
library(psych)
library(ggpubr)
library(ggplot2)
library(onewaytests)
library(tseries)
library(poolr)
library(dplyr)
library(corrplot)
library(FSA)
library(lsr)
library(rstatix)
library(readr)
library(scran)
library(metafor)
library(rcompanion)

my_path <- here::here()
sampling <- c("D", "N", "U")
partitioning <- c(0, 1, 2, 3, 4)
all_levels <- c(3, 5, 10, 20, 50)
RESULTS <- list()

get_z <- function(comparison, p_values, RESULTS) {
  RESULTS <- c(RESULTS, comparison)
  #z <- combinePValues(p_values, method="z", weights=1:5)
  #z <- combinePValues(p_values, method="holm-middle")
  z <- combinePValues(p_values, method="berger")
  RESULTS <- c(RESULTS, z)
  return <- RESULTS
}

for (samp_group in sampling){
  RESULTS <- c(RESULTS, "NDCG")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "MALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_m_3_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_3`[!is.na(data_0_pop$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_poplabel <- c(rep('D0_m_3_ndcg_pop', length(D0_m_3_ndcg_pop)))
      D0_m_3_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_3`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_itemknnlabel <- c(rep('D0_m_3_ndcg_itemknn', length(D0_m_3_ndcg_itemknn)))
      D0_m_3_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_3`[!is.na(data_0_als$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_alslabel <- c(rep('D0_m_3_ndcg_als', length(D0_m_3_ndcg_als)))
      D0_m_3_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_3`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_bprlabel <- c(rep('D0_m_3_ndcg_bpr', length(D0_m_3_ndcg_bpr)))
      D0_m_3_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_3`[!is.na(data_0_slim$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_slimlabel <- c(rep('D0_m_3_ndcg_slim', length(D0_m_3_ndcg_slim)))
      D0_m_3_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_3`[!is.na(data_0_vae$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_vaelabel <- c(rep('D0_m_3_ndcg_vae', length(D0_m_3_ndcg_vae)))
      # combine data
      ndcg_3 <- c(D0_m_3_ndcg_pop, D0_m_3_ndcg_itemknn, D0_m_3_ndcg_als, D0_m_3_ndcg_bpr, D0_m_3_ndcg_slim, D0_m_3_ndcg_vae)
      ndcg_3label <- c(D0_m_3_ndcg_poplabel, D0_m_3_ndcg_itemknnlabel, D0_m_3_ndcg_alslabel, D0_m_3_ndcg_bprlabel, D0_m_3_ndcg_slimlabel, D0_m_3_ndcg_vaelabel)
      
      D0_m_5_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_5`[!is.na(data_0_pop$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_poplabel <- c(rep('D0_m_5_ndcg_pop', length(D0_m_5_ndcg_pop)))
      D0_m_5_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_5`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_itemknnlabel <- c(rep('D0_m_5_ndcg_itemknn', length(D0_m_5_ndcg_itemknn)))
      D0_m_5_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_5`[!is.na(data_0_als$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_alslabel <- c(rep('D0_m_5_ndcg_als', length(D0_m_5_ndcg_als)))
      D0_m_5_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_5`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_bprlabel <- c(rep('D0_m_5_ndcg_bpr', length(D0_m_5_ndcg_bpr)))
      D0_m_5_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_5`[!is.na(data_0_slim$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_slimlabel <- c(rep('D0_m_5_ndcg_slim', length(D0_m_5_ndcg_slim)))
      D0_m_5_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_5`[!is.na(data_0_vae$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_vaelabel <- c(rep('D0_m_5_ndcg_vae', length(D0_m_5_ndcg_vae)))
      # combine data
      ndcg_5 <- c(D0_m_5_ndcg_pop, D0_m_5_ndcg_itemknn, D0_m_5_ndcg_als, D0_m_5_ndcg_bpr, D0_m_5_ndcg_slim, D0_m_5_ndcg_vae)
      ndcg_5label <- c(D0_m_5_ndcg_poplabel, D0_m_5_ndcg_itemknnlabel, D0_m_5_ndcg_alslabel, D0_m_5_ndcg_bprlabel, D0_m_5_ndcg_slimlabel, D0_m_5_ndcg_vaelabel)
      
      D0_m_10_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_10`[!is.na(data_0_pop$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_poplabel <- c(rep('D0_m_10_ndcg_pop', length(D0_m_10_ndcg_pop)))
      D0_m_10_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_10`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_itemknnlabel <- c(rep('D0_m_10_ndcg_itemknn', length(D0_m_10_ndcg_itemknn)))
      D0_m_10_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_10`[!is.na(data_0_als$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_alslabel <- c(rep('D0_m_10_ndcg_als', length(D0_m_10_ndcg_als)))
      D0_m_10_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_10`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_bprlabel <- c(rep('D0_m_10_ndcg_bpr', length(D0_m_10_ndcg_bpr)))
      D0_m_10_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_10`[!is.na(data_0_slim$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_slimlabel <- c(rep('D0_m_10_ndcg_slim', length(D0_m_10_ndcg_slim)))
      D0_m_10_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_10`[!is.na(data_0_vae$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_vaelabel <- c(rep('D0_m_10_ndcg_vae', length(D0_m_10_ndcg_vae)))
      # combine data
      ndcg_10 <- c(D0_m_10_ndcg_pop, D0_m_10_ndcg_itemknn, D0_m_10_ndcg_als, D0_m_10_ndcg_bpr, D0_m_10_ndcg_slim, D0_m_10_ndcg_vae)
      ndcg_10label <- c(D0_m_10_ndcg_poplabel, D0_m_10_ndcg_itemknnlabel, D0_m_10_ndcg_alslabel, D0_m_10_ndcg_bprlabel, D0_m_10_ndcg_slimlabel, D0_m_10_ndcg_vaelabel)
      
      D0_m_20_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_20`[!is.na(data_0_pop$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_poplabel <- c(rep('D0_m_20_ndcg_pop', length(D0_m_20_ndcg_pop)))
      D0_m_20_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_20`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_itemknnlabel <- c(rep('D0_m_20_ndcg_itemknn', length(D0_m_20_ndcg_itemknn)))
      D0_m_20_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_20`[!is.na(data_0_als$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_alslabel <- c(rep('D0_m_20_ndcg_als', length(D0_m_20_ndcg_als)))
      D0_m_20_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_20`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_bprlabel <- c(rep('D0_m_20_ndcg_bpr', length(D0_m_20_ndcg_bpr)))
      D0_m_20_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_20`[!is.na(data_0_slim$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_slimlabel <- c(rep('D0_m_20_ndcg_slim', length(D0_m_20_ndcg_slim)))
      D0_m_20_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_20`[!is.na(data_0_vae$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_vaelabel <- c(rep('D0_m_20_ndcg_vae', length(D0_m_20_ndcg_vae)))
      # combine data
      ndcg_20 <- c(D0_m_20_ndcg_pop, D0_m_20_ndcg_itemknn, D0_m_20_ndcg_als, D0_m_20_ndcg_bpr, D0_m_20_ndcg_slim, D0_m_20_ndcg_vae)
      ndcg_20label <- c(D0_m_20_ndcg_poplabel, D0_m_20_ndcg_itemknnlabel, D0_m_20_ndcg_alslabel, D0_m_20_ndcg_bprlabel, D0_m_20_ndcg_slimlabel, D0_m_20_ndcg_vaelabel)
      
      D0_m_50_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_50`[!is.na(data_0_pop$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_poplabel <- c(rep('D0_m_50_ndcg_pop', length(D0_m_50_ndcg_pop)))
      D0_m_50_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_50`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_itemknnlabel <- c(rep('D0_m_50_ndcg_itemknn', length(D0_m_50_ndcg_itemknn)))
      D0_m_50_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_50`[!is.na(data_0_als$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_alslabel <- c(rep('D0_m_50_ndcg_als', length(D0_m_50_ndcg_als)))
      D0_m_50_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_50`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_bprlabel <- c(rep('D0_m_50_ndcg_bpr', length(D0_m_50_ndcg_bpr)))
      D0_m_50_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_50`[!is.na(data_0_slim$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_slimlabel <- c(rep('D0_m_50_ndcg_slim', length(D0_m_50_ndcg_slim)))
      D0_m_50_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_50`[!is.na(data_0_vae$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_vaelabel <- c(rep('D0_m_50_ndcg_vae', length(D0_m_50_ndcg_vae)))
      # combine data
      ndcg_50 <- c(D0_m_50_ndcg_pop, D0_m_50_ndcg_itemknn, D0_m_50_ndcg_als, D0_m_50_ndcg_bpr, D0_m_50_ndcg_slim, D0_m_50_ndcg_vae)
      ndcg_50label <- c(D0_m_50_ndcg_poplabel, D0_m_50_ndcg_itemknnlabel, D0_m_50_ndcg_alslabel, D0_m_50_ndcg_bprlabel, D0_m_50_ndcg_slimlabel, D0_m_50_ndcg_vaelabel)
      # make dataframe
      ndcg_m_D <- data.frame(ndcg_3, ndcg_3label, ndcg_5, ndcg_5label, ndcg_10, ndcg_10label, ndcg_20, ndcg_20label, ndcg_50, ndcg_50label)
      #View(ndcg_m_D)
      if(threshold == 3){
        dunn <- dunn_test(ndcg_3 ~ ndcg_3label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(ndcg_5 ~ ndcg_5label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(ndcg_10 ~ ndcg_10label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(ndcg_20 ~ ndcg_20label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(ndcg_50 ~ ndcg_50label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}


for (samp_group in sampling){
  RESULTS <- c(RESULTS, "NDCG")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "FEMALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_3`[!is.na(data_0_pop$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_poplabel <- c(rep('D0_f_3_ndcg_pop', length(D0_f_3_ndcg_pop)))
      D0_f_3_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_3`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_itemknnlabel <- c(rep('D0_f_3_ndcg_itemknn', length(D0_f_3_ndcg_itemknn)))
      D0_f_3_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_3`[!is.na(data_0_als$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_alslabel <- c(rep('D0_f_3_ndcg_als', length(D0_f_3_ndcg_als)))
      D0_f_3_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_3`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_bprlabel <- c(rep('D0_f_3_ndcg_bpr', length(D0_f_3_ndcg_bpr)))
      D0_f_3_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_3`[!is.na(data_0_slim$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_slimlabel <- c(rep('D0_f_3_ndcg_slim', length(D0_f_3_ndcg_slim)))
      D0_f_3_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_3`[!is.na(data_0_vae$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_vaelabel <- c(rep('D0_f_3_ndcg_vae', length(D0_f_3_ndcg_vae)))
      # combine data
      ndcg_3 <- c(D0_f_3_ndcg_pop, D0_f_3_ndcg_itemknn, D0_f_3_ndcg_als, D0_f_3_ndcg_bpr, D0_f_3_ndcg_slim, D0_f_3_ndcg_vae)
      ndcg_3label <- c(D0_f_3_ndcg_poplabel, D0_f_3_ndcg_itemknnlabel, D0_f_3_ndcg_alslabel, D0_f_3_ndcg_bprlabel, D0_f_3_ndcg_slimlabel, D0_f_3_ndcg_vaelabel)
      
      D0_f_5_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_5`[!is.na(data_0_pop$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_poplabel <- c(rep('D0_f_5_ndcg_pop', length(D0_f_5_ndcg_pop)))
      D0_f_5_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_5`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_itemknnlabel <- c(rep('D0_f_5_ndcg_itemknn', length(D0_f_5_ndcg_itemknn)))
      D0_f_5_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_5`[!is.na(data_0_als$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_alslabel <- c(rep('D0_f_5_ndcg_als', length(D0_f_5_ndcg_als)))
      D0_f_5_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_5`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_bprlabel <- c(rep('D0_f_5_ndcg_bpr', length(D0_f_5_ndcg_bpr)))
      D0_f_5_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_5`[!is.na(data_0_slim$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_slimlabel <- c(rep('D0_f_5_ndcg_slim', length(D0_f_5_ndcg_slim)))
      D0_f_5_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_5`[!is.na(data_0_vae$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_vaelabel <- c(rep('D0_f_5_ndcg_vae', length(D0_f_5_ndcg_vae)))
      # combine data
      ndcg_5 <- c(D0_f_5_ndcg_pop, D0_f_5_ndcg_itemknn, D0_f_5_ndcg_als, D0_f_5_ndcg_bpr, D0_f_5_ndcg_slim, D0_f_5_ndcg_vae)
      ndcg_5label <- c(D0_f_5_ndcg_poplabel, D0_f_5_ndcg_itemknnlabel, D0_f_5_ndcg_alslabel, D0_f_5_ndcg_bprlabel, D0_f_5_ndcg_slimlabel, D0_f_5_ndcg_vaelabel)
      
      D0_f_10_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_10`[!is.na(data_0_pop$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_poplabel <- c(rep('D0_f_10_ndcg_pop', length(D0_f_10_ndcg_pop)))
      D0_f_10_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_10`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_itemknnlabel <- c(rep('D0_f_10_ndcg_itemknn', length(D0_f_10_ndcg_itemknn)))
      D0_f_10_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_10`[!is.na(data_0_als$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_alslabel <- c(rep('D0_f_10_ndcg_als', length(D0_f_10_ndcg_als)))
      D0_f_10_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_10`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_bprlabel <- c(rep('D0_f_10_ndcg_bpr', length(D0_f_10_ndcg_bpr)))
      D0_f_10_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_10`[!is.na(data_0_slim$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_slimlabel <- c(rep('D0_f_10_ndcg_slim', length(D0_f_10_ndcg_slim)))
      D0_f_10_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_10`[!is.na(data_0_vae$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_vaelabel <- c(rep('D0_f_10_ndcg_vae', length(D0_f_10_ndcg_vae)))
      # combine data
      ndcg_10 <- c(D0_f_10_ndcg_pop, D0_f_10_ndcg_itemknn, D0_f_10_ndcg_als, D0_f_10_ndcg_bpr, D0_f_10_ndcg_slim, D0_f_10_ndcg_vae)
      ndcg_10label <- c(D0_f_10_ndcg_poplabel, D0_f_10_ndcg_itemknnlabel, D0_f_10_ndcg_alslabel, D0_f_10_ndcg_bprlabel, D0_f_10_ndcg_slimlabel, D0_f_10_ndcg_vaelabel)
      
      D0_f_20_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_20`[!is.na(data_0_pop$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_poplabel <- c(rep('D0_f_20_ndcg_pop', length(D0_f_20_ndcg_pop)))
      D0_f_20_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_20`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_itemknnlabel <- c(rep('D0_f_20_ndcg_itemknn', length(D0_f_20_ndcg_itemknn)))
      D0_f_20_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_20`[!is.na(data_0_als$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_alslabel <- c(rep('D0_f_20_ndcg_als', length(D0_f_20_ndcg_als)))
      D0_f_20_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_20`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_bprlabel <- c(rep('D0_f_20_ndcg_bpr', length(D0_f_20_ndcg_bpr)))
      D0_f_20_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_20`[!is.na(data_0_slim$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_slimlabel <- c(rep('D0_f_20_ndcg_slim', length(D0_f_20_ndcg_slim)))
      D0_f_20_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_20`[!is.na(data_0_vae$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_vaelabel <- c(rep('D0_f_20_ndcg_vae', length(D0_f_20_ndcg_vae)))
      # combine data
      ndcg_20 <- c(D0_f_20_ndcg_pop, D0_f_20_ndcg_itemknn, D0_f_20_ndcg_als, D0_f_20_ndcg_bpr, D0_f_20_ndcg_slim, D0_f_20_ndcg_vae)
      ndcg_20label <- c(D0_f_20_ndcg_poplabel, D0_f_20_ndcg_itemknnlabel, D0_f_20_ndcg_alslabel, D0_f_20_ndcg_bprlabel, D0_f_20_ndcg_slimlabel, D0_f_20_ndcg_vaelabel)
      
      D0_f_50_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_50`[!is.na(data_0_pop$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_poplabel <- c(rep('D0_f_50_ndcg_pop', length(D0_f_50_ndcg_pop)))
      D0_f_50_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_50`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_itemknnlabel <- c(rep('D0_f_50_ndcg_itemknn', length(D0_f_50_ndcg_itemknn)))
      D0_f_50_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_50`[!is.na(data_0_als$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_alslabel <- c(rep('D0_f_50_ndcg_als', length(D0_f_50_ndcg_als)))
      D0_f_50_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_50`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_bprlabel <- c(rep('D0_f_50_ndcg_bpr', length(D0_f_50_ndcg_bpr)))
      D0_f_50_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_50`[!is.na(data_0_slim$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_slimlabel <- c(rep('D0_f_50_ndcg_slim', length(D0_f_50_ndcg_slim)))
      D0_f_50_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_50`[!is.na(data_0_vae$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_vaelabel <- c(rep('D0_f_50_ndcg_vae', length(D0_f_50_ndcg_vae)))
      # combine data
      ndcg_50 <- c(D0_f_50_ndcg_pop, D0_f_50_ndcg_itemknn, D0_f_50_ndcg_als, D0_f_50_ndcg_bpr, D0_f_50_ndcg_slim, D0_f_50_ndcg_vae)
      ndcg_50label <- c(D0_f_50_ndcg_poplabel, D0_f_50_ndcg_itemknnlabel, D0_f_50_ndcg_alslabel, D0_f_50_ndcg_bprlabel, D0_f_50_ndcg_slimlabel, D0_f_50_ndcg_vaelabel)
      # make dataframe
      ndcg_f_D <- data.frame(ndcg_3, ndcg_3label, ndcg_5, ndcg_5label, ndcg_10, ndcg_10label, ndcg_20, ndcg_20label, ndcg_50, ndcg_50label)
      #View(ndcg_m_D)
      if(threshold == 3){
        dunn <- dunn_test(ndcg_3 ~ ndcg_3label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(ndcg_5 ~ ndcg_5label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(ndcg_10 ~ ndcg_10label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(ndcg_20 ~ ndcg_20label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(ndcg_50 ~ ndcg_50label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}


for (samp_group in sampling){
  RESULTS <- c(RESULTS, "NDCG")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "ALL")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_3`[!is.na(data_0_pop$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_poplabel <- c(rep('D0_f_3_ndcg_pop', length(D0_f_3_ndcg_pop)))
      D0_f_3_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_3`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_itemknnlabel <- c(rep('D0_f_3_ndcg_itemknn', length(D0_f_3_ndcg_itemknn)))
      D0_f_3_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_3`[!is.na(data_0_als$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_alslabel <- c(rep('D0_f_3_ndcg_als', length(D0_f_3_ndcg_als)))
      D0_f_3_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_3`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_bprlabel <- c(rep('D0_f_3_ndcg_bpr', length(D0_f_3_ndcg_bpr)))
      D0_f_3_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_3`[!is.na(data_0_slim$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_slimlabel <- c(rep('D0_f_3_ndcg_slim', length(D0_f_3_ndcg_slim)))
      D0_f_3_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_3`[!is.na(data_0_vae$`test/gender_m/ndcg_at_3`)]
      D0_f_3_ndcg_vaelabel <- c(rep('D0_f_3_ndcg_vae', length(D0_f_3_ndcg_vae)))
      D0_m_3_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_3`[!is.na(data_0_pop$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_poplabel <- c(rep('D0_f_3_ndcg_pop', length(D0_m_3_ndcg_pop)))
      D0_m_3_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_3`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_itemknnlabel <- c(rep('D0_f_3_ndcg_itemknn', length(D0_m_3_ndcg_itemknn)))
      D0_m_3_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_3`[!is.na(data_0_als$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_alslabel <- c(rep('D0_f_3_ndcg_als', length(D0_m_3_ndcg_als)))
      D0_m_3_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_3`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_bprlabel <- c(rep('D0_f_3_ndcg_bpr', length(D0_m_3_ndcg_bpr)))
      D0_m_3_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_3`[!is.na(data_0_slim$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_slimlabel <- c(rep('D0_f_3_ndcg_slim', length(D0_m_3_ndcg_slim)))
      D0_m_3_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_3`[!is.na(data_0_vae$`test/gender_m/ndcg_at_3`)]
      D0_m_3_ndcg_vaelabel <- c(rep('D0_f_3_ndcg_vae', length(D0_m_3_ndcg_vae)))
      # combine data
      ndcg_3 <- c(D0_f_3_ndcg_pop, D0_f_3_ndcg_itemknn, D0_f_3_ndcg_als, D0_f_3_ndcg_bpr, D0_f_3_ndcg_slim, D0_f_3_ndcg_vae, D0_m_3_ndcg_pop, D0_m_3_ndcg_itemknn, D0_m_3_ndcg_als, D0_m_3_ndcg_bpr, D0_m_3_ndcg_slim, D0_m_3_ndcg_vae)
      ndcg_3label <- c(D0_f_3_ndcg_poplabel, D0_f_3_ndcg_itemknnlabel, D0_f_3_ndcg_alslabel, D0_f_3_ndcg_bprlabel, D0_f_3_ndcg_slimlabel, D0_f_3_ndcg_vaelabel, D0_m_3_ndcg_poplabel, D0_m_3_ndcg_itemknnlabel, D0_m_3_ndcg_alslabel, D0_m_3_ndcg_bprlabel, D0_m_3_ndcg_slimlabel, D0_m_3_ndcg_vaelabel)
      
      D0_f_5_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_5`[!is.na(data_0_pop$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_poplabel <- c(rep('D0_f_5_ndcg_pop', length(D0_f_5_ndcg_pop)))
      D0_f_5_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_5`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_itemknnlabel <- c(rep('D0_f_5_ndcg_itemknn', length(D0_f_5_ndcg_itemknn)))
      D0_f_5_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_5`[!is.na(data_0_als$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_alslabel <- c(rep('D0_f_5_ndcg_als', length(D0_f_5_ndcg_als)))
      D0_f_5_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_5`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_bprlabel <- c(rep('D0_f_5_ndcg_bpr', length(D0_f_5_ndcg_bpr)))
      D0_f_5_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_5`[!is.na(data_0_slim$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_slimlabel <- c(rep('D0_f_5_ndcg_slim', length(D0_f_5_ndcg_slim)))
      D0_f_5_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_5`[!is.na(data_0_vae$`test/gender_m/ndcg_at_5`)]
      D0_f_5_ndcg_vaelabel <- c(rep('D0_f_5_ndcg_vae', length(D0_f_5_ndcg_vae)))
      D0_m_5_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_5`[!is.na(data_0_pop$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_poplabel <- c(rep('D0_f_5_ndcg_pop', length(D0_m_5_ndcg_pop)))
      D0_m_5_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_5`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_itemknnlabel <- c(rep('D0_f_5_ndcg_itemknn', length(D0_m_5_ndcg_itemknn)))
      D0_m_5_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_5`[!is.na(data_0_als$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_alslabel <- c(rep('D0_f_5_ndcg_als', length(D0_m_5_ndcg_als)))
      D0_m_5_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_5`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_bprlabel <- c(rep('D0_f_5_ndcg_bpr', length(D0_m_5_ndcg_bpr)))
      D0_m_5_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_5`[!is.na(data_0_slim$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_slimlabel <- c(rep('D0_f_5_ndcg_slim', length(D0_m_5_ndcg_slim)))
      D0_m_5_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_5`[!is.na(data_0_vae$`test/gender_m/ndcg_at_5`)]
      D0_m_5_ndcg_vaelabel <- c(rep('D0_f_5_ndcg_vae', length(D0_m_5_ndcg_vae)))
      # combine data
      ndcg_5 <- c(D0_f_5_ndcg_pop, D0_f_5_ndcg_itemknn, D0_f_5_ndcg_als, D0_f_5_ndcg_bpr, D0_f_5_ndcg_slim, D0_f_5_ndcg_vae, D0_m_5_ndcg_pop, D0_m_5_ndcg_itemknn, D0_m_5_ndcg_als, D0_m_5_ndcg_bpr, D0_m_5_ndcg_slim, D0_m_5_ndcg_vae)
      ndcg_5label <- c(D0_f_5_ndcg_poplabel, D0_f_5_ndcg_itemknnlabel, D0_f_5_ndcg_alslabel, D0_f_5_ndcg_bprlabel, D0_f_5_ndcg_slimlabel, D0_f_5_ndcg_vaelabel, D0_m_5_ndcg_poplabel, D0_m_5_ndcg_itemknnlabel, D0_m_5_ndcg_alslabel, D0_m_5_ndcg_bprlabel, D0_m_5_ndcg_slimlabel, D0_m_5_ndcg_vaelabel)
      
      
      D0_f_10_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_10`[!is.na(data_0_pop$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_poplabel <- c(rep('D0_f_10_ndcg_pop', length(D0_f_10_ndcg_pop)))
      D0_f_10_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_10`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_itemknnlabel <- c(rep('D0_f_10_ndcg_itemknn', length(D0_f_10_ndcg_itemknn)))
      D0_f_10_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_10`[!is.na(data_0_als$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_alslabel <- c(rep('D0_f_10_ndcg_als', length(D0_f_10_ndcg_als)))
      D0_f_10_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_10`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_bprlabel <- c(rep('D0_f_10_ndcg_bpr', length(D0_f_10_ndcg_bpr)))
      D0_f_10_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_10`[!is.na(data_0_slim$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_slimlabel <- c(rep('D0_f_10_ndcg_slim', length(D0_f_10_ndcg_slim)))
      D0_f_10_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_10`[!is.na(data_0_vae$`test/gender_m/ndcg_at_10`)]
      D0_f_10_ndcg_vaelabel <- c(rep('D0_f_10_ndcg_vae', length(D0_f_10_ndcg_vae)))
      D0_m_10_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_10`[!is.na(data_0_pop$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_poplabel <- c(rep('D0_f_10_ndcg_pop', length(D0_m_10_ndcg_pop)))
      D0_m_10_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_10`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_itemknnlabel <- c(rep('D0_f_10_ndcg_itemknn', length(D0_m_10_ndcg_itemknn)))
      D0_m_10_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_10`[!is.na(data_0_als$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_alslabel <- c(rep('D0_f_10_ndcg_als', length(D0_m_10_ndcg_als)))
      D0_m_10_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_10`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_bprlabel <- c(rep('D0_f_10_ndcg_bpr', length(D0_m_10_ndcg_bpr)))
      D0_m_10_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_10`[!is.na(data_0_slim$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_slimlabel <- c(rep('D0_f_10_ndcg_slim', length(D0_m_10_ndcg_slim)))
      D0_m_10_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_10`[!is.na(data_0_vae$`test/gender_m/ndcg_at_10`)]
      D0_m_10_ndcg_vaelabel <- c(rep('D0_f_10_ndcg_vae', length(D0_m_10_ndcg_vae)))
      # combine data
      ndcg_10 <- c(D0_f_10_ndcg_pop, D0_f_10_ndcg_itemknn, D0_f_10_ndcg_als, D0_f_10_ndcg_bpr, D0_f_10_ndcg_slim, D0_f_10_ndcg_vae, D0_m_10_ndcg_pop, D0_m_10_ndcg_itemknn, D0_m_10_ndcg_als, D0_m_10_ndcg_bpr, D0_m_10_ndcg_slim, D0_m_10_ndcg_vae)
      ndcg_10label <- c(D0_f_10_ndcg_poplabel, D0_f_10_ndcg_itemknnlabel, D0_f_10_ndcg_alslabel, D0_f_10_ndcg_bprlabel, D0_f_10_ndcg_slimlabel, D0_f_10_ndcg_vaelabel, D0_m_10_ndcg_poplabel, D0_m_10_ndcg_itemknnlabel, D0_m_10_ndcg_alslabel, D0_m_10_ndcg_bprlabel, D0_m_10_ndcg_slimlabel, D0_m_10_ndcg_vaelabel)
      
      
      D0_f_20_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_20`[!is.na(data_0_pop$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_poplabel <- c(rep('D0_f_20_ndcg_pop', length(D0_f_20_ndcg_pop)))
      D0_f_20_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_20`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_itemknnlabel <- c(rep('D0_f_20_ndcg_itemknn', length(D0_f_20_ndcg_itemknn)))
      D0_f_20_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_20`[!is.na(data_0_als$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_alslabel <- c(rep('D0_f_20_ndcg_als', length(D0_f_20_ndcg_als)))
      D0_f_20_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_20`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_bprlabel <- c(rep('D0_f_20_ndcg_bpr', length(D0_f_20_ndcg_bpr)))
      D0_f_20_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_20`[!is.na(data_0_slim$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_slimlabel <- c(rep('D0_f_20_ndcg_slim', length(D0_f_20_ndcg_slim)))
      D0_f_20_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_20`[!is.na(data_0_vae$`test/gender_m/ndcg_at_20`)]
      D0_f_20_ndcg_vaelabel <- c(rep('D0_f_20_ndcg_vae', length(D0_f_20_ndcg_vae)))
      D0_m_20_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_20`[!is.na(data_0_pop$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_poplabel <- c(rep('D0_f_20_ndcg_pop', length(D0_m_20_ndcg_pop)))
      D0_m_20_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_20`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_itemknnlabel <- c(rep('D0_f_20_ndcg_itemknn', length(D0_m_20_ndcg_itemknn)))
      D0_m_20_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_20`[!is.na(data_0_als$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_alslabel <- c(rep('D0_f_20_ndcg_als', length(D0_m_20_ndcg_als)))
      D0_m_20_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_20`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_bprlabel <- c(rep('D0_f_20_ndcg_bpr', length(D0_m_20_ndcg_bpr)))
      D0_m_20_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_20`[!is.na(data_0_slim$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_slimlabel <- c(rep('D0_f_20_ndcg_slim', length(D0_m_20_ndcg_slim)))
      D0_m_20_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_20`[!is.na(data_0_vae$`test/gender_m/ndcg_at_20`)]
      D0_m_20_ndcg_vaelabel <- c(rep('D0_f_20_ndcg_vae', length(D0_m_20_ndcg_vae)))
      # combine data
      ndcg_20 <- c(D0_f_20_ndcg_pop, D0_f_20_ndcg_itemknn, D0_f_20_ndcg_als, D0_f_20_ndcg_bpr, D0_f_20_ndcg_slim, D0_f_20_ndcg_vae, D0_m_20_ndcg_pop, D0_m_20_ndcg_itemknn, D0_m_20_ndcg_als, D0_m_20_ndcg_bpr, D0_m_20_ndcg_slim, D0_m_20_ndcg_vae)
      ndcg_20label <- c(D0_f_20_ndcg_poplabel, D0_f_20_ndcg_itemknnlabel, D0_f_20_ndcg_alslabel, D0_f_20_ndcg_bprlabel, D0_f_20_ndcg_slimlabel, D0_f_20_ndcg_vaelabel, D0_m_20_ndcg_poplabel, D0_m_20_ndcg_itemknnlabel, D0_m_20_ndcg_alslabel, D0_m_20_ndcg_bprlabel, D0_m_20_ndcg_slimlabel, D0_m_20_ndcg_vaelabel)
      
      
      D0_f_50_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_50`[!is.na(data_0_pop$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_poplabel <- c(rep('D0_f_50_ndcg_pop', length(D0_f_50_ndcg_pop)))
      D0_f_50_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_50`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_itemknnlabel <- c(rep('D0_f_50_ndcg_itemknn', length(D0_f_50_ndcg_itemknn)))
      D0_f_50_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_50`[!is.na(data_0_als$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_alslabel <- c(rep('D0_f_50_ndcg_als', length(D0_f_50_ndcg_als)))
      D0_f_50_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_50`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_bprlabel <- c(rep('D0_f_50_ndcg_bpr', length(D0_f_50_ndcg_bpr)))
      D0_f_50_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_50`[!is.na(data_0_slim$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_slimlabel <- c(rep('D0_f_50_ndcg_slim', length(D0_f_50_ndcg_slim)))
      D0_f_50_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_50`[!is.na(data_0_vae$`test/gender_m/ndcg_at_50`)]
      D0_f_50_ndcg_vaelabel <- c(rep('D0_f_50_ndcg_vae', length(D0_f_50_ndcg_vae)))
      D0_m_50_ndcg_pop <- data_0_pop$`test/gender_m/ndcg_at_50`[!is.na(data_0_pop$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_poplabel <- c(rep('D0_f_50_ndcg_pop', length(D0_m_50_ndcg_pop)))
      D0_m_50_ndcg_itemknn <- data_0_itemknn$`test/gender_m/ndcg_at_50`[!is.na(data_0_itemknn$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_itemknnlabel <- c(rep('D0_f_50_ndcg_itemknn', length(D0_m_50_ndcg_itemknn)))
      D0_m_50_ndcg_als <- data_0_als$`test/gender_m/ndcg_at_50`[!is.na(data_0_als$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_alslabel <- c(rep('D0_f_50_ndcg_als', length(D0_m_50_ndcg_als)))
      D0_m_50_ndcg_bpr <- data_0_bpr$`test/gender_m/ndcg_at_50`[!is.na(data_0_bpr$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_bprlabel <- c(rep('D0_f_50_ndcg_bpr', length(D0_m_50_ndcg_bpr)))
      D0_m_50_ndcg_slim <- data_0_slim$`test/gender_m/ndcg_at_50`[!is.na(data_0_slim$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_slimlabel <- c(rep('D0_f_50_ndcg_slim', length(D0_m_50_ndcg_slim)))
      D0_m_50_ndcg_vae <- data_0_vae$`test/gender_m/ndcg_at_50`[!is.na(data_0_vae$`test/gender_m/ndcg_at_50`)]
      D0_m_50_ndcg_vaelabel <- c(rep('D0_f_50_ndcg_vae', length(D0_m_50_ndcg_vae)))
      # combine data
      ndcg_50 <- c(D0_f_50_ndcg_pop, D0_f_50_ndcg_itemknn, D0_f_50_ndcg_als, D0_f_50_ndcg_bpr, D0_f_50_ndcg_slim, D0_f_50_ndcg_vae, D0_m_50_ndcg_pop, D0_m_50_ndcg_itemknn, D0_m_50_ndcg_als, D0_m_50_ndcg_bpr, D0_m_50_ndcg_slim, D0_m_50_ndcg_vae)
      ndcg_50label <- c(D0_f_50_ndcg_poplabel, D0_f_50_ndcg_itemknnlabel, D0_f_50_ndcg_alslabel, D0_f_50_ndcg_bprlabel, D0_f_50_ndcg_slimlabel, D0_f_50_ndcg_vaelabel, D0_m_50_ndcg_poplabel, D0_m_50_ndcg_itemknnlabel, D0_m_50_ndcg_alslabel, D0_m_50_ndcg_bprlabel, D0_m_50_ndcg_slimlabel, D0_m_50_ndcg_vaelabel)
      # make dataframe
      ndcg_f_D <- data.frame(ndcg_3, ndcg_3label, ndcg_5, ndcg_5label, ndcg_10, ndcg_10label, ndcg_20, ndcg_20label, ndcg_50, ndcg_50label)
      #View(ndcg_f_D)
      if(threshold == 3){
        dunn <- dunn_test(ndcg_3 ~ ndcg_3label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(ndcg_5 ~ ndcg_5label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(ndcg_10 ~ ndcg_10label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(ndcg_20 ~ ndcg_20label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(ndcg_50 ~ ndcg_50label, data=ndcg_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}


for (samp_group in sampling){
  RESULTS <- c(RESULTS, "recall")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "MALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_m_3_recall_pop <- data_0_pop$`test/gender_m/recall_at_3`[!is.na(data_0_pop$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_poplabel <- c(rep('D0_m_3_recall_pop', length(D0_m_3_recall_pop)))
      D0_m_3_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_3`[!is.na(data_0_itemknn$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_itemknnlabel <- c(rep('D0_m_3_recall_itemknn', length(D0_m_3_recall_itemknn)))
      D0_m_3_recall_als <- data_0_als$`test/gender_m/recall_at_3`[!is.na(data_0_als$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_alslabel <- c(rep('D0_m_3_recall_als', length(D0_m_3_recall_als)))
      D0_m_3_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_3`[!is.na(data_0_bpr$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_bprlabel <- c(rep('D0_m_3_recall_bpr', length(D0_m_3_recall_bpr)))
      D0_m_3_recall_slim <- data_0_slim$`test/gender_m/recall_at_3`[!is.na(data_0_slim$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_slimlabel <- c(rep('D0_m_3_recall_slim', length(D0_m_3_recall_slim)))
      D0_m_3_recall_vae <- data_0_vae$`test/gender_m/recall_at_3`[!is.na(data_0_vae$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_vaelabel <- c(rep('D0_m_3_recall_vae', length(D0_m_3_recall_vae)))
      # combine data
      recall_3 <- c(D0_m_3_recall_pop, D0_m_3_recall_itemknn, D0_m_3_recall_als, D0_m_3_recall_bpr, D0_m_3_recall_slim, D0_m_3_recall_vae)
      recall_3label <- c(D0_m_3_recall_poplabel, D0_m_3_recall_itemknnlabel, D0_m_3_recall_alslabel, D0_m_3_recall_bprlabel, D0_m_3_recall_slimlabel, D0_m_3_recall_vaelabel)
      
      D0_m_5_recall_pop <- data_0_pop$`test/gender_m/recall_at_5`[!is.na(data_0_pop$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_poplabel <- c(rep('D0_m_5_recall_pop', length(D0_m_5_recall_pop)))
      D0_m_5_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_5`[!is.na(data_0_itemknn$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_itemknnlabel <- c(rep('D0_m_5_recall_itemknn', length(D0_m_5_recall_itemknn)))
      D0_m_5_recall_als <- data_0_als$`test/gender_m/recall_at_5`[!is.na(data_0_als$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_alslabel <- c(rep('D0_m_5_recall_als', length(D0_m_5_recall_als)))
      D0_m_5_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_5`[!is.na(data_0_bpr$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_bprlabel <- c(rep('D0_m_5_recall_bpr', length(D0_m_5_recall_bpr)))
      D0_m_5_recall_slim <- data_0_slim$`test/gender_m/recall_at_5`[!is.na(data_0_slim$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_slimlabel <- c(rep('D0_m_5_recall_slim', length(D0_m_5_recall_slim)))
      D0_m_5_recall_vae <- data_0_vae$`test/gender_m/recall_at_5`[!is.na(data_0_vae$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_vaelabel <- c(rep('D0_m_5_recall_vae', length(D0_m_5_recall_vae)))
      # combine data
      recall_5 <- c(D0_m_5_recall_pop, D0_m_5_recall_itemknn, D0_m_5_recall_als, D0_m_5_recall_bpr, D0_m_5_recall_slim, D0_m_5_recall_vae)
      recall_5label <- c(D0_m_5_recall_poplabel, D0_m_5_recall_itemknnlabel, D0_m_5_recall_alslabel, D0_m_5_recall_bprlabel, D0_m_5_recall_slimlabel, D0_m_5_recall_vaelabel)
      
      D0_m_10_recall_pop <- data_0_pop$`test/gender_m/recall_at_10`[!is.na(data_0_pop$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_poplabel <- c(rep('D0_m_10_recall_pop', length(D0_m_10_recall_pop)))
      D0_m_10_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_10`[!is.na(data_0_itemknn$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_itemknnlabel <- c(rep('D0_m_10_recall_itemknn', length(D0_m_10_recall_itemknn)))
      D0_m_10_recall_als <- data_0_als$`test/gender_m/recall_at_10`[!is.na(data_0_als$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_alslabel <- c(rep('D0_m_10_recall_als', length(D0_m_10_recall_als)))
      D0_m_10_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_10`[!is.na(data_0_bpr$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_bprlabel <- c(rep('D0_m_10_recall_bpr', length(D0_m_10_recall_bpr)))
      D0_m_10_recall_slim <- data_0_slim$`test/gender_m/recall_at_10`[!is.na(data_0_slim$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_slimlabel <- c(rep('D0_m_10_recall_slim', length(D0_m_10_recall_slim)))
      D0_m_10_recall_vae <- data_0_vae$`test/gender_m/recall_at_10`[!is.na(data_0_vae$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_vaelabel <- c(rep('D0_m_10_recall_vae', length(D0_m_10_recall_vae)))
      # combine data
      recall_10 <- c(D0_m_10_recall_pop, D0_m_10_recall_itemknn, D0_m_10_recall_als, D0_m_10_recall_bpr, D0_m_10_recall_slim, D0_m_10_recall_vae)
      recall_10label <- c(D0_m_10_recall_poplabel, D0_m_10_recall_itemknnlabel, D0_m_10_recall_alslabel, D0_m_10_recall_bprlabel, D0_m_10_recall_slimlabel, D0_m_10_recall_vaelabel)
      
      D0_m_20_recall_pop <- data_0_pop$`test/gender_m/recall_at_20`[!is.na(data_0_pop$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_poplabel <- c(rep('D0_m_20_recall_pop', length(D0_m_20_recall_pop)))
      D0_m_20_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_20`[!is.na(data_0_itemknn$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_itemknnlabel <- c(rep('D0_m_20_recall_itemknn', length(D0_m_20_recall_itemknn)))
      D0_m_20_recall_als <- data_0_als$`test/gender_m/recall_at_20`[!is.na(data_0_als$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_alslabel <- c(rep('D0_m_20_recall_als', length(D0_m_20_recall_als)))
      D0_m_20_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_20`[!is.na(data_0_bpr$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_bprlabel <- c(rep('D0_m_20_recall_bpr', length(D0_m_20_recall_bpr)))
      D0_m_20_recall_slim <- data_0_slim$`test/gender_m/recall_at_20`[!is.na(data_0_slim$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_slimlabel <- c(rep('D0_m_20_recall_slim', length(D0_m_20_recall_slim)))
      D0_m_20_recall_vae <- data_0_vae$`test/gender_m/recall_at_20`[!is.na(data_0_vae$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_vaelabel <- c(rep('D0_m_20_recall_vae', length(D0_m_20_recall_vae)))
      # combine data
      recall_20 <- c(D0_m_20_recall_pop, D0_m_20_recall_itemknn, D0_m_20_recall_als, D0_m_20_recall_bpr, D0_m_20_recall_slim, D0_m_20_recall_vae)
      recall_20label <- c(D0_m_20_recall_poplabel, D0_m_20_recall_itemknnlabel, D0_m_20_recall_alslabel, D0_m_20_recall_bprlabel, D0_m_20_recall_slimlabel, D0_m_20_recall_vaelabel)
      
      D0_m_50_recall_pop <- data_0_pop$`test/gender_m/recall_at_50`[!is.na(data_0_pop$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_poplabel <- c(rep('D0_m_50_recall_pop', length(D0_m_50_recall_pop)))
      D0_m_50_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_50`[!is.na(data_0_itemknn$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_itemknnlabel <- c(rep('D0_m_50_recall_itemknn', length(D0_m_50_recall_itemknn)))
      D0_m_50_recall_als <- data_0_als$`test/gender_m/recall_at_50`[!is.na(data_0_als$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_alslabel <- c(rep('D0_m_50_recall_als', length(D0_m_50_recall_als)))
      D0_m_50_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_50`[!is.na(data_0_bpr$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_bprlabel <- c(rep('D0_m_50_recall_bpr', length(D0_m_50_recall_bpr)))
      D0_m_50_recall_slim <- data_0_slim$`test/gender_m/recall_at_50`[!is.na(data_0_slim$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_slimlabel <- c(rep('D0_m_50_recall_slim', length(D0_m_50_recall_slim)))
      D0_m_50_recall_vae <- data_0_vae$`test/gender_m/recall_at_50`[!is.na(data_0_vae$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_vaelabel <- c(rep('D0_m_50_recall_vae', length(D0_m_50_recall_vae)))
      # combine data
      recall_50 <- c(D0_m_50_recall_pop, D0_m_50_recall_itemknn, D0_m_50_recall_als, D0_m_50_recall_bpr, D0_m_50_recall_slim, D0_m_50_recall_vae)
      recall_50label <- c(D0_m_50_recall_poplabel, D0_m_50_recall_itemknnlabel, D0_m_50_recall_alslabel, D0_m_50_recall_bprlabel, D0_m_50_recall_slimlabel, D0_m_50_recall_vaelabel)
      # make dataframe
      recall_m_D <- data.frame(recall_3, recall_3label, recall_5, recall_5label, recall_10, recall_10label, recall_20, recall_20label, recall_50, recall_50label)
      #View(recall_m_D)
      if(threshold == 3){
        dunn <- dunn_test(recall_3 ~ recall_3label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(recall_5 ~ recall_5label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(recall_10 ~ recall_10label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(recall_20 ~ recall_20label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(recall_50 ~ recall_50label, data=recall_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}


for (samp_group in sampling){
  RESULTS <- c(RESULTS, "recall")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "FEMALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_recall_pop <- data_0_pop$`test/gender_m/recall_at_3`[!is.na(data_0_pop$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_poplabel <- c(rep('D0_f_3_recall_pop', length(D0_f_3_recall_pop)))
      D0_f_3_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_3`[!is.na(data_0_itemknn$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_itemknnlabel <- c(rep('D0_f_3_recall_itemknn', length(D0_f_3_recall_itemknn)))
      D0_f_3_recall_als <- data_0_als$`test/gender_m/recall_at_3`[!is.na(data_0_als$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_alslabel <- c(rep('D0_f_3_recall_als', length(D0_f_3_recall_als)))
      D0_f_3_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_3`[!is.na(data_0_bpr$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_bprlabel <- c(rep('D0_f_3_recall_bpr', length(D0_f_3_recall_bpr)))
      D0_f_3_recall_slim <- data_0_slim$`test/gender_m/recall_at_3`[!is.na(data_0_slim$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_slimlabel <- c(rep('D0_f_3_recall_slim', length(D0_f_3_recall_slim)))
      D0_f_3_recall_vae <- data_0_vae$`test/gender_m/recall_at_3`[!is.na(data_0_vae$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_vaelabel <- c(rep('D0_f_3_recall_vae', length(D0_f_3_recall_vae)))
      # combine data
      recall_3 <- c(D0_f_3_recall_pop, D0_f_3_recall_itemknn, D0_f_3_recall_als, D0_f_3_recall_bpr, D0_f_3_recall_slim, D0_f_3_recall_vae)
      recall_3label <- c(D0_f_3_recall_poplabel, D0_f_3_recall_itemknnlabel, D0_f_3_recall_alslabel, D0_f_3_recall_bprlabel, D0_f_3_recall_slimlabel, D0_f_3_recall_vaelabel)
      
      D0_f_5_recall_pop <- data_0_pop$`test/gender_m/recall_at_5`[!is.na(data_0_pop$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_poplabel <- c(rep('D0_f_5_recall_pop', length(D0_f_5_recall_pop)))
      D0_f_5_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_5`[!is.na(data_0_itemknn$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_itemknnlabel <- c(rep('D0_f_5_recall_itemknn', length(D0_f_5_recall_itemknn)))
      D0_f_5_recall_als <- data_0_als$`test/gender_m/recall_at_5`[!is.na(data_0_als$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_alslabel <- c(rep('D0_f_5_recall_als', length(D0_f_5_recall_als)))
      D0_f_5_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_5`[!is.na(data_0_bpr$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_bprlabel <- c(rep('D0_f_5_recall_bpr', length(D0_f_5_recall_bpr)))
      D0_f_5_recall_slim <- data_0_slim$`test/gender_m/recall_at_5`[!is.na(data_0_slim$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_slimlabel <- c(rep('D0_f_5_recall_slim', length(D0_f_5_recall_slim)))
      D0_f_5_recall_vae <- data_0_vae$`test/gender_m/recall_at_5`[!is.na(data_0_vae$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_vaelabel <- c(rep('D0_f_5_recall_vae', length(D0_f_5_recall_vae)))
      # combine data
      recall_5 <- c(D0_f_5_recall_pop, D0_f_5_recall_itemknn, D0_f_5_recall_als, D0_f_5_recall_bpr, D0_f_5_recall_slim, D0_f_5_recall_vae)
      recall_5label <- c(D0_f_5_recall_poplabel, D0_f_5_recall_itemknnlabel, D0_f_5_recall_alslabel, D0_f_5_recall_bprlabel, D0_f_5_recall_slimlabel, D0_f_5_recall_vaelabel)
      
      D0_f_10_recall_pop <- data_0_pop$`test/gender_m/recall_at_10`[!is.na(data_0_pop$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_poplabel <- c(rep('D0_f_10_recall_pop', length(D0_f_10_recall_pop)))
      D0_f_10_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_10`[!is.na(data_0_itemknn$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_itemknnlabel <- c(rep('D0_f_10_recall_itemknn', length(D0_f_10_recall_itemknn)))
      D0_f_10_recall_als <- data_0_als$`test/gender_m/recall_at_10`[!is.na(data_0_als$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_alslabel <- c(rep('D0_f_10_recall_als', length(D0_f_10_recall_als)))
      D0_f_10_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_10`[!is.na(data_0_bpr$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_bprlabel <- c(rep('D0_f_10_recall_bpr', length(D0_f_10_recall_bpr)))
      D0_f_10_recall_slim <- data_0_slim$`test/gender_m/recall_at_10`[!is.na(data_0_slim$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_slimlabel <- c(rep('D0_f_10_recall_slim', length(D0_f_10_recall_slim)))
      D0_f_10_recall_vae <- data_0_vae$`test/gender_m/recall_at_10`[!is.na(data_0_vae$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_vaelabel <- c(rep('D0_f_10_recall_vae', length(D0_f_10_recall_vae)))
      # combine data
      recall_10 <- c(D0_f_10_recall_pop, D0_f_10_recall_itemknn, D0_f_10_recall_als, D0_f_10_recall_bpr, D0_f_10_recall_slim, D0_f_10_recall_vae)
      recall_10label <- c(D0_f_10_recall_poplabel, D0_f_10_recall_itemknnlabel, D0_f_10_recall_alslabel, D0_f_10_recall_bprlabel, D0_f_10_recall_slimlabel, D0_f_10_recall_vaelabel)
      
      D0_f_20_recall_pop <- data_0_pop$`test/gender_m/recall_at_20`[!is.na(data_0_pop$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_poplabel <- c(rep('D0_f_20_recall_pop', length(D0_f_20_recall_pop)))
      D0_f_20_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_20`[!is.na(data_0_itemknn$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_itemknnlabel <- c(rep('D0_f_20_recall_itemknn', length(D0_f_20_recall_itemknn)))
      D0_f_20_recall_als <- data_0_als$`test/gender_m/recall_at_20`[!is.na(data_0_als$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_alslabel <- c(rep('D0_f_20_recall_als', length(D0_f_20_recall_als)))
      D0_f_20_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_20`[!is.na(data_0_bpr$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_bprlabel <- c(rep('D0_f_20_recall_bpr', length(D0_f_20_recall_bpr)))
      D0_f_20_recall_slim <- data_0_slim$`test/gender_m/recall_at_20`[!is.na(data_0_slim$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_slimlabel <- c(rep('D0_f_20_recall_slim', length(D0_f_20_recall_slim)))
      D0_f_20_recall_vae <- data_0_vae$`test/gender_m/recall_at_20`[!is.na(data_0_vae$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_vaelabel <- c(rep('D0_f_20_recall_vae', length(D0_f_20_recall_vae)))
      # combine data
      recall_20 <- c(D0_f_20_recall_pop, D0_f_20_recall_itemknn, D0_f_20_recall_als, D0_f_20_recall_bpr, D0_f_20_recall_slim, D0_f_20_recall_vae)
      recall_20label <- c(D0_f_20_recall_poplabel, D0_f_20_recall_itemknnlabel, D0_f_20_recall_alslabel, D0_f_20_recall_bprlabel, D0_f_20_recall_slimlabel, D0_f_20_recall_vaelabel)
      
      D0_f_50_recall_pop <- data_0_pop$`test/gender_m/recall_at_50`[!is.na(data_0_pop$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_poplabel <- c(rep('D0_f_50_recall_pop', length(D0_f_50_recall_pop)))
      D0_f_50_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_50`[!is.na(data_0_itemknn$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_itemknnlabel <- c(rep('D0_f_50_recall_itemknn', length(D0_f_50_recall_itemknn)))
      D0_f_50_recall_als <- data_0_als$`test/gender_m/recall_at_50`[!is.na(data_0_als$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_alslabel <- c(rep('D0_f_50_recall_als', length(D0_f_50_recall_als)))
      D0_f_50_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_50`[!is.na(data_0_bpr$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_bprlabel <- c(rep('D0_f_50_recall_bpr', length(D0_f_50_recall_bpr)))
      D0_f_50_recall_slim <- data_0_slim$`test/gender_m/recall_at_50`[!is.na(data_0_slim$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_slimlabel <- c(rep('D0_f_50_recall_slim', length(D0_f_50_recall_slim)))
      D0_f_50_recall_vae <- data_0_vae$`test/gender_m/recall_at_50`[!is.na(data_0_vae$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_vaelabel <- c(rep('D0_f_50_recall_vae', length(D0_f_50_recall_vae)))
      # combine data
      recall_50 <- c(D0_f_50_recall_pop, D0_f_50_recall_itemknn, D0_f_50_recall_als, D0_f_50_recall_bpr, D0_f_50_recall_slim, D0_f_50_recall_vae)
      recall_50label <- c(D0_f_50_recall_poplabel, D0_f_50_recall_itemknnlabel, D0_f_50_recall_alslabel, D0_f_50_recall_bprlabel, D0_f_50_recall_slimlabel, D0_f_50_recall_vaelabel)
      # make dataframe
      recall_f_D <- data.frame(recall_3, recall_3label, recall_5, recall_5label, recall_10, recall_10label, recall_20, recall_20label, recall_50, recall_50label)
      #View(recall_m_D)
      if(threshold == 3){
        dunn <- dunn_test(recall_3 ~ recall_3label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(recall_5 ~ recall_5label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(recall_10 ~ recall_10label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(recall_20 ~ recall_20label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(recall_50 ~ recall_50label, data=recall_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}


for (samp_group in sampling){
  RESULTS <- c(RESULTS, "recall")
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "ALL")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      data_0_pop <- read_delim(paste("full_raw_metrics_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_recall_pop <- data_0_pop$`test/gender_m/recall_at_3`[!is.na(data_0_pop$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_poplabel <- c(rep('D0_f_3_recall_pop', length(D0_f_3_recall_pop)))
      D0_f_3_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_3`[!is.na(data_0_itemknn$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_itemknnlabel <- c(rep('D0_f_3_recall_itemknn', length(D0_f_3_recall_itemknn)))
      D0_f_3_recall_als <- data_0_als$`test/gender_m/recall_at_3`[!is.na(data_0_als$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_alslabel <- c(rep('D0_f_3_recall_als', length(D0_f_3_recall_als)))
      D0_f_3_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_3`[!is.na(data_0_bpr$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_bprlabel <- c(rep('D0_f_3_recall_bpr', length(D0_f_3_recall_bpr)))
      D0_f_3_recall_slim <- data_0_slim$`test/gender_m/recall_at_3`[!is.na(data_0_slim$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_slimlabel <- c(rep('D0_f_3_recall_slim', length(D0_f_3_recall_slim)))
      D0_f_3_recall_vae <- data_0_vae$`test/gender_m/recall_at_3`[!is.na(data_0_vae$`test/gender_m/recall_at_3`)]
      D0_f_3_recall_vaelabel <- c(rep('D0_f_3_recall_vae', length(D0_f_3_recall_vae)))
      D0_m_3_recall_pop <- data_0_pop$`test/gender_m/recall_at_3`[!is.na(data_0_pop$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_poplabel <- c(rep('D0_f_3_recall_pop', length(D0_m_3_recall_pop)))
      D0_m_3_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_3`[!is.na(data_0_itemknn$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_itemknnlabel <- c(rep('D0_f_3_recall_itemknn', length(D0_m_3_recall_itemknn)))
      D0_m_3_recall_als <- data_0_als$`test/gender_m/recall_at_3`[!is.na(data_0_als$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_alslabel <- c(rep('D0_f_3_recall_als', length(D0_m_3_recall_als)))
      D0_m_3_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_3`[!is.na(data_0_bpr$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_bprlabel <- c(rep('D0_f_3_recall_bpr', length(D0_m_3_recall_bpr)))
      D0_m_3_recall_slim <- data_0_slim$`test/gender_m/recall_at_3`[!is.na(data_0_slim$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_slimlabel <- c(rep('D0_f_3_recall_slim', length(D0_m_3_recall_slim)))
      D0_m_3_recall_vae <- data_0_vae$`test/gender_m/recall_at_3`[!is.na(data_0_vae$`test/gender_m/recall_at_3`)]
      D0_m_3_recall_vaelabel <- c(rep('D0_f_3_recall_vae', length(D0_m_3_recall_vae)))
      # combine data
      recall_3 <- c(D0_f_3_recall_pop, D0_f_3_recall_itemknn, D0_f_3_recall_als, D0_f_3_recall_bpr, D0_f_3_recall_slim, D0_f_3_recall_vae, D0_m_3_recall_pop, D0_m_3_recall_itemknn, D0_m_3_recall_als, D0_m_3_recall_bpr, D0_m_3_recall_slim, D0_m_3_recall_vae)
      recall_3label <- c(D0_f_3_recall_poplabel, D0_f_3_recall_itemknnlabel, D0_f_3_recall_alslabel, D0_f_3_recall_bprlabel, D0_f_3_recall_slimlabel, D0_f_3_recall_vaelabel, D0_m_3_recall_poplabel, D0_m_3_recall_itemknnlabel, D0_m_3_recall_alslabel, D0_m_3_recall_bprlabel, D0_m_3_recall_slimlabel, D0_m_3_recall_vaelabel)
      
      D0_f_5_recall_pop <- data_0_pop$`test/gender_m/recall_at_5`[!is.na(data_0_pop$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_poplabel <- c(rep('D0_f_5_recall_pop', length(D0_f_5_recall_pop)))
      D0_f_5_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_5`[!is.na(data_0_itemknn$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_itemknnlabel <- c(rep('D0_f_5_recall_itemknn', length(D0_f_5_recall_itemknn)))
      D0_f_5_recall_als <- data_0_als$`test/gender_m/recall_at_5`[!is.na(data_0_als$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_alslabel <- c(rep('D0_f_5_recall_als', length(D0_f_5_recall_als)))
      D0_f_5_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_5`[!is.na(data_0_bpr$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_bprlabel <- c(rep('D0_f_5_recall_bpr', length(D0_f_5_recall_bpr)))
      D0_f_5_recall_slim <- data_0_slim$`test/gender_m/recall_at_5`[!is.na(data_0_slim$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_slimlabel <- c(rep('D0_f_5_recall_slim', length(D0_f_5_recall_slim)))
      D0_f_5_recall_vae <- data_0_vae$`test/gender_m/recall_at_5`[!is.na(data_0_vae$`test/gender_m/recall_at_5`)]
      D0_f_5_recall_vaelabel <- c(rep('D0_f_5_recall_vae', length(D0_f_5_recall_vae)))
      D0_m_5_recall_pop <- data_0_pop$`test/gender_m/recall_at_5`[!is.na(data_0_pop$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_poplabel <- c(rep('D0_f_5_recall_pop', length(D0_m_5_recall_pop)))
      D0_m_5_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_5`[!is.na(data_0_itemknn$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_itemknnlabel <- c(rep('D0_f_5_recall_itemknn', length(D0_m_5_recall_itemknn)))
      D0_m_5_recall_als <- data_0_als$`test/gender_m/recall_at_5`[!is.na(data_0_als$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_alslabel <- c(rep('D0_f_5_recall_als', length(D0_m_5_recall_als)))
      D0_m_5_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_5`[!is.na(data_0_bpr$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_bprlabel <- c(rep('D0_f_5_recall_bpr', length(D0_m_5_recall_bpr)))
      D0_m_5_recall_slim <- data_0_slim$`test/gender_m/recall_at_5`[!is.na(data_0_slim$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_slimlabel <- c(rep('D0_f_5_recall_slim', length(D0_m_5_recall_slim)))
      D0_m_5_recall_vae <- data_0_vae$`test/gender_m/recall_at_5`[!is.na(data_0_vae$`test/gender_m/recall_at_5`)]
      D0_m_5_recall_vaelabel <- c(rep('D0_f_5_recall_vae', length(D0_m_5_recall_vae)))
      # combine data
      recall_5 <- c(D0_f_5_recall_pop, D0_f_5_recall_itemknn, D0_f_5_recall_als, D0_f_5_recall_bpr, D0_f_5_recall_slim, D0_f_5_recall_vae, D0_m_5_recall_pop, D0_m_5_recall_itemknn, D0_m_5_recall_als, D0_m_5_recall_bpr, D0_m_5_recall_slim, D0_m_5_recall_vae)
      recall_5label <- c(D0_f_5_recall_poplabel, D0_f_5_recall_itemknnlabel, D0_f_5_recall_alslabel, D0_f_5_recall_bprlabel, D0_f_5_recall_slimlabel, D0_f_5_recall_vaelabel, D0_m_5_recall_poplabel, D0_m_5_recall_itemknnlabel, D0_m_5_recall_alslabel, D0_m_5_recall_bprlabel, D0_m_5_recall_slimlabel, D0_m_5_recall_vaelabel)
      
      
      D0_f_10_recall_pop <- data_0_pop$`test/gender_m/recall_at_10`[!is.na(data_0_pop$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_poplabel <- c(rep('D0_f_10_recall_pop', length(D0_f_10_recall_pop)))
      D0_f_10_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_10`[!is.na(data_0_itemknn$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_itemknnlabel <- c(rep('D0_f_10_recall_itemknn', length(D0_f_10_recall_itemknn)))
      D0_f_10_recall_als <- data_0_als$`test/gender_m/recall_at_10`[!is.na(data_0_als$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_alslabel <- c(rep('D0_f_10_recall_als', length(D0_f_10_recall_als)))
      D0_f_10_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_10`[!is.na(data_0_bpr$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_bprlabel <- c(rep('D0_f_10_recall_bpr', length(D0_f_10_recall_bpr)))
      D0_f_10_recall_slim <- data_0_slim$`test/gender_m/recall_at_10`[!is.na(data_0_slim$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_slimlabel <- c(rep('D0_f_10_recall_slim', length(D0_f_10_recall_slim)))
      D0_f_10_recall_vae <- data_0_vae$`test/gender_m/recall_at_10`[!is.na(data_0_vae$`test/gender_m/recall_at_10`)]
      D0_f_10_recall_vaelabel <- c(rep('D0_f_10_recall_vae', length(D0_f_10_recall_vae)))
      D0_m_10_recall_pop <- data_0_pop$`test/gender_m/recall_at_10`[!is.na(data_0_pop$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_poplabel <- c(rep('D0_f_10_recall_pop', length(D0_m_10_recall_pop)))
      D0_m_10_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_10`[!is.na(data_0_itemknn$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_itemknnlabel <- c(rep('D0_f_10_recall_itemknn', length(D0_m_10_recall_itemknn)))
      D0_m_10_recall_als <- data_0_als$`test/gender_m/recall_at_10`[!is.na(data_0_als$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_alslabel <- c(rep('D0_f_10_recall_als', length(D0_m_10_recall_als)))
      D0_m_10_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_10`[!is.na(data_0_bpr$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_bprlabel <- c(rep('D0_f_10_recall_bpr', length(D0_m_10_recall_bpr)))
      D0_m_10_recall_slim <- data_0_slim$`test/gender_m/recall_at_10`[!is.na(data_0_slim$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_slimlabel <- c(rep('D0_f_10_recall_slim', length(D0_m_10_recall_slim)))
      D0_m_10_recall_vae <- data_0_vae$`test/gender_m/recall_at_10`[!is.na(data_0_vae$`test/gender_m/recall_at_10`)]
      D0_m_10_recall_vaelabel <- c(rep('D0_f_10_recall_vae', length(D0_m_10_recall_vae)))
      # combine data
      recall_10 <- c(D0_f_10_recall_pop, D0_f_10_recall_itemknn, D0_f_10_recall_als, D0_f_10_recall_bpr, D0_f_10_recall_slim, D0_f_10_recall_vae, D0_m_10_recall_pop, D0_m_10_recall_itemknn, D0_m_10_recall_als, D0_m_10_recall_bpr, D0_m_10_recall_slim, D0_m_10_recall_vae)
      recall_10label <- c(D0_f_10_recall_poplabel, D0_f_10_recall_itemknnlabel, D0_f_10_recall_alslabel, D0_f_10_recall_bprlabel, D0_f_10_recall_slimlabel, D0_f_10_recall_vaelabel, D0_m_10_recall_poplabel, D0_m_10_recall_itemknnlabel, D0_m_10_recall_alslabel, D0_m_10_recall_bprlabel, D0_m_10_recall_slimlabel, D0_m_10_recall_vaelabel)
      
      
      D0_f_20_recall_pop <- data_0_pop$`test/gender_m/recall_at_20`[!is.na(data_0_pop$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_poplabel <- c(rep('D0_f_20_recall_pop', length(D0_f_20_recall_pop)))
      D0_f_20_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_20`[!is.na(data_0_itemknn$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_itemknnlabel <- c(rep('D0_f_20_recall_itemknn', length(D0_f_20_recall_itemknn)))
      D0_f_20_recall_als <- data_0_als$`test/gender_m/recall_at_20`[!is.na(data_0_als$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_alslabel <- c(rep('D0_f_20_recall_als', length(D0_f_20_recall_als)))
      D0_f_20_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_20`[!is.na(data_0_bpr$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_bprlabel <- c(rep('D0_f_20_recall_bpr', length(D0_f_20_recall_bpr)))
      D0_f_20_recall_slim <- data_0_slim$`test/gender_m/recall_at_20`[!is.na(data_0_slim$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_slimlabel <- c(rep('D0_f_20_recall_slim', length(D0_f_20_recall_slim)))
      D0_f_20_recall_vae <- data_0_vae$`test/gender_m/recall_at_20`[!is.na(data_0_vae$`test/gender_m/recall_at_20`)]
      D0_f_20_recall_vaelabel <- c(rep('D0_f_20_recall_vae', length(D0_f_20_recall_vae)))
      D0_m_20_recall_pop <- data_0_pop$`test/gender_m/recall_at_20`[!is.na(data_0_pop$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_poplabel <- c(rep('D0_f_20_recall_pop', length(D0_m_20_recall_pop)))
      D0_m_20_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_20`[!is.na(data_0_itemknn$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_itemknnlabel <- c(rep('D0_f_20_recall_itemknn', length(D0_m_20_recall_itemknn)))
      D0_m_20_recall_als <- data_0_als$`test/gender_m/recall_at_20`[!is.na(data_0_als$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_alslabel <- c(rep('D0_f_20_recall_als', length(D0_m_20_recall_als)))
      D0_m_20_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_20`[!is.na(data_0_bpr$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_bprlabel <- c(rep('D0_f_20_recall_bpr', length(D0_m_20_recall_bpr)))
      D0_m_20_recall_slim <- data_0_slim$`test/gender_m/recall_at_20`[!is.na(data_0_slim$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_slimlabel <- c(rep('D0_f_20_recall_slim', length(D0_m_20_recall_slim)))
      D0_m_20_recall_vae <- data_0_vae$`test/gender_m/recall_at_20`[!is.na(data_0_vae$`test/gender_m/recall_at_20`)]
      D0_m_20_recall_vaelabel <- c(rep('D0_f_20_recall_vae', length(D0_m_20_recall_vae)))
      # combine data
      recall_20 <- c(D0_f_20_recall_pop, D0_f_20_recall_itemknn, D0_f_20_recall_als, D0_f_20_recall_bpr, D0_f_20_recall_slim, D0_f_20_recall_vae, D0_m_20_recall_pop, D0_m_20_recall_itemknn, D0_m_20_recall_als, D0_m_20_recall_bpr, D0_m_20_recall_slim, D0_m_20_recall_vae)
      recall_20label <- c(D0_f_20_recall_poplabel, D0_f_20_recall_itemknnlabel, D0_f_20_recall_alslabel, D0_f_20_recall_bprlabel, D0_f_20_recall_slimlabel, D0_f_20_recall_vaelabel, D0_m_20_recall_poplabel, D0_m_20_recall_itemknnlabel, D0_m_20_recall_alslabel, D0_m_20_recall_bprlabel, D0_m_20_recall_slimlabel, D0_m_20_recall_vaelabel)
      
      
      D0_f_50_recall_pop <- data_0_pop$`test/gender_m/recall_at_50`[!is.na(data_0_pop$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_poplabel <- c(rep('D0_f_50_recall_pop', length(D0_f_50_recall_pop)))
      D0_f_50_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_50`[!is.na(data_0_itemknn$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_itemknnlabel <- c(rep('D0_f_50_recall_itemknn', length(D0_f_50_recall_itemknn)))
      D0_f_50_recall_als <- data_0_als$`test/gender_m/recall_at_50`[!is.na(data_0_als$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_alslabel <- c(rep('D0_f_50_recall_als', length(D0_f_50_recall_als)))
      D0_f_50_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_50`[!is.na(data_0_bpr$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_bprlabel <- c(rep('D0_f_50_recall_bpr', length(D0_f_50_recall_bpr)))
      D0_f_50_recall_slim <- data_0_slim$`test/gender_m/recall_at_50`[!is.na(data_0_slim$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_slimlabel <- c(rep('D0_f_50_recall_slim', length(D0_f_50_recall_slim)))
      D0_f_50_recall_vae <- data_0_vae$`test/gender_m/recall_at_50`[!is.na(data_0_vae$`test/gender_m/recall_at_50`)]
      D0_f_50_recall_vaelabel <- c(rep('D0_f_50_recall_vae', length(D0_f_50_recall_vae)))
      D0_m_50_recall_pop <- data_0_pop$`test/gender_m/recall_at_50`[!is.na(data_0_pop$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_poplabel <- c(rep('D0_f_50_recall_pop', length(D0_m_50_recall_pop)))
      D0_m_50_recall_itemknn <- data_0_itemknn$`test/gender_m/recall_at_50`[!is.na(data_0_itemknn$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_itemknnlabel <- c(rep('D0_f_50_recall_itemknn', length(D0_m_50_recall_itemknn)))
      D0_m_50_recall_als <- data_0_als$`test/gender_m/recall_at_50`[!is.na(data_0_als$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_alslabel <- c(rep('D0_f_50_recall_als', length(D0_m_50_recall_als)))
      D0_m_50_recall_bpr <- data_0_bpr$`test/gender_m/recall_at_50`[!is.na(data_0_bpr$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_bprlabel <- c(rep('D0_f_50_recall_bpr', length(D0_m_50_recall_bpr)))
      D0_m_50_recall_slim <- data_0_slim$`test/gender_m/recall_at_50`[!is.na(data_0_slim$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_slimlabel <- c(rep('D0_f_50_recall_slim', length(D0_m_50_recall_slim)))
      D0_m_50_recall_vae <- data_0_vae$`test/gender_m/recall_at_50`[!is.na(data_0_vae$`test/gender_m/recall_at_50`)]
      D0_m_50_recall_vaelabel <- c(rep('D0_f_50_recall_vae', length(D0_m_50_recall_vae)))
      # combine data
      recall_50 <- c(D0_f_50_recall_pop, D0_f_50_recall_itemknn, D0_f_50_recall_als, D0_f_50_recall_bpr, D0_f_50_recall_slim, D0_f_50_recall_vae, D0_m_50_recall_pop, D0_m_50_recall_itemknn, D0_m_50_recall_als, D0_m_50_recall_bpr, D0_m_50_recall_slim, D0_m_50_recall_vae)
      recall_50label <- c(D0_f_50_recall_poplabel, D0_f_50_recall_itemknnlabel, D0_f_50_recall_alslabel, D0_f_50_recall_bprlabel, D0_f_50_recall_slimlabel, D0_f_50_recall_vaelabel, D0_m_50_recall_poplabel, D0_m_50_recall_itemknnlabel, D0_m_50_recall_alslabel, D0_m_50_recall_bprlabel, D0_m_50_recall_slimlabel, D0_m_50_recall_vaelabel)
      # make dataframe
      recall_f_D <- data.frame(recall_3, recall_3label, recall_5, recall_5label, recall_10, recall_10label, recall_20, recall_20label, recall_50, recall_50label)
      #View(recall_f_D)
      if(threshold == 3){
        dunn <- dunn_test(recall_3 ~ recall_3label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(recall_5 ~ recall_5label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(recall_10 ~ recall_10label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(recall_20 ~ recall_20label, data=recall_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(recall_50 ~ recall_50label, data=recall_m_D, p.adjust.method = "bonferroni")
      }
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    
    ##########################
    #   PERFORM STATISTICS   #
    ##########################
    RESULTS <- get_z("alsVSbpr", as.list(p_d$"1"), RESULTS)
    RESULTS <- get_z("alsVitemknn", as.list(p_d$"2"), RESULTS)
    RESULTS <- get_z("alsVSpop", as.list(p_d$"3"), RESULTS)
    RESULTS <- get_z("alsVSslim", as.list(p_d$"4"), RESULTS)
    RESULTS <- get_z("alsVSvae", as.list(p_d$"5"), RESULTS)
    RESULTS <- get_z("bprVSitemknn", as.list(p_d$"6"), RESULTS)
    RESULTS <- get_z("bprVSpop", as.list(p_d$"7"), RESULTS)
    RESULTS <- get_z("bprVSslim", as.list(p_d$"8"), RESULTS)
    RESULTS <- get_z("bprVSvae", as.list(p_d$"9"), RESULTS)
    RESULTS <- get_z("itemknnVSpop", as.list(p_d$"10"), RESULTS)
    RESULTS <- get_z("itemknnVSslim", as.list(p_d$"11"), RESULTS)
    RESULTS <- get_z("itemknnVSvae", as.list(p_d$"12"), RESULTS)
    RESULTS <- get_z("popVSslim", as.list(p_d$"13"), RESULTS)
    RESULTS <- get_z("popVSvae", as.list(p_d$"14"), RESULTS)
    RESULTS <- get_z("slimVSvae", as.list(p_d$"NA"), RESULTS)
  }
}

sink('analysis-output_recall_NDCG.txt')
for (elem in RESULTS){
  print(elem)  
} 
sink('analysis-output_recall_NDCG.txt', append=TRUE)
sink()
