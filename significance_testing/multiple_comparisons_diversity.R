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
RESULTS <- c(RESULTS, "DIVERSITY")

get_z <- function(comparison, p_values, RESULTS) {
  print(comparison)
  RESULTS <- c(RESULTS, comparison)
  #z <- combinePValues(p_values, method="z", weights=1:5)
  #z <- combinePValues(p_values, method="holm-middle")
  z <- combinePValues(p_values, method="berger")
  print(z)
  RESULTS <- c(RESULTS, z)
  return <- RESULTS
}

for (samp_group in sampling){
  print("RESULTS FOR MALES/DIVERSITY")
  print(samp_group)
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "MALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      print(fold)
      data_0_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_m_3_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_3`[!is.na(data_0_pop$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_poplabel <- c(rep('D0_m_3_diversity_pop', length(D0_m_3_diversity_pop)))
      D0_m_3_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_3`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_itemknnlabel <- c(rep('D0_m_3_diversity_itemknn', length(D0_m_3_diversity_itemknn)))
      D0_m_3_diversity_als <- data_0_als$`test/gender_m/diversity_at_3`[!is.na(data_0_als$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_alslabel <- c(rep('D0_m_3_diversity_als', length(D0_m_3_diversity_als)))
      D0_m_3_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_3`[!is.na(data_0_bpr$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_bprlabel <- c(rep('D0_m_3_diversity_bpr', length(D0_m_3_diversity_bpr)))
      D0_m_3_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_3`[!is.na(data_0_slim$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_slimlabel <- c(rep('D0_m_3_diversity_slim', length(D0_m_3_diversity_slim)))
      D0_m_3_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_3`[!is.na(data_0_vae$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_vaelabel <- c(rep('D0_m_3_diversity_vae', length(D0_m_3_diversity_vae)))
      # combine data
      diversity_3 <- c(D0_m_3_diversity_pop, D0_m_3_diversity_itemknn, D0_m_3_diversity_als, D0_m_3_diversity_bpr, D0_m_3_diversity_slim, D0_m_3_diversity_vae)
      diversity_3label <- c(D0_m_3_diversity_poplabel, D0_m_3_diversity_itemknnlabel, D0_m_3_diversity_alslabel, D0_m_3_diversity_bprlabel, D0_m_3_diversity_slimlabel, D0_m_3_diversity_vaelabel)
      
      D0_m_5_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_5`[!is.na(data_0_pop$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_poplabel <- c(rep('D0_m_5_diversity_pop', length(D0_m_5_diversity_pop)))
      D0_m_5_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_5`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_itemknnlabel <- c(rep('D0_m_5_diversity_itemknn', length(D0_m_5_diversity_itemknn)))
      D0_m_5_diversity_als <- data_0_als$`test/gender_m/diversity_at_5`[!is.na(data_0_als$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_alslabel <- c(rep('D0_m_5_diversity_als', length(D0_m_5_diversity_als)))
      D0_m_5_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_5`[!is.na(data_0_bpr$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_bprlabel <- c(rep('D0_m_5_diversity_bpr', length(D0_m_5_diversity_bpr)))
      D0_m_5_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_5`[!is.na(data_0_slim$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_slimlabel <- c(rep('D0_m_5_diversity_slim', length(D0_m_5_diversity_slim)))
      D0_m_5_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_5`[!is.na(data_0_vae$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_vaelabel <- c(rep('D0_m_5_diversity_vae', length(D0_m_5_diversity_vae)))
      # combine data
      diversity_5 <- c(D0_m_5_diversity_pop, D0_m_5_diversity_itemknn, D0_m_5_diversity_als, D0_m_5_diversity_bpr, D0_m_5_diversity_slim, D0_m_5_diversity_vae)
      diversity_5label <- c(D0_m_5_diversity_poplabel, D0_m_5_diversity_itemknnlabel, D0_m_5_diversity_alslabel, D0_m_5_diversity_bprlabel, D0_m_5_diversity_slimlabel, D0_m_5_diversity_vaelabel)
      
      D0_m_10_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_10`[!is.na(data_0_pop$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_poplabel <- c(rep('D0_m_10_diversity_pop', length(D0_m_10_diversity_pop)))
      D0_m_10_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_10`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_itemknnlabel <- c(rep('D0_m_10_diversity_itemknn', length(D0_m_10_diversity_itemknn)))
      D0_m_10_diversity_als <- data_0_als$`test/gender_m/diversity_at_10`[!is.na(data_0_als$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_alslabel <- c(rep('D0_m_10_diversity_als', length(D0_m_10_diversity_als)))
      D0_m_10_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_10`[!is.na(data_0_bpr$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_bprlabel <- c(rep('D0_m_10_diversity_bpr', length(D0_m_10_diversity_bpr)))
      D0_m_10_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_10`[!is.na(data_0_slim$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_slimlabel <- c(rep('D0_m_10_diversity_slim', length(D0_m_10_diversity_slim)))
      D0_m_10_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_10`[!is.na(data_0_vae$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_vaelabel <- c(rep('D0_m_10_diversity_vae', length(D0_m_10_diversity_vae)))
      # combine data
      diversity_10 <- c(D0_m_10_diversity_pop, D0_m_10_diversity_itemknn, D0_m_10_diversity_als, D0_m_10_diversity_bpr, D0_m_10_diversity_slim, D0_m_10_diversity_vae)
      diversity_10label <- c(D0_m_10_diversity_poplabel, D0_m_10_diversity_itemknnlabel, D0_m_10_diversity_alslabel, D0_m_10_diversity_bprlabel, D0_m_10_diversity_slimlabel, D0_m_10_diversity_vaelabel)
      
      D0_m_20_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_20`[!is.na(data_0_pop$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_poplabel <- c(rep('D0_m_20_diversity_pop', length(D0_m_20_diversity_pop)))
      D0_m_20_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_20`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_itemknnlabel <- c(rep('D0_m_20_diversity_itemknn', length(D0_m_20_diversity_itemknn)))
      D0_m_20_diversity_als <- data_0_als$`test/gender_m/diversity_at_20`[!is.na(data_0_als$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_alslabel <- c(rep('D0_m_20_diversity_als', length(D0_m_20_diversity_als)))
      D0_m_20_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_20`[!is.na(data_0_bpr$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_bprlabel <- c(rep('D0_m_20_diversity_bpr', length(D0_m_20_diversity_bpr)))
      D0_m_20_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_20`[!is.na(data_0_slim$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_slimlabel <- c(rep('D0_m_20_diversity_slim', length(D0_m_20_diversity_slim)))
      D0_m_20_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_20`[!is.na(data_0_vae$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_vaelabel <- c(rep('D0_m_20_diversity_vae', length(D0_m_20_diversity_vae)))
      # combine data
      diversity_20 <- c(D0_m_20_diversity_pop, D0_m_20_diversity_itemknn, D0_m_20_diversity_als, D0_m_20_diversity_bpr, D0_m_20_diversity_slim, D0_m_20_diversity_vae)
      diversity_20label <- c(D0_m_20_diversity_poplabel, D0_m_20_diversity_itemknnlabel, D0_m_20_diversity_alslabel, D0_m_20_diversity_bprlabel, D0_m_20_diversity_slimlabel, D0_m_20_diversity_vaelabel)
      
      D0_m_50_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_50`[!is.na(data_0_pop$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_poplabel <- c(rep('D0_m_50_diversity_pop', length(D0_m_50_diversity_pop)))
      D0_m_50_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_50`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_itemknnlabel <- c(rep('D0_m_50_diversity_itemknn', length(D0_m_50_diversity_itemknn)))
      D0_m_50_diversity_als <- data_0_als$`test/gender_m/diversity_at_50`[!is.na(data_0_als$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_alslabel <- c(rep('D0_m_50_diversity_als', length(D0_m_50_diversity_als)))
      D0_m_50_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_50`[!is.na(data_0_bpr$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_bprlabel <- c(rep('D0_m_50_diversity_bpr', length(D0_m_50_diversity_bpr)))
      D0_m_50_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_50`[!is.na(data_0_slim$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_slimlabel <- c(rep('D0_m_50_diversity_slim', length(D0_m_50_diversity_slim)))
      D0_m_50_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_50`[!is.na(data_0_vae$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_vaelabel <- c(rep('D0_m_50_diversity_vae', length(D0_m_50_diversity_vae)))
      # combine data
      diversity_50 <- c(D0_m_50_diversity_pop, D0_m_50_diversity_itemknn, D0_m_50_diversity_als, D0_m_50_diversity_bpr, D0_m_50_diversity_slim, D0_m_50_diversity_vae)
      diversity_50label <- c(D0_m_50_diversity_poplabel, D0_m_50_diversity_itemknnlabel, D0_m_50_diversity_alslabel, D0_m_50_diversity_bprlabel, D0_m_50_diversity_slimlabel, D0_m_50_diversity_vaelabel)
      # make dataframe
      diversity_m_D <- data.frame(diversity_3, diversity_3label, diversity_5, diversity_5label, diversity_10, diversity_10label, diversity_20, diversity_20label, diversity_50, diversity_50label)
      #View(diversity_m_D)
      if(threshold == 3){
        dunn <- dunn_test(diversity_3 ~ diversity_3label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(diversity_5 ~ diversity_5label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(diversity_10 ~ diversity_10label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(diversity_20 ~ diversity_20label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(diversity_50 ~ diversity_50label, data=diversity_m_D, p.adjust.method = "bonferroni")
      }
      print(dunn$group1)
      print(dunn$group2)
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    print(p_d)
  
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
  print("RESULTS FOR FEMALES/DIVERSITY")
  print(samp_group)
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "FEMALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      print(fold)
      data_0_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_3`[!is.na(data_0_pop$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_poplabel <- c(rep('D0_f_3_diversity_pop', length(D0_f_3_diversity_pop)))
      D0_f_3_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_3`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_itemknnlabel <- c(rep('D0_f_3_diversity_itemknn', length(D0_f_3_diversity_itemknn)))
      D0_f_3_diversity_als <- data_0_als$`test/gender_m/diversity_at_3`[!is.na(data_0_als$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_alslabel <- c(rep('D0_f_3_diversity_als', length(D0_f_3_diversity_als)))
      D0_f_3_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_3`[!is.na(data_0_bpr$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_bprlabel <- c(rep('D0_f_3_diversity_bpr', length(D0_f_3_diversity_bpr)))
      D0_f_3_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_3`[!is.na(data_0_slim$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_slimlabel <- c(rep('D0_f_3_diversity_slim', length(D0_f_3_diversity_slim)))
      D0_f_3_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_3`[!is.na(data_0_vae$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_vaelabel <- c(rep('D0_f_3_diversity_vae', length(D0_f_3_diversity_vae)))
      # combine data
      diversity_3 <- c(D0_f_3_diversity_pop, D0_f_3_diversity_itemknn, D0_f_3_diversity_als, D0_f_3_diversity_bpr, D0_f_3_diversity_slim, D0_f_3_diversity_vae)
      diversity_3label <- c(D0_f_3_diversity_poplabel, D0_f_3_diversity_itemknnlabel, D0_f_3_diversity_alslabel, D0_f_3_diversity_bprlabel, D0_f_3_diversity_slimlabel, D0_f_3_diversity_vaelabel)
      
      D0_f_5_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_5`[!is.na(data_0_pop$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_poplabel <- c(rep('D0_f_5_diversity_pop', length(D0_f_5_diversity_pop)))
      D0_f_5_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_5`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_itemknnlabel <- c(rep('D0_f_5_diversity_itemknn', length(D0_f_5_diversity_itemknn)))
      D0_f_5_diversity_als <- data_0_als$`test/gender_m/diversity_at_5`[!is.na(data_0_als$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_alslabel <- c(rep('D0_f_5_diversity_als', length(D0_f_5_diversity_als)))
      D0_f_5_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_5`[!is.na(data_0_bpr$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_bprlabel <- c(rep('D0_f_5_diversity_bpr', length(D0_f_5_diversity_bpr)))
      D0_f_5_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_5`[!is.na(data_0_slim$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_slimlabel <- c(rep('D0_f_5_diversity_slim', length(D0_f_5_diversity_slim)))
      D0_f_5_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_5`[!is.na(data_0_vae$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_vaelabel <- c(rep('D0_f_5_diversity_vae', length(D0_f_5_diversity_vae)))
      # combine data
      diversity_5 <- c(D0_f_5_diversity_pop, D0_f_5_diversity_itemknn, D0_f_5_diversity_als, D0_f_5_diversity_bpr, D0_f_5_diversity_slim, D0_f_5_diversity_vae)
      diversity_5label <- c(D0_f_5_diversity_poplabel, D0_f_5_diversity_itemknnlabel, D0_f_5_diversity_alslabel, D0_f_5_diversity_bprlabel, D0_f_5_diversity_slimlabel, D0_f_5_diversity_vaelabel)
      
      D0_f_10_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_10`[!is.na(data_0_pop$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_poplabel <- c(rep('D0_f_10_diversity_pop', length(D0_f_10_diversity_pop)))
      D0_f_10_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_10`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_itemknnlabel <- c(rep('D0_f_10_diversity_itemknn', length(D0_f_10_diversity_itemknn)))
      D0_f_10_diversity_als <- data_0_als$`test/gender_m/diversity_at_10`[!is.na(data_0_als$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_alslabel <- c(rep('D0_f_10_diversity_als', length(D0_f_10_diversity_als)))
      D0_f_10_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_10`[!is.na(data_0_bpr$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_bprlabel <- c(rep('D0_f_10_diversity_bpr', length(D0_f_10_diversity_bpr)))
      D0_f_10_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_10`[!is.na(data_0_slim$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_slimlabel <- c(rep('D0_f_10_diversity_slim', length(D0_f_10_diversity_slim)))
      D0_f_10_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_10`[!is.na(data_0_vae$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_vaelabel <- c(rep('D0_f_10_diversity_vae', length(D0_f_10_diversity_vae)))
      # combine data
      diversity_10 <- c(D0_f_10_diversity_pop, D0_f_10_diversity_itemknn, D0_f_10_diversity_als, D0_f_10_diversity_bpr, D0_f_10_diversity_slim, D0_f_10_diversity_vae)
      diversity_10label <- c(D0_f_10_diversity_poplabel, D0_f_10_diversity_itemknnlabel, D0_f_10_diversity_alslabel, D0_f_10_diversity_bprlabel, D0_f_10_diversity_slimlabel, D0_f_10_diversity_vaelabel)
      
      D0_f_20_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_20`[!is.na(data_0_pop$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_poplabel <- c(rep('D0_f_20_diversity_pop', length(D0_f_20_diversity_pop)))
      D0_f_20_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_20`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_itemknnlabel <- c(rep('D0_f_20_diversity_itemknn', length(D0_f_20_diversity_itemknn)))
      D0_f_20_diversity_als <- data_0_als$`test/gender_m/diversity_at_20`[!is.na(data_0_als$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_alslabel <- c(rep('D0_f_20_diversity_als', length(D0_f_20_diversity_als)))
      D0_f_20_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_20`[!is.na(data_0_bpr$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_bprlabel <- c(rep('D0_f_20_diversity_bpr', length(D0_f_20_diversity_bpr)))
      D0_f_20_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_20`[!is.na(data_0_slim$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_slimlabel <- c(rep('D0_f_20_diversity_slim', length(D0_f_20_diversity_slim)))
      D0_f_20_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_20`[!is.na(data_0_vae$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_vaelabel <- c(rep('D0_f_20_diversity_vae', length(D0_f_20_diversity_vae)))
      # combine data
      diversity_20 <- c(D0_f_20_diversity_pop, D0_f_20_diversity_itemknn, D0_f_20_diversity_als, D0_f_20_diversity_bpr, D0_f_20_diversity_slim, D0_f_20_diversity_vae)
      diversity_20label <- c(D0_f_20_diversity_poplabel, D0_f_20_diversity_itemknnlabel, D0_f_20_diversity_alslabel, D0_f_20_diversity_bprlabel, D0_f_20_diversity_slimlabel, D0_f_20_diversity_vaelabel)
      
      D0_f_50_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_50`[!is.na(data_0_pop$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_poplabel <- c(rep('D0_f_50_diversity_pop', length(D0_f_50_diversity_pop)))
      D0_f_50_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_50`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_itemknnlabel <- c(rep('D0_f_50_diversity_itemknn', length(D0_f_50_diversity_itemknn)))
      D0_f_50_diversity_als <- data_0_als$`test/gender_m/diversity_at_50`[!is.na(data_0_als$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_alslabel <- c(rep('D0_f_50_diversity_als', length(D0_f_50_diversity_als)))
      D0_f_50_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_50`[!is.na(data_0_bpr$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_bprlabel <- c(rep('D0_f_50_diversity_bpr', length(D0_f_50_diversity_bpr)))
      D0_f_50_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_50`[!is.na(data_0_slim$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_slimlabel <- c(rep('D0_f_50_diversity_slim', length(D0_f_50_diversity_slim)))
      D0_f_50_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_50`[!is.na(data_0_vae$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_vaelabel <- c(rep('D0_f_50_diversity_vae', length(D0_f_50_diversity_vae)))
      # combine data
      diversity_50 <- c(D0_f_50_diversity_pop, D0_f_50_diversity_itemknn, D0_f_50_diversity_als, D0_f_50_diversity_bpr, D0_f_50_diversity_slim, D0_f_50_diversity_vae)
      diversity_50label <- c(D0_f_50_diversity_poplabel, D0_f_50_diversity_itemknnlabel, D0_f_50_diversity_alslabel, D0_f_50_diversity_bprlabel, D0_f_50_diversity_slimlabel, D0_f_50_diversity_vaelabel)
      # make dataframe
      diversity_f_D <- data.frame(diversity_3, diversity_3label, diversity_5, diversity_5label, diversity_10, diversity_10label, diversity_20, diversity_20label, diversity_50, diversity_50label)
      #View(diversity_m_D)
      if(threshold == 3){
        dunn <- dunn_test(diversity_3 ~ diversity_3label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(diversity_5 ~ diversity_5label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(diversity_10 ~ diversity_10label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(diversity_20 ~ diversity_20label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(diversity_50 ~ diversity_50label, data=diversity_m_D, p.adjust.method = "bonferroni")
      }
      print(dunn$group1)
      print(dunn$group2)
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    print(p_d)
    
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
  print("RESULTS FOR ALL/DIVERSITY")
  print(samp_group)
  RESULTS <- c(RESULTS, "===========================================")
  RESULTS <- c(RESULTS, samp_group)
  RESULTS <- c(RESULTS, "ALL")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "__________________________________________")
    RESULTS <- c(RESULTS, threshold)
    p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
    for (fold in partitioning){
      print(fold)
      data_0_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      data_0_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_", fold, ".csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
      
      D0_f_3_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_3`[!is.na(data_0_pop$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_poplabel <- c(rep('D0_f_3_diversity_pop', length(D0_f_3_diversity_pop)))
      D0_f_3_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_3`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_itemknnlabel <- c(rep('D0_f_3_diversity_itemknn', length(D0_f_3_diversity_itemknn)))
      D0_f_3_diversity_als <- data_0_als$`test/gender_m/diversity_at_3`[!is.na(data_0_als$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_alslabel <- c(rep('D0_f_3_diversity_als', length(D0_f_3_diversity_als)))
      D0_f_3_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_3`[!is.na(data_0_bpr$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_bprlabel <- c(rep('D0_f_3_diversity_bpr', length(D0_f_3_diversity_bpr)))
      D0_f_3_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_3`[!is.na(data_0_slim$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_slimlabel <- c(rep('D0_f_3_diversity_slim', length(D0_f_3_diversity_slim)))
      D0_f_3_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_3`[!is.na(data_0_vae$`test/gender_m/diversity_at_3`)]
      D0_f_3_diversity_vaelabel <- c(rep('D0_f_3_diversity_vae', length(D0_f_3_diversity_vae)))
      D0_m_3_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_3`[!is.na(data_0_pop$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_poplabel <- c(rep('D0_f_3_diversity_pop', length(D0_m_3_diversity_pop)))
      D0_m_3_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_3`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_itemknnlabel <- c(rep('D0_f_3_diversity_itemknn', length(D0_m_3_diversity_itemknn)))
      D0_m_3_diversity_als <- data_0_als$`test/gender_m/diversity_at_3`[!is.na(data_0_als$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_alslabel <- c(rep('D0_f_3_diversity_als', length(D0_m_3_diversity_als)))
      D0_m_3_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_3`[!is.na(data_0_bpr$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_bprlabel <- c(rep('D0_f_3_diversity_bpr', length(D0_m_3_diversity_bpr)))
      D0_m_3_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_3`[!is.na(data_0_slim$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_slimlabel <- c(rep('D0_f_3_diversity_slim', length(D0_m_3_diversity_slim)))
      D0_m_3_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_3`[!is.na(data_0_vae$`test/gender_m/diversity_at_3`)]
      D0_m_3_diversity_vaelabel <- c(rep('D0_f_3_diversity_vae', length(D0_m_3_diversity_vae)))
      # combine data
      diversity_3 <- c(D0_f_3_diversity_pop, D0_f_3_diversity_itemknn, D0_f_3_diversity_als, D0_f_3_diversity_bpr, D0_f_3_diversity_slim, D0_f_3_diversity_vae, D0_m_3_diversity_pop, D0_m_3_diversity_itemknn, D0_m_3_diversity_als, D0_m_3_diversity_bpr, D0_m_3_diversity_slim, D0_m_3_diversity_vae)
      diversity_3label <- c(D0_f_3_diversity_poplabel, D0_f_3_diversity_itemknnlabel, D0_f_3_diversity_alslabel, D0_f_3_diversity_bprlabel, D0_f_3_diversity_slimlabel, D0_f_3_diversity_vaelabel, D0_m_3_diversity_poplabel, D0_m_3_diversity_itemknnlabel, D0_m_3_diversity_alslabel, D0_m_3_diversity_bprlabel, D0_m_3_diversity_slimlabel, D0_m_3_diversity_vaelabel)
      
      D0_f_5_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_5`[!is.na(data_0_pop$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_poplabel <- c(rep('D0_f_5_diversity_pop', length(D0_f_5_diversity_pop)))
      D0_f_5_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_5`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_itemknnlabel <- c(rep('D0_f_5_diversity_itemknn', length(D0_f_5_diversity_itemknn)))
      D0_f_5_diversity_als <- data_0_als$`test/gender_m/diversity_at_5`[!is.na(data_0_als$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_alslabel <- c(rep('D0_f_5_diversity_als', length(D0_f_5_diversity_als)))
      D0_f_5_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_5`[!is.na(data_0_bpr$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_bprlabel <- c(rep('D0_f_5_diversity_bpr', length(D0_f_5_diversity_bpr)))
      D0_f_5_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_5`[!is.na(data_0_slim$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_slimlabel <- c(rep('D0_f_5_diversity_slim', length(D0_f_5_diversity_slim)))
      D0_f_5_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_5`[!is.na(data_0_vae$`test/gender_m/diversity_at_5`)]
      D0_f_5_diversity_vaelabel <- c(rep('D0_f_5_diversity_vae', length(D0_f_5_diversity_vae)))
      D0_m_5_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_5`[!is.na(data_0_pop$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_poplabel <- c(rep('D0_f_5_diversity_pop', length(D0_m_5_diversity_pop)))
      D0_m_5_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_5`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_itemknnlabel <- c(rep('D0_f_5_diversity_itemknn', length(D0_m_5_diversity_itemknn)))
      D0_m_5_diversity_als <- data_0_als$`test/gender_m/diversity_at_5`[!is.na(data_0_als$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_alslabel <- c(rep('D0_f_5_diversity_als', length(D0_m_5_diversity_als)))
      D0_m_5_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_5`[!is.na(data_0_bpr$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_bprlabel <- c(rep('D0_f_5_diversity_bpr', length(D0_m_5_diversity_bpr)))
      D0_m_5_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_5`[!is.na(data_0_slim$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_slimlabel <- c(rep('D0_f_5_diversity_slim', length(D0_m_5_diversity_slim)))
      D0_m_5_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_5`[!is.na(data_0_vae$`test/gender_m/diversity_at_5`)]
      D0_m_5_diversity_vaelabel <- c(rep('D0_f_5_diversity_vae', length(D0_m_5_diversity_vae)))
      # combine data
      diversity_5 <- c(D0_f_5_diversity_pop, D0_f_5_diversity_itemknn, D0_f_5_diversity_als, D0_f_5_diversity_bpr, D0_f_5_diversity_slim, D0_f_5_diversity_vae, D0_m_5_diversity_pop, D0_m_5_diversity_itemknn, D0_m_5_diversity_als, D0_m_5_diversity_bpr, D0_m_5_diversity_slim, D0_m_5_diversity_vae)
      diversity_5label <- c(D0_f_5_diversity_poplabel, D0_f_5_diversity_itemknnlabel, D0_f_5_diversity_alslabel, D0_f_5_diversity_bprlabel, D0_f_5_diversity_slimlabel, D0_f_5_diversity_vaelabel, D0_m_5_diversity_poplabel, D0_m_5_diversity_itemknnlabel, D0_m_5_diversity_alslabel, D0_m_5_diversity_bprlabel, D0_m_5_diversity_slimlabel, D0_m_5_diversity_vaelabel)
      
      
      D0_f_10_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_10`[!is.na(data_0_pop$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_poplabel <- c(rep('D0_f_10_diversity_pop', length(D0_f_10_diversity_pop)))
      D0_f_10_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_10`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_itemknnlabel <- c(rep('D0_f_10_diversity_itemknn', length(D0_f_10_diversity_itemknn)))
      D0_f_10_diversity_als <- data_0_als$`test/gender_m/diversity_at_10`[!is.na(data_0_als$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_alslabel <- c(rep('D0_f_10_diversity_als', length(D0_f_10_diversity_als)))
      D0_f_10_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_10`[!is.na(data_0_bpr$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_bprlabel <- c(rep('D0_f_10_diversity_bpr', length(D0_f_10_diversity_bpr)))
      D0_f_10_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_10`[!is.na(data_0_slim$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_slimlabel <- c(rep('D0_f_10_diversity_slim', length(D0_f_10_diversity_slim)))
      D0_f_10_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_10`[!is.na(data_0_vae$`test/gender_m/diversity_at_10`)]
      D0_f_10_diversity_vaelabel <- c(rep('D0_f_10_diversity_vae', length(D0_f_10_diversity_vae)))
      D0_m_10_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_10`[!is.na(data_0_pop$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_poplabel <- c(rep('D0_f_10_diversity_pop', length(D0_m_10_diversity_pop)))
      D0_m_10_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_10`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_itemknnlabel <- c(rep('D0_f_10_diversity_itemknn', length(D0_m_10_diversity_itemknn)))
      D0_m_10_diversity_als <- data_0_als$`test/gender_m/diversity_at_10`[!is.na(data_0_als$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_alslabel <- c(rep('D0_f_10_diversity_als', length(D0_m_10_diversity_als)))
      D0_m_10_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_10`[!is.na(data_0_bpr$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_bprlabel <- c(rep('D0_f_10_diversity_bpr', length(D0_m_10_diversity_bpr)))
      D0_m_10_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_10`[!is.na(data_0_slim$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_slimlabel <- c(rep('D0_f_10_diversity_slim', length(D0_m_10_diversity_slim)))
      D0_m_10_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_10`[!is.na(data_0_vae$`test/gender_m/diversity_at_10`)]
      D0_m_10_diversity_vaelabel <- c(rep('D0_f_10_diversity_vae', length(D0_m_10_diversity_vae)))
      # combine data
      diversity_10 <- c(D0_f_10_diversity_pop, D0_f_10_diversity_itemknn, D0_f_10_diversity_als, D0_f_10_diversity_bpr, D0_f_10_diversity_slim, D0_f_10_diversity_vae, D0_m_10_diversity_pop, D0_m_10_diversity_itemknn, D0_m_10_diversity_als, D0_m_10_diversity_bpr, D0_m_10_diversity_slim, D0_m_10_diversity_vae)
      diversity_10label <- c(D0_f_10_diversity_poplabel, D0_f_10_diversity_itemknnlabel, D0_f_10_diversity_alslabel, D0_f_10_diversity_bprlabel, D0_f_10_diversity_slimlabel, D0_f_10_diversity_vaelabel, D0_m_10_diversity_poplabel, D0_m_10_diversity_itemknnlabel, D0_m_10_diversity_alslabel, D0_m_10_diversity_bprlabel, D0_m_10_diversity_slimlabel, D0_m_10_diversity_vaelabel)
      
      
      D0_f_20_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_20`[!is.na(data_0_pop$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_poplabel <- c(rep('D0_f_20_diversity_pop', length(D0_f_20_diversity_pop)))
      D0_f_20_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_20`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_itemknnlabel <- c(rep('D0_f_20_diversity_itemknn', length(D0_f_20_diversity_itemknn)))
      D0_f_20_diversity_als <- data_0_als$`test/gender_m/diversity_at_20`[!is.na(data_0_als$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_alslabel <- c(rep('D0_f_20_diversity_als', length(D0_f_20_diversity_als)))
      D0_f_20_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_20`[!is.na(data_0_bpr$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_bprlabel <- c(rep('D0_f_20_diversity_bpr', length(D0_f_20_diversity_bpr)))
      D0_f_20_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_20`[!is.na(data_0_slim$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_slimlabel <- c(rep('D0_f_20_diversity_slim', length(D0_f_20_diversity_slim)))
      D0_f_20_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_20`[!is.na(data_0_vae$`test/gender_m/diversity_at_20`)]
      D0_f_20_diversity_vaelabel <- c(rep('D0_f_20_diversity_vae', length(D0_f_20_diversity_vae)))
      D0_m_20_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_20`[!is.na(data_0_pop$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_poplabel <- c(rep('D0_f_20_diversity_pop', length(D0_m_20_diversity_pop)))
      D0_m_20_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_20`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_itemknnlabel <- c(rep('D0_f_20_diversity_itemknn', length(D0_m_20_diversity_itemknn)))
      D0_m_20_diversity_als <- data_0_als$`test/gender_m/diversity_at_20`[!is.na(data_0_als$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_alslabel <- c(rep('D0_f_20_diversity_als', length(D0_m_20_diversity_als)))
      D0_m_20_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_20`[!is.na(data_0_bpr$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_bprlabel <- c(rep('D0_f_20_diversity_bpr', length(D0_m_20_diversity_bpr)))
      D0_m_20_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_20`[!is.na(data_0_slim$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_slimlabel <- c(rep('D0_f_20_diversity_slim', length(D0_m_20_diversity_slim)))
      D0_m_20_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_20`[!is.na(data_0_vae$`test/gender_m/diversity_at_20`)]
      D0_m_20_diversity_vaelabel <- c(rep('D0_f_20_diversity_vae', length(D0_m_20_diversity_vae)))
      # combine data
      diversity_20 <- c(D0_f_20_diversity_pop, D0_f_20_diversity_itemknn, D0_f_20_diversity_als, D0_f_20_diversity_bpr, D0_f_20_diversity_slim, D0_f_20_diversity_vae, D0_m_20_diversity_pop, D0_m_20_diversity_itemknn, D0_m_20_diversity_als, D0_m_20_diversity_bpr, D0_m_20_diversity_slim, D0_m_20_diversity_vae)
      diversity_20label <- c(D0_f_20_diversity_poplabel, D0_f_20_diversity_itemknnlabel, D0_f_20_diversity_alslabel, D0_f_20_diversity_bprlabel, D0_f_20_diversity_slimlabel, D0_f_20_diversity_vaelabel, D0_m_20_diversity_poplabel, D0_m_20_diversity_itemknnlabel, D0_m_20_diversity_alslabel, D0_m_20_diversity_bprlabel, D0_m_20_diversity_slimlabel, D0_m_20_diversity_vaelabel)
      
      
      D0_f_50_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_50`[!is.na(data_0_pop$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_poplabel <- c(rep('D0_f_50_diversity_pop', length(D0_f_50_diversity_pop)))
      D0_f_50_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_50`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_itemknnlabel <- c(rep('D0_f_50_diversity_itemknn', length(D0_f_50_diversity_itemknn)))
      D0_f_50_diversity_als <- data_0_als$`test/gender_m/diversity_at_50`[!is.na(data_0_als$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_alslabel <- c(rep('D0_f_50_diversity_als', length(D0_f_50_diversity_als)))
      D0_f_50_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_50`[!is.na(data_0_bpr$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_bprlabel <- c(rep('D0_f_50_diversity_bpr', length(D0_f_50_diversity_bpr)))
      D0_f_50_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_50`[!is.na(data_0_slim$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_slimlabel <- c(rep('D0_f_50_diversity_slim', length(D0_f_50_diversity_slim)))
      D0_f_50_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_50`[!is.na(data_0_vae$`test/gender_m/diversity_at_50`)]
      D0_f_50_diversity_vaelabel <- c(rep('D0_f_50_diversity_vae', length(D0_f_50_diversity_vae)))
      D0_m_50_diversity_pop <- data_0_pop$`test/gender_m/diversity_at_50`[!is.na(data_0_pop$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_poplabel <- c(rep('D0_f_50_diversity_pop', length(D0_m_50_diversity_pop)))
      D0_m_50_diversity_itemknn <- data_0_itemknn$`test/gender_m/diversity_at_50`[!is.na(data_0_itemknn$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_itemknnlabel <- c(rep('D0_f_50_diversity_itemknn', length(D0_m_50_diversity_itemknn)))
      D0_m_50_diversity_als <- data_0_als$`test/gender_m/diversity_at_50`[!is.na(data_0_als$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_alslabel <- c(rep('D0_f_50_diversity_als', length(D0_m_50_diversity_als)))
      D0_m_50_diversity_bpr <- data_0_bpr$`test/gender_m/diversity_at_50`[!is.na(data_0_bpr$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_bprlabel <- c(rep('D0_f_50_diversity_bpr', length(D0_m_50_diversity_bpr)))
      D0_m_50_diversity_slim <- data_0_slim$`test/gender_m/diversity_at_50`[!is.na(data_0_slim$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_slimlabel <- c(rep('D0_f_50_diversity_slim', length(D0_m_50_diversity_slim)))
      D0_m_50_diversity_vae <- data_0_vae$`test/gender_m/diversity_at_50`[!is.na(data_0_vae$`test/gender_m/diversity_at_50`)]
      D0_m_50_diversity_vaelabel <- c(rep('D0_f_50_diversity_vae', length(D0_m_50_diversity_vae)))
      # combine data
      diversity_50 <- c(D0_f_50_diversity_pop, D0_f_50_diversity_itemknn, D0_f_50_diversity_als, D0_f_50_diversity_bpr, D0_f_50_diversity_slim, D0_f_50_diversity_vae, D0_m_50_diversity_pop, D0_m_50_diversity_itemknn, D0_m_50_diversity_als, D0_m_50_diversity_bpr, D0_m_50_diversity_slim, D0_m_50_diversity_vae)
      diversity_50label <- c(D0_f_50_diversity_poplabel, D0_f_50_diversity_itemknnlabel, D0_f_50_diversity_alslabel, D0_f_50_diversity_bprlabel, D0_f_50_diversity_slimlabel, D0_f_50_diversity_vaelabel, D0_m_50_diversity_poplabel, D0_m_50_diversity_itemknnlabel, D0_m_50_diversity_alslabel, D0_m_50_diversity_bprlabel, D0_m_50_diversity_slimlabel, D0_m_50_diversity_vaelabel)
      # make dataframe
      diversity_f_D <- data.frame(diversity_3, diversity_3label, diversity_5, diversity_5label, diversity_10, diversity_10label, diversity_20, diversity_20label, diversity_50, diversity_50label)
      #View(diversity_f_D)
      if(threshold == 3){
        dunn <- dunn_test(diversity_3 ~ diversity_3label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 5){
        dunn <- dunn_test(diversity_5 ~ diversity_5label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 10){
        dunn <- dunn_test(diversity_10 ~ diversity_10label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 20){
        dunn <- dunn_test(diversity_20 ~ diversity_20label, data=diversity_m_D, p.adjust.method = "bonferroni")
      } else if (threshold == 50){
        dunn <- dunn_test(diversity_50 ~ diversity_50label, data=diversity_m_D, p.adjust.method = "bonferroni")
      }
      print(dunn$group1)
      print(dunn$group2)
      mult_compar <- data.frame(t(dunn$p.adj))
      my_name_vector <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
      colnames(p_d) <- my_name_vector
      colnames(mult_compar) <- my_name_vector
      p_d <- rbind(p_d, mult_compar)
    }
    print(p_d)
    
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

sink('analysis-output_diversity.txt')
for (elem in RESULTS){
  print(elem)  
} 
sink('analysis-output_diversity.txt', append=TRUE)
sink()
