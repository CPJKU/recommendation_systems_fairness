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


get_result <- function(dunn.res, RESULTS) {
  p_d <- data.frame(N1 = numeric(0), N2 = numeric(0), N3 = numeric(0), N4 = numeric(0), N5 = numeric(0), N6 = numeric(0), N7 = numeric(0), N8 = numeric(0), N9 = numeric(0), N10 = numeric(0), N11 = numeric(0), N12 = numeric(0), N13 = numeric(0), N14 = numeric(0), N15 = numeric(0))
  mult_compar <- data.frame(t(dunn.res$p.adj))
  my_name_vector <- c("alsVSbpr", "alsVitemknn", "alsVSpop", "alsVSslim", "alsVSvae", "bprVSitemknn", "bprVSpop", "bprVSslim", "bprVSvae", "itemknnVSpop", "itemknnVSslim", "itemknnVSvae", "popVSslim", "popVSvae", "slimVSvae")
  colnames(p_d) <- my_name_vector
  colnames(mult_compar) <- my_name_vector
  p_d <- rbind(p_d, mult_compar)
  RESULTS <- c(RESULTS, p_d)
  return <- RESULTS
}


my_path <- here::here()
sampling <- c("D", "N", "U")
all_levels <- c(3, 5, 10, 20, 50)
RESULTS <- list()
RESULTS <- c(RESULTS, "COVERAGE")

for (samp_group in sampling){
  RESULTS <- c(RESULTS, samp_group)
  #RESULTS <- c(RESULTS, "MALE")
  for (threshold in all_levels){
    RESULTS <- c(RESULTS, "===========================================")
    RESULTS <- c(RESULTS, threshold)
    RESULTS <- c(RESULTS, "===========================================")
    data_0_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_1_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_2_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data3_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_pop <- read_delim(paste("full_raw_metrics_beyond_accuracy_pop", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_itemknn <- read_delim(paste("full_raw_metrics_beyond_accuracy_itemknn", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_als <- read_delim(paste("full_raw_metrics_beyond_accuracy_als", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_bpr <- read_delim(paste("full_raw_metrics_beyond_accuracy_bpr", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_slim <- read_delim(paste("full_raw_metrics_beyond_accuracy_slim", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_4_vae <- read_delim(paste("full_raw_metrics_beyond_accuracy_vae", samp_group, "_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
    data_0_m_3_coverage_pop <- data_0_pop$`test/gender_m/coverage_at_3`[!is.na(data_0_pop$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_poplabel <- c(rep('pop', length(data_0_m_3_coverage_pop)))
    data_0_m_3_coverage_itemknn <- data_0_itemknn$`test/gender_m/coverage_at_3`[!is.na(data_0_itemknn$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_m_3_coverage_itemknn)))
    data_0_m_3_coverage_als <- data_0_als$`test/gender_m/coverage_at_3`[!is.na(data_0_als$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_alslabel <- c(rep('als', length(data_0_m_3_coverage_als)))
    data_0_m_3_coverage_bpr <- data_0_bpr$`test/gender_m/coverage_at_3`[!is.na(data_0_bpr$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_bprlabel <- c(rep('bpr', length(data_0_m_3_coverage_bpr)))
    data_0_m_3_coverage_slim <- data_0_slim$`test/gender_m/coverage_at_3`[!is.na(data_0_slim$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_slimlabel <- c(rep('slim', length(data_0_m_3_coverage_slim)))
    data_0_m_3_coverage_vae <- data_0_vae$`test/gender_m/coverage_at_3`[!is.na(data_0_vae$`test/gender_m/coverage_at_3`)]
    data_0_m_3_coverage_vaelabel <- c(rep('vae', length(data_0_m_3_coverage_vae)))
    data_1_m_3_coverage_pop <- data_1_pop$`test/gender_m/coverage_at_3`[!is.na(data_1_pop$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_poplabel <- c(rep('pop', length(data_1_m_3_coverage_pop)))
    data_1_m_3_coverage_itemknn <- data_1_itemknn$`test/gender_m/coverage_at_3`[!is.na(data_1_itemknn$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_m_3_coverage_itemknn)))
    data_1_m_3_coverage_als <- data_1_als$`test/gender_m/coverage_at_3`[!is.na(data_1_als$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_alslabel <- c(rep('als', length(data_1_m_3_coverage_als)))
    data_1_m_3_coverage_bpr <- data_1_bpr$`test/gender_m/coverage_at_3`[!is.na(data_1_bpr$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_bprlabel <- c(rep('bpr', length(data_1_m_3_coverage_bpr)))
    data_1_m_3_coverage_slim <- data_1_slim$`test/gender_m/coverage_at_3`[!is.na(data_1_slim$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_slimlabel <- c(rep('slim', length(data_1_m_3_coverage_slim)))
    data_1_m_3_coverage_vae <- data_1_vae$`test/gender_m/coverage_at_3`[!is.na(data_1_vae$`test/gender_m/coverage_at_3`)]
    data_1_m_3_coverage_vaelabel <- c(rep('vae', length(data_1_m_3_coverage_vae)))
    data_2_m_3_coverage_pop <- data_2_pop$`test/gender_m/coverage_at_3`[!is.na(data_2_pop$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_poplabel <- c(rep('pop', length(data_2_m_3_coverage_pop)))
    data_2_m_3_coverage_itemknn <- data_2_itemknn$`test/gender_m/coverage_at_3`[!is.na(data_2_itemknn$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_m_3_coverage_itemknn)))
    data_2_m_3_coverage_als <- data_2_als$`test/gender_m/coverage_at_3`[!is.na(data_2_als$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_alslabel <- c(rep('als', length(data_2_m_3_coverage_als)))
    data_2_m_3_coverage_bpr <- data_2_bpr$`test/gender_m/coverage_at_3`[!is.na(data_2_bpr$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_bprlabel <- c(rep('bpr', length(data_2_m_3_coverage_bpr)))
    data_2_m_3_coverage_slim <- data_2_slim$`test/gender_m/coverage_at_3`[!is.na(data_2_slim$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_slimlabel <- c(rep('slim', length(data_2_m_3_coverage_slim)))
    data_2_m_3_coverage_vae <- data_2_vae$`test/gender_m/coverage_at_3`[!is.na(data_2_vae$`test/gender_m/coverage_at_3`)]
    data_2_m_3_coverage_vaelabel <- c(rep('vae', length(data_2_m_3_coverage_vae)))
    data3_m_3_coverage_pop <- data3_pop$`test/gender_m/coverage_at_3`[!is.na(data3_pop$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_poplabel <- c(rep('pop', length(data3_m_3_coverage_pop)))
    data3_m_3_coverage_itemknn <- data3_itemknn$`test/gender_m/coverage_at_3`[!is.na(data3_itemknn$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_itemknnlabel <- c(rep('itemknn', length(data3_m_3_coverage_itemknn)))
    data3_m_3_coverage_als <- data3_als$`test/gender_m/coverage_at_3`[!is.na(data3_als$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_alslabel <- c(rep('als', length(data3_m_3_coverage_als)))
    data3_m_3_coverage_bpr <- data3_bpr$`test/gender_m/coverage_at_3`[!is.na(data3_bpr$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_bprlabel <- c(rep('bpr', length(data3_m_3_coverage_bpr)))
    data3_m_3_coverage_slim <- data3_slim$`test/gender_m/coverage_at_3`[!is.na(data3_slim$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_slimlabel <- c(rep('slim', length(data3_m_3_coverage_slim)))
    data3_m_3_coverage_vae <- data3_vae$`test/gender_m/coverage_at_3`[!is.na(data3_vae$`test/gender_m/coverage_at_3`)]
    data3_m_3_coverage_vaelabel <- c(rep('vae', length(data3_m_3_coverage_vae)))
    data_4_m_3_coverage_pop <- data_4_pop$`test/gender_m/coverage_at_3`[!is.na(data_4_pop$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_poplabel <- c(rep('pop', length(data_4_m_3_coverage_pop)))
    data_4_m_3_coverage_itemknn <- data_4_itemknn$`test/gender_m/coverage_at_3`[!is.na(data_4_itemknn$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_m_3_coverage_itemknn)))
    data_4_m_3_coverage_als <- data_4_als$`test/gender_m/coverage_at_3`[!is.na(data_4_als$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_alslabel <- c(rep('als', length(data_4_m_3_coverage_als)))
    data_4_m_3_coverage_bpr <- data_4_bpr$`test/gender_m/coverage_at_3`[!is.na(data_4_bpr$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_bprlabel <- c(rep('bpr', length(data_4_m_3_coverage_bpr)))
    data_4_m_3_coverage_slim <- data_4_slim$`test/gender_m/coverage_at_3`[!is.na(data_4_slim$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_slimlabel <- c(rep('slim', length(data_4_m_3_coverage_slim)))
    data_4_m_3_coverage_vae <- data_4_vae$`test/gender_m/coverage_at_3`[!is.na(data_4_vae$`test/gender_m/coverage_at_3`)]
    data_4_m_3_coverage_vaelabel <- c(rep('vae', length(data_4_m_3_coverage_vae)))
    # combine data
    coverage_3M <- c(data_0_m_3_coverage_pop, data_0_m_3_coverage_itemknn, data_0_m_3_coverage_als, data_0_m_3_coverage_bpr, data_0_m_3_coverage_slim, data_0_m_3_coverage_vae, data_1_m_3_coverage_pop, data_1_m_3_coverage_itemknn, data_1_m_3_coverage_als, data_1_m_3_coverage_bpr, data_1_m_3_coverage_slim, data_1_m_3_coverage_vae, data_2_m_3_coverage_pop, data_2_m_3_coverage_itemknn, data_2_m_3_coverage_als, data_2_m_3_coverage_bpr, data_2_m_3_coverage_slim, data_2_m_3_coverage_vae, data3_m_3_coverage_pop, data3_m_3_coverage_itemknn, data3_m_3_coverage_als, data3_m_3_coverage_bpr, data3_m_3_coverage_slim, data3_m_3_coverage_vae, data_4_m_3_coverage_pop, data_4_m_3_coverage_itemknn, data_4_m_3_coverage_als, data_4_m_3_coverage_bpr, data_4_m_3_coverage_slim, data_4_m_3_coverage_vae)
    coverage_3labelM <- c(data_0_m_3_coverage_poplabel, data_0_m_3_coverage_itemknnlabel, data_0_m_3_coverage_alslabel, data_0_m_3_coverage_bprlabel, data_0_m_3_coverage_slimlabel, data_0_m_3_coverage_vaelabel, data_1_m_3_coverage_poplabel, data_1_m_3_coverage_itemknnlabel, data_1_m_3_coverage_alslabel, data_1_m_3_coverage_bprlabel, data_1_m_3_coverage_slimlabel, data_1_m_3_coverage_vaelabel, data_2_m_3_coverage_poplabel, data_2_m_3_coverage_itemknnlabel, data_2_m_3_coverage_alslabel, data_2_m_3_coverage_bprlabel, data_2_m_3_coverage_slimlabel, data_2_m_3_coverage_vaelabel, data3_m_3_coverage_poplabel, data3_m_3_coverage_itemknnlabel, data3_m_3_coverage_alslabel, data3_m_3_coverage_bprlabel, data3_m_3_coverage_slimlabel, data3_m_3_coverage_vaelabel, data_4_m_3_coverage_poplabel, data_4_m_3_coverage_itemknnlabel, data_4_m_3_coverage_alslabel, data_4_m_3_coverage_bprlabel, data_4_m_3_coverage_slimlabel, data_4_m_3_coverage_vaelabel)
    #
    data_0_m_5_coverage_pop <- data_0_pop$`test/gender_m/coverage_at_5`[!is.na(data_0_pop$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_poplabel <- c(rep('pop', length(data_0_m_5_coverage_pop)))
    data_0_m_5_coverage_itemknn <- data_0_itemknn$`test/gender_m/coverage_at_5`[!is.na(data_0_itemknn$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_m_5_coverage_itemknn)))
    data_0_m_5_coverage_als <- data_0_als$`test/gender_m/coverage_at_5`[!is.na(data_0_als$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_alslabel <- c(rep('als', length(data_0_m_5_coverage_als)))
    data_0_m_5_coverage_bpr <- data_0_bpr$`test/gender_m/coverage_at_5`[!is.na(data_0_bpr$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_bprlabel <- c(rep('bpr', length(data_0_m_5_coverage_bpr)))
    data_0_m_5_coverage_slim <- data_0_slim$`test/gender_m/coverage_at_5`[!is.na(data_0_slim$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_slimlabel <- c(rep('slim', length(data_0_m_5_coverage_slim)))
    data_0_m_5_coverage_vae <- data_0_vae$`test/gender_m/coverage_at_5`[!is.na(data_0_vae$`test/gender_m/coverage_at_5`)]
    data_0_m_5_coverage_vaelabel <- c(rep('vae', length(data_0_m_5_coverage_vae)))
    data_1_m_5_coverage_pop <- data_1_pop$`test/gender_m/coverage_at_5`[!is.na(data_1_pop$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_poplabel <- c(rep('pop', length(data_1_m_5_coverage_pop)))
    data_1_m_5_coverage_itemknn <- data_1_itemknn$`test/gender_m/coverage_at_5`[!is.na(data_1_itemknn$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_m_5_coverage_itemknn)))
    data_1_m_5_coverage_als <- data_1_als$`test/gender_m/coverage_at_5`[!is.na(data_1_als$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_alslabel <- c(rep('als', length(data_1_m_5_coverage_als)))
    data_1_m_5_coverage_bpr <- data_1_bpr$`test/gender_m/coverage_at_5`[!is.na(data_1_bpr$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_bprlabel <- c(rep('bpr', length(data_1_m_5_coverage_bpr)))
    data_1_m_5_coverage_slim <- data_1_slim$`test/gender_m/coverage_at_5`[!is.na(data_1_slim$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_slimlabel <- c(rep('slim', length(data_1_m_5_coverage_slim)))
    data_1_m_5_coverage_vae <- data_1_vae$`test/gender_m/coverage_at_5`[!is.na(data_1_vae$`test/gender_m/coverage_at_5`)]
    data_1_m_5_coverage_vaelabel <- c(rep('vae', length(data_1_m_5_coverage_vae)))
    data_2_m_5_coverage_pop <- data_2_pop$`test/gender_m/coverage_at_5`[!is.na(data_2_pop$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_poplabel <- c(rep('pop', length(data_2_m_5_coverage_pop)))
    data_2_m_5_coverage_itemknn <- data_2_itemknn$`test/gender_m/coverage_at_5`[!is.na(data_2_itemknn$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_m_5_coverage_itemknn)))
    data_2_m_5_coverage_als <- data_2_als$`test/gender_m/coverage_at_5`[!is.na(data_2_als$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_alslabel <- c(rep('als', length(data_2_m_5_coverage_als)))
    data_2_m_5_coverage_bpr <- data_2_bpr$`test/gender_m/coverage_at_5`[!is.na(data_2_bpr$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_bprlabel <- c(rep('bpr', length(data_2_m_5_coverage_bpr)))
    data_2_m_5_coverage_slim <- data_2_slim$`test/gender_m/coverage_at_5`[!is.na(data_2_slim$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_slimlabel <- c(rep('slim', length(data_2_m_5_coverage_slim)))
    data_2_m_5_coverage_vae <- data_2_vae$`test/gender_m/coverage_at_5`[!is.na(data_2_vae$`test/gender_m/coverage_at_5`)]
    data_2_m_5_coverage_vaelabel <- c(rep('vae', length(data_2_m_5_coverage_vae)))
    data3_m_5_coverage_pop <- data3_pop$`test/gender_m/coverage_at_5`[!is.na(data3_pop$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_poplabel <- c(rep('pop', length(data3_m_5_coverage_pop)))
    data3_m_5_coverage_itemknn <- data3_itemknn$`test/gender_m/coverage_at_5`[!is.na(data3_itemknn$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_itemknnlabel <- c(rep('itemknn', length(data3_m_5_coverage_itemknn)))
    data3_m_5_coverage_als <- data3_als$`test/gender_m/coverage_at_5`[!is.na(data3_als$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_alslabel <- c(rep('als', length(data3_m_5_coverage_als)))
    data3_m_5_coverage_bpr <- data3_bpr$`test/gender_m/coverage_at_5`[!is.na(data3_bpr$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_bprlabel <- c(rep('bpr', length(data3_m_5_coverage_bpr)))
    data3_m_5_coverage_slim <- data3_slim$`test/gender_m/coverage_at_5`[!is.na(data3_slim$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_slimlabel <- c(rep('slim', length(data3_m_5_coverage_slim)))
    data3_m_5_coverage_vae <- data3_vae$`test/gender_m/coverage_at_5`[!is.na(data3_vae$`test/gender_m/coverage_at_5`)]
    data3_m_5_coverage_vaelabel <- c(rep('vae', length(data3_m_5_coverage_vae)))
    data_4_m_5_coverage_pop <- data_4_pop$`test/gender_m/coverage_at_5`[!is.na(data_4_pop$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_poplabel <- c(rep('pop', length(data_4_m_5_coverage_pop)))
    data_4_m_5_coverage_itemknn <- data_4_itemknn$`test/gender_m/coverage_at_5`[!is.na(data_4_itemknn$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_m_5_coverage_itemknn)))
    data_4_m_5_coverage_als <- data_4_als$`test/gender_m/coverage_at_5`[!is.na(data_4_als$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_alslabel <- c(rep('als', length(data_4_m_5_coverage_als)))
    data_4_m_5_coverage_bpr <- data_4_bpr$`test/gender_m/coverage_at_5`[!is.na(data_4_bpr$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_bprlabel <- c(rep('bpr', length(data_4_m_5_coverage_bpr)))
    data_4_m_5_coverage_slim <- data_4_slim$`test/gender_m/coverage_at_5`[!is.na(data_4_slim$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_slimlabel <- c(rep('slim', length(data_4_m_5_coverage_slim)))
    data_4_m_5_coverage_vae <- data_4_vae$`test/gender_m/coverage_at_5`[!is.na(data_4_vae$`test/gender_m/coverage_at_5`)]
    data_4_m_5_coverage_vaelabel <- c(rep('vae', length(data_4_m_5_coverage_vae)))
    # combine data
    coverage_5M <- c(data_0_m_5_coverage_pop, data_0_m_5_coverage_itemknn, data_0_m_5_coverage_als, data_0_m_5_coverage_bpr, data_0_m_5_coverage_slim, data_0_m_5_coverage_vae, data_1_m_5_coverage_pop, data_1_m_5_coverage_itemknn, data_1_m_5_coverage_als, data_1_m_5_coverage_bpr, data_1_m_5_coverage_slim, data_1_m_5_coverage_vae, data_2_m_5_coverage_pop, data_2_m_5_coverage_itemknn, data_2_m_5_coverage_als, data_2_m_5_coverage_bpr, data_2_m_5_coverage_slim, data_2_m_5_coverage_vae, data3_m_5_coverage_pop, data3_m_5_coverage_itemknn, data3_m_5_coverage_als, data3_m_5_coverage_bpr, data3_m_5_coverage_slim, data3_m_5_coverage_vae, data_4_m_5_coverage_pop, data_4_m_5_coverage_itemknn, data_4_m_5_coverage_als, data_4_m_5_coverage_bpr, data_4_m_5_coverage_slim, data_4_m_5_coverage_vae)
    coverage_5labelM <- c(data_0_m_5_coverage_poplabel, data_0_m_5_coverage_itemknnlabel, data_0_m_5_coverage_alslabel, data_0_m_5_coverage_bprlabel, data_0_m_5_coverage_slimlabel, data_0_m_5_coverage_vaelabel, data_1_m_5_coverage_poplabel, data_1_m_5_coverage_itemknnlabel, data_1_m_5_coverage_alslabel, data_1_m_5_coverage_bprlabel, data_1_m_5_coverage_slimlabel, data_1_m_5_coverage_vaelabel, data_2_m_5_coverage_poplabel, data_2_m_5_coverage_itemknnlabel, data_2_m_5_coverage_alslabel, data_2_m_5_coverage_bprlabel, data_2_m_5_coverage_slimlabel, data_2_m_5_coverage_vaelabel, data3_m_5_coverage_poplabel, data3_m_5_coverage_itemknnlabel, data3_m_5_coverage_alslabel, data3_m_5_coverage_bprlabel, data3_m_5_coverage_slimlabel, data3_m_5_coverage_vaelabel, data_4_m_5_coverage_poplabel, data_4_m_5_coverage_itemknnlabel, data_4_m_5_coverage_alslabel, data_4_m_5_coverage_bprlabel, data_4_m_5_coverage_slimlabel, data_4_m_5_coverage_vaelabel)
    #
    data_0_m_10_coverage_pop <- data_0_pop$`test/gender_m/coverage_at_10`[!is.na(data_0_pop$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_poplabel <- c(rep('pop', length(data_0_m_10_coverage_pop)))
    data_0_m_10_coverage_itemknn <- data_0_itemknn$`test/gender_m/coverage_at_10`[!is.na(data_0_itemknn$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_m_10_coverage_itemknn)))
    data_0_m_10_coverage_als <- data_0_als$`test/gender_m/coverage_at_10`[!is.na(data_0_als$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_alslabel <- c(rep('als', length(data_0_m_10_coverage_als)))
    data_0_m_10_coverage_bpr <- data_0_bpr$`test/gender_m/coverage_at_10`[!is.na(data_0_bpr$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_bprlabel <- c(rep('bpr', length(data_0_m_10_coverage_bpr)))
    data_0_m_10_coverage_slim <- data_0_slim$`test/gender_m/coverage_at_10`[!is.na(data_0_slim$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_slimlabel <- c(rep('slim', length(data_0_m_10_coverage_slim)))
    data_0_m_10_coverage_vae <- data_0_vae$`test/gender_m/coverage_at_10`[!is.na(data_0_vae$`test/gender_m/coverage_at_10`)]
    data_0_m_10_coverage_vaelabel <- c(rep('vae', length(data_0_m_10_coverage_vae)))
    data_1_m_10_coverage_pop <- data_1_pop$`test/gender_m/coverage_at_10`[!is.na(data_1_pop$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_poplabel <- c(rep('pop', length(data_1_m_10_coverage_pop)))
    data_1_m_10_coverage_itemknn <- data_1_itemknn$`test/gender_m/coverage_at_10`[!is.na(data_1_itemknn$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_m_10_coverage_itemknn)))
    data_1_m_10_coverage_als <- data_1_als$`test/gender_m/coverage_at_10`[!is.na(data_1_als$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_alslabel <- c(rep('als', length(data_1_m_10_coverage_als)))
    data_1_m_10_coverage_bpr <- data_1_bpr$`test/gender_m/coverage_at_10`[!is.na(data_1_bpr$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_bprlabel <- c(rep('bpr', length(data_1_m_10_coverage_bpr)))
    data_1_m_10_coverage_slim <- data_1_slim$`test/gender_m/coverage_at_10`[!is.na(data_1_slim$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_slimlabel <- c(rep('slim', length(data_1_m_10_coverage_slim)))
    data_1_m_10_coverage_vae <- data_1_vae$`test/gender_m/coverage_at_10`[!is.na(data_1_vae$`test/gender_m/coverage_at_10`)]
    data_1_m_10_coverage_vaelabel <- c(rep('vae', length(data_1_m_10_coverage_vae)))
    data_2_m_10_coverage_pop <- data_2_pop$`test/gender_m/coverage_at_10`[!is.na(data_2_pop$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_poplabel <- c(rep('pop', length(data_2_m_10_coverage_pop)))
    data_2_m_10_coverage_itemknn <- data_2_itemknn$`test/gender_m/coverage_at_10`[!is.na(data_2_itemknn$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_m_10_coverage_itemknn)))
    data_2_m_10_coverage_als <- data_2_als$`test/gender_m/coverage_at_10`[!is.na(data_2_als$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_alslabel <- c(rep('als', length(data_2_m_10_coverage_als)))
    data_2_m_10_coverage_bpr <- data_2_bpr$`test/gender_m/coverage_at_10`[!is.na(data_2_bpr$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_bprlabel <- c(rep('bpr', length(data_2_m_10_coverage_bpr)))
    data_2_m_10_coverage_slim <- data_2_slim$`test/gender_m/coverage_at_10`[!is.na(data_2_slim$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_slimlabel <- c(rep('slim', length(data_2_m_10_coverage_slim)))
    data_2_m_10_coverage_vae <- data_2_vae$`test/gender_m/coverage_at_10`[!is.na(data_2_vae$`test/gender_m/coverage_at_10`)]
    data_2_m_10_coverage_vaelabel <- c(rep('vae', length(data_2_m_10_coverage_vae)))
    data3_m_10_coverage_pop <- data3_pop$`test/gender_m/coverage_at_10`[!is.na(data3_pop$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_poplabel <- c(rep('pop', length(data3_m_10_coverage_pop)))
    data3_m_10_coverage_itemknn <- data3_itemknn$`test/gender_m/coverage_at_10`[!is.na(data3_itemknn$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_itemknnlabel <- c(rep('itemknn', length(data3_m_10_coverage_itemknn)))
    data3_m_10_coverage_als <- data3_als$`test/gender_m/coverage_at_10`[!is.na(data3_als$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_alslabel <- c(rep('als', length(data3_m_10_coverage_als)))
    data3_m_10_coverage_bpr <- data3_bpr$`test/gender_m/coverage_at_10`[!is.na(data3_bpr$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_bprlabel <- c(rep('bpr', length(data3_m_10_coverage_bpr)))
    data3_m_10_coverage_slim <- data3_slim$`test/gender_m/coverage_at_10`[!is.na(data3_slim$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_slimlabel <- c(rep('slim', length(data3_m_10_coverage_slim)))
    data3_m_10_coverage_vae <- data3_vae$`test/gender_m/coverage_at_10`[!is.na(data3_vae$`test/gender_m/coverage_at_10`)]
    data3_m_10_coverage_vaelabel <- c(rep('vae', length(data3_m_10_coverage_vae)))
    data_4_m_10_coverage_pop <- data_4_pop$`test/gender_m/coverage_at_10`[!is.na(data_4_pop$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_poplabel <- c(rep('pop', length(data_4_m_10_coverage_pop)))
    data_4_m_10_coverage_itemknn <- data_4_itemknn$`test/gender_m/coverage_at_10`[!is.na(data_4_itemknn$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_m_10_coverage_itemknn)))
    data_4_m_10_coverage_als <- data_4_als$`test/gender_m/coverage_at_10`[!is.na(data_4_als$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_alslabel <- c(rep('als', length(data_4_m_10_coverage_als)))
    data_4_m_10_coverage_bpr <- data_4_bpr$`test/gender_m/coverage_at_10`[!is.na(data_4_bpr$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_bprlabel <- c(rep('bpr', length(data_4_m_10_coverage_bpr)))
    data_4_m_10_coverage_slim <- data_4_slim$`test/gender_m/coverage_at_10`[!is.na(data_4_slim$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_slimlabel <- c(rep('slim', length(data_4_m_10_coverage_slim)))
    data_4_m_10_coverage_vae <- data_4_vae$`test/gender_m/coverage_at_10`[!is.na(data_4_vae$`test/gender_m/coverage_at_10`)]
    data_4_m_10_coverage_vaelabel <- c(rep('vae', length(data_4_m_10_coverage_vae)))
    # combine data
    coverage_10M <- c(data_0_m_10_coverage_pop, data_0_m_10_coverage_itemknn, data_0_m_10_coverage_als, data_0_m_10_coverage_bpr, data_0_m_10_coverage_slim, data_0_m_10_coverage_vae, data_1_m_10_coverage_pop, data_1_m_10_coverage_itemknn, data_1_m_10_coverage_als, data_1_m_10_coverage_bpr, data_1_m_10_coverage_slim, data_1_m_10_coverage_vae, data_2_m_10_coverage_pop, data_2_m_10_coverage_itemknn, data_2_m_10_coverage_als, data_2_m_10_coverage_bpr, data_2_m_10_coverage_slim, data_2_m_10_coverage_vae, data3_m_10_coverage_pop, data3_m_10_coverage_itemknn, data3_m_10_coverage_als, data3_m_10_coverage_bpr, data3_m_10_coverage_slim, data3_m_10_coverage_vae, data_4_m_10_coverage_pop, data_4_m_10_coverage_itemknn, data_4_m_10_coverage_als, data_4_m_10_coverage_bpr, data_4_m_10_coverage_slim, data_4_m_10_coverage_vae)
    coverage_10labelM <- c(data_0_m_10_coverage_poplabel, data_0_m_10_coverage_itemknnlabel, data_0_m_10_coverage_alslabel, data_0_m_10_coverage_bprlabel, data_0_m_10_coverage_slimlabel, data_0_m_10_coverage_vaelabel, data_1_m_10_coverage_poplabel, data_1_m_10_coverage_itemknnlabel, data_1_m_10_coverage_alslabel, data_1_m_10_coverage_bprlabel, data_1_m_10_coverage_slimlabel, data_1_m_10_coverage_vaelabel, data_2_m_10_coverage_poplabel, data_2_m_10_coverage_itemknnlabel, data_2_m_10_coverage_alslabel, data_2_m_10_coverage_bprlabel, data_2_m_10_coverage_slimlabel, data_2_m_10_coverage_vaelabel, data3_m_10_coverage_poplabel, data3_m_10_coverage_itemknnlabel, data3_m_10_coverage_alslabel, data3_m_10_coverage_bprlabel, data3_m_10_coverage_slimlabel, data3_m_10_coverage_vaelabel, data_4_m_10_coverage_poplabel, data_4_m_10_coverage_itemknnlabel, data_4_m_10_coverage_alslabel, data_4_m_10_coverage_bprlabel, data_4_m_10_coverage_slimlabel, data_4_m_10_coverage_vaelabel)
    #
    data_0_m_20_coverage_pop <- data_0_pop$`test/gender_m/coverage_at_20`[!is.na(data_0_pop$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_poplabel <- c(rep('pop', length(data_0_m_20_coverage_pop)))
    data_0_m_20_coverage_itemknn <- data_0_itemknn$`test/gender_m/coverage_at_20`[!is.na(data_0_itemknn$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_m_20_coverage_itemknn)))
    data_0_m_20_coverage_als <- data_0_als$`test/gender_m/coverage_at_20`[!is.na(data_0_als$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_alslabel <- c(rep('als', length(data_0_m_20_coverage_als)))
    data_0_m_20_coverage_bpr <- data_0_bpr$`test/gender_m/coverage_at_20`[!is.na(data_0_bpr$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_bprlabel <- c(rep('bpr', length(data_0_m_20_coverage_bpr)))
    data_0_m_20_coverage_slim <- data_0_slim$`test/gender_m/coverage_at_20`[!is.na(data_0_slim$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_slimlabel <- c(rep('slim', length(data_0_m_20_coverage_slim)))
    data_0_m_20_coverage_vae <- data_0_vae$`test/gender_m/coverage_at_20`[!is.na(data_0_vae$`test/gender_m/coverage_at_20`)]
    data_0_m_20_coverage_vaelabel <- c(rep('vae', length(data_0_m_20_coverage_vae)))
    data_1_m_20_coverage_pop <- data_1_pop$`test/gender_m/coverage_at_20`[!is.na(data_1_pop$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_poplabel <- c(rep('pop', length(data_1_m_20_coverage_pop)))
    data_1_m_20_coverage_itemknn <- data_1_itemknn$`test/gender_m/coverage_at_20`[!is.na(data_1_itemknn$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_m_20_coverage_itemknn)))
    data_1_m_20_coverage_als <- data_1_als$`test/gender_m/coverage_at_20`[!is.na(data_1_als$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_alslabel <- c(rep('als', length(data_1_m_20_coverage_als)))
    data_1_m_20_coverage_bpr <- data_1_bpr$`test/gender_m/coverage_at_20`[!is.na(data_1_bpr$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_bprlabel <- c(rep('bpr', length(data_1_m_20_coverage_bpr)))
    data_1_m_20_coverage_slim <- data_1_slim$`test/gender_m/coverage_at_20`[!is.na(data_1_slim$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_slimlabel <- c(rep('slim', length(data_1_m_20_coverage_slim)))
    data_1_m_20_coverage_vae <- data_1_vae$`test/gender_m/coverage_at_20`[!is.na(data_1_vae$`test/gender_m/coverage_at_20`)]
    data_1_m_20_coverage_vaelabel <- c(rep('vae', length(data_1_m_20_coverage_vae)))
    data_2_m_20_coverage_pop <- data_2_pop$`test/gender_m/coverage_at_20`[!is.na(data_2_pop$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_poplabel <- c(rep('pop', length(data_2_m_20_coverage_pop)))
    data_2_m_20_coverage_itemknn <- data_2_itemknn$`test/gender_m/coverage_at_20`[!is.na(data_2_itemknn$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_m_20_coverage_itemknn)))
    data_2_m_20_coverage_als <- data_2_als$`test/gender_m/coverage_at_20`[!is.na(data_2_als$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_alslabel <- c(rep('als', length(data_2_m_20_coverage_als)))
    data_2_m_20_coverage_bpr <- data_2_bpr$`test/gender_m/coverage_at_20`[!is.na(data_2_bpr$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_bprlabel <- c(rep('bpr', length(data_2_m_20_coverage_bpr)))
    data_2_m_20_coverage_slim <- data_2_slim$`test/gender_m/coverage_at_20`[!is.na(data_2_slim$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_slimlabel <- c(rep('slim', length(data_2_m_20_coverage_slim)))
    data_2_m_20_coverage_vae <- data_2_vae$`test/gender_m/coverage_at_20`[!is.na(data_2_vae$`test/gender_m/coverage_at_20`)]
    data_2_m_20_coverage_vaelabel <- c(rep('vae', length(data_2_m_20_coverage_vae)))
    data3_m_20_coverage_pop <- data3_pop$`test/gender_m/coverage_at_20`[!is.na(data3_pop$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_poplabel <- c(rep('pop', length(data3_m_20_coverage_pop)))
    data3_m_20_coverage_itemknn <- data3_itemknn$`test/gender_m/coverage_at_20`[!is.na(data3_itemknn$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_itemknnlabel <- c(rep('itemknn', length(data3_m_20_coverage_itemknn)))
    data3_m_20_coverage_als <- data3_als$`test/gender_m/coverage_at_20`[!is.na(data3_als$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_alslabel <- c(rep('als', length(data3_m_20_coverage_als)))
    data3_m_20_coverage_bpr <- data3_bpr$`test/gender_m/coverage_at_20`[!is.na(data3_bpr$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_bprlabel <- c(rep('bpr', length(data3_m_20_coverage_bpr)))
    data3_m_20_coverage_slim <- data3_slim$`test/gender_m/coverage_at_20`[!is.na(data3_slim$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_slimlabel <- c(rep('slim', length(data3_m_20_coverage_slim)))
    data3_m_20_coverage_vae <- data3_vae$`test/gender_m/coverage_at_20`[!is.na(data3_vae$`test/gender_m/coverage_at_20`)]
    data3_m_20_coverage_vaelabel <- c(rep('vae', length(data3_m_20_coverage_vae)))
    data_4_m_20_coverage_pop <- data_4_pop$`test/gender_m/coverage_at_20`[!is.na(data_4_pop$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_poplabel <- c(rep('pop', length(data_4_m_20_coverage_pop)))
    data_4_m_20_coverage_itemknn <- data_4_itemknn$`test/gender_m/coverage_at_20`[!is.na(data_4_itemknn$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_m_20_coverage_itemknn)))
    data_4_m_20_coverage_als <- data_4_als$`test/gender_m/coverage_at_20`[!is.na(data_4_als$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_alslabel <- c(rep('als', length(data_4_m_20_coverage_als)))
    data_4_m_20_coverage_bpr <- data_4_bpr$`test/gender_m/coverage_at_20`[!is.na(data_4_bpr$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_bprlabel <- c(rep('bpr', length(data_4_m_20_coverage_bpr)))
    data_4_m_20_coverage_slim <- data_4_slim$`test/gender_m/coverage_at_20`[!is.na(data_4_slim$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_slimlabel <- c(rep('slim', length(data_4_m_20_coverage_slim)))
    data_4_m_20_coverage_vae <- data_4_vae$`test/gender_m/coverage_at_20`[!is.na(data_4_vae$`test/gender_m/coverage_at_20`)]
    data_4_m_20_coverage_vaelabel <- c(rep('vae', length(data_4_m_20_coverage_vae)))
    # combine data
    coverage_20M <- c(data_0_m_20_coverage_pop, data_0_m_20_coverage_itemknn, data_0_m_20_coverage_als, data_0_m_20_coverage_bpr, data_0_m_20_coverage_slim, data_0_m_20_coverage_vae, data_1_m_20_coverage_pop, data_1_m_20_coverage_itemknn, data_1_m_20_coverage_als, data_1_m_20_coverage_bpr, data_1_m_20_coverage_slim, data_1_m_20_coverage_vae, data_2_m_20_coverage_pop, data_2_m_20_coverage_itemknn, data_2_m_20_coverage_als, data_2_m_20_coverage_bpr, data_2_m_20_coverage_slim, data_2_m_20_coverage_vae, data3_m_20_coverage_pop, data3_m_20_coverage_itemknn, data3_m_20_coverage_als, data3_m_20_coverage_bpr, data3_m_20_coverage_slim, data3_m_20_coverage_vae, data_4_m_20_coverage_pop, data_4_m_20_coverage_itemknn, data_4_m_20_coverage_als, data_4_m_20_coverage_bpr, data_4_m_20_coverage_slim, data_4_m_20_coverage_vae)
    coverage_20labelM <- c(data_0_m_20_coverage_poplabel, data_0_m_20_coverage_itemknnlabel, data_0_m_20_coverage_alslabel, data_0_m_20_coverage_bprlabel, data_0_m_20_coverage_slimlabel, data_0_m_20_coverage_vaelabel, data_1_m_20_coverage_poplabel, data_1_m_20_coverage_itemknnlabel, data_1_m_20_coverage_alslabel, data_1_m_20_coverage_bprlabel, data_1_m_20_coverage_slimlabel, data_1_m_20_coverage_vaelabel, data_2_m_20_coverage_poplabel, data_2_m_20_coverage_itemknnlabel, data_2_m_20_coverage_alslabel, data_2_m_20_coverage_bprlabel, data_2_m_20_coverage_slimlabel, data_2_m_20_coverage_vaelabel, data3_m_20_coverage_poplabel, data3_m_20_coverage_itemknnlabel, data3_m_20_coverage_alslabel, data3_m_20_coverage_bprlabel, data3_m_20_coverage_slimlabel, data3_m_20_coverage_vaelabel, data_4_m_20_coverage_poplabel, data_4_m_20_coverage_itemknnlabel, data_4_m_20_coverage_alslabel, data_4_m_20_coverage_bprlabel, data_4_m_20_coverage_slimlabel, data_4_m_20_coverage_vaelabel)
    #
    data_0_m_50_coverage_pop <- data_0_pop$`test/gender_m/coverage_at_50`[!is.na(data_0_pop$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_poplabel <- c(rep('pop', length(data_0_m_50_coverage_pop)))
    data_0_m_50_coverage_itemknn <- data_0_itemknn$`test/gender_m/coverage_at_50`[!is.na(data_0_itemknn$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_m_50_coverage_itemknn)))
    data_0_m_50_coverage_als <- data_0_als$`test/gender_m/coverage_at_50`[!is.na(data_0_als$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_alslabel <- c(rep('als', length(data_0_m_50_coverage_als)))
    data_0_m_50_coverage_bpr <- data_0_bpr$`test/gender_m/coverage_at_50`[!is.na(data_0_bpr$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_bprlabel <- c(rep('bpr', length(data_0_m_50_coverage_bpr)))
    data_0_m_50_coverage_slim <- data_0_slim$`test/gender_m/coverage_at_50`[!is.na(data_0_slim$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_slimlabel <- c(rep('slim', length(data_0_m_50_coverage_slim)))
    data_0_m_50_coverage_vae <- data_0_vae$`test/gender_m/coverage_at_50`[!is.na(data_0_vae$`test/gender_m/coverage_at_50`)]
    data_0_m_50_coverage_vaelabel <- c(rep('vae', length(data_0_m_50_coverage_vae)))
    data_1_m_50_coverage_pop <- data_1_pop$`test/gender_m/coverage_at_50`[!is.na(data_1_pop$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_poplabel <- c(rep('pop', length(data_1_m_50_coverage_pop)))
    data_1_m_50_coverage_itemknn <- data_1_itemknn$`test/gender_m/coverage_at_50`[!is.na(data_1_itemknn$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_m_50_coverage_itemknn)))
    data_1_m_50_coverage_als <- data_1_als$`test/gender_m/coverage_at_50`[!is.na(data_1_als$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_alslabel <- c(rep('als', length(data_1_m_50_coverage_als)))
    data_1_m_50_coverage_bpr <- data_1_bpr$`test/gender_m/coverage_at_50`[!is.na(data_1_bpr$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_bprlabel <- c(rep('bpr', length(data_1_m_50_coverage_bpr)))
    data_1_m_50_coverage_slim <- data_1_slim$`test/gender_m/coverage_at_50`[!is.na(data_1_slim$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_slimlabel <- c(rep('slim', length(data_1_m_50_coverage_slim)))
    data_1_m_50_coverage_vae <- data_1_vae$`test/gender_m/coverage_at_50`[!is.na(data_1_vae$`test/gender_m/coverage_at_50`)]
    data_1_m_50_coverage_vaelabel <- c(rep('vae', length(data_1_m_50_coverage_vae)))
    data_2_m_50_coverage_pop <- data_2_pop$`test/gender_m/coverage_at_50`[!is.na(data_2_pop$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_poplabel <- c(rep('pop', length(data_2_m_50_coverage_pop)))
    data_2_m_50_coverage_itemknn <- data_2_itemknn$`test/gender_m/coverage_at_50`[!is.na(data_2_itemknn$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_m_50_coverage_itemknn)))
    data_2_m_50_coverage_als <- data_2_als$`test/gender_m/coverage_at_50`[!is.na(data_2_als$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_alslabel <- c(rep('als', length(data_2_m_50_coverage_als)))
    data_2_m_50_coverage_bpr <- data_2_bpr$`test/gender_m/coverage_at_50`[!is.na(data_2_bpr$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_bprlabel <- c(rep('bpr', length(data_2_m_50_coverage_bpr)))
    data_2_m_50_coverage_slim <- data_2_slim$`test/gender_m/coverage_at_50`[!is.na(data_2_slim$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_slimlabel <- c(rep('slim', length(data_2_m_50_coverage_slim)))
    data_2_m_50_coverage_vae <- data_2_vae$`test/gender_m/coverage_at_50`[!is.na(data_2_vae$`test/gender_m/coverage_at_50`)]
    data_2_m_50_coverage_vaelabel <- c(rep('vae', length(data_2_m_50_coverage_vae)))
    data3_m_50_coverage_pop <- data3_pop$`test/gender_m/coverage_at_50`[!is.na(data3_pop$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_poplabel <- c(rep('pop', length(data3_m_50_coverage_pop)))
    data3_m_50_coverage_itemknn <- data3_itemknn$`test/gender_m/coverage_at_50`[!is.na(data3_itemknn$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_itemknnlabel <- c(rep('itemknn', length(data3_m_50_coverage_itemknn)))
    data3_m_50_coverage_als <- data3_als$`test/gender_m/coverage_at_50`[!is.na(data3_als$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_alslabel <- c(rep('als', length(data3_m_50_coverage_als)))
    data3_m_50_coverage_bpr <- data3_bpr$`test/gender_m/coverage_at_50`[!is.na(data3_bpr$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_bprlabel <- c(rep('bpr', length(data3_m_50_coverage_bpr)))
    data3_m_50_coverage_slim <- data3_slim$`test/gender_m/coverage_at_50`[!is.na(data3_slim$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_slimlabel <- c(rep('slim', length(data3_m_50_coverage_slim)))
    data3_m_50_coverage_vae <- data3_vae$`test/gender_m/coverage_at_50`[!is.na(data3_vae$`test/gender_m/coverage_at_50`)]
    data3_m_50_coverage_vaelabel <- c(rep('vae', length(data3_m_50_coverage_vae)))
    data_4_m_50_coverage_pop <- data_4_pop$`test/gender_m/coverage_at_50`[!is.na(data_4_pop$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_poplabel <- c(rep('pop', length(data_4_m_50_coverage_pop)))
    data_4_m_50_coverage_itemknn <- data_4_itemknn$`test/gender_m/coverage_at_50`[!is.na(data_4_itemknn$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_m_50_coverage_itemknn)))
    data_4_m_50_coverage_als <- data_4_als$`test/gender_m/coverage_at_50`[!is.na(data_4_als$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_alslabel <- c(rep('als', length(data_4_m_50_coverage_als)))
    data_4_m_50_coverage_bpr <- data_4_bpr$`test/gender_m/coverage_at_50`[!is.na(data_4_bpr$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_bprlabel <- c(rep('bpr', length(data_4_m_50_coverage_bpr)))
    data_4_m_50_coverage_slim <- data_4_slim$`test/gender_m/coverage_at_50`[!is.na(data_4_slim$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_slimlabel <- c(rep('slim', length(data_4_m_50_coverage_slim)))
    data_4_m_50_coverage_vae <- data_4_vae$`test/gender_m/coverage_at_50`[!is.na(data_4_vae$`test/gender_m/coverage_at_50`)]
    data_4_m_50_coverage_vaelabel <- c(rep('vae', length(data_4_m_50_coverage_vae)))
    # combine data
    coverage_50M <- c(data_0_m_50_coverage_pop, data_0_m_50_coverage_itemknn, data_0_m_50_coverage_als, data_0_m_50_coverage_bpr, data_0_m_50_coverage_slim, data_0_m_50_coverage_vae, data_1_m_50_coverage_pop, data_1_m_50_coverage_itemknn, data_1_m_50_coverage_als, data_1_m_50_coverage_bpr, data_1_m_50_coverage_slim, data_1_m_50_coverage_vae, data_2_m_50_coverage_pop, data_2_m_50_coverage_itemknn, data_2_m_50_coverage_als, data_2_m_50_coverage_bpr, data_2_m_50_coverage_slim, data_2_m_50_coverage_vae, data3_m_50_coverage_pop, data3_m_50_coverage_itemknn, data3_m_50_coverage_als, data3_m_50_coverage_bpr, data3_m_50_coverage_slim, data3_m_50_coverage_vae, data_4_m_50_coverage_pop, data_4_m_50_coverage_itemknn, data_4_m_50_coverage_als, data_4_m_50_coverage_bpr, data_4_m_50_coverage_slim, data_4_m_50_coverage_vae)
    coverage_50labelM <- c(data_0_m_50_coverage_poplabel, data_0_m_50_coverage_itemknnlabel, data_0_m_50_coverage_alslabel, data_0_m_50_coverage_bprlabel, data_0_m_50_coverage_slimlabel, data_0_m_50_coverage_vaelabel, data_1_m_50_coverage_poplabel, data_1_m_50_coverage_itemknnlabel, data_1_m_50_coverage_alslabel, data_1_m_50_coverage_bprlabel, data_1_m_50_coverage_slimlabel, data_1_m_50_coverage_vaelabel, data_2_m_50_coverage_poplabel, data_2_m_50_coverage_itemknnlabel, data_2_m_50_coverage_alslabel, data_2_m_50_coverage_bprlabel, data_2_m_50_coverage_slimlabel, data_2_m_50_coverage_vaelabel, data3_m_50_coverage_poplabel, data3_m_50_coverage_itemknnlabel, data3_m_50_coverage_alslabel, data3_m_50_coverage_bprlabel, data3_m_50_coverage_slimlabel, data3_m_50_coverage_vaelabel, data_4_m_50_coverage_poplabel, data_4_m_50_coverage_itemknnlabel, data_4_m_50_coverage_alslabel, data_4_m_50_coverage_bprlabel, data_4_m_50_coverage_slimlabel, data_4_m_50_coverage_vaelabel)
    ##
    # make dataframe
    coverage_m <- data.frame(coverage_3M, coverage_3labelM, coverage_5M, coverage_5labelM, coverage_10M, coverage_10labelM, coverage_20M, coverage_20labelM, coverage_50M, coverage_50labelM)
    #View(coverage_m)
    ##
    ## 
    data_0_f_3_coverage_pop <- data_0_pop$`test/gender_f/coverage_at_3`[!is.na(data_0_pop$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_poplabel <- c(rep('pop', length(data_0_f_3_coverage_pop)))
    data_0_f_3_coverage_itemknn <- data_0_itemknn$`test/gender_f/coverage_at_3`[!is.na(data_0_itemknn$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_f_3_coverage_itemknn)))
    data_0_f_3_coverage_als <- data_0_als$`test/gender_f/coverage_at_3`[!is.na(data_0_als$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_alslabel <- c(rep('als', length(data_0_f_3_coverage_als)))
    data_0_f_3_coverage_bpr <- data_0_bpr$`test/gender_f/coverage_at_3`[!is.na(data_0_bpr$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_bprlabel <- c(rep('bpr', length(data_0_f_3_coverage_bpr)))
    data_0_f_3_coverage_slim <- data_0_slim$`test/gender_f/coverage_at_3`[!is.na(data_0_slim$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_slimlabel <- c(rep('slim', length(data_0_f_3_coverage_slim)))
    data_0_f_3_coverage_vae <- data_0_vae$`test/gender_f/coverage_at_3`[!is.na(data_0_vae$`test/gender_f/coverage_at_3`)]
    data_0_f_3_coverage_vaelabel <- c(rep('vae', length(data_0_f_3_coverage_vae)))
    data_1_f_3_coverage_pop <- data_1_pop$`test/gender_f/coverage_at_3`[!is.na(data_1_pop$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_poplabel <- c(rep('pop', length(data_1_f_3_coverage_pop)))
    data_1_f_3_coverage_itemknn <- data_1_itemknn$`test/gender_f/coverage_at_3`[!is.na(data_1_itemknn$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_f_3_coverage_itemknn)))
    data_1_f_3_coverage_als <- data_1_als$`test/gender_f/coverage_at_3`[!is.na(data_1_als$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_alslabel <- c(rep('als', length(data_1_f_3_coverage_als)))
    data_1_f_3_coverage_bpr <- data_1_bpr$`test/gender_f/coverage_at_3`[!is.na(data_1_bpr$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_bprlabel <- c(rep('bpr', length(data_1_f_3_coverage_bpr)))
    data_1_f_3_coverage_slim <- data_1_slim$`test/gender_f/coverage_at_3`[!is.na(data_1_slim$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_slimlabel <- c(rep('slim', length(data_1_f_3_coverage_slim)))
    data_1_f_3_coverage_vae <- data_1_vae$`test/gender_f/coverage_at_3`[!is.na(data_1_vae$`test/gender_f/coverage_at_3`)]
    data_1_f_3_coverage_vaelabel <- c(rep('vae', length(data_1_f_3_coverage_vae)))
    data_2_f_3_coverage_pop <- data_2_pop$`test/gender_f/coverage_at_3`[!is.na(data_2_pop$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_poplabel <- c(rep('pop', length(data_2_f_3_coverage_pop)))
    data_2_f_3_coverage_itemknn <- data_2_itemknn$`test/gender_f/coverage_at_3`[!is.na(data_2_itemknn$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_f_3_coverage_itemknn)))
    data_2_f_3_coverage_als <- data_2_als$`test/gender_f/coverage_at_3`[!is.na(data_2_als$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_alslabel <- c(rep('als', length(data_2_f_3_coverage_als)))
    data_2_f_3_coverage_bpr <- data_2_bpr$`test/gender_f/coverage_at_3`[!is.na(data_2_bpr$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_bprlabel <- c(rep('bpr', length(data_2_f_3_coverage_bpr)))
    data_2_f_3_coverage_slim <- data_2_slim$`test/gender_f/coverage_at_3`[!is.na(data_2_slim$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_slimlabel <- c(rep('slim', length(data_2_f_3_coverage_slim)))
    data_2_f_3_coverage_vae <- data_2_vae$`test/gender_f/coverage_at_3`[!is.na(data_2_vae$`test/gender_f/coverage_at_3`)]
    data_2_f_3_coverage_vaelabel <- c(rep('vae', length(data_2_f_3_coverage_vae)))
    data3_f_3_coverage_pop <- data3_pop$`test/gender_f/coverage_at_3`[!is.na(data3_pop$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_poplabel <- c(rep('pop', length(data3_f_3_coverage_pop)))
    data3_f_3_coverage_itemknn <- data3_itemknn$`test/gender_f/coverage_at_3`[!is.na(data3_itemknn$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_itemknnlabel <- c(rep('itemknn', length(data3_f_3_coverage_itemknn)))
    data3_f_3_coverage_als <- data3_als$`test/gender_f/coverage_at_3`[!is.na(data3_als$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_alslabel <- c(rep('als', length(data3_f_3_coverage_als)))
    data3_f_3_coverage_bpr <- data3_bpr$`test/gender_f/coverage_at_3`[!is.na(data3_bpr$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_bprlabel <- c(rep('bpr', length(data3_f_3_coverage_bpr)))
    data3_f_3_coverage_slim <- data3_slim$`test/gender_f/coverage_at_3`[!is.na(data3_slim$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_slimlabel <- c(rep('slim', length(data3_f_3_coverage_slim)))
    data3_f_3_coverage_vae <- data3_vae$`test/gender_f/coverage_at_3`[!is.na(data3_vae$`test/gender_f/coverage_at_3`)]
    data3_f_3_coverage_vaelabel <- c(rep('vae', length(data3_f_3_coverage_vae)))
    data_4_f_3_coverage_pop <- data_4_pop$`test/gender_f/coverage_at_3`[!is.na(data_4_pop$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_poplabel <- c(rep('pop', length(data_4_f_3_coverage_pop)))
    data_4_f_3_coverage_itemknn <- data_4_itemknn$`test/gender_f/coverage_at_3`[!is.na(data_4_itemknn$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_f_3_coverage_itemknn)))
    data_4_f_3_coverage_als <- data_4_als$`test/gender_f/coverage_at_3`[!is.na(data_4_als$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_alslabel <- c(rep('als', length(data_4_f_3_coverage_als)))
    data_4_f_3_coverage_bpr <- data_4_bpr$`test/gender_f/coverage_at_3`[!is.na(data_4_bpr$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_bprlabel <- c(rep('bpr', length(data_4_f_3_coverage_bpr)))
    data_4_f_3_coverage_slim <- data_4_slim$`test/gender_f/coverage_at_3`[!is.na(data_4_slim$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_slimlabel <- c(rep('slim', length(data_4_f_3_coverage_slim)))
    data_4_f_3_coverage_vae <- data_4_vae$`test/gender_f/coverage_at_3`[!is.na(data_4_vae$`test/gender_f/coverage_at_3`)]
    data_4_f_3_coverage_vaelabel <- c(rep('vae', length(data_4_f_3_coverage_vae)))
    # combine data
    coverage_3F <- c(data_0_f_3_coverage_pop, data_0_f_3_coverage_itemknn, data_0_f_3_coverage_als, data_0_f_3_coverage_bpr, data_0_f_3_coverage_slim, data_0_f_3_coverage_vae, data_1_f_3_coverage_pop, data_1_f_3_coverage_itemknn, data_1_f_3_coverage_als, data_1_f_3_coverage_bpr, data_1_f_3_coverage_slim, data_1_f_3_coverage_vae, data_2_f_3_coverage_pop, data_2_f_3_coverage_itemknn, data_2_f_3_coverage_als, data_2_f_3_coverage_bpr, data_2_f_3_coverage_slim, data_2_f_3_coverage_vae, data3_f_3_coverage_pop, data3_f_3_coverage_itemknn, data3_f_3_coverage_als, data3_f_3_coverage_bpr, data3_f_3_coverage_slim, data3_f_3_coverage_vae, data_4_f_3_coverage_pop, data_4_f_3_coverage_itemknn, data_4_f_3_coverage_als, data_4_f_3_coverage_bpr, data_4_f_3_coverage_slim, data_4_f_3_coverage_vae)
    coverage_3labelF <- c(data_0_f_3_coverage_poplabel, data_0_f_3_coverage_itemknnlabel, data_0_f_3_coverage_alslabel, data_0_f_3_coverage_bprlabel, data_0_f_3_coverage_slimlabel, data_0_f_3_coverage_vaelabel, data_1_f_3_coverage_poplabel, data_1_f_3_coverage_itemknnlabel, data_1_f_3_coverage_alslabel, data_1_f_3_coverage_bprlabel, data_1_f_3_coverage_slimlabel, data_1_f_3_coverage_vaelabel, data_2_f_3_coverage_poplabel, data_2_f_3_coverage_itemknnlabel, data_2_f_3_coverage_alslabel, data_2_f_3_coverage_bprlabel, data_2_f_3_coverage_slimlabel, data_2_f_3_coverage_vaelabel, data3_f_3_coverage_poplabel, data3_f_3_coverage_itemknnlabel, data3_f_3_coverage_alslabel, data3_f_3_coverage_bprlabel, data3_f_3_coverage_slimlabel, data3_f_3_coverage_vaelabel, data_4_f_3_coverage_poplabel, data_4_f_3_coverage_itemknnlabel, data_4_f_3_coverage_alslabel, data_4_f_3_coverage_bprlabel, data_4_f_3_coverage_slimlabel, data_4_f_3_coverage_vaelabel)
    #
    data_0_f_5_coverage_pop <- data_0_pop$`test/gender_f/coverage_at_5`[!is.na(data_0_pop$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_poplabel <- c(rep('pop', length(data_0_f_5_coverage_pop)))
    data_0_f_5_coverage_itemknn <- data_0_itemknn$`test/gender_f/coverage_at_5`[!is.na(data_0_itemknn$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_f_5_coverage_itemknn)))
    data_0_f_5_coverage_als <- data_0_als$`test/gender_f/coverage_at_5`[!is.na(data_0_als$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_alslabel <- c(rep('als', length(data_0_f_5_coverage_als)))
    data_0_f_5_coverage_bpr <- data_0_bpr$`test/gender_f/coverage_at_5`[!is.na(data_0_bpr$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_bprlabel <- c(rep('bpr', length(data_0_f_5_coverage_bpr)))
    data_0_f_5_coverage_slim <- data_0_slim$`test/gender_f/coverage_at_5`[!is.na(data_0_slim$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_slimlabel <- c(rep('slim', length(data_0_f_5_coverage_slim)))
    data_0_f_5_coverage_vae <- data_0_vae$`test/gender_f/coverage_at_5`[!is.na(data_0_vae$`test/gender_f/coverage_at_5`)]
    data_0_f_5_coverage_vaelabel <- c(rep('vae', length(data_0_f_5_coverage_vae)))
    data_1_f_5_coverage_pop <- data_1_pop$`test/gender_f/coverage_at_5`[!is.na(data_1_pop$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_poplabel <- c(rep('pop', length(data_1_f_5_coverage_pop)))
    data_1_f_5_coverage_itemknn <- data_1_itemknn$`test/gender_f/coverage_at_5`[!is.na(data_1_itemknn$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_f_5_coverage_itemknn)))
    data_1_f_5_coverage_als <- data_1_als$`test/gender_f/coverage_at_5`[!is.na(data_1_als$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_alslabel <- c(rep('als', length(data_1_f_5_coverage_als)))
    data_1_f_5_coverage_bpr <- data_1_bpr$`test/gender_f/coverage_at_5`[!is.na(data_1_bpr$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_bprlabel <- c(rep('bpr', length(data_1_f_5_coverage_bpr)))
    data_1_f_5_coverage_slim <- data_1_slim$`test/gender_f/coverage_at_5`[!is.na(data_1_slim$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_slimlabel <- c(rep('slim', length(data_1_f_5_coverage_slim)))
    data_1_f_5_coverage_vae <- data_1_vae$`test/gender_f/coverage_at_5`[!is.na(data_1_vae$`test/gender_f/coverage_at_5`)]
    data_1_f_5_coverage_vaelabel <- c(rep('vae', length(data_1_f_5_coverage_vae)))
    data_2_f_5_coverage_pop <- data_2_pop$`test/gender_f/coverage_at_5`[!is.na(data_2_pop$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_poplabel <- c(rep('pop', length(data_2_f_5_coverage_pop)))
    data_2_f_5_coverage_itemknn <- data_2_itemknn$`test/gender_f/coverage_at_5`[!is.na(data_2_itemknn$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_f_5_coverage_itemknn)))
    data_2_f_5_coverage_als <- data_2_als$`test/gender_f/coverage_at_5`[!is.na(data_2_als$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_alslabel <- c(rep('als', length(data_2_f_5_coverage_als)))
    data_2_f_5_coverage_bpr <- data_2_bpr$`test/gender_f/coverage_at_5`[!is.na(data_2_bpr$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_bprlabel <- c(rep('bpr', length(data_2_f_5_coverage_bpr)))
    data_2_f_5_coverage_slim <- data_2_slim$`test/gender_f/coverage_at_5`[!is.na(data_2_slim$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_slimlabel <- c(rep('slim', length(data_2_f_5_coverage_slim)))
    data_2_f_5_coverage_vae <- data_2_vae$`test/gender_f/coverage_at_5`[!is.na(data_2_vae$`test/gender_f/coverage_at_5`)]
    data_2_f_5_coverage_vaelabel <- c(rep('vae', length(data_2_f_5_coverage_vae)))
    data3_f_5_coverage_pop <- data3_pop$`test/gender_f/coverage_at_5`[!is.na(data3_pop$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_poplabel <- c(rep('pop', length(data3_f_5_coverage_pop)))
    data3_f_5_coverage_itemknn <- data3_itemknn$`test/gender_f/coverage_at_5`[!is.na(data3_itemknn$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_itemknnlabel <- c(rep('itemknn', length(data3_f_5_coverage_itemknn)))
    data3_f_5_coverage_als <- data3_als$`test/gender_f/coverage_at_5`[!is.na(data3_als$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_alslabel <- c(rep('als', length(data3_f_5_coverage_als)))
    data3_f_5_coverage_bpr <- data3_bpr$`test/gender_f/coverage_at_5`[!is.na(data3_bpr$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_bprlabel <- c(rep('bpr', length(data3_f_5_coverage_bpr)))
    data3_f_5_coverage_slim <- data3_slim$`test/gender_f/coverage_at_5`[!is.na(data3_slim$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_slimlabel <- c(rep('slim', length(data3_f_5_coverage_slim)))
    data3_f_5_coverage_vae <- data3_vae$`test/gender_f/coverage_at_5`[!is.na(data3_vae$`test/gender_f/coverage_at_5`)]
    data3_f_5_coverage_vaelabel <- c(rep('vae', length(data3_f_5_coverage_vae)))
    data_4_f_5_coverage_pop <- data_4_pop$`test/gender_f/coverage_at_5`[!is.na(data_4_pop$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_poplabel <- c(rep('pop', length(data_4_f_5_coverage_pop)))
    data_4_f_5_coverage_itemknn <- data_4_itemknn$`test/gender_f/coverage_at_5`[!is.na(data_4_itemknn$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_f_5_coverage_itemknn)))
    data_4_f_5_coverage_als <- data_4_als$`test/gender_f/coverage_at_5`[!is.na(data_4_als$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_alslabel <- c(rep('als', length(data_4_f_5_coverage_als)))
    data_4_f_5_coverage_bpr <- data_4_bpr$`test/gender_f/coverage_at_5`[!is.na(data_4_bpr$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_bprlabel <- c(rep('bpr', length(data_4_f_5_coverage_bpr)))
    data_4_f_5_coverage_slim <- data_4_slim$`test/gender_f/coverage_at_5`[!is.na(data_4_slim$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_slimlabel <- c(rep('slim', length(data_4_f_5_coverage_slim)))
    data_4_f_5_coverage_vae <- data_4_vae$`test/gender_f/coverage_at_5`[!is.na(data_4_vae$`test/gender_f/coverage_at_5`)]
    data_4_f_5_coverage_vaelabel <- c(rep('vae', length(data_4_f_5_coverage_vae)))
    # combine data
    coverage_5F <- c(data_0_f_5_coverage_pop, data_0_f_5_coverage_itemknn, data_0_f_5_coverage_als, data_0_f_5_coverage_bpr, data_0_f_5_coverage_slim, data_0_f_5_coverage_vae, data_1_f_5_coverage_pop, data_1_f_5_coverage_itemknn, data_1_f_5_coverage_als, data_1_f_5_coverage_bpr, data_1_f_5_coverage_slim, data_1_f_5_coverage_vae, data_2_f_5_coverage_pop, data_2_f_5_coverage_itemknn, data_2_f_5_coverage_als, data_2_f_5_coverage_bpr, data_2_f_5_coverage_slim, data_2_f_5_coverage_vae, data3_f_5_coverage_pop, data3_f_5_coverage_itemknn, data3_f_5_coverage_als, data3_f_5_coverage_bpr, data3_f_5_coverage_slim, data3_f_5_coverage_vae, data_4_f_5_coverage_pop, data_4_f_5_coverage_itemknn, data_4_f_5_coverage_als, data_4_f_5_coverage_bpr, data_4_f_5_coverage_slim, data_4_f_5_coverage_vae)
    coverage_5labelF <- c(data_0_f_5_coverage_poplabel, data_0_f_5_coverage_itemknnlabel, data_0_f_5_coverage_alslabel, data_0_f_5_coverage_bprlabel, data_0_f_5_coverage_slimlabel, data_0_f_5_coverage_vaelabel, data_1_f_5_coverage_poplabel, data_1_f_5_coverage_itemknnlabel, data_1_f_5_coverage_alslabel, data_1_f_5_coverage_bprlabel, data_1_f_5_coverage_slimlabel, data_1_f_5_coverage_vaelabel, data_2_f_5_coverage_poplabel, data_2_f_5_coverage_itemknnlabel, data_2_f_5_coverage_alslabel, data_2_f_5_coverage_bprlabel, data_2_f_5_coverage_slimlabel, data_2_f_5_coverage_vaelabel, data3_f_5_coverage_poplabel, data3_f_5_coverage_itemknnlabel, data3_f_5_coverage_alslabel, data3_f_5_coverage_bprlabel, data3_f_5_coverage_slimlabel, data3_f_5_coverage_vaelabel, data_4_f_5_coverage_poplabel, data_4_f_5_coverage_itemknnlabel, data_4_f_5_coverage_alslabel, data_4_f_5_coverage_bprlabel, data_4_f_5_coverage_slimlabel, data_4_f_5_coverage_vaelabel)
    #
    data_0_f_10_coverage_pop <- data_0_pop$`test/gender_f/coverage_at_10`[!is.na(data_0_pop$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_poplabel <- c(rep('pop', length(data_0_f_10_coverage_pop)))
    data_0_f_10_coverage_itemknn <- data_0_itemknn$`test/gender_f/coverage_at_10`[!is.na(data_0_itemknn$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_f_10_coverage_itemknn)))
    data_0_f_10_coverage_als <- data_0_als$`test/gender_f/coverage_at_10`[!is.na(data_0_als$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_alslabel <- c(rep('als', length(data_0_f_10_coverage_als)))
    data_0_f_10_coverage_bpr <- data_0_bpr$`test/gender_f/coverage_at_10`[!is.na(data_0_bpr$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_bprlabel <- c(rep('bpr', length(data_0_f_10_coverage_bpr)))
    data_0_f_10_coverage_slim <- data_0_slim$`test/gender_f/coverage_at_10`[!is.na(data_0_slim$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_slimlabel <- c(rep('slim', length(data_0_f_10_coverage_slim)))
    data_0_f_10_coverage_vae <- data_0_vae$`test/gender_f/coverage_at_10`[!is.na(data_0_vae$`test/gender_f/coverage_at_10`)]
    data_0_f_10_coverage_vaelabel <- c(rep('vae', length(data_0_f_10_coverage_vae)))
    data_1_f_10_coverage_pop <- data_1_pop$`test/gender_f/coverage_at_10`[!is.na(data_1_pop$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_poplabel <- c(rep('pop', length(data_1_f_10_coverage_pop)))
    data_1_f_10_coverage_itemknn <- data_1_itemknn$`test/gender_f/coverage_at_10`[!is.na(data_1_itemknn$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_f_10_coverage_itemknn)))
    data_1_f_10_coverage_als <- data_1_als$`test/gender_f/coverage_at_10`[!is.na(data_1_als$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_alslabel <- c(rep('als', length(data_1_f_10_coverage_als)))
    data_1_f_10_coverage_bpr <- data_1_bpr$`test/gender_f/coverage_at_10`[!is.na(data_1_bpr$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_bprlabel <- c(rep('bpr', length(data_1_f_10_coverage_bpr)))
    data_1_f_10_coverage_slim <- data_1_slim$`test/gender_f/coverage_at_10`[!is.na(data_1_slim$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_slimlabel <- c(rep('slim', length(data_1_f_10_coverage_slim)))
    data_1_f_10_coverage_vae <- data_1_vae$`test/gender_f/coverage_at_10`[!is.na(data_1_vae$`test/gender_f/coverage_at_10`)]
    data_1_f_10_coverage_vaelabel <- c(rep('vae', length(data_1_f_10_coverage_vae)))
    data_2_f_10_coverage_pop <- data_2_pop$`test/gender_f/coverage_at_10`[!is.na(data_2_pop$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_poplabel <- c(rep('pop', length(data_2_f_10_coverage_pop)))
    data_2_f_10_coverage_itemknn <- data_2_itemknn$`test/gender_f/coverage_at_10`[!is.na(data_2_itemknn$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_f_10_coverage_itemknn)))
    data_2_f_10_coverage_als <- data_2_als$`test/gender_f/coverage_at_10`[!is.na(data_2_als$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_alslabel <- c(rep('als', length(data_2_f_10_coverage_als)))
    data_2_f_10_coverage_bpr <- data_2_bpr$`test/gender_f/coverage_at_10`[!is.na(data_2_bpr$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_bprlabel <- c(rep('bpr', length(data_2_f_10_coverage_bpr)))
    data_2_f_10_coverage_slim <- data_2_slim$`test/gender_f/coverage_at_10`[!is.na(data_2_slim$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_slimlabel <- c(rep('slim', length(data_2_f_10_coverage_slim)))
    data_2_f_10_coverage_vae <- data_2_vae$`test/gender_f/coverage_at_10`[!is.na(data_2_vae$`test/gender_f/coverage_at_10`)]
    data_2_f_10_coverage_vaelabel <- c(rep('vae', length(data_2_f_10_coverage_vae)))
    data3_f_10_coverage_pop <- data3_pop$`test/gender_f/coverage_at_10`[!is.na(data3_pop$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_poplabel <- c(rep('pop', length(data3_f_10_coverage_pop)))
    data3_f_10_coverage_itemknn <- data3_itemknn$`test/gender_f/coverage_at_10`[!is.na(data3_itemknn$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_itemknnlabel <- c(rep('itemknn', length(data3_f_10_coverage_itemknn)))
    data3_f_10_coverage_als <- data3_als$`test/gender_f/coverage_at_10`[!is.na(data3_als$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_alslabel <- c(rep('als', length(data3_f_10_coverage_als)))
    data3_f_10_coverage_bpr <- data3_bpr$`test/gender_f/coverage_at_10`[!is.na(data3_bpr$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_bprlabel <- c(rep('bpr', length(data3_f_10_coverage_bpr)))
    data3_f_10_coverage_slim <- data3_slim$`test/gender_f/coverage_at_10`[!is.na(data3_slim$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_slimlabel <- c(rep('slim', length(data3_f_10_coverage_slim)))
    data3_f_10_coverage_vae <- data3_vae$`test/gender_f/coverage_at_10`[!is.na(data3_vae$`test/gender_f/coverage_at_10`)]
    data3_f_10_coverage_vaelabel <- c(rep('vae', length(data3_f_10_coverage_vae)))
    data_4_f_10_coverage_pop <- data_4_pop$`test/gender_f/coverage_at_10`[!is.na(data_4_pop$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_poplabel <- c(rep('pop', length(data_4_f_10_coverage_pop)))
    data_4_f_10_coverage_itemknn <- data_4_itemknn$`test/gender_f/coverage_at_10`[!is.na(data_4_itemknn$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_f_10_coverage_itemknn)))
    data_4_f_10_coverage_als <- data_4_als$`test/gender_f/coverage_at_10`[!is.na(data_4_als$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_alslabel <- c(rep('als', length(data_4_f_10_coverage_als)))
    data_4_f_10_coverage_bpr <- data_4_bpr$`test/gender_f/coverage_at_10`[!is.na(data_4_bpr$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_bprlabel <- c(rep('bpr', length(data_4_f_10_coverage_bpr)))
    data_4_f_10_coverage_slim <- data_4_slim$`test/gender_f/coverage_at_10`[!is.na(data_4_slim$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_slimlabel <- c(rep('slim', length(data_4_f_10_coverage_slim)))
    data_4_f_10_coverage_vae <- data_4_vae$`test/gender_f/coverage_at_10`[!is.na(data_4_vae$`test/gender_f/coverage_at_10`)]
    data_4_f_10_coverage_vaelabel <- c(rep('vae', length(data_4_f_10_coverage_vae)))
    # combine data
    coverage_10F <- c(data_0_f_10_coverage_pop, data_0_f_10_coverage_itemknn, data_0_f_10_coverage_als, data_0_f_10_coverage_bpr, data_0_f_10_coverage_slim, data_0_f_10_coverage_vae, data_1_f_10_coverage_pop, data_1_f_10_coverage_itemknn, data_1_f_10_coverage_als, data_1_f_10_coverage_bpr, data_1_f_10_coverage_slim, data_1_f_10_coverage_vae, data_2_f_10_coverage_pop, data_2_f_10_coverage_itemknn, data_2_f_10_coverage_als, data_2_f_10_coverage_bpr, data_2_f_10_coverage_slim, data_2_f_10_coverage_vae, data3_f_10_coverage_pop, data3_f_10_coverage_itemknn, data3_f_10_coverage_als, data3_f_10_coverage_bpr, data3_f_10_coverage_slim, data3_f_10_coverage_vae, data_4_f_10_coverage_pop, data_4_f_10_coverage_itemknn, data_4_f_10_coverage_als, data_4_f_10_coverage_bpr, data_4_f_10_coverage_slim, data_4_f_10_coverage_vae)
    coverage_10labelF <- c(data_0_f_10_coverage_poplabel, data_0_f_10_coverage_itemknnlabel, data_0_f_10_coverage_alslabel, data_0_f_10_coverage_bprlabel, data_0_f_10_coverage_slimlabel, data_0_f_10_coverage_vaelabel, data_1_f_10_coverage_poplabel, data_1_f_10_coverage_itemknnlabel, data_1_f_10_coverage_alslabel, data_1_f_10_coverage_bprlabel, data_1_f_10_coverage_slimlabel, data_1_f_10_coverage_vaelabel, data_2_f_10_coverage_poplabel, data_2_f_10_coverage_itemknnlabel, data_2_f_10_coverage_alslabel, data_2_f_10_coverage_bprlabel, data_2_f_10_coverage_slimlabel, data_2_f_10_coverage_vaelabel, data3_f_10_coverage_poplabel, data3_f_10_coverage_itemknnlabel, data3_f_10_coverage_alslabel, data3_f_10_coverage_bprlabel, data3_f_10_coverage_slimlabel, data3_f_10_coverage_vaelabel, data_4_f_10_coverage_poplabel, data_4_f_10_coverage_itemknnlabel, data_4_f_10_coverage_alslabel, data_4_f_10_coverage_bprlabel, data_4_f_10_coverage_slimlabel, data_4_f_10_coverage_vaelabel)
    #
    data_0_f_20_coverage_pop <- data_0_pop$`test/gender_f/coverage_at_20`[!is.na(data_0_pop$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_poplabel <- c(rep('pop', length(data_0_f_20_coverage_pop)))
    data_0_f_20_coverage_itemknn <- data_0_itemknn$`test/gender_f/coverage_at_20`[!is.na(data_0_itemknn$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_f_20_coverage_itemknn)))
    data_0_f_20_coverage_als <- data_0_als$`test/gender_f/coverage_at_20`[!is.na(data_0_als$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_alslabel <- c(rep('als', length(data_0_f_20_coverage_als)))
    data_0_f_20_coverage_bpr <- data_0_bpr$`test/gender_f/coverage_at_20`[!is.na(data_0_bpr$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_bprlabel <- c(rep('bpr', length(data_0_f_20_coverage_bpr)))
    data_0_f_20_coverage_slim <- data_0_slim$`test/gender_f/coverage_at_20`[!is.na(data_0_slim$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_slimlabel <- c(rep('slim', length(data_0_f_20_coverage_slim)))
    data_0_f_20_coverage_vae <- data_0_vae$`test/gender_f/coverage_at_20`[!is.na(data_0_vae$`test/gender_f/coverage_at_20`)]
    data_0_f_20_coverage_vaelabel <- c(rep('vae', length(data_0_f_20_coverage_vae)))
    data_1_f_20_coverage_pop <- data_1_pop$`test/gender_f/coverage_at_20`[!is.na(data_1_pop$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_poplabel <- c(rep('pop', length(data_1_f_20_coverage_pop)))
    data_1_f_20_coverage_itemknn <- data_1_itemknn$`test/gender_f/coverage_at_20`[!is.na(data_1_itemknn$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_f_20_coverage_itemknn)))
    data_1_f_20_coverage_als <- data_1_als$`test/gender_f/coverage_at_20`[!is.na(data_1_als$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_alslabel <- c(rep('als', length(data_1_f_20_coverage_als)))
    data_1_f_20_coverage_bpr <- data_1_bpr$`test/gender_f/coverage_at_20`[!is.na(data_1_bpr$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_bprlabel <- c(rep('bpr', length(data_1_f_20_coverage_bpr)))
    data_1_f_20_coverage_slim <- data_1_slim$`test/gender_f/coverage_at_20`[!is.na(data_1_slim$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_slimlabel <- c(rep('slim', length(data_1_f_20_coverage_slim)))
    data_1_f_20_coverage_vae <- data_1_vae$`test/gender_f/coverage_at_20`[!is.na(data_1_vae$`test/gender_f/coverage_at_20`)]
    data_1_f_20_coverage_vaelabel <- c(rep('vae', length(data_1_f_20_coverage_vae)))
    data_2_f_20_coverage_pop <- data_2_pop$`test/gender_f/coverage_at_20`[!is.na(data_2_pop$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_poplabel <- c(rep('pop', length(data_2_f_20_coverage_pop)))
    data_2_f_20_coverage_itemknn <- data_2_itemknn$`test/gender_f/coverage_at_20`[!is.na(data_2_itemknn$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_f_20_coverage_itemknn)))
    data_2_f_20_coverage_als <- data_2_als$`test/gender_f/coverage_at_20`[!is.na(data_2_als$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_alslabel <- c(rep('als', length(data_2_f_20_coverage_als)))
    data_2_f_20_coverage_bpr <- data_2_bpr$`test/gender_f/coverage_at_20`[!is.na(data_2_bpr$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_bprlabel <- c(rep('bpr', length(data_2_f_20_coverage_bpr)))
    data_2_f_20_coverage_slim <- data_2_slim$`test/gender_f/coverage_at_20`[!is.na(data_2_slim$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_slimlabel <- c(rep('slim', length(data_2_f_20_coverage_slim)))
    data_2_f_20_coverage_vae <- data_2_vae$`test/gender_f/coverage_at_20`[!is.na(data_2_vae$`test/gender_f/coverage_at_20`)]
    data_2_f_20_coverage_vaelabel <- c(rep('vae', length(data_2_f_20_coverage_vae)))
    data3_f_20_coverage_pop <- data3_pop$`test/gender_f/coverage_at_20`[!is.na(data3_pop$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_poplabel <- c(rep('pop', length(data3_f_20_coverage_pop)))
    data3_f_20_coverage_itemknn <- data3_itemknn$`test/gender_f/coverage_at_20`[!is.na(data3_itemknn$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_itemknnlabel <- c(rep('itemknn', length(data3_f_20_coverage_itemknn)))
    data3_f_20_coverage_als <- data3_als$`test/gender_f/coverage_at_20`[!is.na(data3_als$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_alslabel <- c(rep('als', length(data3_f_20_coverage_als)))
    data3_f_20_coverage_bpr <- data3_bpr$`test/gender_f/coverage_at_20`[!is.na(data3_bpr$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_bprlabel <- c(rep('bpr', length(data3_f_20_coverage_bpr)))
    data3_f_20_coverage_slim <- data3_slim$`test/gender_f/coverage_at_20`[!is.na(data3_slim$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_slimlabel <- c(rep('slim', length(data3_f_20_coverage_slim)))
    data3_f_20_coverage_vae <- data3_vae$`test/gender_f/coverage_at_20`[!is.na(data3_vae$`test/gender_f/coverage_at_20`)]
    data3_f_20_coverage_vaelabel <- c(rep('vae', length(data3_f_20_coverage_vae)))
    data_4_f_20_coverage_pop <- data_4_pop$`test/gender_f/coverage_at_20`[!is.na(data_4_pop$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_poplabel <- c(rep('pop', length(data_4_f_20_coverage_pop)))
    data_4_f_20_coverage_itemknn <- data_4_itemknn$`test/gender_f/coverage_at_20`[!is.na(data_4_itemknn$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_f_20_coverage_itemknn)))
    data_4_f_20_coverage_als <- data_4_als$`test/gender_f/coverage_at_20`[!is.na(data_4_als$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_alslabel <- c(rep('als', length(data_4_f_20_coverage_als)))
    data_4_f_20_coverage_bpr <- data_4_bpr$`test/gender_f/coverage_at_20`[!is.na(data_4_bpr$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_bprlabel <- c(rep('bpr', length(data_4_f_20_coverage_bpr)))
    data_4_f_20_coverage_slim <- data_4_slim$`test/gender_f/coverage_at_20`[!is.na(data_4_slim$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_slimlabel <- c(rep('slim', length(data_4_f_20_coverage_slim)))
    data_4_f_20_coverage_vae <- data_4_vae$`test/gender_f/coverage_at_20`[!is.na(data_4_vae$`test/gender_f/coverage_at_20`)]
    data_4_f_20_coverage_vaelabel <- c(rep('vae', length(data_4_f_20_coverage_vae)))
    # combine data
    coverage_20F <- c(data_0_f_20_coverage_pop, data_0_f_20_coverage_itemknn, data_0_f_20_coverage_als, data_0_f_20_coverage_bpr, data_0_f_20_coverage_slim, data_0_f_20_coverage_vae, data_1_f_20_coverage_pop, data_1_f_20_coverage_itemknn, data_1_f_20_coverage_als, data_1_f_20_coverage_bpr, data_1_f_20_coverage_slim, data_1_f_20_coverage_vae, data_2_f_20_coverage_pop, data_2_f_20_coverage_itemknn, data_2_f_20_coverage_als, data_2_f_20_coverage_bpr, data_2_f_20_coverage_slim, data_2_f_20_coverage_vae, data3_f_20_coverage_pop, data3_f_20_coverage_itemknn, data3_f_20_coverage_als, data3_f_20_coverage_bpr, data3_f_20_coverage_slim, data3_f_20_coverage_vae, data_4_f_20_coverage_pop, data_4_f_20_coverage_itemknn, data_4_f_20_coverage_als, data_4_f_20_coverage_bpr, data_4_f_20_coverage_slim, data_4_f_20_coverage_vae)
    coverage_20labelF <- c(data_0_f_20_coverage_poplabel, data_0_f_20_coverage_itemknnlabel, data_0_f_20_coverage_alslabel, data_0_f_20_coverage_bprlabel, data_0_f_20_coverage_slimlabel, data_0_f_20_coverage_vaelabel, data_1_f_20_coverage_poplabel, data_1_f_20_coverage_itemknnlabel, data_1_f_20_coverage_alslabel, data_1_f_20_coverage_bprlabel, data_1_f_20_coverage_slimlabel, data_1_f_20_coverage_vaelabel, data_2_f_20_coverage_poplabel, data_2_f_20_coverage_itemknnlabel, data_2_f_20_coverage_alslabel, data_2_f_20_coverage_bprlabel, data_2_f_20_coverage_slimlabel, data_2_f_20_coverage_vaelabel, data3_f_20_coverage_poplabel, data3_f_20_coverage_itemknnlabel, data3_f_20_coverage_alslabel, data3_f_20_coverage_bprlabel, data3_f_20_coverage_slimlabel, data3_f_20_coverage_vaelabel, data_4_f_20_coverage_poplabel, data_4_f_20_coverage_itemknnlabel, data_4_f_20_coverage_alslabel, data_4_f_20_coverage_bprlabel, data_4_f_20_coverage_slimlabel, data_4_f_20_coverage_vaelabel)
    #
    data_0_f_50_coverage_pop <- data_0_pop$`test/gender_f/coverage_at_50`[!is.na(data_0_pop$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_poplabel <- c(rep('pop', length(data_0_f_50_coverage_pop)))
    data_0_f_50_coverage_itemknn <- data_0_itemknn$`test/gender_f/coverage_at_50`[!is.na(data_0_itemknn$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_0_f_50_coverage_itemknn)))
    data_0_f_50_coverage_als <- data_0_als$`test/gender_f/coverage_at_50`[!is.na(data_0_als$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_alslabel <- c(rep('als', length(data_0_f_50_coverage_als)))
    data_0_f_50_coverage_bpr <- data_0_bpr$`test/gender_f/coverage_at_50`[!is.na(data_0_bpr$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_bprlabel <- c(rep('bpr', length(data_0_f_50_coverage_bpr)))
    data_0_f_50_coverage_slim <- data_0_slim$`test/gender_f/coverage_at_50`[!is.na(data_0_slim$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_slimlabel <- c(rep('slim', length(data_0_f_50_coverage_slim)))
    data_0_f_50_coverage_vae <- data_0_vae$`test/gender_f/coverage_at_50`[!is.na(data_0_vae$`test/gender_f/coverage_at_50`)]
    data_0_f_50_coverage_vaelabel <- c(rep('vae', length(data_0_f_50_coverage_vae)))
    data_1_f_50_coverage_pop <- data_1_pop$`test/gender_f/coverage_at_50`[!is.na(data_1_pop$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_poplabel <- c(rep('pop', length(data_1_f_50_coverage_pop)))
    data_1_f_50_coverage_itemknn <- data_1_itemknn$`test/gender_f/coverage_at_50`[!is.na(data_1_itemknn$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_1_f_50_coverage_itemknn)))
    data_1_f_50_coverage_als <- data_1_als$`test/gender_f/coverage_at_50`[!is.na(data_1_als$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_alslabel <- c(rep('als', length(data_1_f_50_coverage_als)))
    data_1_f_50_coverage_bpr <- data_1_bpr$`test/gender_f/coverage_at_50`[!is.na(data_1_bpr$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_bprlabel <- c(rep('bpr', length(data_1_f_50_coverage_bpr)))
    data_1_f_50_coverage_slim <- data_1_slim$`test/gender_f/coverage_at_50`[!is.na(data_1_slim$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_slimlabel <- c(rep('slim', length(data_1_f_50_coverage_slim)))
    data_1_f_50_coverage_vae <- data_1_vae$`test/gender_f/coverage_at_50`[!is.na(data_1_vae$`test/gender_f/coverage_at_50`)]
    data_1_f_50_coverage_vaelabel <- c(rep('vae', length(data_1_f_50_coverage_vae)))
    data_2_f_50_coverage_pop <- data_2_pop$`test/gender_f/coverage_at_50`[!is.na(data_2_pop$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_poplabel <- c(rep('pop', length(data_2_f_50_coverage_pop)))
    data_2_f_50_coverage_itemknn <- data_2_itemknn$`test/gender_f/coverage_at_50`[!is.na(data_2_itemknn$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_2_f_50_coverage_itemknn)))
    data_2_f_50_coverage_als <- data_2_als$`test/gender_f/coverage_at_50`[!is.na(data_2_als$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_alslabel <- c(rep('als', length(data_2_f_50_coverage_als)))
    data_2_f_50_coverage_bpr <- data_2_bpr$`test/gender_f/coverage_at_50`[!is.na(data_2_bpr$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_bprlabel <- c(rep('bpr', length(data_2_f_50_coverage_bpr)))
    data_2_f_50_coverage_slim <- data_2_slim$`test/gender_f/coverage_at_50`[!is.na(data_2_slim$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_slimlabel <- c(rep('slim', length(data_2_f_50_coverage_slim)))
    data_2_f_50_coverage_vae <- data_2_vae$`test/gender_f/coverage_at_50`[!is.na(data_2_vae$`test/gender_f/coverage_at_50`)]
    data_2_f_50_coverage_vaelabel <- c(rep('vae', length(data_2_f_50_coverage_vae)))
    data3_f_50_coverage_pop <- data3_pop$`test/gender_f/coverage_at_50`[!is.na(data3_pop$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_poplabel <- c(rep('pop', length(data3_f_50_coverage_pop)))
    data3_f_50_coverage_itemknn <- data3_itemknn$`test/gender_f/coverage_at_50`[!is.na(data3_itemknn$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_itemknnlabel <- c(rep('itemknn', length(data3_f_50_coverage_itemknn)))
    data3_f_50_coverage_als <- data3_als$`test/gender_f/coverage_at_50`[!is.na(data3_als$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_alslabel <- c(rep('als', length(data3_f_50_coverage_als)))
    data3_f_50_coverage_bpr <- data3_bpr$`test/gender_f/coverage_at_50`[!is.na(data3_bpr$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_bprlabel <- c(rep('bpr', length(data3_f_50_coverage_bpr)))
    data3_f_50_coverage_slim <- data3_slim$`test/gender_f/coverage_at_50`[!is.na(data3_slim$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_slimlabel <- c(rep('slim', length(data3_f_50_coverage_slim)))
    data3_f_50_coverage_vae <- data3_vae$`test/gender_f/coverage_at_50`[!is.na(data3_vae$`test/gender_f/coverage_at_50`)]
    data3_f_50_coverage_vaelabel <- c(rep('vae', length(data3_f_50_coverage_vae)))
    data_4_f_50_coverage_pop <- data_4_pop$`test/gender_f/coverage_at_50`[!is.na(data_4_pop$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_poplabel <- c(rep('pop', length(data_4_f_50_coverage_pop)))
    data_4_f_50_coverage_itemknn <- data_4_itemknn$`test/gender_f/coverage_at_50`[!is.na(data_4_itemknn$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_itemknnlabel <- c(rep('itemknn', length(data_4_f_50_coverage_itemknn)))
    data_4_f_50_coverage_als <- data_4_als$`test/gender_f/coverage_at_50`[!is.na(data_4_als$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_alslabel <- c(rep('als', length(data_4_f_50_coverage_als)))
    data_4_f_50_coverage_bpr <- data_4_bpr$`test/gender_f/coverage_at_50`[!is.na(data_4_bpr$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_bprlabel <- c(rep('bpr', length(data_4_f_50_coverage_bpr)))
    data_4_f_50_coverage_slim <- data_4_slim$`test/gender_f/coverage_at_50`[!is.na(data_4_slim$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_slimlabel <- c(rep('slim', length(data_4_f_50_coverage_slim)))
    data_4_f_50_coverage_vae <- data_4_vae$`test/gender_f/coverage_at_50`[!is.na(data_4_vae$`test/gender_f/coverage_at_50`)]
    data_4_f_50_coverage_vaelabel <- c(rep('vae', length(data_4_f_50_coverage_vae)))
    # combine data
    coverage_50F <- c(data_0_f_50_coverage_pop, data_0_f_50_coverage_itemknn, data_0_f_50_coverage_als, data_0_f_50_coverage_bpr, data_0_f_50_coverage_slim, data_0_f_50_coverage_vae, data_1_f_50_coverage_pop, data_1_f_50_coverage_itemknn, data_1_f_50_coverage_als, data_1_f_50_coverage_bpr, data_1_f_50_coverage_slim, data_1_f_50_coverage_vae, data_2_f_50_coverage_pop, data_2_f_50_coverage_itemknn, data_2_f_50_coverage_als, data_2_f_50_coverage_bpr, data_2_f_50_coverage_slim, data_2_f_50_coverage_vae, data3_f_50_coverage_pop, data3_f_50_coverage_itemknn, data3_f_50_coverage_als, data3_f_50_coverage_bpr, data3_f_50_coverage_slim, data3_f_50_coverage_vae, data_4_f_50_coverage_pop, data_4_f_50_coverage_itemknn, data_4_f_50_coverage_als, data_4_f_50_coverage_bpr, data_4_f_50_coverage_slim, data_4_f_50_coverage_vae)
    coverage_50labelF <- c(data_0_f_50_coverage_poplabel, data_0_f_50_coverage_itemknnlabel, data_0_f_50_coverage_alslabel, data_0_f_50_coverage_bprlabel, data_0_f_50_coverage_slimlabel, data_0_f_50_coverage_vaelabel, data_1_f_50_coverage_poplabel, data_1_f_50_coverage_itemknnlabel, data_1_f_50_coverage_alslabel, data_1_f_50_coverage_bprlabel, data_1_f_50_coverage_slimlabel, data_1_f_50_coverage_vaelabel, data_2_f_50_coverage_poplabel, data_2_f_50_coverage_itemknnlabel, data_2_f_50_coverage_alslabel, data_2_f_50_coverage_bprlabel, data_2_f_50_coverage_slimlabel, data_2_f_50_coverage_vaelabel, data3_f_50_coverage_poplabel, data3_f_50_coverage_itemknnlabel, data3_f_50_coverage_alslabel, data3_f_50_coverage_bprlabel, data3_f_50_coverage_slimlabel, data3_f_50_coverage_vaelabel, data_4_f_50_coverage_poplabel, data_4_f_50_coverage_itemknnlabel, data_4_f_50_coverage_alslabel, data_4_f_50_coverage_bprlabel, data_4_f_50_coverage_slimlabel, data_4_f_50_coverage_vaelabel)
    ##
    # make dataframe
    coverage_f <- data.frame(coverage_3F, coverage_3labelF, coverage_5F, coverage_5labelF, coverage_10F, coverage_10labelF, coverage_20F, coverage_20labelF, coverage_50F, coverage_50labelF)
    #View(coverage_f)
    ##
    # combine data FM
    coverage_3FM <- c(coverage_3F, coverage_3M)
    coverage_3labelFM <- c(coverage_3labelF, coverage_3labelM)
    coverage_5FM <- c(coverage_5F, coverage_5M)
    coverage_5labelFM <- c(coverage_5labelF, coverage_5labelM)
    coverage_10FM <- c(coverage_10F, coverage_10M)
    coverage_10labelFM <- c(coverage_10labelF, coverage_10labelM)
    coverage_20FM <- c(coverage_20F, coverage_20M)
    coverage_20labelFM <- c(coverage_20labelF, coverage_20labelM)
    coverage_50FM <- c(coverage_50F, coverage_50M)
    coverage_50labelFM <- c(coverage_50labelF, coverage_50labelM)
    # make dataframe FM
    coverage_FM <- data.frame(coverage_3FM, coverage_3labelFM, coverage_5FM, coverage_5labelFM, coverage_10FM, coverage_10labelFM, coverage_20FM, coverage_20labelFM, coverage_50FM, coverage_50labelFM)
    #View(coverage_FM)
    ##
    if(threshold == 3){
      dunn_M <- dunn_test(coverage_3M ~ coverage_3labelM, data=coverage_m, p.adjust.method = "bonferroni")
      dunn_F <- dunn_test(coverage_3F ~ coverage_3labelF, data=coverage_f, p.adjust.method = "bonferroni")
      dunn_FM <- dunn_test(coverage_3FM ~ coverage_3labelFM, data=coverage_FM, p.adjust.method = "bonferroni")
    } else if (threshold == 5){
      dunn_M <- dunn_test(coverage_5M ~ coverage_5labelM, data=coverage_m, p.adjust.method = "bonferroni")
      dunn_F <- dunn_test(coverage_5F ~ coverage_5labelF, data=coverage_f, p.adjust.method = "bonferroni")
      dunn_FM <- dunn_test(coverage_5FM ~ coverage_5labelFM, data=coverage_FM, p.adjust.method = "bonferroni")
    } else if (threshold == 10){
      dunn_M <- dunn_test(coverage_10M ~ coverage_10labelM, data=coverage_m, p.adjust.method = "bonferroni")
      dunn_F <- dunn_test(coverage_10F ~ coverage_10labelF, data=coverage_f, p.adjust.method = "bonferroni")
      dunn_FM <- dunn_test(coverage_10FM ~ coverage_10labelFM, data=coverage_FM, p.adjust.method = "bonferroni")
    } else if (threshold == 20){
      dunn_M <- dunn_test(coverage_20M ~ coverage_20labelM, data=coverage_m, p.adjust.method = "bonferroni")
      dunn_F <- dunn_test(coverage_20F ~ coverage_20labelF, data=coverage_f, p.adjust.method = "bonferroni")
      dunn_FM <- dunn_test(coverage_20FM ~ coverage_20labelFM, data=coverage_FM, p.adjust.method = "bonferroni")
    } else if (threshold == 50){
      dunn_M <- dunn_test(coverage_50M ~ coverage_50labelM, data=coverage_m, p.adjust.method = "bonferroni")
      dunn_F <- dunn_test(coverage_50F ~ coverage_50labelF, data=coverage_f, p.adjust.method = "bonferroni")
      dunn_FM <- dunn_test(coverage_50FM ~ coverage_50labelFM, data=coverage_FM, p.adjust.method = "bonferroni")
    }
    RESULTS <- c(RESULTS, "MALE")
    RESULTS <- get_result(dunn_M, RESULTS)
    RESULTS <- c(RESULTS, "FEMALE")
    RESULTS <- get_result(dunn_F, RESULTS)
    RESULTS <- c(RESULTS, "ALL")
    RESULTS <- get_result(dunn_FM, RESULTS)
  }
}

sink('analysis-output_coverage.txt')
print(RESULTS)
sink('analysis-output_coverage.txt', append=TRUE)
sink()
