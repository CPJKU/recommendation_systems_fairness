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
models <- c("pop", "itemknn", "bpr", "als", "slim", "vae")
RESULTS <- list()

get_p <- function(F_val, M_val, p_d) {
  MannResult <- wilcox.test(M_val, F_val)
  Zstat<-qnorm(MannResult$p.value/2)
  new_row <- data.frame(MannResult$p.value, abs(Zstat)/sqrt(length(F_val + length(M_val))))
  print(new_row)
  names(new_row) <- c("p_val", "effect_size")
  p_d <- rbind(new_row, p_d)
  return <- p_d
}

for (model in models){
  print("Pocessing:")
  print(model)
  RESULTS <- c(RESULTS, model)
  N_data_0 <- read_delim(paste("full_raw_metrics_", model, "N_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_1 <- read_delim(paste("full_raw_metrics_", model, "N_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_2 <- read_delim(paste("full_raw_metrics_", model, "N_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_3 <- read_delim(paste("full_raw_metrics_", model, "N_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_4 <- read_delim(paste("full_raw_metrics_", model, "N_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  
  U_data_0 <- read_delim(paste("full_raw_metrics_", model, "U_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_1 <- read_delim(paste("full_raw_metrics_", model, "U_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_2 <- read_delim(paste("full_raw_metrics_", model, "U_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_3 <- read_delim(paste("full_raw_metrics_", model, "U_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_4 <- read_delim(paste("full_raw_metrics_", model, "U_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  
  #ndcg
  # get labels
  D0_N_3_ndcg <- N_data_0$`test/ndcg_at_3`[!is.na(N_data_0$`test/ndcg_at_3`)]
  D0_U_3_ndcg <- U_data_0$`test/ndcg_at_3`[!is.na(U_data_0$`test/ndcg_at_3`)]
  D0_3_ndcg_label <- c(rep('D0_N_ndcg_3', length(D0_N_3_ndcg)), rep('D0_U_ndcg_3', length(D0_U_3_ndcg)))
  D1_N_3_ndcg <- N_data_1$`test/ndcg_at_3`[!is.na(N_data_1$`test/ndcg_at_3`)]
  D1_U_3_ndcg <- U_data_1$`test/ndcg_at_3`[!is.na(U_data_1$`test/ndcg_at_3`)]
  D1_3_ndcg_label <- c(rep('D1_N_ndcg_3', length(D1_N_3_ndcg)), rep('D1_U_ndcg_3', length(D1_U_3_ndcg)))
  D2_N_3_ndcg <- N_data_2$`test/ndcg_at_3`[!is.na(N_data_2$`test/ndcg_at_3`)]
  D2_U_3_ndcg <- U_data_2$`test/ndcg_at_3`[!is.na(U_data_2$`test/ndcg_at_3`)]
  D2_3_ndcg_label <- c(rep('D2_N_ndcg_3', length(D2_N_3_ndcg)), rep('D2_U_ndcg_3', length(D2_U_3_ndcg)))
  D3_N_3_ndcg <- N_data_3$`test/ndcg_at_3`[!is.na(N_data_3$`test/ndcg_at_3`)]
  D3_U_3_ndcg <- U_data_3$`test/ndcg_at_3`[!is.na(U_data_3$`test/ndcg_at_3`)]
  D3_3_ndcg_label <- c(rep('D3_N_ndcg_3', length(D3_N_3_ndcg)), rep('D3_U_ndcg_3', length(D3_U_3_ndcg)))
  D4_N_3_ndcg <- N_data_4$`test/ndcg_at_3`[!is.na(N_data_4$`test/ndcg_at_3`)]
  D4_U_3_ndcg <- U_data_4$`test/ndcg_at_3`[!is.na(U_data_4$`test/ndcg_at_3`)]
  D4_3_ndcg_label <- c(rep('D4_N_ndcg_3', length(D4_N_3_ndcg)), rep('D4_U_ndcg_3', length(D4_U_3_ndcg)))
  # combine data
  ndcg_3 <- c(D0_N_3_ndcg, D0_U_3_ndcg, D1_N_3_ndcg, D1_U_3_ndcg, D2_N_3_ndcg, D2_U_3_ndcg, D3_N_3_ndcg, D3_U_3_ndcg, D4_N_3_ndcg, D4_U_3_ndcg)
  ndcg_3_label <- c(D0_3_ndcg_label, D1_3_ndcg_label, D2_3_ndcg_label, D3_3_ndcg_label, D4_3_ndcg_label)
  
  D0_N_5_ndcg <- N_data_0$`test/ndcg_at_5`[!is.na(N_data_0$`test/ndcg_at_5`)]
  D0_U_5_ndcg <- U_data_0$`test/ndcg_at_5`[!is.na(U_data_0$`test/ndcg_at_5`)]
  D0_5_ndcg_label <- c(rep('D0_N_ndcg_5', length(D0_N_5_ndcg)), rep('D0_U_ndcg_5', length(D0_U_5_ndcg)))
  D1_N_5_ndcg <- N_data_1$`test/ndcg_at_5`[!is.na(N_data_1$`test/ndcg_at_5`)]
  D1_U_5_ndcg <- U_data_1$`test/ndcg_at_5`[!is.na(U_data_1$`test/ndcg_at_5`)]
  D1_5_ndcg_label <- c(rep('D1_N_ndcg_5', length(D1_N_5_ndcg)), rep('D1_U_ndcg_5', length(D1_U_5_ndcg)))
  D2_N_5_ndcg <- N_data_2$`test/ndcg_at_5`[!is.na(N_data_2$`test/ndcg_at_5`)]
  D2_U_5_ndcg <- U_data_2$`test/ndcg_at_5`[!is.na(U_data_2$`test/ndcg_at_5`)]
  D2_5_ndcg_label <- c(rep('D2_N_ndcg_5', length(D2_N_5_ndcg)), rep('D2_U_ndcg_5', length(D2_U_5_ndcg)))
  D3_N_5_ndcg <- N_data_3$`test/ndcg_at_5`[!is.na(N_data_3$`test/ndcg_at_5`)]
  D3_U_5_ndcg <- U_data_3$`test/ndcg_at_5`[!is.na(U_data_3$`test/ndcg_at_5`)]
  D3_5_ndcg_label <- c(rep('D3_N_ndcg_5', length(D3_N_5_ndcg)), rep('D3_U_ndcg_5', length(D3_U_5_ndcg)))
  D4_N_5_ndcg <- N_data_4$`test/ndcg_at_5`[!is.na(N_data_4$`test/ndcg_at_5`)]
  D4_U_5_ndcg <- U_data_4$`test/ndcg_at_5`[!is.na(U_data_4$`test/ndcg_at_5`)]
  D4_5_ndcg_label <- c(rep('D4_N_ndcg_5', length(D4_N_5_ndcg)), rep('D4_U_ndcg_5', length(D4_U_5_ndcg)))
  # combine data
  ndcg_5 <- c(D0_N_5_ndcg, D0_U_5_ndcg, D1_N_5_ndcg, D1_U_5_ndcg, D2_N_5_ndcg, D2_U_5_ndcg, D3_N_5_ndcg, D3_U_5_ndcg, D4_N_5_ndcg, D4_U_5_ndcg)
  ndcg_5_label <- c(D0_5_ndcg_label, D1_5_ndcg_label, D2_5_ndcg_label, D3_5_ndcg_label, D4_5_ndcg_label)
  
  D0_N_10_ndcg <- N_data_0$`test/ndcg_at_10`[!is.na(N_data_0$`test/ndcg_at_10`)]
  D0_U_10_ndcg <- U_data_0$`test/ndcg_at_10`[!is.na(U_data_0$`test/ndcg_at_10`)]
  D0_10_ndcg_label <- c(rep('D0_N_ndcg_10', length(D0_N_10_ndcg)), rep('D0_U_ndcg_10', length(D0_U_10_ndcg)))
  D1_N_10_ndcg <- N_data_1$`test/ndcg_at_10`[!is.na(N_data_1$`test/ndcg_at_10`)]
  D1_U_10_ndcg <- U_data_1$`test/ndcg_at_10`[!is.na(U_data_1$`test/ndcg_at_10`)]
  D1_10_ndcg_label <- c(rep('D1_N_ndcg_10', length(D1_N_10_ndcg)), rep('D1_U_ndcg_10', length(D1_U_10_ndcg)))
  D2_N_10_ndcg <- N_data_2$`test/ndcg_at_10`[!is.na(N_data_2$`test/ndcg_at_10`)]
  D2_U_10_ndcg <- U_data_2$`test/ndcg_at_10`[!is.na(U_data_2$`test/ndcg_at_10`)]
  D2_10_ndcg_label <- c(rep('D2_N_ndcg_10', length(D2_N_10_ndcg)), rep('D2_U_ndcg_10', length(D2_U_10_ndcg)))
  D3_N_10_ndcg <- N_data_3$`test/ndcg_at_10`[!is.na(N_data_3$`test/ndcg_at_10`)]
  D3_U_10_ndcg <- U_data_3$`test/ndcg_at_10`[!is.na(U_data_3$`test/ndcg_at_10`)]
  D3_10_ndcg_label <- c(rep('D3_N_ndcg_10', length(D3_N_10_ndcg)), rep('D3_U_ndcg_10', length(D3_U_10_ndcg)))
  D4_N_10_ndcg <- N_data_4$`test/ndcg_at_10`[!is.na(N_data_4$`test/ndcg_at_10`)]
  D4_U_10_ndcg <- U_data_4$`test/ndcg_at_10`[!is.na(U_data_4$`test/ndcg_at_10`)]
  D4_10_ndcg_label <- c(rep('D4_N_ndcg_10', length(D4_N_10_ndcg)), rep('D4_U_ndcg_10', length(D4_U_10_ndcg)))
  # combine data
  ndcg_10 <- c(D0_N_10_ndcg, D0_U_10_ndcg, D1_N_10_ndcg, D1_U_10_ndcg, D2_N_10_ndcg, D2_U_10_ndcg, D3_N_10_ndcg, D3_U_10_ndcg, D4_N_10_ndcg, D4_U_10_ndcg)
  ndcg_10_label <- c(D0_10_ndcg_label, D1_10_ndcg_label, D2_10_ndcg_label, D3_10_ndcg_label, D4_10_ndcg_label)
  
  D0_N_20_ndcg <- N_data_0$`test/ndcg_at_20`[!is.na(N_data_0$`test/ndcg_at_20`)]
  D0_U_20_ndcg <- U_data_0$`test/ndcg_at_20`[!is.na(U_data_0$`test/ndcg_at_20`)]
  D0_20_ndcg_label <- c(rep('D0_N_ndcg_20', length(D0_N_20_ndcg)), rep('D0_U_ndcg_20', length(D0_U_20_ndcg)))
  D1_N_20_ndcg <- N_data_1$`test/ndcg_at_20`[!is.na(N_data_1$`test/ndcg_at_20`)]
  D1_U_20_ndcg <- U_data_1$`test/ndcg_at_20`[!is.na(U_data_1$`test/ndcg_at_20`)]
  D1_20_ndcg_label <- c(rep('D1_N_ndcg_20', length(D1_N_20_ndcg)), rep('D1_U_ndcg_20', length(D1_U_20_ndcg)))
  D2_N_20_ndcg <- N_data_2$`test/ndcg_at_20`[!is.na(N_data_2$`test/ndcg_at_20`)]
  D2_U_20_ndcg <- U_data_2$`test/ndcg_at_20`[!is.na(U_data_2$`test/ndcg_at_20`)]
  D2_20_ndcg_label <- c(rep('D2_N_ndcg_20', length(D2_N_20_ndcg)), rep('D2_U_ndcg_20', length(D2_U_20_ndcg)))
  D3_N_20_ndcg <- N_data_3$`test/ndcg_at_20`[!is.na(N_data_3$`test/ndcg_at_20`)]
  D3_U_20_ndcg <- U_data_3$`test/ndcg_at_20`[!is.na(U_data_3$`test/ndcg_at_20`)]
  D3_20_ndcg_label <- c(rep('D3_N_ndcg_20', length(D3_N_20_ndcg)), rep('D3_U_ndcg_20', length(D3_U_20_ndcg)))
  D4_N_20_ndcg <- N_data_4$`test/ndcg_at_20`[!is.na(N_data_4$`test/ndcg_at_20`)]
  D4_U_20_ndcg <- U_data_4$`test/ndcg_at_20`[!is.na(U_data_4$`test/ndcg_at_20`)]
  D4_20_ndcg_label <- c(rep('D4_N_ndcg_20', length(D4_N_20_ndcg)), rep('D4_U_ndcg_20', length(D4_U_20_ndcg)))
  # combine data
  ndcg_20 <- c(D0_N_20_ndcg, D0_U_20_ndcg, D1_N_20_ndcg, D1_U_20_ndcg, D2_N_20_ndcg, D2_U_20_ndcg, D3_N_20_ndcg, D3_U_20_ndcg, D4_N_20_ndcg, D4_U_20_ndcg)
  ndcg_20_label <- c(D0_20_ndcg_label, D1_20_ndcg_label, D2_20_ndcg_label, D3_20_ndcg_label, D4_20_ndcg_label)
  
  D0_N_50_ndcg <- N_data_0$`test/ndcg_at_50`[!is.na(N_data_0$`test/ndcg_at_50`)]
  D0_U_50_ndcg <- U_data_0$`test/ndcg_at_50`[!is.na(U_data_0$`test/ndcg_at_50`)]
  D0_50_ndcg_label <- c(rep('D0_N_ndcg_50', length(D0_N_50_ndcg)), rep('D0_U_ndcg_50', length(D0_U_50_ndcg)))
  D1_N_50_ndcg <- N_data_1$`test/ndcg_at_50`[!is.na(N_data_1$`test/ndcg_at_50`)]
  D1_U_50_ndcg <- U_data_1$`test/ndcg_at_50`[!is.na(U_data_1$`test/ndcg_at_50`)]
  D1_50_ndcg_label <- c(rep('D1_N_ndcg_50', length(D1_N_50_ndcg)), rep('D1_U_ndcg_50', length(D1_U_50_ndcg)))
  D2_N_50_ndcg <- N_data_2$`test/ndcg_at_50`[!is.na(N_data_2$`test/ndcg_at_50`)]
  D2_U_50_ndcg <- U_data_2$`test/ndcg_at_50`[!is.na(U_data_2$`test/ndcg_at_50`)]
  D2_50_ndcg_label <- c(rep('D2_N_ndcg_50', length(D2_N_50_ndcg)), rep('D2_U_ndcg_50', length(D2_U_50_ndcg)))
  D3_N_50_ndcg <- N_data_3$`test/ndcg_at_50`[!is.na(N_data_3$`test/ndcg_at_50`)]
  D3_U_50_ndcg <- U_data_3$`test/ndcg_at_50`[!is.na(U_data_3$`test/ndcg_at_50`)]
  D3_50_ndcg_label <- c(rep('D3_N_ndcg_50', length(D3_N_50_ndcg)), rep('D3_U_ndcg_50', length(D3_U_50_ndcg)))
  D4_N_50_ndcg <- N_data_4$`test/ndcg_at_50`[!is.na(N_data_4$`test/ndcg_at_50`)]
  D4_U_50_ndcg <- U_data_4$`test/ndcg_at_50`[!is.na(U_data_4$`test/ndcg_at_50`)]
  D4_50_ndcg_label <- c(rep('D4_N_ndcg_50', length(D4_N_50_ndcg)), rep('D4_U_ndcg_50', length(D4_U_50_ndcg)))
  # combine data
  ndcg_50 <- c(D0_N_50_ndcg, D0_U_50_ndcg, D1_N_50_ndcg, D1_U_50_ndcg, D2_N_50_ndcg, D2_U_50_ndcg, D3_N_50_ndcg, D3_U_50_ndcg, D4_N_50_ndcg, D4_U_50_ndcg)
  ndcg_50_label <- c(D0_50_ndcg_label, D1_50_ndcg_label, D2_50_ndcg_label, D3_50_ndcg_label, D4_50_ndcg_label)
  # make dataframe
  ndcg_ALL <- data.frame(ndcg_3, ndcg_3_label, ndcg_5, ndcg_5_label, ndcg_10, ndcg_10_label, ndcg_20, ndcg_20_label, ndcg_50, ndcg_50_label)
  #View(ndcg_ALL)
  
  #recall
  # get labels
  D0_N_3_recall <- N_data_0$`test/recall_at_3`[!is.na(N_data_0$`test/recall_at_3`)]
  D0_U_3_recall <- U_data_0$`test/recall_at_3`[!is.na(U_data_0$`test/recall_at_3`)]
  D0_3_recall_label <- c(rep('D0_N_recall_3', length(D0_N_3_recall)), rep('D0_U_recall_3', length(D0_U_3_recall)))
  D1_N_3_recall <- N_data_1$`test/recall_at_3`[!is.na(N_data_1$`test/recall_at_3`)]
  D1_U_3_recall <- U_data_1$`test/recall_at_3`[!is.na(U_data_1$`test/recall_at_3`)]
  D1_3_recall_label <- c(rep('D1_N_recall_3', length(D1_N_3_recall)), rep('D1_U_recall_3', length(D1_U_3_recall)))
  D2_N_3_recall <- N_data_2$`test/recall_at_3`[!is.na(N_data_2$`test/recall_at_3`)]
  D2_U_3_recall <- U_data_2$`test/recall_at_3`[!is.na(U_data_2$`test/recall_at_3`)]
  D2_3_recall_label <- c(rep('D2_N_recall_3', length(D2_N_3_recall)), rep('D2_U_recall_3', length(D2_U_3_recall)))
  D3_N_3_recall <- N_data_3$`test/recall_at_3`[!is.na(N_data_3$`test/recall_at_3`)]
  D3_U_3_recall <- U_data_3$`test/recall_at_3`[!is.na(U_data_3$`test/recall_at_3`)]
  D3_3_recall_label <- c(rep('D3_N_recall_3', length(D3_N_3_recall)), rep('D3_U_recall_3', length(D3_U_3_recall)))
  D4_N_3_recall <- N_data_4$`test/recall_at_3`[!is.na(N_data_4$`test/recall_at_3`)]
  D4_U_3_recall <- U_data_4$`test/recall_at_3`[!is.na(U_data_4$`test/recall_at_3`)]
  D4_3_recall_label <- c(rep('D4_N_recall_3', length(D4_N_3_recall)), rep('D4_U_recall_3', length(D4_U_3_recall)))
  # combine data
  recall_3 <- c(D0_N_3_recall, D0_U_3_recall, D1_N_3_recall, D1_U_3_recall, D2_N_3_recall, D2_U_3_recall, D3_N_3_recall, D3_U_3_recall, D4_N_3_recall, D4_U_3_recall)
  recall_3_label <- c(D0_3_recall_label, D1_3_recall_label, D2_3_recall_label, D3_3_recall_label, D4_3_recall_label)
  
  D0_N_5_recall <- N_data_0$`test/recall_at_5`[!is.na(N_data_0$`test/recall_at_5`)]
  D0_U_5_recall <- U_data_0$`test/recall_at_5`[!is.na(U_data_0$`test/recall_at_5`)]
  D0_5_recall_label <- c(rep('D0_N_recall_5', length(D0_N_5_recall)), rep('D0_U_recall_5', length(D0_U_5_recall)))
  D1_N_5_recall <- N_data_1$`test/recall_at_5`[!is.na(N_data_1$`test/recall_at_5`)]
  D1_U_5_recall <- U_data_1$`test/recall_at_5`[!is.na(U_data_1$`test/recall_at_5`)]
  D1_5_recall_label <- c(rep('D1_N_recall_5', length(D1_N_5_recall)), rep('D1_U_recall_5', length(D1_U_5_recall)))
  D2_N_5_recall <- N_data_2$`test/recall_at_5`[!is.na(N_data_2$`test/recall_at_5`)]
  D2_U_5_recall <- U_data_2$`test/recall_at_5`[!is.na(U_data_2$`test/recall_at_5`)]
  D2_5_recall_label <- c(rep('D2_N_recall_5', length(D2_N_5_recall)), rep('D2_U_recall_5', length(D2_U_5_recall)))
  D3_N_5_recall <- N_data_3$`test/recall_at_5`[!is.na(N_data_3$`test/recall_at_5`)]
  D3_U_5_recall <- U_data_3$`test/recall_at_5`[!is.na(U_data_3$`test/recall_at_5`)]
  D3_5_recall_label <- c(rep('D3_N_recall_5', length(D3_N_5_recall)), rep('D3_U_recall_5', length(D3_U_5_recall)))
  D4_N_5_recall <- N_data_4$`test/recall_at_5`[!is.na(N_data_4$`test/recall_at_5`)]
  D4_U_5_recall <- U_data_4$`test/recall_at_5`[!is.na(U_data_4$`test/recall_at_5`)]
  D4_5_recall_label <- c(rep('D4_N_recall_5', length(D4_N_5_recall)), rep('D4_U_recall_5', length(D4_U_5_recall)))
  # combine data
  recall_5 <- c(D0_N_5_recall, D0_U_5_recall, D1_N_5_recall, D1_U_5_recall, D2_N_5_recall, D2_U_5_recall, D3_N_5_recall, D3_U_5_recall, D4_N_5_recall, D4_U_5_recall)
  recall_5_label <- c(D0_5_recall_label, D1_5_recall_label, D2_5_recall_label, D3_5_recall_label, D4_5_recall_label)
  
  D0_N_10_recall <- N_data_0$`test/recall_at_10`[!is.na(N_data_0$`test/recall_at_10`)]
  D0_U_10_recall <- U_data_0$`test/recall_at_10`[!is.na(U_data_0$`test/recall_at_10`)]
  D0_10_recall_label <- c(rep('D0_N_recall_10', length(D0_N_10_recall)), rep('D0_U_recall_10', length(D0_U_10_recall)))
  D1_N_10_recall <- N_data_1$`test/recall_at_10`[!is.na(N_data_1$`test/recall_at_10`)]
  D1_U_10_recall <- U_data_1$`test/recall_at_10`[!is.na(U_data_1$`test/recall_at_10`)]
  D1_10_recall_label <- c(rep('D1_N_recall_10', length(D1_N_10_recall)), rep('D1_U_recall_10', length(D1_U_10_recall)))
  D2_N_10_recall <- N_data_2$`test/recall_at_10`[!is.na(N_data_2$`test/recall_at_10`)]
  D2_U_10_recall <- U_data_2$`test/recall_at_10`[!is.na(U_data_2$`test/recall_at_10`)]
  D2_10_recall_label <- c(rep('D2_N_recall_10', length(D2_N_10_recall)), rep('D2_U_recall_10', length(D2_U_10_recall)))
  D3_N_10_recall <- N_data_3$`test/recall_at_10`[!is.na(N_data_3$`test/recall_at_10`)]
  D3_U_10_recall <- U_data_3$`test/recall_at_10`[!is.na(U_data_3$`test/recall_at_10`)]
  D3_10_recall_label <- c(rep('D3_N_recall_10', length(D3_N_10_recall)), rep('D3_U_recall_10', length(D3_U_10_recall)))
  D4_N_10_recall <- N_data_4$`test/recall_at_10`[!is.na(N_data_4$`test/recall_at_10`)]
  D4_U_10_recall <- U_data_4$`test/recall_at_10`[!is.na(U_data_4$`test/recall_at_10`)]
  D4_10_recall_label <- c(rep('D4_N_recall_10', length(D4_N_10_recall)), rep('D4_U_recall_10', length(D4_U_10_recall)))
  # combine data
  recall_10 <- c(D0_N_10_recall, D0_U_10_recall, D1_N_10_recall, D1_U_10_recall, D2_N_10_recall, D2_U_10_recall, D3_N_10_recall, D3_U_10_recall, D4_N_10_recall, D4_U_10_recall)
  recall_10_label <- c(D0_10_recall_label, D1_10_recall_label, D2_10_recall_label, D3_10_recall_label, D4_10_recall_label)
  
  D0_N_20_recall <- N_data_0$`test/recall_at_20`[!is.na(N_data_0$`test/recall_at_20`)]
  D0_U_20_recall <- U_data_0$`test/recall_at_20`[!is.na(U_data_0$`test/recall_at_20`)]
  D0_20_recall_label <- c(rep('D0_N_recall_20', length(D0_N_20_recall)), rep('D0_U_recall_20', length(D0_U_20_recall)))
  D1_N_20_recall <- N_data_1$`test/recall_at_20`[!is.na(N_data_1$`test/recall_at_20`)]
  D1_U_20_recall <- U_data_1$`test/recall_at_20`[!is.na(U_data_1$`test/recall_at_20`)]
  D1_20_recall_label <- c(rep('D1_N_recall_20', length(D1_N_20_recall)), rep('D1_U_recall_20', length(D1_U_20_recall)))
  D2_N_20_recall <- N_data_2$`test/recall_at_20`[!is.na(N_data_2$`test/recall_at_20`)]
  D2_U_20_recall <- U_data_2$`test/recall_at_20`[!is.na(U_data_2$`test/recall_at_20`)]
  D2_20_recall_label <- c(rep('D2_N_recall_20', length(D2_N_20_recall)), rep('D2_U_recall_20', length(D2_U_20_recall)))
  D3_N_20_recall <- N_data_3$`test/recall_at_20`[!is.na(N_data_3$`test/recall_at_20`)]
  D3_U_20_recall <- U_data_3$`test/recall_at_20`[!is.na(U_data_3$`test/recall_at_20`)]
  D3_20_recall_label <- c(rep('D3_N_recall_20', length(D3_N_20_recall)), rep('D3_U_recall_20', length(D3_U_20_recall)))
  D4_N_20_recall <- N_data_4$`test/recall_at_20`[!is.na(N_data_4$`test/recall_at_20`)]
  D4_U_20_recall <- U_data_4$`test/recall_at_20`[!is.na(U_data_4$`test/recall_at_20`)]
  D4_20_recall_label <- c(rep('D4_N_recall_20', length(D4_N_20_recall)), rep('D4_U_recall_20', length(D4_U_20_recall)))
  # combine data
  recall_20 <- c(D0_N_20_recall, D0_U_20_recall, D1_N_20_recall, D1_U_20_recall, D2_N_20_recall, D2_U_20_recall, D3_N_20_recall, D3_U_20_recall, D4_N_20_recall, D4_U_20_recall)
  recall_20_label <- c(D0_20_recall_label, D1_20_recall_label, D2_20_recall_label, D3_20_recall_label, D4_20_recall_label)
  
  D0_N_50_recall <- N_data_0$`test/recall_at_50`[!is.na(N_data_0$`test/recall_at_50`)]
  D0_U_50_recall <- U_data_0$`test/recall_at_50`[!is.na(U_data_0$`test/recall_at_50`)]
  D0_50_recall_label <- c(rep('D0_N_recall_50', length(D0_N_50_recall)), rep('D0_U_recall_50', length(D0_U_50_recall)))
  D1_N_50_recall <- N_data_1$`test/recall_at_50`[!is.na(N_data_1$`test/recall_at_50`)]
  D1_U_50_recall <- U_data_1$`test/recall_at_50`[!is.na(U_data_1$`test/recall_at_50`)]
  D1_50_recall_label <- c(rep('D1_N_recall_50', length(D1_N_50_recall)), rep('D1_U_recall_50', length(D1_U_50_recall)))
  D2_N_50_recall <- N_data_2$`test/recall_at_50`[!is.na(N_data_2$`test/recall_at_50`)]
  D2_U_50_recall <- U_data_2$`test/recall_at_50`[!is.na(U_data_2$`test/recall_at_50`)]
  D2_50_recall_label <- c(rep('D2_N_recall_50', length(D2_N_50_recall)), rep('D2_U_recall_50', length(D2_U_50_recall)))
  D3_N_50_recall <- N_data_3$`test/recall_at_50`[!is.na(N_data_3$`test/recall_at_50`)]
  D3_U_50_recall <- U_data_3$`test/recall_at_50`[!is.na(U_data_3$`test/recall_at_50`)]
  D3_50_recall_label <- c(rep('D3_N_recall_50', length(D3_N_50_recall)), rep('D3_U_recall_50', length(D3_U_50_recall)))
  D4_N_50_recall <- N_data_4$`test/recall_at_50`[!is.na(N_data_4$`test/recall_at_50`)]
  D4_U_50_recall <- U_data_4$`test/recall_at_50`[!is.na(U_data_4$`test/recall_at_50`)]
  D4_50_recall_label <- c(rep('D4_N_recall_50', length(D4_N_50_recall)), rep('D4_U_recall_50', length(D4_U_50_recall)))
  # combine data
  recall_50 <- c(D0_N_50_recall, D0_U_50_recall, D1_N_50_recall, D1_U_50_recall, D2_N_50_recall, D2_U_50_recall, D3_N_50_recall, D3_U_50_recall, D4_N_50_recall, D4_U_50_recall)
  recall_50_label <- c(D0_50_recall_label, D1_50_recall_label, D2_50_recall_label, D3_50_recall_label, D4_50_recall_label)
  # make dataframe
  recall_ALL <- data.frame(recall_3, recall_3_label, recall_5, recall_5_label, recall_10, recall_10_label, recall_20, recall_20_label, recall_50, recall_50_label)
  print("DONE!")
  
  
  # choose configuration: feature (ndcg, recall); threshold (3, 5, 10, 20, 50)
  metrics = c("ndcg", "recall")
  #thresholds = c(3, 5, 10, 20, 50)
  thresholds = c(10)
  for (threshold in thresholds){
    print(threshold)
    RESULTS <- c(RESULTS, threshold)
    for (metric in metrics){
      if (metric == "ndcg"){
        p_d <- data.frame(p_val = numeric(0), effect_size = numeric(0))
        if (threshold == 3){
          p_d <- get_p(D0_N_3_ndcg, D0_U_3_ndcg, p_d)
          p_d <- get_p(D1_N_3_ndcg, D1_U_3_ndcg, p_d)
          p_d <- get_p(D2_N_3_ndcg, D2_U_3_ndcg, p_d)
          p_d <- get_p(D3_N_3_ndcg, D3_U_3_ndcg, p_d)
          p_d <- get_p(D4_N_3_ndcg, D4_U_3_ndcg, p_d)
        } else if (threshold == 5){
          p_d <- get_p(D0_N_5_ndcg, D0_U_5_ndcg, p_d)
          p_d <- get_p(D1_N_5_ndcg, D1_U_5_ndcg, p_d)
          p_d <- get_p(D2_N_5_ndcg, D2_U_5_ndcg, p_d)
          p_d <- get_p(D3_N_5_ndcg, D3_U_5_ndcg, p_d)
          p_d <- get_p(D4_N_5_ndcg, D4_U_5_ndcg, p_d)
        } else if (threshold == 10){
          p_d <- get_p(D0_N_10_ndcg, D0_U_10_ndcg, p_d)
          p_d <- get_p(D1_N_10_ndcg, D1_U_10_ndcg, p_d)
          p_d <- get_p(D2_N_10_ndcg, D2_U_10_ndcg, p_d)
          p_d <- get_p(D3_N_10_ndcg, D3_U_10_ndcg, p_d)
          p_d <- get_p(D4_N_10_ndcg, D4_U_10_ndcg, p_d)
        } else if (threshold == 20){
          p_d <- get_p(D0_N_20_ndcg, D0_U_20_ndcg, p_d)
          p_d <- get_p(D1_N_20_ndcg, D1_U_20_ndcg, p_d)
          p_d <- get_p(D2_N_20_ndcg, D2_U_20_ndcg, p_d)
          p_d <- get_p(D3_N_20_ndcg, D3_U_20_ndcg, p_d)
          p_d <- get_p(D4_N_20_ndcg, D4_U_20_ndcg, p_d)
        } else if (threshold == 50){
          p_d <- get_p(D0_N_50_ndcg, D0_U_50_ndcg, p_d)
          p_d <- get_p(D1_N_50_ndcg, D1_U_50_ndcg, p_d)
          p_d <- get_p(D2_N_50_ndcg, D2_U_50_ndcg, p_d)
          p_d <- get_p(D3_N_50_ndcg, D3_U_50_ndcg, p_d)
          p_d <- get_p(D4_N_50_ndcg, D4_U_50_ndcg, p_d)
        }
      } else {
        p_d <- data.frame(p_val = numeric(0), effect_size = numeric(0))
        if (threshold == 3){
          p_d <- get_p(D0_N_3_recall, D0_U_3_recall, p_d)
          p_d <- get_p(D1_N_3_recall, D1_U_3_recall, p_d)
          p_d <- get_p(D2_N_3_recall, D2_U_3_recall, p_d)
          p_d <- get_p(D3_N_3_recall, D3_U_3_recall, p_d)
          p_d <- get_p(D4_N_3_recall, D4_U_3_recall, p_d)
        } else if (threshold == 5){
          p_d <- get_p(D0_N_5_recall, D0_U_5_recall, p_d)
          p_d <- get_p(D1_N_5_recall, D1_U_5_recall, p_d)
          p_d <- get_p(D2_N_5_recall, D2_U_5_recall, p_d)
          p_d <- get_p(D3_N_5_recall, D3_U_5_recall, p_d)
          p_d <- get_p(D4_N_5_recall, D4_U_5_recall, p_d)
        } else if (threshold == 10){
          p_d <- get_p(D0_N_10_recall, D0_U_10_recall, p_d)
          p_d <- get_p(D1_N_10_recall, D1_U_10_recall, p_d)
          p_d <- get_p(D2_N_10_recall, D2_U_10_recall, p_d)
          p_d <- get_p(D3_N_10_recall, D3_U_10_recall, p_d)
          p_d <- get_p(D4_N_10_recall, D4_U_10_recall, p_d)
        } else if (threshold == 20){
          p_d <- get_p(D0_N_20_recall, D0_U_20_recall, p_d)
          p_d <- get_p(D1_N_20_recall, D1_U_20_recall, p_d)
          p_d <- get_p(D2_N_20_recall, D2_U_20_recall, p_d)
          p_d <- get_p(D3_N_20_recall, D3_U_20_recall, p_d)
          p_d <- get_p(D4_N_20_recall, D4_U_20_recall, p_d)
        } else if (threshold == 50){
          p_d <- get_p(D0_N_50_recall, D0_U_50_recall, p_d)
          p_d <- get_p(D1_N_50_recall, D1_U_50_recall, p_d)
          p_d <- get_p(D2_N_50_recall, D2_U_50_recall, p_d)
          p_d <- get_p(D3_N_50_recall, D3_U_50_recall, p_d)
          p_d <- get_p(D4_N_50_recall, D4_U_50_recall, p_d)
        }
      }
      ##########################
      #   PERFORM STATISTICS   #
      ##########################
      p_values <- as.list(p_d$p_val)
      print(metric)
      RESULTS <- c(RESULTS, metric)
      z <- combinePValues(p_values, method="z", weights=1:5)
      print(z)
      RESULTS <- c(RESULTS, z)
    }
  }
}


for (elem in RESULTS){
  print(elem)  
} 

