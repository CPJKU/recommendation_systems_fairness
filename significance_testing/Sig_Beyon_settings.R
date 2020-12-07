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
  N_data_0 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "N_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_1 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "N_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_2 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "N_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_3 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "N_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  N_data_4 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "N_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  
  U_data_0 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "U_0.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_1 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "U_1.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_2 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "U_2.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_3 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "U_3.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  U_data_4 <- read_delim(paste("full_raw_metrics_beyond_accuracy_", model, "U_4.csv", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
  
  #diversity
  # get labels
  D0_N_3_diversity <- N_data_0$`test/diversity_at_3`[!is.na(N_data_0$`test/diversity_at_3`)]
  D0_U_3_diversity <- U_data_0$`test/diversity_at_3`[!is.na(U_data_0$`test/diversity_at_3`)]
  D0_3_diversity_label <- c(rep('D0_N_diversity_3', length(D0_N_3_diversity)), rep('D0_U_diversity_3', length(D0_U_3_diversity)))
  D1_N_3_diversity <- N_data_1$`test/diversity_at_3`[!is.na(N_data_1$`test/diversity_at_3`)]
  D1_U_3_diversity <- U_data_1$`test/diversity_at_3`[!is.na(U_data_1$`test/diversity_at_3`)]
  D1_3_diversity_label <- c(rep('D1_N_diversity_3', length(D1_N_3_diversity)), rep('D1_U_diversity_3', length(D1_U_3_diversity)))
  D2_N_3_diversity <- N_data_2$`test/diversity_at_3`[!is.na(N_data_2$`test/diversity_at_3`)]
  D2_U_3_diversity <- U_data_2$`test/diversity_at_3`[!is.na(U_data_2$`test/diversity_at_3`)]
  D2_3_diversity_label <- c(rep('D2_N_diversity_3', length(D2_N_3_diversity)), rep('D2_U_diversity_3', length(D2_U_3_diversity)))
  D3_N_3_diversity <- N_data_3$`test/diversity_at_3`[!is.na(N_data_3$`test/diversity_at_3`)]
  D3_U_3_diversity <- U_data_3$`test/diversity_at_3`[!is.na(U_data_3$`test/diversity_at_3`)]
  D3_3_diversity_label <- c(rep('D3_N_diversity_3', length(D3_N_3_diversity)), rep('D3_U_diversity_3', length(D3_U_3_diversity)))
  D4_N_3_diversity <- N_data_4$`test/diversity_at_3`[!is.na(N_data_4$`test/diversity_at_3`)]
  D4_U_3_diversity <- U_data_4$`test/diversity_at_3`[!is.na(U_data_4$`test/diversity_at_3`)]
  D4_3_diversity_label <- c(rep('D4_N_diversity_3', length(D4_N_3_diversity)), rep('D4_U_diversity_3', length(D4_U_3_diversity)))
  # combine data
  diversity_3 <- c(D0_N_3_diversity, D0_U_3_diversity, D1_N_3_diversity, D1_U_3_diversity, D2_N_3_diversity, D2_U_3_diversity, D3_N_3_diversity, D3_U_3_diversity, D4_N_3_diversity, D4_U_3_diversity)
  diversity_3_label <- c(D0_3_diversity_label, D1_3_diversity_label, D2_3_diversity_label, D3_3_diversity_label, D4_3_diversity_label)
  
  D0_N_5_diversity <- N_data_0$`test/diversity_at_5`[!is.na(N_data_0$`test/diversity_at_5`)]
  D0_U_5_diversity <- U_data_0$`test/diversity_at_5`[!is.na(U_data_0$`test/diversity_at_5`)]
  D0_5_diversity_label <- c(rep('D0_N_diversity_5', length(D0_N_5_diversity)), rep('D0_U_diversity_5', length(D0_U_5_diversity)))
  D1_N_5_diversity <- N_data_1$`test/diversity_at_5`[!is.na(N_data_1$`test/diversity_at_5`)]
  D1_U_5_diversity <- U_data_1$`test/diversity_at_5`[!is.na(U_data_1$`test/diversity_at_5`)]
  D1_5_diversity_label <- c(rep('D1_N_diversity_5', length(D1_N_5_diversity)), rep('D1_U_diversity_5', length(D1_U_5_diversity)))
  D2_N_5_diversity <- N_data_2$`test/diversity_at_5`[!is.na(N_data_2$`test/diversity_at_5`)]
  D2_U_5_diversity <- U_data_2$`test/diversity_at_5`[!is.na(U_data_2$`test/diversity_at_5`)]
  D2_5_diversity_label <- c(rep('D2_N_diversity_5', length(D2_N_5_diversity)), rep('D2_U_diversity_5', length(D2_U_5_diversity)))
  D3_N_5_diversity <- N_data_3$`test/diversity_at_5`[!is.na(N_data_3$`test/diversity_at_5`)]
  D3_U_5_diversity <- U_data_3$`test/diversity_at_5`[!is.na(U_data_3$`test/diversity_at_5`)]
  D3_5_diversity_label <- c(rep('D3_N_diversity_5', length(D3_N_5_diversity)), rep('D3_U_diversity_5', length(D3_U_5_diversity)))
  D4_N_5_diversity <- N_data_4$`test/diversity_at_5`[!is.na(N_data_4$`test/diversity_at_5`)]
  D4_U_5_diversity <- U_data_4$`test/diversity_at_5`[!is.na(U_data_4$`test/diversity_at_5`)]
  D4_5_diversity_label <- c(rep('D4_N_diversity_5', length(D4_N_5_diversity)), rep('D4_U_diversity_5', length(D4_U_5_diversity)))
  # combine data
  diversity_5 <- c(D0_N_5_diversity, D0_U_5_diversity, D1_N_5_diversity, D1_U_5_diversity, D2_N_5_diversity, D2_U_5_diversity, D3_N_5_diversity, D3_U_5_diversity, D4_N_5_diversity, D4_U_5_diversity)
  diversity_5_label <- c(D0_5_diversity_label, D1_5_diversity_label, D2_5_diversity_label, D3_5_diversity_label, D4_5_diversity_label)
  
  D0_N_10_diversity <- N_data_0$`test/diversity_at_10`[!is.na(N_data_0$`test/diversity_at_10`)]
  D0_U_10_diversity <- U_data_0$`test/diversity_at_10`[!is.na(U_data_0$`test/diversity_at_10`)]
  D0_10_diversity_label <- c(rep('D0_N_diversity_10', length(D0_N_10_diversity)), rep('D0_U_diversity_10', length(D0_U_10_diversity)))
  D1_N_10_diversity <- N_data_1$`test/diversity_at_10`[!is.na(N_data_1$`test/diversity_at_10`)]
  D1_U_10_diversity <- U_data_1$`test/diversity_at_10`[!is.na(U_data_1$`test/diversity_at_10`)]
  D1_10_diversity_label <- c(rep('D1_N_diversity_10', length(D1_N_10_diversity)), rep('D1_U_diversity_10', length(D1_U_10_diversity)))
  D2_N_10_diversity <- N_data_2$`test/diversity_at_10`[!is.na(N_data_2$`test/diversity_at_10`)]
  D2_U_10_diversity <- U_data_2$`test/diversity_at_10`[!is.na(U_data_2$`test/diversity_at_10`)]
  D2_10_diversity_label <- c(rep('D2_N_diversity_10', length(D2_N_10_diversity)), rep('D2_U_diversity_10', length(D2_U_10_diversity)))
  D3_N_10_diversity <- N_data_3$`test/diversity_at_10`[!is.na(N_data_3$`test/diversity_at_10`)]
  D3_U_10_diversity <- U_data_3$`test/diversity_at_10`[!is.na(U_data_3$`test/diversity_at_10`)]
  D3_10_diversity_label <- c(rep('D3_N_diversity_10', length(D3_N_10_diversity)), rep('D3_U_diversity_10', length(D3_U_10_diversity)))
  D4_N_10_diversity <- N_data_4$`test/diversity_at_10`[!is.na(N_data_4$`test/diversity_at_10`)]
  D4_U_10_diversity <- U_data_4$`test/diversity_at_10`[!is.na(U_data_4$`test/diversity_at_10`)]
  D4_10_diversity_label <- c(rep('D4_N_diversity_10', length(D4_N_10_diversity)), rep('D4_U_diversity_10', length(D4_U_10_diversity)))
  # combine data
  diversity_10 <- c(D0_N_10_diversity, D0_U_10_diversity, D1_N_10_diversity, D1_U_10_diversity, D2_N_10_diversity, D2_U_10_diversity, D3_N_10_diversity, D3_U_10_diversity, D4_N_10_diversity, D4_U_10_diversity)
  diversity_10_label <- c(D0_10_diversity_label, D1_10_diversity_label, D2_10_diversity_label, D3_10_diversity_label, D4_10_diversity_label)
  
  D0_N_20_diversity <- N_data_0$`test/diversity_at_20`[!is.na(N_data_0$`test/diversity_at_20`)]
  D0_U_20_diversity <- U_data_0$`test/diversity_at_20`[!is.na(U_data_0$`test/diversity_at_20`)]
  D0_20_diversity_label <- c(rep('D0_N_diversity_20', length(D0_N_20_diversity)), rep('D0_U_diversity_20', length(D0_U_20_diversity)))
  D1_N_20_diversity <- N_data_1$`test/diversity_at_20`[!is.na(N_data_1$`test/diversity_at_20`)]
  D1_U_20_diversity <- U_data_1$`test/diversity_at_20`[!is.na(U_data_1$`test/diversity_at_20`)]
  D1_20_diversity_label <- c(rep('D1_N_diversity_20', length(D1_N_20_diversity)), rep('D1_U_diversity_20', length(D1_U_20_diversity)))
  D2_N_20_diversity <- N_data_2$`test/diversity_at_20`[!is.na(N_data_2$`test/diversity_at_20`)]
  D2_U_20_diversity <- U_data_2$`test/diversity_at_20`[!is.na(U_data_2$`test/diversity_at_20`)]
  D2_20_diversity_label <- c(rep('D2_N_diversity_20', length(D2_N_20_diversity)), rep('D2_U_diversity_20', length(D2_U_20_diversity)))
  D3_N_20_diversity <- N_data_3$`test/diversity_at_20`[!is.na(N_data_3$`test/diversity_at_20`)]
  D3_U_20_diversity <- U_data_3$`test/diversity_at_20`[!is.na(U_data_3$`test/diversity_at_20`)]
  D3_20_diversity_label <- c(rep('D3_N_diversity_20', length(D3_N_20_diversity)), rep('D3_U_diversity_20', length(D3_U_20_diversity)))
  D4_N_20_diversity <- N_data_4$`test/diversity_at_20`[!is.na(N_data_4$`test/diversity_at_20`)]
  D4_U_20_diversity <- U_data_4$`test/diversity_at_20`[!is.na(U_data_4$`test/diversity_at_20`)]
  D4_20_diversity_label <- c(rep('D4_N_diversity_20', length(D4_N_20_diversity)), rep('D4_U_diversity_20', length(D4_U_20_diversity)))
  # combine data
  diversity_20 <- c(D0_N_20_diversity, D0_U_20_diversity, D1_N_20_diversity, D1_U_20_diversity, D2_N_20_diversity, D2_U_20_diversity, D3_N_20_diversity, D3_U_20_diversity, D4_N_20_diversity, D4_U_20_diversity)
  diversity_20_label <- c(D0_20_diversity_label, D1_20_diversity_label, D2_20_diversity_label, D3_20_diversity_label, D4_20_diversity_label)
  
  D0_N_50_diversity <- N_data_0$`test/diversity_at_50`[!is.na(N_data_0$`test/diversity_at_50`)]
  D0_U_50_diversity <- U_data_0$`test/diversity_at_50`[!is.na(U_data_0$`test/diversity_at_50`)]
  D0_50_diversity_label <- c(rep('D0_N_diversity_50', length(D0_N_50_diversity)), rep('D0_U_diversity_50', length(D0_U_50_diversity)))
  D1_N_50_diversity <- N_data_1$`test/diversity_at_50`[!is.na(N_data_1$`test/diversity_at_50`)]
  D1_U_50_diversity <- U_data_1$`test/diversity_at_50`[!is.na(U_data_1$`test/diversity_at_50`)]
  D1_50_diversity_label <- c(rep('D1_N_diversity_50', length(D1_N_50_diversity)), rep('D1_U_diversity_50', length(D1_U_50_diversity)))
  D2_N_50_diversity <- N_data_2$`test/diversity_at_50`[!is.na(N_data_2$`test/diversity_at_50`)]
  D2_U_50_diversity <- U_data_2$`test/diversity_at_50`[!is.na(U_data_2$`test/diversity_at_50`)]
  D2_50_diversity_label <- c(rep('D2_N_diversity_50', length(D2_N_50_diversity)), rep('D2_U_diversity_50', length(D2_U_50_diversity)))
  D3_N_50_diversity <- N_data_3$`test/diversity_at_50`[!is.na(N_data_3$`test/diversity_at_50`)]
  D3_U_50_diversity <- U_data_3$`test/diversity_at_50`[!is.na(U_data_3$`test/diversity_at_50`)]
  D3_50_diversity_label <- c(rep('D3_N_diversity_50', length(D3_N_50_diversity)), rep('D3_U_diversity_50', length(D3_U_50_diversity)))
  D4_N_50_diversity <- N_data_4$`test/diversity_at_50`[!is.na(N_data_4$`test/diversity_at_50`)]
  D4_U_50_diversity <- U_data_4$`test/diversity_at_50`[!is.na(U_data_4$`test/diversity_at_50`)]
  D4_50_diversity_label <- c(rep('D4_N_diversity_50', length(D4_N_50_diversity)), rep('D4_U_diversity_50', length(D4_U_50_diversity)))
  # combine data
  diversity_50 <- c(D0_N_50_diversity, D0_U_50_diversity, D1_N_50_diversity, D1_U_50_diversity, D2_N_50_diversity, D2_U_50_diversity, D3_N_50_diversity, D3_U_50_diversity, D4_N_50_diversity, D4_U_50_diversity)
  diversity_50_label <- c(D0_50_diversity_label, D1_50_diversity_label, D2_50_diversity_label, D3_50_diversity_label, D4_50_diversity_label)
  # make dataframe
  diversity_ALL <- data.frame(diversity_3, diversity_3_label, diversity_5, diversity_5_label, diversity_10, diversity_10_label, diversity_20, diversity_20_label, diversity_50, diversity_50_label)
  #View(diversity_ALL)
  
  #coverage
  # get labels
  D0_N_3_coverage <- N_data_0$`test/coverage_at_3`[!is.na(N_data_0$`test/coverage_at_3`)]
  D0_U_3_coverage <- U_data_0$`test/coverage_at_3`[!is.na(U_data_0$`test/coverage_at_3`)]
  D0_3_coverage_label <- c(rep('D0_N_coverage_3', length(D0_N_3_coverage)), rep('D0_U_coverage_3', length(D0_U_3_coverage)))
  D1_N_3_coverage <- N_data_1$`test/coverage_at_3`[!is.na(N_data_1$`test/coverage_at_3`)]
  D1_U_3_coverage <- U_data_1$`test/coverage_at_3`[!is.na(U_data_1$`test/coverage_at_3`)]
  D1_3_coverage_label <- c(rep('D1_N_coverage_3', length(D1_N_3_coverage)), rep('D1_U_coverage_3', length(D1_U_3_coverage)))
  D2_N_3_coverage <- N_data_2$`test/coverage_at_3`[!is.na(N_data_2$`test/coverage_at_3`)]
  D2_U_3_coverage <- U_data_2$`test/coverage_at_3`[!is.na(U_data_2$`test/coverage_at_3`)]
  D2_3_coverage_label <- c(rep('D2_N_coverage_3', length(D2_N_3_coverage)), rep('D2_U_coverage_3', length(D2_U_3_coverage)))
  D3_N_3_coverage <- N_data_3$`test/coverage_at_3`[!is.na(N_data_3$`test/coverage_at_3`)]
  D3_U_3_coverage <- U_data_3$`test/coverage_at_3`[!is.na(U_data_3$`test/coverage_at_3`)]
  D3_3_coverage_label <- c(rep('D3_N_coverage_3', length(D3_N_3_coverage)), rep('D3_U_coverage_3', length(D3_U_3_coverage)))
  D4_N_3_coverage <- N_data_4$`test/coverage_at_3`[!is.na(N_data_4$`test/coverage_at_3`)]
  D4_U_3_coverage <- U_data_4$`test/coverage_at_3`[!is.na(U_data_4$`test/coverage_at_3`)]
  D4_3_coverage_label <- c(rep('D4_N_coverage_3', length(D4_N_3_coverage)), rep('D4_U_coverage_3', length(D4_U_3_coverage)))
  # combine data
  coverage_3 <- c(D0_N_3_coverage, D0_U_3_coverage, D1_N_3_coverage, D1_U_3_coverage, D2_N_3_coverage, D2_U_3_coverage, D3_N_3_coverage, D3_U_3_coverage, D4_N_3_coverage, D4_U_3_coverage)
  coverage_3_label <- c(D0_3_coverage_label, D1_3_coverage_label, D2_3_coverage_label, D3_3_coverage_label, D4_3_coverage_label)
  
  D0_N_5_coverage <- N_data_0$`test/coverage_at_5`[!is.na(N_data_0$`test/coverage_at_5`)]
  D0_U_5_coverage <- U_data_0$`test/coverage_at_5`[!is.na(U_data_0$`test/coverage_at_5`)]
  D0_5_coverage_label <- c(rep('D0_N_coverage_5', length(D0_N_5_coverage)), rep('D0_U_coverage_5', length(D0_U_5_coverage)))
  D1_N_5_coverage <- N_data_1$`test/coverage_at_5`[!is.na(N_data_1$`test/coverage_at_5`)]
  D1_U_5_coverage <- U_data_1$`test/coverage_at_5`[!is.na(U_data_1$`test/coverage_at_5`)]
  D1_5_coverage_label <- c(rep('D1_N_coverage_5', length(D1_N_5_coverage)), rep('D1_U_coverage_5', length(D1_U_5_coverage)))
  D2_N_5_coverage <- N_data_2$`test/coverage_at_5`[!is.na(N_data_2$`test/coverage_at_5`)]
  D2_U_5_coverage <- U_data_2$`test/coverage_at_5`[!is.na(U_data_2$`test/coverage_at_5`)]
  D2_5_coverage_label <- c(rep('D2_N_coverage_5', length(D2_N_5_coverage)), rep('D2_U_coverage_5', length(D2_U_5_coverage)))
  D3_N_5_coverage <- N_data_3$`test/coverage_at_5`[!is.na(N_data_3$`test/coverage_at_5`)]
  D3_U_5_coverage <- U_data_3$`test/coverage_at_5`[!is.na(U_data_3$`test/coverage_at_5`)]
  D3_5_coverage_label <- c(rep('D3_N_coverage_5', length(D3_N_5_coverage)), rep('D3_U_coverage_5', length(D3_U_5_coverage)))
  D4_N_5_coverage <- N_data_4$`test/coverage_at_5`[!is.na(N_data_4$`test/coverage_at_5`)]
  D4_U_5_coverage <- U_data_4$`test/coverage_at_5`[!is.na(U_data_4$`test/coverage_at_5`)]
  D4_5_coverage_label <- c(rep('D4_N_coverage_5', length(D4_N_5_coverage)), rep('D4_U_coverage_5', length(D4_U_5_coverage)))
  # combine data
  coverage_5 <- c(D0_N_5_coverage, D0_U_5_coverage, D1_N_5_coverage, D1_U_5_coverage, D2_N_5_coverage, D2_U_5_coverage, D3_N_5_coverage, D3_U_5_coverage, D4_N_5_coverage, D4_U_5_coverage)
  coverage_5_label <- c(D0_5_coverage_label, D1_5_coverage_label, D2_5_coverage_label, D3_5_coverage_label, D4_5_coverage_label)
  
  D0_N_10_coverage <- N_data_0$`test/coverage_at_10`[!is.na(N_data_0$`test/coverage_at_10`)]
  D0_U_10_coverage <- U_data_0$`test/coverage_at_10`[!is.na(U_data_0$`test/coverage_at_10`)]
  D0_10_coverage_label <- c(rep('D0_N_coverage_10', length(D0_N_10_coverage)), rep('D0_U_coverage_10', length(D0_U_10_coverage)))
  D1_N_10_coverage <- N_data_1$`test/coverage_at_10`[!is.na(N_data_1$`test/coverage_at_10`)]
  D1_U_10_coverage <- U_data_1$`test/coverage_at_10`[!is.na(U_data_1$`test/coverage_at_10`)]
  D1_10_coverage_label <- c(rep('D1_N_coverage_10', length(D1_N_10_coverage)), rep('D1_U_coverage_10', length(D1_U_10_coverage)))
  D2_N_10_coverage <- N_data_2$`test/coverage_at_10`[!is.na(N_data_2$`test/coverage_at_10`)]
  D2_U_10_coverage <- U_data_2$`test/coverage_at_10`[!is.na(U_data_2$`test/coverage_at_10`)]
  D2_10_coverage_label <- c(rep('D2_N_coverage_10', length(D2_N_10_coverage)), rep('D2_U_coverage_10', length(D2_U_10_coverage)))
  D3_N_10_coverage <- N_data_3$`test/coverage_at_10`[!is.na(N_data_3$`test/coverage_at_10`)]
  D3_U_10_coverage <- U_data_3$`test/coverage_at_10`[!is.na(U_data_3$`test/coverage_at_10`)]
  D3_10_coverage_label <- c(rep('D3_N_coverage_10', length(D3_N_10_coverage)), rep('D3_U_coverage_10', length(D3_U_10_coverage)))
  D4_N_10_coverage <- N_data_4$`test/coverage_at_10`[!is.na(N_data_4$`test/coverage_at_10`)]
  D4_U_10_coverage <- U_data_4$`test/coverage_at_10`[!is.na(U_data_4$`test/coverage_at_10`)]
  D4_10_coverage_label <- c(rep('D4_N_coverage_10', length(D4_N_10_coverage)), rep('D4_U_coverage_10', length(D4_U_10_coverage)))
  # combine data
  coverage_10 <- c(D0_N_10_coverage, D0_U_10_coverage, D1_N_10_coverage, D1_U_10_coverage, D2_N_10_coverage, D2_U_10_coverage, D3_N_10_coverage, D3_U_10_coverage, D4_N_10_coverage, D4_U_10_coverage)
  coverage_10_label <- c(D0_10_coverage_label, D1_10_coverage_label, D2_10_coverage_label, D3_10_coverage_label, D4_10_coverage_label)
  
  D0_N_20_coverage <- N_data_0$`test/coverage_at_20`[!is.na(N_data_0$`test/coverage_at_20`)]
  D0_U_20_coverage <- U_data_0$`test/coverage_at_20`[!is.na(U_data_0$`test/coverage_at_20`)]
  D0_20_coverage_label <- c(rep('D0_N_coverage_20', length(D0_N_20_coverage)), rep('D0_U_coverage_20', length(D0_U_20_coverage)))
  D1_N_20_coverage <- N_data_1$`test/coverage_at_20`[!is.na(N_data_1$`test/coverage_at_20`)]
  D1_U_20_coverage <- U_data_1$`test/coverage_at_20`[!is.na(U_data_1$`test/coverage_at_20`)]
  D1_20_coverage_label <- c(rep('D1_N_coverage_20', length(D1_N_20_coverage)), rep('D1_U_coverage_20', length(D1_U_20_coverage)))
  D2_N_20_coverage <- N_data_2$`test/coverage_at_20`[!is.na(N_data_2$`test/coverage_at_20`)]
  D2_U_20_coverage <- U_data_2$`test/coverage_at_20`[!is.na(U_data_2$`test/coverage_at_20`)]
  D2_20_coverage_label <- c(rep('D2_N_coverage_20', length(D2_N_20_coverage)), rep('D2_U_coverage_20', length(D2_U_20_coverage)))
  D3_N_20_coverage <- N_data_3$`test/coverage_at_20`[!is.na(N_data_3$`test/coverage_at_20`)]
  D3_U_20_coverage <- U_data_3$`test/coverage_at_20`[!is.na(U_data_3$`test/coverage_at_20`)]
  D3_20_coverage_label <- c(rep('D3_N_coverage_20', length(D3_N_20_coverage)), rep('D3_U_coverage_20', length(D3_U_20_coverage)))
  D4_N_20_coverage <- N_data_4$`test/coverage_at_20`[!is.na(N_data_4$`test/coverage_at_20`)]
  D4_U_20_coverage <- U_data_4$`test/coverage_at_20`[!is.na(U_data_4$`test/coverage_at_20`)]
  D4_20_coverage_label <- c(rep('D4_N_coverage_20', length(D4_N_20_coverage)), rep('D4_U_coverage_20', length(D4_U_20_coverage)))
  # combine data
  coverage_20 <- c(D0_N_20_coverage, D0_U_20_coverage, D1_N_20_coverage, D1_U_20_coverage, D2_N_20_coverage, D2_U_20_coverage, D3_N_20_coverage, D3_U_20_coverage, D4_N_20_coverage, D4_U_20_coverage)
  coverage_20_label <- c(D0_20_coverage_label, D1_20_coverage_label, D2_20_coverage_label, D3_20_coverage_label, D4_20_coverage_label)
  
  D0_N_50_coverage <- N_data_0$`test/coverage_at_50`[!is.na(N_data_0$`test/coverage_at_50`)]
  D0_U_50_coverage <- U_data_0$`test/coverage_at_50`[!is.na(U_data_0$`test/coverage_at_50`)]
  D0_50_coverage_label <- c(rep('D0_N_coverage_50', length(D0_N_50_coverage)), rep('D0_U_coverage_50', length(D0_U_50_coverage)))
  D1_N_50_coverage <- N_data_1$`test/coverage_at_50`[!is.na(N_data_1$`test/coverage_at_50`)]
  D1_U_50_coverage <- U_data_1$`test/coverage_at_50`[!is.na(U_data_1$`test/coverage_at_50`)]
  D1_50_coverage_label <- c(rep('D1_N_coverage_50', length(D1_N_50_coverage)), rep('D1_U_coverage_50', length(D1_U_50_coverage)))
  D2_N_50_coverage <- N_data_2$`test/coverage_at_50`[!is.na(N_data_2$`test/coverage_at_50`)]
  D2_U_50_coverage <- U_data_2$`test/coverage_at_50`[!is.na(U_data_2$`test/coverage_at_50`)]
  D2_50_coverage_label <- c(rep('D2_N_coverage_50', length(D2_N_50_coverage)), rep('D2_U_coverage_50', length(D2_U_50_coverage)))
  D3_N_50_coverage <- N_data_3$`test/coverage_at_50`[!is.na(N_data_3$`test/coverage_at_50`)]
  D3_U_50_coverage <- U_data_3$`test/coverage_at_50`[!is.na(U_data_3$`test/coverage_at_50`)]
  D3_50_coverage_label <- c(rep('D3_N_coverage_50', length(D3_N_50_coverage)), rep('D3_U_coverage_50', length(D3_U_50_coverage)))
  D4_N_50_coverage <- N_data_4$`test/coverage_at_50`[!is.na(N_data_4$`test/coverage_at_50`)]
  D4_U_50_coverage <- U_data_4$`test/coverage_at_50`[!is.na(U_data_4$`test/coverage_at_50`)]
  D4_50_coverage_label <- c(rep('D4_N_coverage_50', length(D4_N_50_coverage)), rep('D4_U_coverage_50', length(D4_U_50_coverage)))
  # combine data
  coverage_50 <- c(D0_N_50_coverage, D0_U_50_coverage, D1_N_50_coverage, D1_U_50_coverage, D2_N_50_coverage, D2_U_50_coverage, D3_N_50_coverage, D3_U_50_coverage, D4_N_50_coverage, D4_U_50_coverage)
  coverage_50_label <- c(D0_50_coverage_label, D1_50_coverage_label, D2_50_coverage_label, D3_50_coverage_label, D4_50_coverage_label)
  # make dataframe
  coverage_ALL <- data.frame(coverage_3, coverage_3_label, coverage_5, coverage_5_label, coverage_10, coverage_10_label, coverage_20, coverage_20_label, coverage_50, coverage_50_label)
  print("DONE!")
  
  
  # choose configuration: feature (ndcg, recall); threshold (3, 5, 10, 20, 50)
  metrics = c("diversity", "coverage")
  #thresholds = c(3, 5, 10, 20, 50)
  thresholds = c(5, 10, 20, 50)
  for (threshold in thresholds){
    print(threshold)
    RESULTS <- c(RESULTS, threshold)
    for (metric in metrics){
      if (metric == "diversity"){
        p_d <- data.frame(p_val = numeric(0), effect_size = numeric(0))
        if (threshold == 3){
          p_d <- get_p(D0_N_3_diversity, D0_U_3_diversity, p_d)
          p_d <- get_p(D1_N_3_diversity, D1_U_3_diversity, p_d)
          p_d <- get_p(D2_N_3_diversity, D2_U_3_diversity, p_d)
          p_d <- get_p(D3_N_3_diversity, D3_U_3_diversity, p_d)
          p_d <- get_p(D4_N_3_diversity, D4_U_3_diversity, p_d)
        } else if (threshold == 5){
          p_d <- get_p(D0_N_5_diversity, D0_U_5_diversity, p_d)
          p_d <- get_p(D1_N_5_diversity, D1_U_5_diversity, p_d)
          p_d <- get_p(D2_N_5_diversity, D2_U_5_diversity, p_d)
          p_d <- get_p(D3_N_5_diversity, D3_U_5_diversity, p_d)
          p_d <- get_p(D4_N_5_diversity, D4_U_5_diversity, p_d)
        } else if (threshold == 10){
          p_d <- get_p(D0_N_10_diversity, D0_U_10_diversity, p_d)
          p_d <- get_p(D1_N_10_diversity, D1_U_10_diversity, p_d)
          p_d <- get_p(D2_N_10_diversity, D2_U_10_diversity, p_d)
          p_d <- get_p(D3_N_10_diversity, D3_U_10_diversity, p_d)
          p_d <- get_p(D4_N_10_diversity, D4_U_10_diversity, p_d)
        } else if (threshold == 20){
          p_d <- get_p(D0_N_20_diversity, D0_U_20_diversity, p_d)
          p_d <- get_p(D1_N_20_diversity, D1_U_20_diversity, p_d)
          p_d <- get_p(D2_N_20_diversity, D2_U_20_diversity, p_d)
          p_d <- get_p(D3_N_20_diversity, D3_U_20_diversity, p_d)
          p_d <- get_p(D4_N_20_diversity, D4_U_20_diversity, p_d)
        } else if (threshold == 50){
          p_d <- get_p(D0_N_50_diversity, D0_U_50_diversity, p_d)
          p_d <- get_p(D1_N_50_diversity, D1_U_50_diversity, p_d)
          p_d <- get_p(D2_N_50_diversity, D2_U_50_diversity, p_d)
          p_d <- get_p(D3_N_50_diversity, D3_U_50_diversity, p_d)
          p_d <- get_p(D4_N_50_diversity, D4_U_50_diversity, p_d)
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
      } else {
        if (threshold == 3){
          target_f <- c('D0_U_coverage_3', 'D1_U_coverage_3','D2_U_coverage_3','D3_U_coverage_3','D4_U_coverage_3')
          target_m <- c('D0_N_coverage_3', 'D1_N_coverage_3','D2_N_coverage_3','D3_N_coverage_3','D4_N_coverage_3')
          y <- coverage_ALL[which(coverage_ALL$coverage_3_label %in% target_f),]
          y <- y$coverage_3
          x <- coverage_ALL[which(coverage_ALL$coverage_3_label %in% target_m),]
          x <- x$coverage_3
        } else if (threshold == 5){
          target_f <- c('D0_U_coverage_5', 'D1_U_coverage_5','D2_U_coverage_5','D3_U_coverage_5','D4_U_coverage_5')
          target_m <- c('D0_N_coverage_5', 'D1_N_coverage_5','D2_N_coverage_5','D3_N_coverage_5','D4_N_coverage_5')
          y <- coverage_ALL[which(coverage_ALL$coverage_5_label %in% target_f),]
          y <- y$coverage_5
          x <- coverage_ALL[which(coverage_ALL$coverage_5_label %in% target_m),]
          x <- x$coverage_5
        } else if (threshold == 10){
          target_f <- c('D0_U_coverage_10', 'D1_U_coverage_10','D2_U_coverage_10','D3_U_coverage_10','D4_U_coverage_10')
          target_m <- c('D0_N_coverage_10', 'D1_N_coverage_10','D2_N_coverage_10','D3_N_coverage_10','D4_N_coverage_10')
          y <- coverage_ALL[which(coverage_ALL$coverage_10_label %in% target_f),]
          y <- y$coverage_10
          x <- coverage_ALL[which(coverage_ALL$coverage_10_label %in% target_m),]
          x <- x$coverage_10
        } else if (threshold == 20){
          target_f <- c('D0_U_coverage_20', 'D1_U_coverage_20','D2_U_coverage_20','D3_U_coverage_20','D4_U_coverage_20')
          target_m <- c('D0_N_coverage_20', 'D1_N_coverage_20','D2_N_coverage_20','D3_N_coverage_20','D4_N_coverage_20')
          y <- coverage_ALL[which(coverage_ALL$coverage_20_label %in% target_f),]
          y <- y$coverage_20
          x <- coverage_ALL[which(coverage_ALL$coverage_20_label %in% target_m),]
          x <- x$coverage_20
        } else if (threshold == 50){
          target_f <- c('D0_U_coverage_50', 'D1_U_coverage_50','D2_U_coverage_50','D3_U_coverage_50','D4_U_coverage_50')
          target_m <- c('D0_N_coverage_50', 'D1_N_coverage_50','D2_N_coverage_50','D3_N_coverage_50','D4_N_coverage_50')
          y <- coverage_ALL[which(coverage_ALL$coverage_50_label %in% target_f),]
          y <- y$coverage_50
          x <- coverage_ALL[which(coverage_ALL$coverage_50_label %in% target_m),]
          x <- x$coverage_50
        }
        MannResult <- wilcox.test(y, x)
        print(metric)
        RESULTS <- c(RESULTS, metric)
        print(MannResult$p.value)
        RESULTS <- c(RESULTS, MannResult$p.value)
      }
    }
  }
}


for (elem in RESULTS){
  print(elem)  
} 

