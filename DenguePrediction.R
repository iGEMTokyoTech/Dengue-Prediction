co_prediction <- function(degue_series, climate_series, Time){
  concatenated <- c(scale(degue_series), scale(climate_series))
  lib1 <- c(1, length(degue_series))
  lib2 <- length(degue_series) + c(1, length(climate_series))
  
  simplex_out_1 <- simplex(concatenated, lib = lib1, pred = lib1, silent = TRUE, stats_only = F)
  best_E_1 <- simplex_out_1$E[which.max(simplex_out_1$rho)]
  copred_1_to_2 <- simplex(concatenated, lib = lib1, pred = lib2, E = best_E_1, stats_only = F)
  
  simplex_out_2 <- simplex(concatenated, lib = lib2, pred = lib2, silent = TRUE, stats_only = F)
  best_E_2 <- simplex_out_2$E[which.max(simplex_out_2$rho)]
  copred_2_to_1 <- simplex(concatenated, lib = lib2, pred = lib1, E = best_E_2, stats_only = F)
  
  groups <- c(paste("Pred of ", as.character(as.list(substitute(list(degue_series)))[-1L]),  "from ", as.character(as.list(substitute(list(degue_series)))[-1L])),
              paste("Copred of ",as.character(as.list(substitute(list(degue_series)))[-1L]),  "from ", as.character(as.list(substitute(list(climate_series)))[-1L])))
  to_plot <- data.frame(label = factor(groups, levels = groups),
                        rbind(simplex_out_1[which.max(simplex_out_1$rho), ],
                              copred_2_to_1)
  )
  {plot(copred_2_to_1$model_output[[1]]$obs, x = Time, type = "l", lwd = 2, xlab = "Year", ylab = "Scaled", main = paste("Pred of ", as.character(as.list(substitute(list(degue_series)))[-1L]),  "from ", as.character(as.list(substitute(list(climate_series)))[-1L])))
    lines(copred_2_to_1$model_output[[1]]$pred, col = "red", type = "l", lwd = 2, x = Time)}
  library(ggplot2)
  ggplot(to_plot, aes(x = label, y = rho)) +
    geom_col() + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

co_pred_df<- function(df_dengue, df_climate, Time){
  pdf("CopredWithClimate.pdf")
  colnames_of_dengue <- colnames(df_dengue)
  colnames_of_dengue <- colnames_of_dengue[colnames_of_dengue != Time]
  colnames_of_climate <- colnames(df_climate)
  colnames_of_climate <- colnames_of_climate[colnames_of_climate != Time]
  for (i in colnames_of_dengue){
    for (k in colnames_of_climate){
      dengue_series <- df_dengue[, i]
      climate_series <- df_climate[, k]
      concatenated <- c(scale(dengue_series), scale(climate_series))
      lib1 <- c(1, length(dengue_series))
      lib2 <- length(dengue_series) + c(1, length(climate_series))
      
      simplex_out_1 <- simplex(concatenated, lib = lib1, pred = lib1, silent = TRUE, stats_only = F)
      best_E_1 <- simplex_out_1$E[which.max(simplex_out_1$rho)]
      copred_1_to_2 <- simplex(concatenated, lib = lib1, pred = lib2, E = best_E_1, stats_only = F)
      
      simplex_out_2 <- simplex(concatenated, lib = lib2, pred = lib2, silent = TRUE, stats_only = F)
      best_E_2 <- simplex_out_2$E[which.max(simplex_out_2$rho)]
      copred_2_to_1 <- simplex(concatenated, lib = lib2, pred = lib1, E = best_E_2, stats_only = F)
      
      groups <- c(paste("Pred of ", i, "from ", i),
                  paste("Copred of ",i , "from ", k))
      to_plot <- data.frame(label = factor(groups, levels = groups),
                            rbind(simplex_out_1[which.max(simplex_out_1$rho), ],
                                  copred_2_to_1))
      library(ggplot2)
      a <- ggplot(to_plot, aes(x = label, y = rho)) + 
        geom_col() + theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      print(a)
      {plot(scale(dengue_series), x = df_Time, type = "l", lwd = 2, xlab = "Year", ylab = "Scaled", main = paste("Pred of ", i, "from ", k))
        lines(copred_2_to_1$model_output[[1]]$pred, col = "red", type = "l", lwd = 2, x = df_Time)}
    }
  }
  dev.off()
}

config_file <- "/Users/ryogamisu/iGEM2018/Dengue-Prediction/config.R"
source(config_file)
library(openxlsx)
library(rEDM)
pdf("./plot/Prediction.pdf", width = 12, height = 6)
current_dir <- getwd()
df_subject <- openxlsx::read.xlsx(subject_data, rows = subject_rows, sheet = subject_sheetname)
df_climate <- openxlsx::read.xlsx(climate_data, rows = climate_rows, sheet = climate_sheetname)
df_climate <- df_climate[df_climate$Year %in% df_Time, ]

df_Time <-  df_subject[, Time]
df_subject_each <- df_subject[, c(DEN1_subject, DEN2_subject, DEN3_subject, DEN4_subject)]
df_DEN_total <- df_subject[, DEN_total]
df_DEN1234_total <- apply(X = df_subject_each, FUN = sum, MARGIN = 1)
df_rate <- df_subject_each / df_DEN1234_total


df_subject_each_simplex <- apply(df_subject_each, 2, simplex, stats_only = F, lib = c(1, length(df_Time)), pred = c(1 , length(df_Time) + 1), E = 1:10)
{plot(df_subject_each_simplex[[1]]$rho, ylim = c(0, 1), col = "black", xlab = "E", ylab = "rho", type = "l", main = "rho about each subject data", sub = "Black:DEN1_subject, Red:DEN2_subject, Bule:Den3_subject, Green:DEN4_subject")
  lines(df_subject_each_simplex[[2]]$rho, col = "red")
  lines(df_subject_each_simplex[[3]]$rho, col = "blue")
  lines(df_subject_each_simplex[[4]]$rho, col = "green")
}

best_E_subject <- sapply(df_subject_each_simplex, function(df) {
  df$E[which.max(df$rho)] 
})

{plot(c(df_subject_each[[1]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), xlab = "Time", ylab = "DEN1_subject")
  lines(c(NA, df_subject_each_simplex[[1]]$model_output[[best_E_subject[1]]]$pred), type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_subject_each[[2]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), xlab = "Time", ylab = "DEN2_subject")
  lines(c(NA, df_subject_each_simplex[[2]]$model_output[[best_E_subject[2]]]$pred), type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_subject_each[[3]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), xlab = "Time", ylab = "DEN3_subject")
  lines(c(NA, df_subject_each_simplex[[3]]$model_output[[best_E_subject[3]]]$pred), type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_subject_each[[4]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), xlab = "Time", ylab = "DEN4_subject")
  lines(c(NA, df_subject_each_simplex[[4]]$model_output[[best_E_subject[4]]]$pred), type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}

df_rate_simplex <- apply(df_rate, 2, simplex, stats_only = F, lib = c(1, length(df_Time)), pred = c(1 , length(df_Time) + 1), E = 1:10)
{plot(df_rate_simplex[[1]]$rho, ylim = c(0, 1), col = "black", xlab = "E", ylab = "rho", type = "l", main = "rho about each data", sub = "Black:DEN1_rate, Red:DEN2_rate, Bule:Den3_rate, Green:DEN4_rate")
  lines(df_rate_simplex[[2]]$rho, col = "red")
  lines(df_rate_simplex[[3]]$rho, col = "blue")
  lines(df_rate_simplex[[4]]$rho, col = "green")
}

best_E_rate <- sapply(df_rate_simplex, function(df) {
  df$E[which.max(df$rho)] 
})

{plot(c(df_rate[[1]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), lwd = 2, xlab = "Time", ylab = "Ratio", main = "Changes in existence ratio type 1")
  lines(c(NA, df_rate_simplex[[1]]$model_output[[best_E_rate[1]]]$pred), lwd = 2, type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_rate[[2]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), lwd = 2, xlab = "Time", ylab = "Ratio", main = "Changes in existence ratio type 2")
  lines(c(NA, df_rate_simplex[[2]]$model_output[[best_E_rate[2]]]$pred), lwd = 2, type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_rate[[3]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), lwd = 2, xlab = "Time", ylab = "Ratio", main = "Changes in existence ratio type 3")
  lines(c(NA, df_rate_simplex[[3]]$model_output[[best_E_rate[3]]]$pred), lwd = 2, type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}
{plot(c(df_rate[[4]], NA), type = "l", x = c(df_Time, df_Time[length(df_Time)] + 1), lwd = 2, xlab = "Time", ylab = "Ratio", main = "Changes in existence ratio type 4")
  lines(c(NA, df_rate_simplex[[4]]$model_output[[best_E_rate[4]]]$pred), lwd = 2, type = "l", col = "red", x =c(df_Time, df_Time[length(df_Time)] + 1))}

co_pred_df(df_subject, df_climate, Time)

