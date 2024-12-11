
library(forecast)
library(readr)

# Функция для нахождения всех пиков
find_peaks <- function(x, min_peak_height) {
  peaks <- c()
  for (i in 2:(length(x) - 1)) {
    if (x[i] > x[i - 1] && x[i] > x[i + 1] && x[i] >= min_peak_height) {
      peaks <- c(peaks, i)
    }
  }
  return(peaks)
}

data <- read_table("work/Kursach/2ndtry/19992022rawdata", 
                   col_names = FALSE)

coefficients <- data[, -1]  
num_columns <- ncol(coefficients)
#примерные положения значимых частот
approx_frequencies <- c(364, 732, 1097.1944 ,1461, 1825)
frequency_tolerance <- 10
min_peak_height <- 0

results <- data.frame(Number = integer(), 
                      Significant_Frequency = numeric(), 
                      Period_Years = numeric())

for (i in 1:num_columns) {
  print(i)
  
  series <- ts(coefficients[[i]], start = c(1999, 1), frequency = 12*365)
  spm <- spec.ar(series, order = 250, plot = TRUE, method = "burg")
  
  spm_data <- data.frame(Frequency = spm$freq, Spec = spm$spec)
  
  peak_indices <- find_peaks(spm_data$Spec, min_peak_height)
  
  peak_frequencies <- spm_data$Frequency[peak_indices]
  peak_values <- spm_data$Spec[peak_indices]

  significant_frequencies <- c()
  
  for (freq in approx_frequencies) {
    close_peaks <- peak_frequencies[abs(peak_frequencies - freq) < frequency_tolerance]
    
    if (length(close_peaks) > 0) {
      max_peak_index <- which.max(peak_values[match(close_peaks, peak_frequencies)])
      significant_frequencies <- c(significant_frequencies, close_peaks[max_peak_index])
    }
  }
  
  significant_frequencies <- unique(significant_frequencies)

  if (length(significant_frequencies) > 5) {
    significant_frequencies <- significant_frequencies[1:5]
  }
  
  significant_periods_years <- 1 / significant_frequencies
  
  significant_periods_days <- significant_periods_years * 365 * 12
  
  for (j in seq_along(significant_frequencies)) {
    results <- rbind(results, data.frame(Number = i, 
                                         Significant_Frequency = significant_frequencies[j], 
                                         Period_Years = significant_periods_days[j]))
  }
  
  pdf(paste("/home/vladislav/work/Kursach/2ndtry/graph_sign_freq/plot", i, ".pdf", sep=""), width=10, height=6)
  
  plot(spm_data$Frequency , spm_data$Spec, 
       main = paste("PSD #", i),
       ylab = "Spectrum",          
       xlab = "Frequency",                         
       col = "black", type = 'l', log = "y",                            
       lwd = 2 )
  
  points(significant_frequencies, 
         spm_data$Spec[match(significant_frequencies, spm_data$Frequency)], 
         col = "red", pch = 19)  # Отметим значимые частоты
  
  legend(x = "topright",          # Position
         legend = c("Order = 250", "Significant Frequencies"),
         lwd = c(2, NA), pch = c(NA, 19), col = c("black", "red"))          
  
  grid()
  dev.off()
  
   print(paste("Significant frequencies for series", i, ":", paste(significant_frequencies, collapse=", ")))
}
write.csv(results, file = "significant_frequencies.csv", row.names = FALSE)
print("Results saved to significant_frequencies.csv")
