# No. 2
# a. Formulate Your Question
# Pertanyaan:
# "Apakah tingkat kepuasan karyawan (satisfaction_level) akan lebih tinggi pada karyawan yang menerima promosi dalam lima tahun terakhir dibandingkan mereka yang tidak menerima promosi?"
library(dplyr)
library(ggplot2)

# b. Read in Your Data
Data <- read.csv("C:/Users/syafa/Analisis Data.csv", header = TRUE, sep = ";")
Data

# c. Check the Packaging
nrow(Data)
ncol(Data)
str(Data)

# d. Look at the Top and the Bottom of Your Data
print(head(Data))
print(tail(Data))

# e. Check Your "n"s
table(Data$promotion_last_5years)
colSums(is.na(Data))

# f. Validate with at Least One External Data Source
summary(Data$promotion_last_5years)
summary(Data$satisfaction_level)
quantile(Data$satisfaction_level, seq(0, 1, 0.1), na.rm = TRUE)
t.test(satisfaction_level ~ promotion_last_5years, data = data)

# g. Make a Plot
ggplot(Data, aes(x = factor(promotion_last_5years), 
                 y = satisfaction_level)) +
  geom_boxplot() +
  labs(
    title = "Distribusi Satisfaction Level Berdasarkan Promotion Last 5 Years",
    x = "Promotion Last 5 Years",
    y = "Satisfaction Level"
  ) +
  theme_minimal()

# h. Try the Easy Solution First
summary_table <- Data %>%
  group_by(promotion_last_5years) %>%
  summarize(
    mean_satisfaction = mean(satisfaction_level, na.rm = TRUE),
    median_satisfaction = median(satisfaction_level, na.rm = TRUE),
    count = n()
  )
summary_table


# No. 3
# a.	Models as Expectations
model<-lm(satisfaction_level~promotion_last_5years,data=Data)
summary(model)

# b. Histogram satisfaction_level
ggplot(Data, aes(x = satisfaction_level)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Histogram Satisfaction Level",
    x = "Satisfaction Level",
    y = "Frequency"
  ) +
  theme_minimal()

# Membandingkan dengan distribusi normal
library(ggplot2)
ggplot(Data, aes(x = satisfaction_level)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, args = list(mean = mean(Data$satisfaction_level, na.rm = TRUE),
                                         sd = sd(Data$satisfaction_level, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(
    title = "Perbandingan Histogram dengan Distribusi Normal",
    x = "Satisfaction Level",
    y = "Density"
  ) +
  theme_minimal()