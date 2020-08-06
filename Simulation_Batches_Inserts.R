### 

# Project: Simulation potency ~ month + insert + (month|batch)
# LMM:  
# Batches have different intercepts and different slopes
# Inserts have dufferent intercepts

---

# Packages
library(lme4)
library(nlme)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(merTools)



# Simulate the data
## Batches with different slopes, different intercepts


set.seed(2020)

# Time 
n_batches <- 3
obs_per_batch <- 9
month <- sort(rep(x=c(0, 3, 6, 9, 12, 18, 24), times = obs_per_batch))
n_total <- length(month)

# Number of batches
batch <- rep(c(1, 1,1, 2, 2, 2, 3, 3, 3), length.out = n_total)
batch_1 <- batch[batch == 1]
batch_2 <- batch[batch == 2]
batch_3 <- batch[batch == 3]

# Intercepts batches
b0_1 <- rnorm(n_total, 8.5, 0.014)             # sigma_a = 0.014
b0_2 <- rnorm(n_total, 8.3, 0.014)
b0_3 <- rnorm(n_total, 8.2, 0.014)

# Slopes batches
b1_1 <- rnorm(n_total, -0.018556, 0.002)       # sigma_b = 0.002
b1_2 <- rnorm(n_total, -0.023772, 0.002)
b1_3 <- rnorm(n_total, -0.011163, 0.002)

# Random error per batch
eps1 <- rnorm(n = n_total, mean=0, sd=0.1)     # sigma_e = 0.1
eps2 <- rnorm(n = n_total, mean=0, sd=0.1)   
eps3 <- rnorm(n = n_total, mean=0, sd=0.1)   


# Linear predictor batches
potency_1 <- b0_1 + b1_1 * month + eps1
potency_2 <- b0_2 + b1_2 * month + eps1
potency_3 <- b0_3 + b1_3 * month + eps1

# Merge dfs
df1 <- data.frame(month, batch_1, potency_1)
df2 <- data.frame(month, batch_2, potency_2)
df3 <- data.frame(month, batch_3, potency_3)
colnames(df1) <- c("month", "batch", "potency")
colnames(df2) <- c("month", "batch", "potency")
colnames(df3) <- c("month", "batch", "potency")

df <- rbind(df1, df2, df3)
df$batch <- as.factor(df$batch)


# Insert intercept, slope, error
insert_b0_a <- 0# rnorm(n_total, mean=0, sd=0.014)
insert_b0_b <- 0.63#rnorm(n_total, mean=0.63, sd=0.014)
insert_b0_c <- 0.78#rnorm(n_total, mean=0.78, sd=0.014)

insert_slope <- -0.001#rnorm(n = n_total, mean=-0.001, sd=0.0004)
insert_eps <- rnorm(n = n_total, mean=0, sd=0.003)  

# Insert linear predictor
insert_a <- insert_b0_a + insert_slope * month #+ insert_eps
insert_b <- insert_b0_b + insert_slope * month #+ insert_eps
insert_c <- insert_b0_c + insert_slope * month #+ insert_eps

insert_df <- data.frame(insert_y = c(insert_a, insert_b, insert_c),
                        insert = rep(c("a", "b", "c"), each = 63),
                        month = month)



# Order and combine everything
target <- rep(c("a", "b", "c"), times = 63)
insert_df_temp <- insert_df[match(target, insert_df$insert),]
df_temp <- df[order(df$month),]
df_temp$insert <- insert_df_temp$insert
df_temp$potency <- df_temp$potency + insert_df_temp$insert_y

df_temp$batch <- plyr::revalue(df_temp$batch, c("1"="X", "2"="Y", "3"="Z"))

# Check that everything is correct
head(df_temp, 20)
table(df_temp$insert, df_temp$month, df_temp$batch) # Batches X,Y,Z, all have 3 observations per insert (a-d), per month (0-24)


ggplot(data      = df_temp,
       aes(x     = month,
           y     = potency,
           col   = insert,
           group = insert)) +
  geom_point(size = 1.2) +
  ylab(TeX("Potency $(\\log_{10}IU/mL)$")) +
  xlab("Month") +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_grid(. ~ batch) +
  theme_bw()




lm_temp <- lmer(potency ~ month + insert + (month|batch), data = df_temp)
summary(lm_temp)


# Remove data when necessary  
df_temp_missing <- df_temp %>% filter(month <= 9)

