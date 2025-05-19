library(tidyverse)
library(scales)


member <- read_csv("data/member_data.csv")
product <- read_csv("data/product_data.csv")
prodgram <- read_csv("data/prodgram_data.csv")
train_trans <- read_csv("data/train_transaction_data.csv")
train_label <- read_csv("data/train_label_data.csv")


# EDA ------
library(skimr)

member %>% 
  skim_without_charts()
product %>% 
  skim_without_charts()
prodgram %>% 
  skim_without_charts()
train_trans %>% 
  skim_without_charts()
train_label %>% 
  skim_without_charts()

## Pelanggan unique
train_trans %>% 
  distinct(MemberID)

train_trans %>% 
  pull(MemberID) %>% 
  n_distinct()

## Transaksi unique
train_trans %>% 
  distinct(TransactionID)

train_trans %>% 
  pull(TransactionID) %>% 
  n_distinct()


train <- train_trans %>% 
  left_join(product, join_by(FK_PRODUCT_ID == productID)) %>% 
  left_join(prodgram, join_by(FK_PROD_GRAM_ID == prodgramID)) %>% 
  mutate(TransactionDatetime = as_datetime(TransactionDatetime), 
         bulan = month(TransactionDatetime, label = TRUE)) 


# Data Pre-processing
## Cara 1: mengganti missing value dengan nilai Price dari tabel prodgram
train_imputed <- train %>% 
  mutate(PricePerUnit = if_else(is.na(PricePerUnit), Price, PricePerUnit))

train_imputed %>% 
  skim_without_charts()



## Cara 2: mengganti missing value dengan median PricePerUnit berdasarkan 
## bulan, Source, ProductName, GrammageName
impute_price <- train %>% 
  left_join(member, join_by(MemberID == MemberID)) %>% 
  mutate(bulan = month(TransactionDatetime, label = TRUE)) %>% 
  group_by(bulan, Source, ProductName, GrammageName) %>% 
  summarise(
    median_price = median(PricePerUnit, na.rm = TRUE)
  ) %>% 
  ungroup()

train_imputed <- train %>% 
  mutate(bulan = month(TransactionDatetime, label = TRUE)) %>% 
  left_join(impute_price, join_by(bulan, Source, ProductName, GrammageName)) %>% 
  mutate(PricePerUnit = if_else(is.na(PricePerUnit), median_price, PricePerUnit), 
         PricePerUnit = if_else(is.na(PricePerUnit), Price, PricePerUnit)) %>% 
  select(-median_price, -Price)

train_imputed %>% 
  skim_without_charts()

# Feature Engineering 

## Hitung total price per product yang dibeli
train_imputed <- train_imputed %>% 
  mutate(total_price = Qty*PricePerUnit)

## Recency: berapa hari sejak terakhir pelanggan belanja?
hari_ini <- as_date("2021-07-01")

recency <- train_imputed %>% 
  group_by(MemberID) %>% 
  summarise(last_trx = max(TransactionDatetime)) %>% 
  mutate(recency = interval(as_date(last_trx), hari_ini)/ddays()) %>% 
  select(-last_trx)

recency %>% 
  summary()

rh <- recency %>% 
  ggplot(aes(x = recency)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "grey") + 
  labs(x = "Recency", 
       y = "Frequency Member")
rh

rb <- recency %>% 
  ggplot(aes(x = recency, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  labs(x = "Recency", 
       y = "")
rb

library(gridExtra)

grid.arrange(
  rh + 
    scale_x_continuous(limits = c(-10, 400)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  rb + 
    scale_x_continuous(limits = c(-10, 400)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Frekuensi: frekuensi belanja dalam setahun terakhir
freq <- train_imputed %>% 
  distinct(MemberID, TransactionID) %>% 
  count(MemberID, name = "frequency")

freq %>% 
  summary()

fh <- freq %>% 
  ggplot(aes(x = frequency)) + 
  geom_histogram(binwidth = 2, fill = "steelblue", color = "grey") + 
  labs(x = "Frequency", 
       y = "Frequency Member")
fh

fb <- freq %>% 
  ggplot(aes(x = frequency, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  labs(x = "Frequency", 
       y = "")
fb

grid.arrange(
  fh + 
    scale_x_continuous(limits = c(-10, 100)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  fb + 
    scale_x_continuous(limits = c(-10, 100)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Monetary: total spending amount
monetary <- train_imputed %>% 
  group_by(MemberID) %>% 
  summarise(
    total_amount = sum(total_price)
  )

monetary %>% 
  summary()

mh <- monetary %>% 
  ggplot(aes(x = total_amount)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Monetary", 
       y = "Frequency Member")
mh

mb <- monetary %>% 
  ggplot(aes(x = total_amount, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(x = "Monetary", 
       y = "")
mb

grid.arrange(
  mh + 
    scale_x_continuous(limits = c(-10, 4e+7)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  mb + 
    scale_x_continuous(labels = comma_format(), 
                       limits = c(-10, 4e+7)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Average Monetary: rata-rata spending amount per transaksi
avg_monetary <- train_imputed %>% 
  group_by(MemberID, TransactionID) %>% 
  summarise(
    total_amount = sum(total_price)
  ) %>% 
  group_by(MemberID) %>% 
  summarise(
    avg_monetary = mean(total_amount, na.rm = TRUE)
  )

avg_monetary %>% 
  summary()


amh <- avg_monetary %>% 
  ggplot(aes(x = avg_monetary)) + 
  geom_histogram(binwidth = 100000, fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Avg Monetary", 
       y = "Frequency Member")
amh

amb <- avg_monetary %>% 
  ggplot(aes(x = avg_monetary, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(x = "Avg Monetary", 
       y = "")
amb

grid.arrange(
  amh + 
    scale_x_continuous(limits = c(-10, 4e+6)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  amb + 
    scale_x_continuous(labels = comma_format(), 
                       limits = c(-10, 4e+6)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Monthly Monetary: rata-rata spending amount per bulan
monthly_monetary <- train_imputed %>% 
  group_by(MemberID, bulan) %>% 
  summarise(
    total_amount = sum(total_price)
  ) %>% 
  group_by(MemberID) %>% 
  summarise(
    monthly_monetary = mean(total_amount, na.rm = TRUE)
  )

monthly_monetary %>% 
  summary()

mmh <- monthly_monetary %>% 
  ggplot(aes(x = monthly_monetary)) + 
  geom_histogram(binwidth = 100000, fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Avg Monthly Monetary", 
       y = "Frequency Member")
mmh

mmb <- monthly_monetary %>% 
  ggplot(aes(x = monthly_monetary, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(x = "Avg Monthly Monetary", 
       y = "")
mmb

grid.arrange(
  mmh + 
    scale_x_continuous(limits = c(-10, 1.3e+7)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  mmb + 
    scale_x_continuous(labels = comma_format(), 
                       limits = c(-10, 1.3e+7)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Monthly Frequency: rata-rata frekuensi per bulan
monthly_freq <- train_imputed %>% 
  distinct(MemberID, TransactionID, bulan) %>% 
  count(MemberID, bulan) %>% 
  group_by(MemberID) %>% 
  summarise(
    monthly_freq = mean(n, na.rm = TRUE)
  )

monthly_freq %>% 
  summary()

mfh <- monthly_freq %>% 
  ggplot(aes(x = monthly_freq)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Avg Monthly Frequency", 
       y = "Frequency Member")
mfh

mfb <- monthly_freq %>% 
  ggplot(aes(x = monthly_freq, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(x = "Avg Monthly Frequency", 
       y = "")
mfb

grid.arrange(
  mfh + 
    scale_x_continuous(limits = c(-1, 30)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  mfb + 
    scale_x_continuous(labels = comma_format(), 
                       limits = c(-1, 30)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))

## Interpurchase: rata-rata berapa hari jeda antar transaksi 
freq_gt1 <- freq %>% 
  filter(frequency > 1)

interpurchase <- train_imputed %>%
  inner_join(freq_gt1, join_by(MemberID == MemberID)) %>% 
  mutate(TransactionDate = as_date(TransactionDatetime)) %>% 
  distinct(MemberID, TransactionDate) %>% 
  arrange(MemberID, TransactionDate) %>% 
  group_by(MemberID) %>% 
  mutate(next_purchase = lead(TransactionDate)) %>% 
  ungroup() %>% 
  mutate(next_purchase_day = interval(TransactionDate, next_purchase)/ddays()) %>% 
  # filter(MemberID == "0003a82569e3202cfecc55d987da0f70") %>% print(n = Inf)
  # filter(!is.na(next_purchase_day)) %>% 
  group_by(MemberID) %>% 
  summarise(
    interpurchase = mean(next_purchase_day, na.rm = TRUE)
  )

interpurchase %>% 
  summary()

interpurchase %>% 
  distinct(MemberID)


inth <- interpurchase %>% 
  ggplot(aes(x = interpurchase)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Interpurchase", 
       y = "Frequency Member")
inth

intb <- interpurchase %>% 
  ggplot(aes(x = interpurchase, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(x = "Interpurchase", 
       y = "")
intb

grid.arrange(
  inth + 
    scale_x_continuous(limits = c(-10, 400)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  intb + 
    scale_x_continuous(labels = comma_format(), 
                       limits = c(-10, 400)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))


## Freq Last 3 Month: frekuensi belanja selama 3 bulan terakhir
freq_last3m <- train_imputed %>%
  filter(between(as_date(TransactionDatetime), hari_ini %m-% months(3), hari_ini)) %>% 
  distinct(MemberID, TransactionID) %>% 
  count(MemberID, name = "freq_last3m")

freq_last3m %>% 
  summary()

freq_last3m %>% 
  distinct(MemberID)


fl3h <- freq_last3m %>% 
  ggplot(aes(x = freq_last3m)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "grey") + 
  scale_x_continuous(breaks = seq(0, 30, by = 2)) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(x = "Frequncy Last 3 Month", 
       y = "Frequency Member")
fl3h

fl3b <- freq_last3m %>% 
  ggplot(aes(x = freq_last3m, y = "")) + 
  geom_boxplot(fill = "steelblue", color = "grey") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") + 
  scale_x_continuous(breaks = seq(0, 30, by = 2)) + 
  labs(x = "Frequncy Last 3 Month", 
       y = "")
fl3b

grid.arrange(
  fl3h + 
    scale_x_continuous(breaks = seq(0, 30, by = 2), 
                       limits = c(-1, 30)) + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank()), 
  fl3b + 
    scale_x_continuous(breaks = seq(0, 30, by = 2), 
                       labels = comma_format(), 
                       limits = c(-1, 30)) + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()), 
  ncol = 1, heights = c(0.75, 0.25))

## Tenure: Lama menjadi pelanggan
## Youngest Kid Age: usia anak paling kecil saat ini
## No Of Child: banyaknya anak
demography <- member %>% 
  transmute(MemberID, 
            tenure = interval(JoinDate, hari_ini)/dmonths(), 
            youngest_kid_age = interval(YoungestKidDOB, hari_ini)/dmonths(), 
            NoOfChild)

train_imputed %>% 
  distinct(MemberID) %>% 
  inner_join(demography, join_by(MemberID)) %>% 
  inner_join(freq, join_by(MemberID)) %>% 
  inner_join(recency, join_by(MemberID)) %>% 
  inner_join(monetary, join_by(MemberID)) %>% 
  inner_join(avg_monetary, join_by(MemberID)) %>% 
  inner_join(monthly_monetary, join_by(MemberID)) %>% 
  inner_join(monthly_freq, join_by(MemberID)) %>% 
  left_join(interpurchase, join_by(MemberID)) %>% 
  left_join(freq_last3m, join_by(MemberID)) %>% 
  summary()

abtx <- train_imputed %>% 
  distinct(MemberID) %>% 
  inner_join(demography, join_by(MemberID)) %>% 
  inner_join(freq, join_by(MemberID)) %>% 
  inner_join(recency, join_by(MemberID)) %>% 
  inner_join(monetary, join_by(MemberID)) %>% 
  inner_join(avg_monetary, join_by(MemberID)) %>% 
  inner_join(monthly_monetary, join_by(MemberID)) %>% 
  inner_join(monthly_freq, join_by(MemberID)) %>% 
  left_join(interpurchase, join_by(MemberID)) %>% 
  left_join(freq_last3m, join_by(MemberID)) %>% 
  mutate(interpurchase = if_else(is.na(interpurchase), 0, interpurchase), 
         freq_last3m = if_else(is.na(freq_last3m), 0, freq_last3m))

abtx %>% 
  summary()

abt <- abtx %>% 
  inner_join(train_label, join_by(MemberID))

abt %>% 
  summary()


# Correlation
library(ggcorrplot)

num_vars <- abtx %>% 
  select(-MemberID)

cor_matrix <- num_vars %>% 
  cor()

pvmat <- num_vars %>% 
  cor_pmat()

cor_matrix %>% 
  ggcorrplot(p.mat = pvmat, lab = TRUE)

round(cor_matrix["monthly_monetary", "avg_monetary"], 5)

abt %>% 
  write_csv("data/abt.csv")
