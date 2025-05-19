library(tidyverse)
library(tidymodels)
library(tidyclust)

member <- read_csv("data/member_data.csv")
product <- read_csv("data/product_data.csv")
prodgram <- read_csv("data/prodgram_data.csv")
test_trans <- read_csv("data/test_transaction_data.csv")
test_label <- read_csv("data/test_label_data.csv")


test <- test_trans %>% 
  left_join(product, join_by(FK_PRODUCT_ID == productID)) %>% 
  left_join(prodgram, join_by(FK_PROD_GRAM_ID == prodgramID)) %>% 
  mutate(TransactionDatetime = as_datetime(TransactionDatetime), 
         bulan = month(TransactionDatetime, label = TRUE)) 

impute_price <- test %>% 
  left_join(member, join_by(MemberID == MemberID)) %>% 
  mutate(bulan = month(TransactionDatetime, label = TRUE)) %>% 
  group_by(bulan, Source, ProductName, GrammageName) %>% 
  summarise(
    median_price = median(PricePerUnit, na.rm = TRUE)
  ) %>% 
  ungroup()

test_imputed <- test %>% 
  mutate(bulan = month(TransactionDatetime, label = TRUE)) %>% 
  left_join(impute_price, join_by(bulan, Source, ProductName, GrammageName)) %>% 
  mutate(PricePerUnit = if_else(is.na(PricePerUnit), median_price, PricePerUnit), 
         PricePerUnit = if_else(is.na(PricePerUnit), Price, PricePerUnit)) %>% 
  select(-median_price, -Price)

test_imputed <- test_imputed %>% 
  mutate(total_price = Qty*PricePerUnit)

## Recency: berapa hari sejak terakhir pelanggan belanja?
hari_ini <- as_date("2021-07-01")

recency <- test_imputed %>% 
  group_by(MemberID) %>% 
  summarise(last_trx = max(TransactionDatetime)) %>% 
  mutate(recency = interval(as_date(last_trx), hari_ini)/ddays()) %>% 
  select(-last_trx)


## Frekuensi: frekuensi belanja dalam setahun terakhir
freq <- test_imputed %>% 
  distinct(MemberID, TransactionID) %>% 
  count(MemberID, name = "frequency")


## Monetary: total spending amount
monetary <- test_imputed %>% 
  group_by(MemberID) %>% 
  summarise(
    total_amount = sum(total_price)
  )


## Average Monetary: rata-rata spending amount per transaksi
avg_monetary <- test_imputed %>% 
  group_by(MemberID, TransactionID) %>% 
  summarise(
    total_amount = sum(total_price)
  ) %>% 
  group_by(MemberID) %>% 
  summarise(
    avg_monetary = mean(total_amount, na.rm = TRUE)
  )

## Monthly Monetary: rata-rata spending amount per bulan
monthly_monetary <- test_imputed %>% 
  group_by(MemberID, TransactionID) %>% 
  summarise(
    total_amount = sum(total_price)
  ) %>% 
  group_by(MemberID) %>% 
  summarise(
    monthly_monetary = mean(total_amount, na.rm = TRUE)
  )

## Monthly Frequency: rata-rata frekuensi per bulan
monthly_freq <- test_imputed %>% 
  distinct(MemberID, TransactionID, bulan) %>% 
  count(MemberID, bulan) %>% 
  group_by(MemberID) %>% 
  summarise(
    monthly_freq = mean(n, na.rm = TRUE)
  )

## Interpurchase: rata-rata berapa hari jeda antar transaksi 
freq_gt1 <- freq %>% 
  filter(frequency > 1)

interpurchase <- test_imputed %>%
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

## Freq Last 3 Month: frekuensi belanja selama 3 bulan terakhir
freq_last3m <- test_imputed %>%
  filter(between(as_date(TransactionDatetime), hari_ini %m-% months(3), hari_ini)) %>% 
  distinct(MemberID, TransactionID) %>% 
  count(MemberID, name = "freq_last3m")

## Tenure: Lama menjadi pelanggan
## Youngest Kid Age: usia anak paling kecil saat ini
## No Of Child: banyaknya anak
demography <- member %>% 
  transmute(MemberID, 
            tenure = interval(JoinDate, hari_ini)/dmonths(), 
            youngest_kid_age = interval(YoungestKidDOB, hari_ini)/dmonths(), 
            NoOfChild)

test_imputed %>% 
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

test_abtx <- test_imputed %>% 
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

test_abtx %>% 
  summary()

test_abt <- test_abtx %>% 
  inner_join(test_label, join_by(MemberID))

