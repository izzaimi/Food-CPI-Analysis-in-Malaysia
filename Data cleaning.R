# FYP: Peramalan Inflasi Harga Makanan Malaysia (ARIMAX, SARIMAX, LSTM)
# Data Cleaning
# Tempoh analisis : Januari 1998 - Disember 2025 (28 tahun, 336 bulan)
# Data tahun 1997 : digunakan hanya untuk kira YoY bagi USD/MYR,
#                   Brent dan FAO (YoY Jan 1998 perlukan nilai Jan 1997)
# Aliran kerja: baca -> bersihkan -> tukar ke bulanan -> kira YoY
#               (setiap dataset satu per satu) -> gabung -> simpan

library(tidyverse)   # dplyr, readr, ggplot2, tidyr
library(readxl)      # baca fail Excel (.xlsx / .xls)
library(lubridate)   # urus tarikh
library(janitor)     # clean_names() -> nama lajur kemas
library(skimr)       # ringkasan statistik

fail_cpi   <- "data_raw/cpi_2d_inflation (1).csv"
fail_myr   <- "data_raw/exchange-rates (4).csv"
fail_brent <- "data_raw/POILBREUSDM (1).csv"
fail_fao   <- "data_raw/food_price_indices_data.csv"

tarikh_mula   <- as.Date("1998-01-01")   # mula tempoh analisis
tarikh_mula_x <- as.Date("1997-01-01")   # mula data luaran (untuk kira YoY)
tarikh_akhir  <- as.Date("2025-12-01")   # akhir tempoh analisis

dir.create("data_clean", showWarnings = FALSE)

# (a) Semak struktur, jenis data, stats ringkas, missing value & duplikat
semak_data <- function(df, nama) {
  cat("\n================", nama, "================\n")
  cat("Dimensi:", nrow(df), "baris x", ncol(df), "lajur\n\n")
  glimpse(df)
  print(summary(df))
  cat("\nMissing value setiap lajur:\n")
  print(colSums(is.na(df)))
  cat("\nBilangan baris duplikat:", sum(duplicated(df)), "\n")
}

# (b) Pastikan tiada bulan yang tertinggal.
#     PENTING: lag(x, 12) mengira ikut BARIS, bukan ikut tarikh.
#     Kalau ada bulan hilang, YoY akan dibandingkan dengan bulan yang salah.
semak_bulan_lengkap <- function(df, mula, akhir, nama) {
  bulan_penuh  <- seq(mula, akhir, by = "month")
  bulan_hilang <- as.Date(setdiff(bulan_penuh, df$date), origin = "1970-01-01")
  if (length(bulan_hilang) > 0) {
    print(bulan_hilang)
    stop(paste("Ada bulan yang hilang dalam data", nama, "- semak data dulu!"))
  }
  cat("OK:", nama, "lengkap", length(bulan_penuh), "bulan\n")
}

# (c) Kira YoY (%) = (nilai bulan ini - nilai 12 bulan lepas) / nilai 12 bulan lepas x 100
kira_yoy <- function(x) {
  (x - lag(x, 12)) / lag(x, 12) * 100
}

# 1. INFLASI HARGA MAKANAN (DOSM) - sudah dalam bentuk YoY
cpi_raw <- read_csv(fail_cpi) %>% clean_names()
semak_data(cpi_raw, "CPI mentah")

unique(cpi_raw$division)   # semak kod; makanan & minuman bukan alkohol = "01"

cpi <- cpi_raw %>%
  filter(division == "01") %>%
  mutate(date = floor_date(as.Date(date), "month")) %>%
  select(date, food_inflation = inflation_yoy) %>%      # sudah YoY, tak perlu kira
  filter(date >= tarikh_mula, date <= tarikh_akhir) %>% # terus 1998-2025
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

semak_bulan_lengkap(cpi, tarikh_mula, tarikh_akhir, "CPI")
semak_data(cpi, "CPI bersih")

head(cpi)

# 2. USD/MYR (BNM, HARIAN) -> BULANAN -> YoY
myr_raw <- read_csv(fail_myr) %>% clean_names()
semak_data(myr_raw, "USD/MYR harian mentah")
names(myr_raw)
head(myr_raw)

# a) Bersihkan data harian
myr_harian <- myr_raw %>%
  select(date = 1, usdmyr = usd) %>%
  mutate(
    date   = as.Date(parse_date_time(date, orders = c("dmy", "ymd"))),
    usdmyr = as.numeric(usdmyr)
  ) %>%
  filter(!is.na(date), !is.na(usdmyr), usdmyr > 0) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

head(myr_harian)

# Langkah 2b: harian -> bulanan (purata bulanan / period average)
myr <- myr_harian %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(
    usdmyr        = mean(usdmyr),
    hari_dagangan = n(),
    .groups = "drop"
  ) %>%
  filter(date >= tarikh_mula_x, date <= tarikh_akhir)   # 1997-2025

myr %>% filter(hari_dagangan < 15)   # bulan yang datanya terlalu sedikit (patut kosong)

# Langkah 2c: kira YoY, kemudian buang tahun 1997
semak_bulan_lengkap(myr, tarikh_mula_x, tarikh_akhir, "USD/MYR")

myr <- myr %>%
  arrange(date) %>%
  mutate(usdmyr_yoy = kira_yoy(usdmyr)) %>%
  filter(date >= tarikh_mula) %>%                        # tinggal 1998-2025
  select(date, usdmyr, usdmyr_yoy)

semak_data(myr, "USD/MYR bulanan bersih")

head(myr)

# 3. HARGA MINYAK BRENT (FRED, sudah bulanan) -> YoY
brent_raw <- read_csv(fail_brent) %>% clean_names()
semak_data(brent_raw, "Brent mentah")

brent <- brent_raw %>%
  rename(date = 1, brent = 2) %>%                 # lajur 1 = tarikh, lajur 2 = harga
  mutate(
    date  = floor_date(as.Date(date), "month"),
    brent = as.numeric(brent)
  ) %>%
  filter(date >= tarikh_mula_x, date <= tarikh_akhir) %>%   # 1997-2025
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

semak_bulan_lengkap(brent, tarikh_mula_x, tarikh_akhir, "Brent")

brent <- brent %>%
  mutate(brent_yoy = kira_yoy(brent)) %>%
  filter(date >= tarikh_mula)                     # tinggal 1998-2025

semak_data(brent, "Brent bersih")

# 4. INDEKS HARGA MAKANAN FAO (sudah bulanan) -> YoY
# Fail FAO ada beberapa baris tajuk di atas. Ubah 'skip' ikut fail anda.
fao_raw <- read_csv(fail_fao, skip = 2) %>% clean_names()
semak_data(fao_raw, "FAO mentah")

fao <- fao_raw %>%
  select(date = 1, fao_fpi = 2) %>%               # lajur 2 = Food Price Index keseluruhan
  mutate(
    date    = floor_date(ym(date), "month"),      # format "1997-01"; ubah jika berbeza
    fao_fpi = as.numeric(fao_fpi)
  ) %>%
  filter(!is.na(date), date >= tarikh_mula_x, date <= tarikh_akhir) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

semak_bulan_lengkap(fao, tarikh_mula_x, tarikh_akhir, "FAO")

fao <- fao %>%
  mutate(fao_yoy = kira_yoy(fao_fpi)) %>%
  filter(date >= tarikh_mula)                     # tinggal 1998-2025

semak_data(fao, "FAO bersih")

# 5.Gabung semua data dan semak
data_final <- cpi %>%
  left_join(myr,   by = "date") %>%
  left_join(brent, by = "date") %>%
  left_join(fao,   by = "date")

semak_data(data_final, "DATA GABUNGAN")

stopifnot(nrow(data_final) == 336)                # 28 tahun x 12 bulan
stopifnot(sum(is.na(data_final)) == 0)            # tiada NA langsung
semak_bulan_lengkap(data_final, tarikh_mula, tarikh_akhir, "DATA GABUNGAN")

# Semakan manual: YoY USD/MYR Jan 1998 patut = (Jan 1998 - Jan 1997) / Jan 1997 x 100
head(data_final)

# Lajur dalam fail akhir:
# date            : tarikh (hari pertama setiap bulan)
# food_inflation  : inflasi harga makanan YoY (%)  <- pembolehubah sasaran
# usdmyr, brent, fao_fpi           : nilai asal (untuk plot trend / EDA)
# usdmyr_yoy, brent_yoy, fao_yoy   : YoY (%) (untuk korelasi, CCF & model)

write_csv(data_final, "data_clean/data_bulanan_1998_2025.csv")
cat("\nSiap! Data bersih disimpan di data_clean/data_bulanan_1998_2025.csv\n")