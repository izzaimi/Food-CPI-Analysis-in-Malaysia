# Skrip 02: Analisis Eksplorasi Data (EDA)
# Objektif 1: Menganalisis trend dan corak bermusim inflasi harga makanan
#             di Malaysia bagi tempoh Januari 1998 - Disember 2025.

library(tidyverse)   # dplyr, ggplot2, readr
library(lubridate)   # urus tarikh
library(scales)      # format paksi

dir.create("output", showWarnings = FALSE)

data <- read_csv("data_clean/all_data_bulanan_1998_2025.csv") %>%
  mutate(
    date  = as.Date(date),
    tahun = year(date),
    bulan = factor(month(date), levels = 1:12,
                   labels = c("Jan", "Feb", "Mac", "Apr", "Mei", "Jun",
                              "Jul", "Ogo", "Sep", "Okt", "Nov", "Dis"))
  )

glimpse(data)   # semak: 336 baris

# Tema rajah yang kemas (latar putih, grid nipis)
warna_utama <- "#2a6fdb"   # satu warna untuk siri utama
tema_fyp <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    axis.line        = element_line(colour = "grey30", linewidth = 0.4),  # garisan paksi-x dan paksi-y
    axis.ticks       = element_line(colour = "grey30", linewidth = 0.4),  # tanda kecil pada paksi
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey40", hjust = 0)
  )

paksi_x_pada_sifar <- theme(
  axis.line.x  = element_blank(),
  axis.ticks.x = element_blank()
)

# Fungsi untuk simpan rajah (300 dpi)
simpan <- function(plot, nama, w = 8, h = 4.5) {
  ggsave(file.path("output", nama), plot, width = w, height = h, dpi = 300, bg = "white")
}

# 1. STATISTIK DESKRIPTIF
statistik <- function(x) {
  tibble(
    n             = sum(!is.na(x)),
    minimum       = min(x, na.rm = TRUE),
    kuartil_1     = quantile(x, 0.25, na.rm = TRUE),
    median        = median(x, na.rm = TRUE),
    purata        = mean(x, na.rm = TRUE),
    kuartil_3     = quantile(x, 0.75, na.rm = TRUE),
    maksimum      = max(x, na.rm = TRUE),
    sisihan_piawai = sd(x, na.rm = TRUE)
  )
}

jadual_deskriptif <- bind_rows(
  statistik(data$food_inflation)     %>% mutate(siri = "Inflasi makanan YoY (%)"),
  statistik(data$food_inflation_mom) %>% mutate(siri = "Inflasi makanan MoM (%)")
) %>%
  relocate(siri) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(jadual_deskriptif)
write_csv(jadual_deskriptif, "output/jadual_statistik_deskriptif.csv")

# Bila inflasi YoY paling tinggi dan paling rendah?
data %>% slice_max(food_inflation, n = 1) %>% select(date, food_inflation)
data %>% slice_min(food_inflation, n = 1) %>% select(date, food_inflation)

# Purata inflasi YoY mengikut tahun (berguna untuk perbincangan trend)
purata_tahunan <- data %>%
  group_by(tahun) %>%
  summarise(purata_yoy = round(mean(food_inflation), 2), .groups = "drop")
print(purata_tahunan, n = 28)
write_csv(purata_tahunan, "output/jadual_purata_tahunan.csv")

# 2. TREND: PLOT SIRI MASA INFLASI YoY + PERISTIWA PENTING
# Tempoh peristiwa (boleh ubah ikut perbincangan anda)
peristiwa <- tribble(
  ~mula,         ~akhir,        ~label,
  "1998-01-01",  "1998-12-01",  "Krisis Kewangan\nAsia",
  "2007-07-01",  "2008-12-01",  "Krisis Makanan\nGlobal",
  "2020-03-01",  "2022-12-01",  "Pandemik\nCOVID-19"
) %>%
  mutate(across(c(mula, akhir), as.Date))

rajah_trend <- ggplot(data, aes(date, food_inflation)) +
  geom_rect(data = peristiwa, inherit.aes = FALSE,
            aes(xmin = mula, xmax = akhir, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = 0.6) +
  geom_text(data = peristiwa, inherit.aes = FALSE,
            aes(x = mula + 15, y = Inf, label = label),
            hjust = 0, vjust = 1.3, size = 3, colour = "grey30", lineheight = 0.9) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +   # paksi-x pada y = 0
  geom_line(colour = warna_utama, linewidth = 0.7) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(
    title   = "Inflasi harga makanan di Malaysia, Januari 1998 - Disember 2025",
    x       = "Tahun",
    y       = "Inflasi YoY (%)",
    caption = "Sumber: OpenDOSM. Kawasan berlorek menandakan peristiwa ekonomi utama."
  ) +
  tema_fyp +
  paksi_x_pada_sifar

rajah_trend
simpan(rajah_trend, "rajah_1_trend_yoy.png")

# 3. CORAK BERMUSIM (GUNA INFLASI MoM)
# 3a. Seasonal plot: setiap tahun = satu garis kelabu nipis,
#     purata semua tahun = garis biru tebal
purata_bulanan <- data %>%
  group_by(bulan) %>%
  summarise(purata_mom = mean(food_inflation_mom), .groups = "drop")

rajah_seasonal <- ggplot(data, aes(bulan, food_inflation_mom, group = tahun)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +   # paksi-x pada y = 0
  geom_line(colour = "grey75", linewidth = 0.3) +
  geom_line(data = purata_bulanan, aes(bulan, purata_mom, group = 1),
            inherit.aes = FALSE, colour = warna_utama, linewidth = 1.2) +
  geom_point(data = purata_bulanan, aes(bulan, purata_mom),
             inherit.aes = FALSE, colour = warna_utama, size = 2.2) +
  labs(
    title   = "Corak bermusim inflasi harga makanan bulan ke bulan (MoM)",
    x       = "Bulan",
    y       = "Inflasi MoM (%)",
    caption = "Garis kelabu: setiap tahun 1998-2025. Garis biru: purata bagi setiap bulan."
  ) +
  tema_fyp +
  paksi_x_pada_sifar

rajah_seasonal
simpan(rajah_seasonal, "rajah_2_seasonal_plot_mom.png")

# 3b. Boxplot inflasi MoM mengikut bulan
rajah_boxplot <- ggplot(data, aes(bulan, food_inflation_mom)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +   # paksi-x pada y = 0
  geom_boxplot(fill = "#dbe7fb", colour = warna_utama, width = 0.6,
               outlier.size = 1, outlier.colour = "grey40") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2.5, colour = "black") +
  labs(
    title   = "Taburan inflasi harga makanan MoM mengikut bulan",
    x       = "Bulan",
    y       = "Inflasi MoM (%)",
    caption = "Kotak: julat antara kuartil; garis tengah: median; berlian hitam: purata."
  ) +
  tema_fyp +
  paksi_x_pada_sifar

rajah_boxplot
simpan(rajah_boxplot, "rajah_3_boxplot_mom.png")

# Jadual purata & median MoM mengikut bulan (untuk dilaporkan dalam Bab 4)
jadual_bulanan <- data %>%
  group_by(bulan) %>%
  summarise(
    purata_mom = round(mean(food_inflation_mom), 3),
    median_mom = round(median(food_inflation_mom), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(purata_mom))
print(jadual_bulanan)
write_csv(jadual_bulanan, "output/jadual_mom_mengikut_bulan.csv")

# 3c. Ujian kehadiran corak bermusim: Kruskal-Wallis
#     H0: taburan inflasi MoM adalah sama bagi semua bulan (tiada corak bermusim)
#     Jika p < 0.05 -> tolak H0 -> wujud corak bermusim yang signifikan
ujian_kw <- kruskal.test(food_inflation_mom ~ bulan, data = data)
print(ujian_kw)

# 3d. Dekomposisi STL bagi inflasi MoM
rajah_stl <- ggplot(stl_df, aes(date, nilai)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +   # paksi-x pada y = 0 bagi setiap panel
  geom_line(colour = warna_utama, linewidth = 0.4) +
  facet_wrap(~ komponen, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Dekomposisi STL inflasi harga makanan MoM",
    x = "Tahun", y = "Inflasi MoM (%)"
  ) +
  tema_fyp +
  paksi_x_pada_sifar +
  theme(strip.placement = "outside", strip.text = element_text(face = "bold"))

rajah_stl
simpan(rajah_stl, "rajah_4_stl_mom.png", h = 7)

# Kekuatan komponen bermusim (Wang, Smith & Hyndman, 2006):
#   F_s = max(0, 1 - Var(baki) / Var(bermusim + baki))
#   Nilai hampir 0 = bermusim lemah; hampir 1 = bermusim kuat
kekuatan_bermusim <- function(stl_obj) {
  s <- stl_obj$time.series[, "seasonal"]
  r <- stl_obj$time.series[, "remainder"]
  max(0, 1 - var(r) / var(s + r))
}
cat("Kekuatan bermusim (MoM):", round(kekuatan_bermusim(stl_mom), 3), "\n")

# 3e. Perbandingan: STL bagi siri YoY (dijangka komponen bermusim sangat lemah)
yoy_ts  <- ts(data$food_inflation, start = c(1998, 1), frequency = 12)
stl_yoy <- stl(yoy_ts, s.window = "periodic")
cat("Kekuatan bermusim (YoY):", round(kekuatan_bermusim(stl_yoy), 3), "\n")