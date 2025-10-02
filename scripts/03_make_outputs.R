#CSV File export into Excel files
# xxw673
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
intall.packages("writexl")
install.packages("readr")

library(tidyverse)
library(readr)
library(ggplot2)
library(writexl)
library(dplyr)

clean_path <- here::here("data/clean/nhanes_clean.rds")
stopifnot(file.exists(clean_path))
d <- readRDS(clean_path)
# this is to fix an error I had with one of my variables "fs_Cat"
if (!("fs_cat" %in% names(d))) {
  if ("fs_hh" %in% names(d)) {
    d <- d %>%
      mutate(
        fs_cat = case_when(
          fs_hh %in% c("High","Marginal") ~ "Marginal/High",
          fs_hh == "Low"                  ~ "Low",
          fs_hh == "Very low"             ~ "Very low",
          TRUE ~ NA_character_
        )
      )
  } else {
    stop("fs_cat is missing and fs_hh not found. Re-run 02_clean.R and confirm it writes fs_cat into data/clean/nhanes_clean.rds.")
  }
}

# Overall
overall <- d %>%
  summarize(
    sample_n = n(),
    hdl_n    = sum(!is.na(hdl)),
    mean_hdl = mean(hdl, na.rm = TRUE),
    sd_hdl   = sd(hdl,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    mean_hdl = round(mean_hdl, 1),
    sd_hdl   = round(sd_hdl,   2)
  )

# Education order
educ_levels <- c("LessHS","HS/GED","SomeCollege","College+")
d <- d %>% mutate(educ4 = factor(educ4, levels = educ_levels))

by_educ <- d %>%
  filter(!is.na(hdl), !is.na(educ4)) %>%
  group_by(educ4, .drop = FALSE) %>%
  summarize(
    n        = n(),
    mean_hdl = mean(hdl, na.rm = TRUE),
    sd_hdl   = sd(hdl,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(educ4) %>%
  mutate(
    mean_hdl = round(mean_hdl, 1),
    sd_hdl   = round(sd_hdl,   2)
  )

#HDL by Food Security 
by_fsq <- d %>%
  filter(!is.na(hdl), !is.na(fs_cat)) %>%
  group_by(fs_cat, .drop = FALSE) %>%
  summarize(
    n        = n(),
    mean_hdl = mean(hdl, na.rm = TRUE),
    sd_hdl   = sd(hdl,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(fs_cat) %>%
  mutate(
    mean_hdl = round(mean_hdl, 1),
    sd_hdl   = round(sd_hdl,   2)
  )

# ensuring output directories
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figs",   recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/rds",    recursive = TRUE, showWarnings = FALSE)

# Excel workbook for the dashboard
writexl::write_xlsx(
  list(
    "Overall"              = overall,
    "HDL by Education"     = by_educ,
    "HDL by Food Security" = by_fsq    # <-- new sheet
  ),
  path = "outputs/tables/dashboard_tables.xlsx"
)

# Figures
p_educ <- ggplot(by_educ, aes(x = educ4, y = mean_hdl)) +
  geom_col() +
  labs(x = NULL, y = "Mean HDL (mg/dL)")
ggsave("outputs/figs/hdl_by_education.png", p_educ, width = 6, height = 3.5, dpi = 150)

p_fsq <- ggplot(by_fsq, aes(x = fs_cat, y = mean_hdl)) +
  geom_col() +
  labs(x = "Food Security", y = "Mean HDL (mg/dL)")
ggsave("outputs/figs/hdl_by_foodsecurity.png", p_fsq, width = 6, height = 3.5, dpi = 150)

saveRDS(
  list(overall = overall, by_educ = by_educ, by_fsq = by_fsq),
  "outputs/rds/dashboard_data.rds"
)