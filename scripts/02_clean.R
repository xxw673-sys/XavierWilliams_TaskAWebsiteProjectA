# Cleaning File - xxw673

install.packages("dplyr")
install.packages("readr")
install.packages("haven")
library(dplyr)
library(readr)
library(haven)
x <- readRDS("data/raw/nhanes_raw.rds")

miss_to_na <- function(v){
  if (!is.numeric(v)) return(v)
  for (code in c(77,88,99,777,888,999,7777,8888,9999)) v <- dplyr::na_if(v, code)
  v
}

dat <- x |>
  mutate(across(where(is.numeric), miss_to_na)) |>
  mutate(
    # handle labels → numeric
    dmdeduc2_num = suppressWarnings(as.numeric(haven::zap_labels(dmdeduc2))),
    riagendr_num = suppressWarnings(as.numeric(haven::zap_labels(riagendr))),
    fsdhh_num    = suppressWarnings(as.numeric(haven::zap_labels(fsdhh))),  # 1..4 if present
    
    # education 4
    educ4 = dplyr::case_when(
      dmdeduc2_num %in% c(1, 2) ~ "LessHS",
      dmdeduc2_num == 3         ~ "HS/GED",
      dmdeduc2_num == 4         ~ "SomeCollege",
      dmdeduc2_num == 5         ~ "College+",
      TRUE ~ NA_character_
    ),
    
    # sex binary (for now)
    sex_male = dplyr::case_when(
      riagendr_num == 1 ~ 1L,
      riagendr_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Food Security (household) label (common in P_FSQ as 1..4)
    fs_hh = dplyr::case_when(
      fsdhh_num == 1 ~ "High",
      fsdhh_num == 2 ~ "Marginal",
      fsdhh_num == 3 ~ "Low",
      fsdhh_num == 4 ~ "Very low",
      TRUE ~ NA_character_
    ),
    fs_cat = dplyr::case_when(       
      fs_hh %in% c("High","Marginal") ~ "Marginal/High",
      fs_hh == "Low"                  ~ "Low",
      fs_hh == "Very low"             ~ "Very low",
      TRUE ~ NA_character_
    )
  ) |>
  transmute(
    seqn,
    age = ridageyr,
    sex_male,
    race6 = ridreth3,
    educ4,
    pir = pmin(indfmpir, 5, na.rm = TRUE),
    hdl,
    fs_hh,                 # <- keep for outputs
    wtmecprp, sdmvpsu, sdmvstra
  ) |>
  filter(dplyr::between(age, 21, 79))


dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(dat, "data/clean/nhanes_clean.csv")
saveRDS(dat, "data/clean/nhanes_clean.rds")