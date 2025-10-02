#Pulling and unpacking nhanesA XPT files
# xxw673
install.packages("nhanesA")
install.packages("janitor")
install.packages("readr")
install.packages("dplyr")
install.packages("here")

library(nhanesA) 
library(janitor) 
library(dplyr)
library(readr)
# created a function to pull within the nhanesA package
pull <- function(tab) nhanes(tab) |> clean_names()

demo <- pull("P_DEMO")  # Demographics
hdl  <- pull("P_HDL")  #HDL
fsq  <- pull("P_FSQ")  #Food Security Questionaire

# detect HDL columns robustly
nm <- names(hdl)
hdl_mg <- intersect(tolower(nm), c("lbxhdd","lbdhdd","lbxhdl","lbxhdl_mgdl"))
hdl_si <- intersect(tolower(nm), c("lbdhddsi","lbdhdsi","lbdhdds","lbdhdl_si"))
mg_col <- if (length(hdl_mg)) hdl_mg[[1]] else NA_character_
si_col <- if (length(hdl_si)) hdl_si[[1]] else NA_character_

hdl_keep <- hdl |>
  select(seqn, any_of(c(mg_col, si_col))) |>
  rename(hdl = !!mg_col, hdl_si = !!si_col)

dat_raw <- demo |>
  dplyr::mutate(ridstatr_lab = as.character(ridstatr)) |>
  select(seqn, ridageyr, riagendr, ridreth3, dmdeduc2, indfmpir,
         wtmecprp, sdmvpsu, sdmvstra) |>                   # <- add weights/design
  left_join(hdl_keep, by = "seqn") |>
  left_join(fsq, by = "seqn")                               # FSQ adds FSDHH/FSDAD + SNAP/WIC

#create in files
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
write_csv(dat_raw, "data/raw/nhanes_raw.csv")
saveRDS(dat_raw, "data/raw/nhanes_raw.rds")
