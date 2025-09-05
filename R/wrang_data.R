source("R/utils.R")

op <- options(digits.secs=4)

data_path <- here::here("raw_data")
files_name <- list.files(path = data_path, pattern="\\.csv$", full.names=T)

participants_df = readr::read_csv(files_name[length(files_name)],
                                  col_select=c(id_record,
                                               age_years,
                                               sex,
                                               diagnosis1,
                                               diagnosis2,
                                               diagnosis3,
                                               va_re_logMar,
                                               va_le_logMar),
                                  show_col_types = F)


ne_oph_disorder <- c("Inherited optic atrophy",
                     "Optic neuropathy",
                     "Nonarteritic anterior ischemic optic neuropathy",
                     "Bilateral optic nerve atrophy",
                     "Traumatic optic neuropathy",
                     "Central nervous system disorder",
                     "Toxic neuropathy",
                     "Sarcoidosis neuropathy",
                     "Retrobulbar neuritis",
                     "Pseudopapiledema",
                     "Neurological disorder right side",
                     "Ischemic optic neuropathy",
                     "Infectious neuritis",
                     "Glaucoma",
                     "External ophtalmoplegia",
                     "Bitemporal hemianopsia by acrylamide poisoning",
                     "Arterio-venous malformation in right thalamus",
                     "Affectation of the chiasm")

diagnosis <- c("Normal", ne_oph_disorder)

perg_df <- files_name[-length(files_name)] |>
  purrr::map(~readr::read_csv(., id="record_path", show_col_types=FALSE, col_select=c(TIME_1, LE_1, RE_1))) |>
  purrr::list_rbind() |>
  dplyr::mutate(id_record = stringr::str_sub(record_path, 51, 54),
                TIME_1 = lubridate::ymd_hms(TIME_1),
                sec = lubridate::second(TIME_1)) |>
  dplyr::left_join(participants_df, by="id_record") |>
  dplyr::mutate(diagnosis = dplyr::case_when(diagnosis1 %in% ne_oph_disorder |
                diagnosis2 %in% ne_oph_disorder |
                diagnosis3 %in% ne_oph_disorder ~ "Neuro-ophthalmological disorder",
                diagnosis1 == "Normal" |
                diagnosis2 == "Normal" |
                diagnosis3 == "Normal" ~ "Normal")) |>
  tidyr::drop_na(diagnosis) |>
  dplyr::group_by(id_record) |>
  dplyr::group_modify(~ .x |>
                      dplyr::mutate(sec=min_max_scaler(sec))) |>
  dplyr::select(time=sec, PERG_left_eye=LE_1, PERG_right_eye=RE_1,
                id_record=id_record, age=age_years, diagnosis=diagnosis,
                sex=sex, va_re_logMar=va_re_logMar, va_le_logMar=va_le_logMar)

write.csv(perg_df, file=here::here("data", "perg_data.csv"), row.names=FALSE)
