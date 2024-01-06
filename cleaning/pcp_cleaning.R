
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "ggplot2",
             "tidylog",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "writexl")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

setwd('/store/part4/Medicaid')
#setwd("C:/Users/shiroa1/OneDrive - Vanderbilt/PCP-prognosis/input")

#patient_data <- read.csv('2023102919_1_Patient_data_031_HCEI.csv.gz', fileEncoding='cp932')
#ef1 <- read.csv('2023102919_71_DPC_FF1_data_031_HCEI.csv.gz', fileEncoding='cp932')
#medication_list <- read.csv('2023102919_101_Drug_codelist_031_HCEI.csv.gz', fileEncoding='cp932')

pcp <- readRDS("inpatient/pcp.rds")
pcp_medication <- readRDS("medication/pcp_medication.rds")
pcp_laboratory <- readRDS("laboratory/pcp_laboratory.rds")
pcp_procedure <- readRDS("procedure/pcp_procedure.rds")

select_dpc <- function(data_to, code, payload, name){
  data <- ef1 %>% 
    arrange(`患者ID`, `入院日`) %>% 
    select(`患者ID`, `入院日`, `コード`, `ペイロード番号`, `データ`) %>%  
    filter(`コード` == code, `ペイロード番号` == payload) %>% 
    distinct(`患者ID`, `入院日`, .keep_all=TRUE) %>% 
    pivot_wider(names_from = c(`コード`, `ペイロード番号`),
                values_from = `データ`,
                values_fill = list(value = NA_character_)) 
  data <- data %>% rename(!!name := colnames(data)[3])
  data_to <- left_join(data_to, data, by = c("患者ID", "入院日"))
}

select_dpc2 <- function(data_to, code, payload, serial, name){
  data <- ef1 %>% 
    arrange(`患者ID`, `入院日`) %>% 
    select(`患者ID`, `入院日`, `コード`, `ペイロード番号`, `連番`, `データ`) %>%  
    filter(`コード` == code, `ペイロード番号` == payload, `連番` == serial) %>% 
    distinct(`患者ID`, `入院日`, .keep_all=TRUE) %>% 
    pivot_wider(names_from = c(`コード`, `ペイロード番号`, `連番`),
                values_from = `データ`,
                values_fill = list(value = NA_character_)) 
  data <- data %>% rename(!!name := colnames(data)[3])
  data_to <- left_join(data_to, data, by = c("患者ID", "入院日"))
}

procedure_detect_before <- function(procedure_list, days, newname){
  
  code_filter <- procedure_list %>% 
    dplyr::select(`診療行為コード`) %>% 
    drop_na(`診療行為コード`) %>% 
    pull(`診療行為コード`) %>% 
    str_c(., collapse = "|")
  
  .data <- pcp_procedure %>% 
    filter(str_detect(診療行為コード, code_filter)) 
  
  .data <- left_join(.data,
                     subset(anti_pcp_medication, select=c(`患者ID`, `入院日`)),
                     by = "患者ID") %>% 
    filter(ymd(`入院日`) - ymd(`対象日`) > 0  & ymd(`入院日`) - ymd(`対象日`) <= days) 
  
  ids <- .data %>%
    unite('new_id', c('患者ID', '入院日')) %>% 
    distinct(new_id) %>%
    pull()
  
  anti_pcp_medication <<- anti_pcp_medication %>% 
    mutate(toss = if_else(new_id %in% ids, 1, 0)) %>% 
    rename(!!newname := toss)
}

procedure_detect_after <- function(procedure_list, days, newname){
  
  code_filter <- procedure_list %>% 
    dplyr::select(`診療行為コード`) %>% 
    drop_na(`診療行為コード`) %>% 
    pull(`診療行為コード`) %>% 
    str_c(., collapse = "|")
  
  .data <- pcp_procedure %>% 
    filter(str_detect(診療行為コード, code_filter)) 
  
  .data <- left_join(.data,
                     subset(anti_pcp_medication, select=c(`患者ID`, `入院日`, disc_date)),
                     by = "患者ID") %>% 
    filter(ymd(`対象日`) - ymd(`入院日`)  >= -1  & ymd(`対象日`) - ymd(`入院日`) <= days & ymd(`対象日`) < ymd(disc_date)) 
  
  ids <- .data %>%
    unite('new_id', c('患者ID', '入院日')) %>% 
    distinct(new_id) %>%
    pull()
  
  anti_pcp_medication <<- anti_pcp_medication %>% 
    mutate(toss = if_else(new_id %in% ids, 1, 0)) %>% 
    rename(!!newname := toss)
}

# Patient selection -------------------------------------------------------

anti_pcp_atc <- c('J01EE01', 'P01AX06', 'P01CX01')

code_filter <- medication_list %>% 
  filter(str_detect(`WHO.ATCコード`, paste(anti_pcp_atc, collapse = "|"))) %>% 
  select(`薬価基準収載医薬品コード`) %>% 
  drop_na(`薬価基準収載医薬品コード`) %>% 
  pull(`薬価基準収載医薬品コード`) %>% 
  str_c(., collapse = "|")

anti_pcp <- pcp_medication %>% 
  filter(str_detect(薬価コード, code_filter))

anti_pcp <- anti_pcp %>% 
  rename(medication_start = `開始日`,
         medication_end = `終了日`)

anti_pcp_medication <- left_join(anti_pcp, pcp, by = c('患者ID')) 

anti_pcp_medication <- anti_pcp_medication %>% 
  filter(ymd(`入院日`)-1 <= ymd(medication_start) & ymd(medication_start) <= ymd(`入院日`)+1) %>% 
  distinct(`患者ID`, `入院日`) %>% 
  group_by(`患者ID`) %>% 
  arrange(ymd(`入院日`)) %>% 
  slice(1) %>% 
  ungroup()

ids <- anti_pcp_medication %>% 
  distinct(`患者ID`) %>% 
  pull()

prophylaxis <- left_join(anti_pcp, pcp, by = c('患者ID')) 

prophylaxis <- prophylaxis %>% 
  filter(ymd(`入院日`)-60 <= ymd(medication_start) & ymd(medication_start) < ymd(`入院日`)-1) %>% 
  distinct(`患者ID`) %>% 
  pull()

anti_pcp_medication <- anti_pcp_medication %>% 
  mutate(prophylaxis = if_else(`患者ID` %in% prophylaxis, 1, 0))

anti_pcp_medication <- anti_pcp_medication %>% 
  unite('new_id', c('患者ID', '入院日'), remove = FALSE)

# DPC ---------------------------------------------------------------------

patient_data <- patient_data %>% 
  filter(`患者ID` %in% ids)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., patient_data, by = '患者ID')

id_key <- anti_pcp_medication %>% 
  distinct(`患者ID`, `入院日`) 

select_dpc(id_key, "A000010", "1", "birth_date") -> id_key  
select_dpc(id_key, "A000010", "2", "sex") -> id_key  
select_dpc(id_key, "A000030", "1", "disc_date") -> id_key  
select_dpc(id_key, "A000030", "2", "disc_to") -> id_key  
select_dpc(id_key, "A000030", "3", "disc_prognosis") -> id_key   
select_dpc(id_key, "A000030", "4", "death24h") -> id_key   

select_dpc2(id_key, "A006040", "9", "1", "com1") -> id_key   
select_dpc2(id_key, "A006040", "9", "2", "com2") -> id_key   
select_dpc2(id_key, "A006040", "9", "3", "com3") -> id_key   
select_dpc2(id_key, "A006040", "9", "4", "com4") -> id_key   
select_dpc2(id_key, "A006040", "9", "5", "com5") -> id_key   
select_dpc2(id_key, "A006040", "9", "6", "com6") -> id_key   
select_dpc2(id_key, "A006040", "9", "7", "com7") -> id_key   
select_dpc2(id_key, "A006040", "9", "8", "com8") -> id_key   
select_dpc2(id_key, "A006040", "9", "9", "com9") -> id_key   
select_dpc2(id_key, "A006040", "9", "10", "com10") -> id_key

select_dpc2(id_key, "A006050", "9", "1", "subs1") -> id_key 
select_dpc2(id_key, "A006050", "9", "2", "subs2") -> id_key
select_dpc2(id_key, "A006050", "9", "3", "subs3") -> id_key
select_dpc2(id_key, "A006050", "9", "4", "subs4") -> id_key
select_dpc2(id_key, "A006050", "9", "5", "subs5") -> id_key
select_dpc(id_key, "A001010", "R1", "bmi") -> id_key

select_dpc(id_key, "ADL0010", "2", "adm_adl") -> id_key
select_dpc(id_key, "ADL0020", "2", "disc_adl") -> id_key
select_dpc(id_key, "JCS0010", "2", "adm_jcs") -> id_key
select_dpc(id_key, "JCS0020", "2", "disc_jcs") -> id_key
select_dpc(id_key, "M040010", "2", "hugh_johns") -> id_key
select_dpc(id_key, "M040020", "2", "severity_score") -> id_key
select_dpc(id_key, "M040020", "3", "cap_hap")-> id_key

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., id_key, by = c('患者ID', '入院日'))

anti_pcp_medication <- anti_pcp_medication %>% 
  mutate(bun_cat = str_sub(severity_score, 1, -7),
         spo2 = str_sub(severity_score, 2, -6),
         ams = str_sub(severity_score, 3, -5),
         sbp = str_sub(severity_score, 4, -4),
         immunodef = str_sub(severity_score, 5, -3),
         crp = str_sub(severity_score, 6, -2),
         dev_place = str_sub(severity_score, 7, -1),
         observation_period = as.numeric(ymd(`観察期間終了日.EMR.`) - ymd(`入院日`) + 1),
         time_to_death = as.numeric(ymd(`死亡日`) - ymd(`入院日`) + 1),
         death30 = case_when(time_to_death <= 30 ~ 1,
                             30 < observation_period & (30 < time_to_death | is.na(time_to_death)) ~ 0,
                             observation_period <= 30 & is.na(time_to_death) ~ 2),
         death90 = case_when(time_to_death <= 90 ~ 1,
                             90 < observation_period & (90 < time_to_death | is.na(time_to_death)) ~ 0,
                             observation_period <= 90 & is.na(time_to_death) ~ 2))

# Laboratory --------------------------------------------------------------

# beta-d

beta_d <- pcp_laboratory %>% 
  filter(`検査名` == '(1→3)-b -Dグルカン;β-D-glu') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`))

beta_d %>% write_rds('beta_d_after.rds')

beta_d_baseline <- beta_d %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(beta_d_baseline = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., beta_d_baseline, by = c('患者ID', '入院日'))

# ldh

ldh <- pcp_laboratory %>% 
  filter(`検査名` == '乳酸脱水素酵素;LDH') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`))

ldh %>% write_rds('ldh_after.rds')

ldh_baseline <- ldh %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(ldh_baseline = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., ldh_baseline, by = c('患者ID', '入院日'))

# ldh2

ldh2 <- pcp_laboratory %>% 
  filter(`検査名` == '乳酸脱水素酵素;LDH(IFCC)') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`))

ldh2 %>% write_rds('ldh_after2.rds')

ldh_baseline2 <- ldh2 %>% 
  filter(ymd(`入院日`)-1 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(ldh_baseline2 = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., ldh_baseline2, by = c('患者ID', '入院日'))

# hbaic
hbaic <- pcp_laboratory %>% 
  filter(`検査名` == 'HbA1c') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-30 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(hbaic = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., hbaic, by = c('患者ID', '入院日'))

hbaic2 <- pcp_laboratory %>% 
  filter(`検査名` == 'HbA1c(JDS)') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-30 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(hbaic2 = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., hbaic2, by = c('患者ID', '入院日'))

hbaic3 <- pcp_laboratory %>% 
  filter(`検査名` == 'HbA1c(NGSP)') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-30 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(hbaic3 = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., hbaic3, by = c('患者ID', '入院日'))

# pao2

pao2 <- pcp_laboratory %>% 
  filter(`検査名` == '酸素分圧;pO2(血液ガス)') %>% 
  filter(`患者ID` %in% ids) %>% 
  left_join(., anti_pcp_medication, by = c('患者ID')) %>% 
  filter(ymd(`入院日`)-30 <= ymd(`検査日`) & ymd(`検査日`) <= ymd(`入院日`)+1) %>% 
  arrange(`検査日`) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  dplyr::select(`患者ID`, `入院日`, `結果`) %>% 
  rename(pao2 = `結果`)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., pao2, by = c('患者ID', '入院日'))

# Procedure ---------------------------------------------------------------

## intubation  

intubation <- c('^J044')

intubation_list <- procedure_list %>% filter(str_detect(`診療点数早見表区分コード`, str_c(intubation, collapse = "|"))) 

intubation_list <- intubation_list %>%
  filter(`診療行為コード` == '140009010') %>% 
  dplyr::select(`診療行為コード`, `診療行為名`)

#procedure_detect_before(intubation_list, 90, 'intubation_prior90')
procedure_detect_after(intubation_list, 7, 'intubation_after07')

## oxygen

oxygen <- c('^J024')

oxygen_list <- procedure_list %>% filter(str_detect(`診療点数早見表区分コード`, str_c(oxygen, collapse = "|"))) 

oxygen_list <- oxygen_list %>%
  filter(`診療行為コード` == '140005610') %>% 
  dplyr::select(`診療行為コード`, `診療行為名`)

#procedure_detect_before(oxygen_list, 90, 'oxygen_prior90')
procedure_detect_after(oxygen_list, 2, 'oxygen_after02')

# ventilation

ventilation <- c('^J045')

ventilation_list <- procedure_list %>% filter(str_detect(`診療点数早見表区分コード`, str_c(ventilation, collapse = "|"))) 

ventilation_code <- c("140009310", "1140009550", "140009750", "140010150", "140023510",
                      "140023750", "140023850", "140024150", "140023950", "140024150",
                      "140024350", "140030830", "140039950", "140009650", "140009750",
                      "140063810", "140064050", "140064150", "140064250", "140064350",
                      "140064450", "140064650", "140039950")
ventilation_list <- ventilation_list %>%
  filter(str_detect(`診療行為コード`, str_c(ventilation_code, collapse = "|"))) %>% 
  dplyr::select(`診療行為コード`, `診療行為名`)

procedure_detect_before(ventilation_list, 90, 'ventilation_prior90')
procedure_detect_after(ventilation_list, 7, 'ventilation_after07')

# hot

hot_code <- c("114003710", "114004310", "114004910", "114005010", "114005410", 
              "114005510", "114006110", "114006210", "114006310", "114006810",
              "114009610", "114011110", "114011210", "114040710", "114040810",
              "114041210", "114041310", "114041610", "114041710", "114042770",
              "114043670", "114045470", "114045670", "114053150", "114055550",
              "114055650", "114055850", "114055950", "114056350", "114061510",
              "114062510", "114062610"
)

hot_list <- procedure_list %>%
  filter(str_detect(`診療行為コード`, str_c(hot_code, collapse = "|"))) %>% 
  dplyr::select(`診療行為コード`, `診療行為名`)

procedure_detect_before(hot_list, 90, 'hot_prior90')

# hfnc 

hfnc <- c('^J026')

hfnc_list <- procedure_list %>% filter(str_detect(`診療点数早見表区分コード`, str_c(hfnc, collapse = "|"))) 

hfnc_list <- hfnc_list %>%
  dplyr::select(`診療行為コード`, `診療行為名`)

procedure_detect_before(hfnc_list, 90, 'hfnc_prior90')
procedure_detect_after(hfnc_list, 7, 'hfnc_after07')

# Medication --------------------------------------------------------------

steroid <- read_rds("steroid_abx/steroid.rds")

steroid_list <- steroid %>% 
  distinct(`薬剤名`, `単位`, .keep_all = TRUE) 

dose_modified <- read_excel('memo/steroid_conversion.xlsx') %>% 
  dplyr::select(`薬剤名`, `単位`, `メチルプレドニゾロン換算`)

steroid <- anti_pcp_medication %>%
  distinct(`患者ID`, `入院日`) %>% 
  left_join(., steroid, by = '患者ID') %>% 
  arrange(`患者ID`, `入院日`)

base_steroid <- steroid %>% 
  filter(ymd(`入院日`) - months(1) < ymd(`開始日`) & ymd(`開始日`) < ymd(`入院日`)) %>% 
  distinct(`患者ID`, `入院日`) %>% 
  mutate(base_steroid = 1)

anti_pcp_medication <- anti_pcp_medication %>% 
  left_join(., base_steroid, by = c('患者ID', '入院日'))
  
after_steroid <- steroid %>% 
  filter(ymd(`入院日`) - 1 < ymd(`開始日`) & ymd(`開始日`) < ymd(`入院日`) + 1) %>% 
  left_join(., dose_modified, by = c('薬剤名', '単位'))

steroid_history <- steroid %>% 
  left_join(., dose_modified, by = c('薬剤名', '単位')) %>% 
  mutate(`メチルプレドニゾロン換算` = as.numeric(`メチルプレドニゾロン換算`),
         dose = `用量` * `メチルプレドニゾロン換算`) %>% 
  drop_na(`開始日`, `終了日`)

id <- c()
date <- c()
dose <- c()

for (i in 1:nrow(steroid_history)) {
  .data <- slice(steroid_history, i)
  .date <- seq(ymd(.data$`開始日`), ymd(.data$`終了日`), by='1 day')
  .row <- length(.date)
  id <- c(id, rep(.data$`患者ID`, times = .row))
  date <- c(date, .date)
  dose <- c(dose, rep(.data$dose, times = .row))
}

steroid_history_long <- bind_cols(id, date, dose) %>% 
  rename(`患者ID` = '...1',
         date = '...2',
         dose = '...3') %>% 
  arrange(`患者ID`, date)

steroid_history_long <- steroid_history_long %>% 
  group_by(`患者ID`, date) %>%
  summarise(sum_dose = sum(dose)) %>% 
  ungroup()

steroid_history_near_hosp <- steroid_history_long %>% 
  left_join(., anti_pcp_medication, by = '患者ID') %>% 
  mutate(date = as.Date(date, origin = "1970-01-01"),
         date_from_hosp = ymd(date) - ymd(`入院日`),
         date_from_hosp = as.numeric(date_from_hosp)) %>% 
  filter(ymd(`入院日`)-2 <= date & date <= ymd(`入院日`)+2)
  
#ggplot(steroid_history_near_hosp, aes(date_from_hosp, sum_dose)) +
#  geom_line(aes(group = `患者ID`, alpha=0.2)) +
#  scale_x_continuous(name='Days from admission') +
#  scale_y_continuous(name='Dose of steroid', limits=c(0,3000)) +
#  theme_bw()

steroid_history_near_hosp_wide <- steroid_history_near_hosp %>%
  dplyr::select(`患者ID`, date_from_hosp, sum_dose) %>% 
  pivot_wider(names_from = date_from_hosp,
              values_from = sum_dose,
              values_fill = list(value = NA_character_)) %>% 
  rename(steroid_before2 = `-2`,
         steroid_before1 = `-1`,
         steroid_admission = `0`,
         steroid_after1 = `1`,
         steroid_after2 = `2`)

anti_pcp_medication <- left_join(anti_pcp_medication, steroid_history_near_hosp_wide, by = '患者ID') 

# Insuline ----------------------------------------------------------------

insuline_atc <- c('A10A')

code_filter <- medication_list %>% 
  filter(str_detect(`WHO.ATCコード`, paste(insuline_atc, collapse = "|"))) %>% 
  dplyr::select(`薬価基準収載医薬品コード`) %>% 
  drop_na(`薬価基準収載医薬品コード`) %>% 
  pull(`薬価基準収載医薬品コード`) %>% 
  str_c(., collapse = "|")

pcp_medication <- readRDS("medication/pcp_medication.rds")

insuline <- pcp_medication %>% 
  filter(str_detect(薬価コード, code_filter))

insuline <- insuline %>% 
  rename(medication_start = `開始日`,
         medication_end = `終了日`)

insuline_medication <- left_join(insuline, pcp, by = c('患者ID')) 

insuline_medication_before <- insuline_medication %>% 
  filter(ymd(`入院日`)-30 <= ymd(medication_start) & ymd(medication_start) < ymd(`入院日`)-1) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  group_by(`患者ID`) %>% 
  arrange(desc(ymd(`medication_start`))) %>% 
  slice(1) %>% 
  ungroup()


insuline_medication_before_ids <- insuline_medication_before %>% 
  distinct(`患者ID`) %>% 
  pull()

insuline_medication_after <- insuline_medication %>% 
  filter(ymd(`入院日`) <= ymd(medication_start) & ymd(medication_start) <= ymd(`入院日`)+90) %>% 
  distinct(`患者ID`, `入院日`, .keep_all = TRUE) %>% 
  group_by(`患者ID`) %>% 
  arrange(ymd(`medication_start`)) %>% 
  slice(1) %>% 
  ungroup()  

insuline_medication_after_ids <- insuline_medication_after %>% 
  distinct(`患者ID`) %>% 
  pull()

# Final cleaning ----------------------------------------------------------

anti_pcp_medication <- anti_pcp_medication %>% 
  mutate(age = as.numeric(str_sub(`入院日`, 1, 4)) - as.numeric(`生年`),
         sex = case_when(`性別` == 1 ~ 1,
                         `性別` == 2 ~ 0),
         bmi = as.numeric(bmi),
         adm_jcs = as.numeric(adm_jcs),
         hugh_johns = as.numeric(hugh_johns),
         bun_cat = as.numeric(bun_cat),
         spo2 = as.numeric(spo2),
         sbp = as.numeric(sbp),
         beta_d_baseline = as.numeric(beta_d_baseline),
         beta_d_baseline_100 = beta_d_baseline/100,
         ldh_baseline = as.numeric(beta_d_baseline),
         ldh_baseline_100 = beta_d_baseline/100,
         ldh_baseline2 = as.numeric(ldh_baseline2),
         ldh_baseline2_100 = ldh_baseline2/100) %>% 
  mutate(across(intubation_prior90:steroid_after2, ~replace_na(.x, 0))) %>% 
  rename(id = `患者ID`) %>% 
  mutate(death30 = if_else(death30 == 2 | is.na(death30), 0, death30), # censoring = 12
         death90 = if_else(death90 == 2 | is.na(death90), 0, death90)) # censoring = 18

anti_pcp_medication$adm_adl <- str_replace_all(anti_pcp_medication$adm_adl, "9", "2") 
anti_pcp_medication$adm_adl <- sapply(strsplit(anti_pcp_medication$adm_adl,""), function(x) sum(as.numeric(x)*5)) 

anti_pcp_medication <- anti_pcp_medication %>% 
  mutate(bmi_cat = case_when(bmi < 18.5 ~ 0,
                             18.5 <= bmi & bmi < 25 ~ 1,
                             25 <= bmi ~ 2),
         adl_cat = case_when(adm_adl < 20 ~ 0,
                             20 <= adm_adl & adm_adl < 50 ~ 1,
                             50 <= adm_adl ~ 2),
         hugh_johns_cat = case_when(hugh_johns <= 3 ~ 0,
                                    hugh_johns > 3 ~ 1),
         spo2 = if_else(spo2 == 2, 1, 0),
         escalation = if_else(steroid_after1 - steroid_before1 > 0 | (is.na(steroid_before1) | steroid_before1 == 0) & (steroid_after1 > 0), 1, 0),
         change_steroid = (steroid_after1 - steroid_before1)/50,
         insuline_before = if_else(id %in% insuline_medication_before_ids, 1, 0),
         insuline_after = if_else(id %in% insuline_medication_after_ids, 1, 0),
         new_insuline = insuline_after - insuline_before == 1)

ids <- anti_pcp_medication %>% 
  distinct(id) %>% 
  pull()


anti_pcp_medication %>% write_xlsx('cleaned/pcp_study_cleaned.xlsx')

beta_d <- beta_d %>% 
  mutate(`結果` = stringr::str_replace(`結果`, '<', ''),
         `結果` = stringr::str_replace(`結果`, '>', ''),
         `結果` = stringr::str_replace(`結果`, '=', ''),
         `結果` = stringr::str_replace(`結果`, '-', ''),
         `結果` = as.numeric(`結果`)
  ) %>% 
  rename(result = `結果`) %>% 
  arrange(`患者ID`, `検査日`) %>% 
  group_by(`患者ID`) %>% 
  mutate(result_lag = lag(result),
         interval = as.numeric(ymd(`検査日`) - ymd(`入院日`) + 1),
         observation_time = as.numeric(ymd(`観察期間終了日.EMR.`) - ymd(`入院日`) + 1)) %>% 
  ungroup() %>% 
  mutate(diff = result - result_lag)

beta_d %>% write_rds('cleaned/beta_d_after.rds')