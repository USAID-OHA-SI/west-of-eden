library(gagglr)
library(tameDP)
library(tidyverse)
library(glue)
library(gt)
library(gtExtras)


file_path <- return_latest(folderpath = si_path(type="path_datapacks"),
                           pattern = "LATEST_TST_DR_20230214200935_17April23")
df_dp<-tame_dp(file_path)
dp_cols<-df_dp %>% names()

# df_dp_tx_curr <- df_dp %>%
#   filter(indicator == 'TX_CURR' & standardizeddisaggregate == 'Age/Sex/HIVStatus') %>%
#   group_by(indicator, standardizeddisaggregate, fiscal_year) %>%
#   summarise(across(c(targets), \(x) sum(x,na.rm = TRUE)), .groups = "drop")

msd_filepath<-return_latest(folderpath = si_path(type="path_datapacks"),
                            pattern = "PSNU_IM")
df_msd<-read_psd(msd_filepath)

df_msd_tx_curr <- df_msd %>%
  filter(indicator == 'TX_CURR' & standardizeddisaggregate == 'Age/Sex/HIVStatus', fiscal_year == "2023") %>%
  group_by(indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(c(cumulative), \(x) sum(x,na.rm = TRUE)), .groups = "drop")

msd_dr_ovc_serv_cargv <- df_msd %>%
  filter(operatingunit == "Dominican Republic" & indicator == "OVC_SERV" & standardizeddisaggregate == "Age/Sex/ProgramStatusCaregiver" & fiscal_year>=2022) %>% 
  select(any_of(dp_cols), funding_agency, mech_code) %>%
  clean_indicator() %>%
  group_by(indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(c(targets, cumulative), \(x) sum(x,na.rm = TRUE)), .groups = "drop")
  

msd_mapping <- msd_historic_disagg_mapping %>%
  mutate(standardizeddisaggregate=case_when(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPop"
                                            ~ "KeyPopAbr", 
                                            TRUE ~ standardizeddisaggregate)) %>%
  filter(indicator!="HTS_SELF" | (indicator=="HTS_SELF" & !(standardizeddisaggregate %in%
                                                              c("Age/Sex/HIVSelfTest","KeyPop/HIVSelfTest")))) 

### Pull only standardizeddisagg
msd_filt <- df_msd %>%
  filter(operatingunit=="Dominican Republic") %>% 
  #filter(country %in% c("El Salvador", "Honduras", "Guatemala",
  #                      "Brazil", "Nicaragua", "Panama")) %>%
  select(any_of(dp_cols),funding_agency, mech_code) %>%
  semi_join(msd_mapping, by = c("indicator", "numeratordenom", "standardizeddisaggregate")) %>%
  clean_indicator() %>%
  mutate(standardizeddisaggregate=case_when(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr"
                                            ~ "KeyPop", 
                                            TRUE ~ standardizeddisaggregate))
  

### Aggregate by OU
msd_agg <- msd_filt %>%
  group_by(indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(c(targets, cumulative), \(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
  rbind(msd_dr_ovc_serv_cargv)

### Clean and aggregate DP
dp_agg <- df_dp %>%
  clean_indicator() %>% 
  filter(fiscal_year==2024) %>% 
  select(-c(operatingunit)) %>% 
  group_by(indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(targets, \(x) sum(x,na.rm = TRUE)), .groups = "drop") 

df_combined <- msd_agg %>%
  bind_rows(dp_agg) %>%
  filter(fiscal_year>=2022) %>%
  filter(!(indicator=="GEND_GBV" & standardizeddisaggregate=="Age/Sex/ViolenceType")) %>%
  filter(!(indicator=="HTS_TST" & standardizeddisaggregate=="KeyPop/Result")) %>%
  filter(!(indicator=="HTS_TST_POS" & standardizeddisaggregate=="KeyPop/Result")) %>%
  filter(!(indicator=="PrEP_CT" & standardizeddisaggregate=="KeyPop")) %>%
  filter(!(indicator=="PrEP_CURR" & standardizeddisaggregate=="KeyPop")) %>%
  filter(!(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPop")) %>%
  filter(!(indicator=="TB_PREV" & standardizeddisaggregate=="Age/Sex/NewExistingArt/HIVStatus")) %>%
  filter(!(indicator=="TB_PREV_D" & standardizeddisaggregate=="Age/Sex/NewExistingArt/HIVStatus")) %>%
  filter(!(indicator=="TX_CURR" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>%
  filter(!(indicator=="TX_NEW" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>%
  filter(!(indicator=="TX_TB_D" & standardizeddisaggregate=="Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>%
  filter(!(indicator=="PrEP_CT" & standardizeddisaggregate=="Age/Sex/HIVStatus")) %>%
  filter(!(indicator=="TX_PVLS" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>%
  filter(!(indicator=="TX_PVLS_D" & standardizeddisaggregate=="KeyPop/HIVStatus")) %>%
  #filter(!(indicator %in% c("TB_STAT","TB_STAT_D"))) %>%
  mutate(all_or_KP=case_when(!str_detect(standardizeddisaggregate, "Key")~"Age/Sex", TRUE~"KeyPop")) %>%
  group_by(indicator, fiscal_year, all_or_KP) %>% 
  summarise(across(c(targets, cumulative), \(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
  mutate(fiscal_year = as.character(fiscal_year)) %>%
  mutate(fiscal_year = str_replace(fiscal_year, "20", "FY")) %>%
  pivot_wider(names_from = "fiscal_year", values_from = c("targets", "cumulative")) %>%
  filter(!(targets_FY23==0 & is.na(targets_FY24))) %>%
  #relocate(`FY22`, `FY23`, .after = 3) %>% 
  select(indicator, all_or_KP, targets_FY22, cumulative_FY22, targets_FY23, cumulative_FY23, targets_FY24) %>%
  mutate(targets_diff_fy_24_23 = `targets_FY24` - `targets_FY23`, targets_percent_change_24_23 = round((`targets_FY24` - `targets_FY23`)/`targets_FY23`,2))
  #mutate(diff_fy_24_22 = `FY24` - `FY22`, percent_change_24_22 = round((`FY24` - `FY22`)/`FY22`,2)) %>%
  #mutate(targets_percent_change_24_23 = case_when(!is.na(targets_percent_change_24_23) ~ paste(targets_percent_change_24_23,"%", sep="")))
  #mutate(percent_change_24_22 = case_when(!is.na(percent_change_24_22) ~ paste(percent_change_24_22,"%", sep="")))
  
### SUMMARY OF TARGETS FOR CENTRAL AMERICA  

t1 <- df_combined %>%
  filter(all_or_KP=="Age/Sex" | indicator== "KP_PREV") %>%
  gt() |> cols_hide(all_or_KP) |>
  tab_header(
    title = md("Dominican Republic Target Summary"), 
    subtitle = md("All Populations")) |>
  cols_label(
    targets_diff_fy_24_23 = "targets change_FY24_23",
    #targets_diff_fy_24_22 = "change_FY24_22",
    targets_percent_change_24_23 = "targets % change FY24_FY23",
    #targets_percent_change_24_22 = "% change FY24-FY22"
    cumulative_FY22 = "results_FY22",
    cumulative_FY23 = "results_FY23"
  ) |>
  fmt_integer(columns=c(targets_FY22, cumulative_FY22, targets_FY23, cumulative_FY23, targets_FY24, targets_diff_fy_24_23)) |>
  fmt_percent(columns=targets_percent_change_24_23, decimals=0)


t2<-df_combined %>% filter(all_or_KP=="KeyPop") %>% 
  gt() |> cols_hide(all_or_KP) |>
  tab_header(
    title = md("Dominican Republic Target Summary"), 
    subtitle = md("Key Populations")) |>
  cols_label(
    targets_diff_fy_24_23 = "targets change_FY24_23",
    #targets_diff_fy_24_22 = "change_FY24_22",
    targets_percent_change_24_23 = "targets % change FY24-FY23",
    #targets_percent_change_24_22 = "% change FY24-FY22"
    cumulative_FY22 = "results_FY22",
    cumulative_FY23 = "results_FY23"
  ) |>
  fmt_integer(columns=c(targets_FY22, cumulative_FY22, targets_FY23, cumulative_FY23, targets_FY24, targets_diff_fy_24_23)) |>
  fmt_percent(columns=targets_percent_change_24_23, decimals=0)

tbl_list<-list(t1, t2)

t1 %>% gtsave(path = "Images", filename = "Dominican_Republic_Target_Diffs_All_041423.html")
t2 %>% gtsave(path = "Images", filename = "Dominican_Republic_Target_Diffs_KP_041423.html")

#gt_two_column_layout(tbl_list) %>%
#gtsave_extra(path = "Outputs", filename = "Central_America_Target_Differences_040723.png")





