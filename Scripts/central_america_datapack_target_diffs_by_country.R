library(gagglr)
library(tameDP)
library(tidyverse)
library(glue)
library(gt)
library(gtExtras)


file_path <- return_latest(folderpath = si_path(type="path_datapacks"),
                           pattern = "Central_America_03_29_2023")
df_dp<-tame_dp(file_path)
dp_cols<-df_dp %>% names()

msd_filepath<-return_latest(folderpath = si_path(),
                            pattern = "PSNU_IM")
df_msd<-read_psd(msd_filepath)


msd_mapping<-msd_historic_disagg_mapping %>%
  mutate(standardizeddisaggregate=case_when(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPop"
                                            ~ "KeyPopAbr", 
                                            TRUE ~ standardizeddisaggregate)) %>%
  filter(indicator!="HTS_SELF" | (indicator=="HTS_SELF" & !(standardizeddisaggregate %in%
                                  c("Age/Sex/HIVSelfTest","KeyPop/HIVSelfTest")))) 

whr <- Wavelength::pull_hierarchy(ou_uid = "IJOkeEIdw9i", username = datim_user(),
                                  password = datim_pwd())


### Pull only standardizeddisagg
msd_filt<-df_msd %>% filter(operatingunit=="Western Hemisphere Region") %>% 
       filter(country %in% c("El Salvador", "Honduras", "Guatemala",
                                         "Brazil", "Nicaragua", "Panama")) %>%
       select(any_of(dp_cols),funding_agency, mech_code) %>%
       semi_join(msd_mapping, by = c("indicator", "numeratordenom", "standardizeddisaggregate")) %>%
       clean_indicator() %>%
       mutate(standardizeddisaggregate=case_when(indicator=="PrEP_NEW" & standardizeddisaggregate=="KeyPopAbr"
                                            ~ "KeyPop", 
                                            TRUE ~ standardizeddisaggregate))

# filter DP, join PSNU map in order to backtrack country; filter to only 2024 targets
dp_filt <- df_dp %>%
  clean_indicator() %>% 
  filter(fiscal_year == 2024) %>% 
  mutate(operatingunit = "Western Hemisphere Region") %>%
  mutate(psnu = stringr::str_remove(psnu, "_Military")) %>% 
  mutate(psnu = stringr::str_trim(psnu)) %>%
  left_join(whr %>% select(operatingunit, countryname, psnu, psnuuid) %>% distinct(),
            by =c("operatingunit", "psnu", "psnuuid")) %>%
  mutate(countryname=case_when(is.na(countryname)==TRUE ~ psnu, TRUE ~ countryname)) %>%
  mutate(country=countryname) %>%
  select(-c(countryname))


### Aggregate by country
msd_agg<-msd_filt %>% group_by(country, indicator, standardizeddisaggregate, fiscal_year) %>% 
  group_by(country, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(targets, \(x) sum(x,na.rm = TRUE)), .groups = "drop")

### Clean and aggregate DP by country
dp_agg<- dp_filt %>%
  select(-c(operatingunit)) %>% 
  group_by(country, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(targets, \(x) sum(x,na.rm = TRUE)), .groups = "drop") 

df_combined<-msd_agg %>% bind_rows(dp_agg) %>%
  filter(fiscal_year>2022) %>%
  filter(!(indicator=="PrEP_CT" & standardizeddisaggregate=="Age/Sex/HIVStatus")) %>%
  filter(!(indicator %in% c("TB_STAT","TB_STAT_D"))) %>%
  mutate(all_or_KP=case_when(!str_detect(standardizeddisaggregate, "Key")~"Age/Sex", TRUE~"KeyPop"))%>%
  select(-c(standardizeddisaggregate)) %>%
  mutate(fiscal_year = as.character(fiscal_year)) %>%
  mutate(fiscal_year = str_replace(fiscal_year, "20", "FY")) %>%
  pivot_wider(names_from = "fiscal_year", values_from = "targets") %>% 
  relocate(`FY23`, .after = 3) %>% 
  mutate(diff = `FY24` - `FY23`, percent_change=round((`FY24` - `FY23`)/`FY23`*100,0)) %>%
  mutate(percent_change=case_when(is.na(percent_change)==FALSE ~ paste(percent_change,"%", sep="")))%>%
  arrange(country,indicator)
  
  
### SUMMARY OF TARGETS FOR CENTRAL AMERICA BY COUNTRY  
#list_countries = as.list(unique(df_combined$country)) 

t1<-df_combined %>% filter(all_or_KP=="Age/Sex" | indicator== "KP_PREV") %>% 
  gt() |> cols_hide(all_or_KP) |>
    tab_header(
    title = md("Central America Target Summary"), 
    subtitle = md("All Populations")) |>
  cols_label(
    diff="change",
    percent_change="% change"
  )  


t2<-df_combined %>% filter(all_or_KP=="KeyPop") %>% 
  gt() |> cols_hide(all_or_KP) |>
    tab_header(
    title = md("Central America Target Summary"), 
    subtitle = md("Key Populations")) |>
    cols_label(
       diff="change",
       percent_change="% change"
      )  

tbl_list<-list(t1, t2)

t1 %>% gtsave(path = "Images", filename = "Central_America_Target_Diffs_All_Countries_0401223.pdf",
              expand=20)
t2 %>% gtsave(path = "Images", filename = "Central_America_Target_Diffs_KP_Countries_041223.pdf",
              expand=20)

#gt_two_column_layout(tbl_list) %>%
#gtsave_extra(path = "Outputs", filename = "Central_America_Target_Differences_040723.png")






