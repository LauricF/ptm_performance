library(table1)

dt <- data_demographics_ogtt
trainData <-
  dt[dt$Draw.Date.x < "2012-09-01",]
variables <- c("T1D","group","Sex","age","GRS2","IA2","Z_BMI","BMI","AUC Cpeptide","AUC glucose","Index60","Cpeptide index 30","Beta2 score","Hba1c")

dt <- data_demographics_ogtt %>% mutate(Sex = Participant.Sex,
                                  age = age_at_first_drawing,
                                  `AUC glucose` = AUC_glucose,
                                  `AUC Cpeptide` = AUC_ceptide,
                                  `Beta2 score` = beta2_score,
                                  `Cpeptide index 30` = C_peptide_index_30,
                                  T1D = ifelse(T1D.Indicator== 1, "T1D", "T1D free"),
                                  IA2 = ifelse(IA2 == 1, "IA2", "IA2 free"),
                                  group = case_when(
                                    AB_group %in% c("GAD","IAA","IA2A") ~ "Stage 0",
                                    ADA_dysglycemia == "Euglycemia" & !AB_group %in% c("GAD","IAA","IA2A") ~ "Stage 1",
                                    TRUE ~ "Stage 2"),
                                  T1D = factor(T1D),
                                  group = factor(group)) %>% 
  select(all_of(variables)) %>% 
  tidyr::drop_na()


table1(~ Sex+age+GRS2+IA2+Z_BMI+BMI+`AUC Cpeptide`+ `AUC glucose` + Index60 + `Cpeptide index 30` + `Beta2 score` + Hba1c | group*T1D, data=dt)

paste(c("Sex","age","GRS2","IA2","Z_BMI","BMI","AUC Cpeptide","AUC glucose","Index60","Cpeptide index 30","Beta2 score","Hba1c"), collapse =  "+")
