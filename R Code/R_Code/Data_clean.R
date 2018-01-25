# Clean  data
library(xlsx)
library(dplyr)
library(magrittr)
data = read.xlsx(file = 'Monthly.xlsx', 1, stringsAsFactors = FALSE)
data = dplyr::tbl_df(data)


#------------------------------------
#Clean Data and Group and Order
#------------------------------------
 
data %<>%
  dplyr::distinct() %>%
  group_by(Farm_ID) %>%
  arrange(DHIDate)

Test_data = 
  data %>%
  dplyr::filter(!is.na(Sample_Number))%>%
  group_by(Farm_ID)

#------------------------------------
#Number of Obs by Farm
#------------------------------------
Number_obs_by_Farm = 
  data %>%
  summarise(length(Farm_ID))

#------------------------------------
#Number of Test Obs by Farm
#------------------------------------
Number_test_obs_by_Farm =   
  data%>%
    dplyr::filter(!is.na(Sample_Number))%>%
    summarise(length(Farm_ID))%>%
    rename(number_obs = `length(Farm_ID)` )
    
  

#Check Sample number by group 
S_N = data %>%
  dplyr::filter(!is.na(Sample_Number)) %>%
  do({
    data.frame(.$Sample_Number)
  })



#------------------------------------
#Row index of Test Data by Farm
#------------------------------------
Test_Row_Index = 
  data %>%
    do({
      data.frame(which(!is.na(.$Sample_Number)))
    })%>%
  rename(row_index = which..is.na...Sample_Number..) %>%
  group_by(Farm_ID)



#------------------------------------
#Fill in NA using Test data
#------------------------------------
data = dplyr::full_join(data, Number_test_obs_by_Farm, by = 'Farm_ID')

a = data.frame()

i = 4

while (i > 0){
  data_1=
    data%>%
      filter(number_obs == i)%>%
      select(Sample_Number:number_obs)%>%
      slice(which(!is.na(Sample_Number))[i]: length(Sample_Number))%>%
      group_by(Farm_ID)%>% 
      mutate_each(.,funs(first))
  a = bind_rows(a, data_1)
  j = i
  while (j > 1){
    data_2=
      data%>%
        filter(number_obs == i)%>%
        select(Sample_Number:number_obs)%>%
        slice(which(!is.na(Sample_Number))[j-1]: (which(!is.na(Sample_Number))[j]-1) )%>%
        group_by(Farm_ID)%>% 
        mutate_each(.,funs(first))
    a = bind_rows(a, data_2)
    j = j - 1
  } 
  data_3=
    data%>%
    filter(number_obs == i)%>%
    select(Sample_Number:number_obs)%>%
    slice(1: which(!is.na(Sample_Number))[1])%>%
    group_by(Farm_ID)%>% 
    mutate_each(.,funs(last))
  a = bind_rows(a, data_3)
  i = i - 1
}  

a%<>%
  group_by(Farm_ID)%>%
  arrange(Farm_ID, Sample_Number)%>%
  slice(-1)


check_a = 
  a%>%
  group_by(Farm_ID, Sample_Number)%>%
  summarise(length(Sample_Number))



data_new  = data
data_new[,17:37] = a[,2:22]



#------------------------------
#Merge with Annual Data
#------------------------------
data_year = read.xlsx(file = "Annual.xlsx", 3)
data_year$Farm_ID = factor(data_year$Farm_ID)
data_new = merge(data_new, data_year, by = c("Farm_ID", "year"), all.x = T)
data_new%<>%
  arrange(Farm_ID, DHIDate)
  
#write.xlsx(data_new, file = "data_new.xlsx")
#data_new = read.xlsx(file = "data_new1.xlsx", 1, stringsAsFactors = FALSE)

  
  
#---------------------------------  
#Check missing values
#---------------------------------

missing_fn = function(local_data, col_index){
  return(sum(is.na(local_data[,col_index])))
}


result = sapply(1:dim(data_new)[2], function(x) missing_fn(data_new, x))
missing_value = as.data.frame(result)
missing_value$Var = colnames(data_new)
colnames(missing_value)[1] = "Number_of_Missing_Values"
missing_value = missing_value[, c(2, 1)]
knitr::kable(missing_value, caption = "A table produced by printr.")



missing_index =
data_new %>%
  group_by(Farm_ID)%>%
  summarise_each(funs(sum(is.na(.))))  

#--------------------------------------------
#Fill missing value by hand in excel
#--------------------------------------------
data_new_1 = read.xlsx(file = "data_new1.xlsx", 1, stringsAsFactors = FALSE)
missing_index_1 =
  data_new_1 %>%
  group_by(Farm_ID)%>%
  summarise_each(funs(sum(is.na(.))))  

#--------------------------------------------
#Replace the rest NAs in NDF_Digestibility (20 NAs) and pH(63) by column mean
#--------------------------------------------

data_good = data_new_1
data_good$pH[is.na(data_good$pH)] = mean(data_good$pH[!is.na(data_good$pH)])
data_good$NDF_Digestibility[is.na(data_good$NDF_Digestibility)] = mean(data_good$NDF_Digestibility[!is.na(data_good$NDF_Digestibility)])
missing_index_2 =
  data_good %>%
    summarise_each(funs(sum(is.na(.))))


#--------------------------------------------------
#DEA Model 2
#--------------------------------------------------
library(Benchmarking)
Inputs_model2 = c("X__Milk_Cows", 
                  "Dry_Matter", 
                  "CP__", 
                  "NDF___DM", 
                  "Starch___DM", 
                  "pH")
Outputs_model2_1 = c("Milk_per_Milk_Cow",
                   "X__Fat",
                   "X__Pro")
Outputs_model2_2 = c("Fecal_Starch",
                     "MUN")
dea_model2_all = dea(data_good[,Inputs_model2],
                 cbind(data_good[,Outputs_model2_1], -data_good[,Outputs_model2_2]))

#on test data
Test_data = 
  data %>%
  dplyr::filter(!is.na(Sample_Number))%>%
  group_by(Farm_ID)

Test_data$pH[is.na(Test_data$pH)] = mean(Test_data$pH[!is.na(Test_data$pH)])
Test_data$MUN[is.na(Test_data$MUN)] = mean(Test_data$MUN[!is.na(Test_data$MUN)])
missing_value2 = 
  Test_data%>%
  ungroup%>%
  summarise_each(funs(sum(is.na(.))))


Test_data = as.data.frame(Test_data)
dea_model2_test = dea(Test_data[,Inputs_model2],
                      cbind(Test_data[,Outputs_model2_1], -Test_data[,Outputs_model2_2]))


summary(dea_model2_all)
summary(dea_model2_test)



#----------------------------------------------------
#DEA MODEL 3
#----------------------------------------------------

Inputs_model3 = c(Inputs_model2,
                  "Dairy_IOFC_Surplus_Group",
                  "Crop_Cost_Calc",
                  "Pur_Feed_Calc",
                  "Feed_Cost__All_Animals__Annual",
                  "Corn_silage"
                  )
data_model3 = merge(Test_data, data_year, by = c("Farm_ID", "year"),
                   all.x = T)
missing_value3 =
  data_model3%>%
  summarise_each(funs(sum(is.na(.))))

data_model3 %<>%
  filter(!(is.na(Dairy_IOFC_Surplus_Group) |
           is.na(Crop_Cost_Calc) |
           is.na(Pur_Feed_Calc) |
            is.na(Feed_Cost__All_Animals__Annual) |
             is.na(Corn_silage))
         )

dea_model3 = dea(data_model3[,Inputs_model2],
                cbind(data_model3[,Outputs_model2_1], -data_model3[,Outputs_model2_2]))
summary(dea_model3)
c = ggplot(as.data.frame(dea_model3$eff), aes(dea_model3$eff))
c + geom_histogram()
