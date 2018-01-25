# Clean  data
library(xlsx)
library(dplyr)
library(magrittr)
library(Benchmarking)
library(ggplot2)
library(knitr)
data = read.xlsx(file = 'Monthly.xlsx', 1, stringsAsFactors = FALSE)
data_year = read.xlsx(file = "Annual.xlsx", 3)


#------------------------------------
#Clean Data and Group and Order
#------------------------------------

data_year$Farm_ID = factor(data_year$Farm_ID)
data = merge(data, data_year, by = c("Farm_ID", "year"), all.x = T)
data$MUN = - data$MUN
data$Fecal_Starch = - data$Fecal_Starch
data = dplyr::tbl_df(data)

Monthly_data =
  data%>%
  dplyr::distinct() %>%
  group_by(Farm_ID) %>%
  arrange(DHIDate)


Test_data = 
  Monthly_data %>%
  dplyr::filter(!is.na(Sample_Number) &
                !is.na(MUN)&
                !is.na(pH)  )%>%
  group_by(Farm_ID)


Annual_data = 
  Test_data %>%
  dplyr::filter(!is.na(Pur_Feed_Calc) &
                !is.na(Feed_Cost__All_Animals__Annual) &
                !is.na(Corn_silage))%>%
  group_by(Farm_ID)
 
Number_Obs_data = lapply(list(Monthly_data, Test_data, Annual_data), function(x)dim(x)[1])
Number_Obs_data

#---------------------------------  
#Check missing values
#---------------------------------
missing_fn = function(local_data, col_index){
  return(sum(is.na(local_data[,col_index])))
}

check_dataset_missing_fn = function(data_set){
  missing_value = sapply(1:dim(data_set)[2], function(x) missing_fn(data_set, x))
  missing_value = data.frame(missing_value)
  missing_value$Var = colnames(data_set)
  colnames(missing_value)[1] = "Number_of_Missing_Values"
  missing_value = missing_value[, c(2, 1)]
  return(missing_value)
}

count_missing_value = lapply(list(Monthly_data, Test_data, Annual_data), check_dataset_missing_fn)

#-----------------------------------
#Model Inputs and Outputs
#-----------------------------------
Inputs = c("X__Cows",
           "Dry_Matter", "CP__" , "Starch___DM", "pH", 
           "Pur_Feed_Calc", "Feed_Cost__All_Animals__Annual", "Corn_silage")

Outputs = c("Milk_per_Milk_Cow", "X__Fat", "X__Pro",
            "MUN", "Fecal_Starch")


dim(Test_data[Inputs])

#-----------------------------------
#Model 4-1
#-----------------------------------
Inputs_Model4 = Inputs[1:5]

for(a in Outputs){
  summary(dea(data.frame(Test_data[Inputs_Model4]), data.frame(Test_data[a])))
}



#-----------------------------------
#Model 4-2
#-----------------------------------


eff_Model42 = data.frame(matrix(NA, ncol = 7, nrow = dim(Test_data)[1]))
colnames(eff_Model42) = c("Farm_ID", "Season_Year", Outputs)
eff_Model42$Farm_ID = Test_data$Farm_ID
eff_Model42$Season_Year = Test_data$Season_Year

for (a in Outputs){
  for (t in unique(Test_data$Season_Year)){
    local_data = Test_data[Test_data$Season_Year == t,]
    local_eff = dea(data.frame(local_data[Inputs_Model4]), data.frame(local_data[a]))$eff
    eff_Model42[Test_data$Season_Year == t, a] = local_eff
    
    plot_ = ggplot(eff_Model42, aes(x = Season_Year, y = eff_Model42[,a], color = Farm_ID))+
    geom_point()+
    ggtitle(paste('Histogram of Efficiency Scores of', a))
    print(plot_)
    }
}


kable(eff_Model42, digits = 3, caption = "Test Data Efficiency Score over Time")


for(a in Outputs){
  EFF = 
  Test_data%>%
    group_by(Season_Year)%>%
    do({
      data.frame(
        dea(data.frame(select(., 
                              X__Cows,
                              Dry_Matter, 
                              CP__, 
                              Starch___DM, 
                              pH)), 
            data.frame(select_(., a))
            )$eff)
    })
  
  colnames(EFF)[2] = "Efficiency"
  
  summary(EFF)
  ggplot(EFF, aes(Efficiency, color = Season_Year))+
    geom_histogram()+
    ggtitle(paste('Histogram of Efficiency Scores of', a))
}



#select_(Test_data, one_of(Inputs_Model4[2]))


#-----------------------------------
#Model 5
#-----------------------------------

for(a in Outputs){
  summary(dea(data.frame(Test_data[Inputs]), data.frame(Test_data[a])))
}
