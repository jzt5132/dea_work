# Clean  data
closeAllConnections()
rm(list=ls())
gc()
library(xlsx)
library(dplyr)
library(magrittr)
library(Benchmarking)
library(ggplot2)
library(knitr)
library(censReg)
library(tidyr)
data_ = read.xlsx(file = 'new_data.xlsx', 2, stringsAsFactors = FALSE)
data = data_


#------------------------------------
#Clean Data and Group and Order
#------------------------------------

data$Farm_ID = factor(data$Farm_ID)
name = colnames(data)
#which(name == "FinanceScore")
# which(grepl("ID",name)) #name containing ID
# which(grepl("Fecal", name))
# which(grepl("Cow", name))
# which(grepl("Prod", name))
# name[which(grepl("Prod", name))]
# name[which(grepl("Cow", name))]
# which(grepl("Milk", name))
# 
# name[which(grepl("Corn" , name))]
# name[which(grepl("Starch", name))]


# data = dplyr::tbl_df(data)

data %>%
  group_by(Year_Type)%>%
  summarise(length(unique(Farm_ID)))


a = list()
year = unique(data$Year_Type)
for (i in 1:length(year)){
  a[[i]] = unique(data$Farm_ID[data$Year_Type == year[i]])
}
a_1 = a[[1]]; a_2 = a[[2]]; a_3 = a[[3]]

dmu = intersect(intersect(a_1, a_2), a_3)


data = data[data$Farm_ID %in% dmu, ]


data$FallMUN = - data$FallMUN
data$SprMUN = - data$SprMUN
data$FallFecalStarch = -data$FallFecalStarch
data$SprFecalStarch = - data$SprFecalStarch








#-----------------------------------
#Model Inputs and Outputs
#-----------------------------------
Inputs = c("Total_Cows", "Pur_Feed_Calc", "Crop_Feed_Cost_Calc",
             "Corn_silage", 
             "FallDM", "SprDM",
             "FallStarch", "SprStarch",
             "FallCP", "SprCP",
             "FallpH", "SprpH")

Outputs = c("AvgTDmilk",
            "FallMUN", "SprMUN", "FallFecalStarch", "SprFecalStarch")





#-----------------------------------
#Model 1
#-----------------------------------
Inputs_Model1 = Inputs[1:3]
Outputs_Model1 = Outputs[1]


#Not considering year
eff_Model1 = dea(data.frame(data[Inputs_Model1]), data.frame(data[Outputs_Model1]))$eff
eff_Model1 = cbind(eff_Model1, data.frame(data[,c("Farm_ID", "Year_Type")]))


colnames(eff_Model1)[1] = "Efficiency_Score"
eff_Model1$Year_Type = gsub(" Actual", "", eff_Model1$Year_Type) 

ggplot(eff_Model1, aes(Efficiency_Score))+
  geom_histogram(bins = 30)+
  facet_grid(.~Year_Type)+
  theme_bw()+
  xlab("Dairy Farm Efficiency")


ggsave("hist_Model1_Appendix.pdf",width = 15, height = 6, units = "in") 


eff_Model1%<>%spread(Year_Type, eff_Model1)

#Consider Dairy Farm as the same one##########################
###Main Model ################################################
EFF = 
  data%>%
  group_by(Year_Type)%>%
  do({
    data.frame(
      dea(data.frame(select(., Total_Cows, Pur_Feed_Calc, Crop_Feed_Cost_Calc)), 
          data.frame(select(., AvgTDmilk)))$eff)
  })

colnames(EFF)[2] = "Efficiency_Score"
EFF$Year_Type = gsub(" Actual", "", EFF$Year_Type) 

ggplot(EFF, aes(Efficiency_Score))+
    geom_histogram(bins = 30)+
    facet_grid(.~Year_Type)+
    theme_bw()+
    xlab("Dairy Farm Efficiency")
  

ggsave("hist_Model1.pdf",width = 15, height = 6, units = "in") 


EFF %<>%
  bind_cols(., data.frame(Farm_ID = data$Farm_ID))%>%
  spread(Year_Type, Efficiency_Score)



#-----------------------------------
#Model 2
#-----------------------------------


data2 = data[c("Year_Type", "Farm_ID", Inputs, Outputs)]



for(b in c(Inputs, Outputs)){
  data2 %<>%
    dplyr::filter(!is.na(data2[b]))
}

data2 %<>%
  group_by(Year_Type)

data2$FecalStarch = data2$FallFecalStarch + data2$SprFecalStarch
data2$MUN = data2$FallMUN + data2$SprMUN


Inputs_Model2 = Inputs[]
Outputs_Model2 = c(Outputs[1], "MUN", "FecalStarch")

EFF_2 = list()
i = 1
for(b in Outputs_Model2){
  print(i)
  name_index = which(colnames(data2) == b)
  local_eff = 
      data2%>%
    group_by(Year_Type)%>%
    do({
      as.data.frame(
      dea(data.frame(select(., Total_Cows, Pur_Feed_Calc, Crop_Feed_Cost_Calc,
                            Corn_silage, 
                            FallDM, SprDM,
                            FallStarch, SprStarch,
                            FallCP, SprCP,
                            FallpH, SprpH)), 
          data.frame(select(., name_index)))$eff
      )
    })
  colnames(local_eff)[2] = "Efficiency_Score"
  local_eff %<>%
    bind_cols(., data.frame(Farm_ID = data2$Farm_ID))%>%
    spread(Year_Type, Efficiency_Score)
  EFF_2[[i]] = local_eff
  i = i + 1
  write.xlsx(local_eff, file = "DEA_new_Model2.xlsx", sheetName = b, append = TRUE)
}


########################################################################
######################    Tobit      ###################################
########################################################################
tobit_1 = censReg(eff_Model1$Efficiency_Score~
                  data$Total_Cows +
                  data$Pur_Feed_Calc,
                  #data$Crop_Feed_Cost_Calc,
                  left = 0, right = 1)
summary(tobit_1)
coef(tobit_1)
margEff(tobit_1)




















###################################################################################################################
###################################################################################################################
###################################################################################################################
# 
# 
# class(data2[Inputs_Model2])
# 
# 
# 
# Monthly_data =
#   data%>%
#   dplyr::distinct() %>%
#   group_by(Farm_ID) %>%
#   arrange(DHIDate)
# 
# 
# Test_data = 
#   Monthly_data %>%
#   dplyr::filter(!is.na(Sample_Number) &
#                   !is.na(MUN)&
#                   !is.na(pH)  )%>%
#   group_by(Farm_ID)
# 
# 
# Annual_data = 
#   Test_data %>%
#   dplyr::filter(!is.na(Pur_Feed_Calc) &
#                   !is.na(Feed_Cost__All_Animals__Annual) &
#                   !is.na(Corn_silage))%>%
#   group_by(Farm_ID)
# 
# Number_Obs_data = lapply(list(Monthly_data, Test_data, Annual_data), function(x)dim(x)[1])
# Number_Obs_data
# 
# #---------------------------------  
# #Check missing values
# #---------------------------------
# missing_fn = function(local_data, col_index){
#   return(sum(is.na(local_data[,col_index])))
# }
# 
# check_dataset_missing_fn = function(data_set){
#   missing_value = sapply(1:dim(data_set)[2], function(x) missing_fn(data_set, x))
#   missing_value = data.frame(missing_value)
#   missing_value$Var = colnames(data_set)
#   colnames(missing_value)[1] = "Number_of_Missing_Values"
#   missing_value = missing_value[, c(2, 1)]
#   return(missing_value)
# }
# 
# count_missing_value = lapply(list(Monthly_data, Test_data, Annual_data), check_dataset_missing_fn)
# 
# #-----------------------------------
# #Model Inputs and Outputs
# #-----------------------------------
# Inputs = c("X__Cows",
#            "Dry_Matter", "CP__" , "Starch___DM", "pH", 
#            "Pur_Feed_Calc", "Feed_Cost__All_Animals__Annual", "Corn_silage")
# 
# Outputs = c("Milk_per_Milk_Cow", "X__Fat", "X__Pro",
#             "MUN", "Fecal_Starch")
# 
# 
# dim(Test_data[Inputs])
# 
# #-----------------------------------
# #Model 4-1
# #-----------------------------------
# Inputs_Model4 = Inputs[1:5]
# 
# for(a in Outputs){
#   summary(dea(data.frame(Test_data[Inputs_Model4]), data.frame(Test_data[a])))
# }
# 
# 
# 
# 
# #-----------------------------------
# #Model 4-2
# #-----------------------------------
# 
# 
# eff_Model42 = data.frame(matrix(NA, ncol = 7, nrow = dim(Test_data)[1]))
# colnames(eff_Model42) = c("Farm_ID", "Season_Year", Outputs)
# eff_Model42$Farm_ID = Test_data$Farm_ID
# eff_Model42$Season_Year = Test_data$Season_Year
# 
# for (a in Outputs){
#   for (t in unique(Test_data$Season_Year)){
#     local_data = Test_data[Test_data$Season_Year == t,]
#     local_eff = dea(data.frame(local_data[Inputs_Model4]), data.frame(local_data[a]))$eff
#     eff_Model42[Test_data$Season_Year == t, a] = local_eff
#     
#     plot_ = ggplot(eff_Model42, aes(x = Season_Year, y = eff_Model42[,a], color = Farm_ID))+
#       geom_point()+
#       ggtitle(paste('Histogram of Efficiency Scores of', a))
#     print(plot_)
#   }
# }
# 
# 
# kable(eff_Model42, digits = 3, caption = "Test Data Efficiency Score over Time")
# 
# 
# for(a in Outputs){
#   EFF = 
#     Test_data%>%
#     group_by(Season_Year)%>%
#     do({
#       data.frame(
#         dea(data.frame(select(., 
#                               X__Cows,
#                               Dry_Matter, 
#                               CP__, 
#                               Starch___DM, 
#                               pH)), 
#             data.frame(select_(., a))
#         )$eff)
#     })
#   
#   colnames(EFF)[2] = "Efficiency"
#   
#   summary(EFF)
#   ggplot(EFF, aes(Efficiency, color = Season_Year))+
#     geom_histogram()+
#     ggtitle(paste('Histogram of Efficiency Scores of', a))
# }
# 
# 
# 
# #select_(Test_data, one_of(Inputs_Model4[2]))
# 
# 
# #-----------------------------------
# #Model 5
# #-----------------------------------
# 
# for(a in Outputs){
#   summary(dea(data.frame(Test_data[Inputs]), data.frame(Test_data[a])))
# }
