library(xlsx)
data = read.xlsx(file = 'Monthly.xlsx', 1)
data$Farm_ID = factor(data$Farm_ID)
summary(data)
dim(data)
#Farm_ID and Number of Farms
unique(data$Farm_ID)
length(unique(data$Farm_ID))

#Check Missing Value
missing_fn = function(col_index){
  return(sum(is.na(data[,col_index])))
}
result = sapply(1:dim(data)[2], function(x) missing_fn(x))
missing_value = as.data.frame(result)
missing_value$Var = colnames(data)
colnames(missing_value)[1] = "Number_of_Missing_Values"
missing_value = missing_value[, c(2, 1)]
knitr::kable(missing_value, caption = "A table produced by printr.")


#Data in Sample
sample = data[!is.na(data$Sample_Number), ]
dim(sample)
unique(sample$Farm_ID)
length(unique(sample$Farm_ID))
#Every Farm has been sampled. Good!

#1
#Heterogeneity Test for sample vs. nonsample data


#Test run
sample_3 = data[(!is.na(data$Sample_Number)) & data$Farm_ID == 3,3]
nonsample_3 = data[(is.na(data$Sample_Number)) & data$Farm_ID == 3,3]
# Kruskal Wallis Test 
kruskal.test(list(sample_3, nonsample_3))$p.value
# Kolmogorov-Smirnov Test  (two-sided)
ks.test(sample_3, nonsample_3, exact = T)$p.value
# Wilcoxon Rank Sum Test(equivalently Mann-Whitney test)
wilcox.test(sample_3, nonsample_3, mu = 0)$p.value # mu = 0 default


kruskal_test_fn = function(Farm_ID, col_index, data){
  local_sample = data[(!is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  local_nonsample = data[(is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  return(kruskal.test(list(local_sample, local_nonsample))$p.value)
}

ks_test_fn = function(Farm_ID, col_index, data){
  local_sample = data[(!is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  local_nonsample = data[(is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  return(ks.test(local_sample, local_nonsample)$p.value)
}


wilcox_test_fn = function(Farm_ID, col_index, data){
  local_sample = data[(!is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  local_nonsample = data[(is.na(data$Sample_Number)) & data$Farm_ID == Farm_ID, col_index]
  return(wilcox.test(local_sample, local_nonsample)$p.value)
}


# Kruskal Wallis Test 

kruskal_test = outer(
  unique(sample$Farm_ID), 
  (3:13),
  Vectorize(function(x, y) kruskal_test_fn(x, y, data))
)
kruskal_test = as.data.frame(kruskal_test)
colnames(kruskal_test) = colnames(data)[3:13]
kruskal_test$Farm_ID = unique(data$Farm_ID)
kruskal_test = kruskal_test[,c(12, 1 : 11)]
knitr::kable(kruskal_test, digits = 2, caption = "A table produced by printr.")


# Kolmogorov-Smirnov Test  (two-sided)

ks_test = outer(
  unique(sample$Farm_ID), 
  (3:13),
  Vectorize(function(x, y) ks_test_fn(x, y, data))
)
ks_test = as.data.frame(ks_test)
colnames(ks_test) = colnames(data)[3:13]
ks_test$Farm_ID = unique(data$Farm_ID)
ks_test = ks_test[,c(12, 1 : 11)]
knitr::kable(ks_test, digits = 2, caption = "A table produced by printr.")


# Wilcoxon Rank Sum Test(equivalently Mann-Whitney test)

wilcox_test = outer(
  unique(sample$Farm_ID), 
  (3:13),
  Vectorize(function(x, y) wilcox_test_fn(x, y, data))
)
wilcox_test = as.data.frame(wilcox_test)
colnames(wilcox_test) = colnames(data)[3:13]
wilcox_test$Farm_ID = unique(data$Farm_ID)
wilcox_test = wilcox_test[,c(12, 1 : 11)]
knitr::kable(wilcox_test, digits = 2, caption = "A table produced by printr.")


#2
#Examine heterogeneity of Number of Cows across farms

#####################################################################
#kruskal_test_Farm_ID_fn = function(col_index, threshold, threshold2 , data){
#  return(kruskal.test(data[data$X__Cows <= threshold2 & data$X__Cows >= threshold1, col_index]~data$Farm_ID)$p.value)
#}
#kruskal_test_by_Farm_ID = sapply(
#  (3:13), function(x) kruskal_test_Farm_ID_fn(x, data))
####################################################################                          
kruskal.test(data$X__Cows ~ data$Farm_ID)               

#Plot Number of Cows (X__Cows) by Farm_ID
library(ggplot2)
ggplot(data = data, aes(x = X__Cows,  fill = Farm_ID))+
geom_histogram(binwidth = 15)+
xlab('Number of Cows') +
ylab('Count') +
ggtitle('Number of Cows by Farm ID')+
labs(colour = "Farm ID")

ggplot(data, aes(X__Cows, colour = Farm_ID)) +
geom_freqpoly(binwidth = 17)+
xlab('Number of Cows') +
ylab('Count') +
ggtitle('Number of Cows by Farm ID')+
labs(colour = "Farm ID")



#Part 3 DEA
#try
library(Benchmarking)
x = matrix(c(1, 2, 2), ncol = 1)
y = matrix(c(1, 2, 2, 1, 3, 2), ncol = 2)
dea(x, y)
dea.plot.frontier(x, y)

dea_Milk = dea(data$X__Milk_Cows, data$Milk_per_Milk_Cow)
dea_fat = dea(data$X__Milk_Cows, data$X__Fat)
dea_pro = dea(data$X__Milk_Cows, data$X__Pro)
eff = cbind(dea_Milk$eff, dea_fat$eff, dea_pro$eff)
eff = as.data.frame(eff)
colnames(eff) = colnames(data)[7:9]
eff$Farm_ID = data$Farm_ID
eff = eff[,c(4,1,2,3)]

eff_Milk = data[dea_Milk$eff == 1,]
eff_fat = data[dea_fat$eff == 1,]
eff_pro = data[dea_pro$eff == 1,]

plot(eff_Milk$X__Milk_Cows, eff_Milk[, 7])
plot(eff_fat$X__Milk_Cows, eff_fat[, 8])
plot(eff_pro$X__Milk_Cows, eff_pro[, 9])

eff_Milk[,c(3,10)]
eff_fat[,c(4, 10)]
eff_pro[,c(5, 10)]


ggplot(data, aes(X__Milk_Cows, Milk_per_Milk_Cow , colour = Farm_ID))+ 
geom_point(size = .7)+
ylim(10, 100)+
geom_segment(aes(x = 25, y = 60 , xend = 25, yend = 71), size = .8, color = 4)+
geom_segment(aes(x = 25, y = 71, xend = 27, yend = 84), size = .8, color = 4)+
geom_segment(aes(x = 27, y = 84, xend = 36, yend = 94), size = .8, color = 4)+
geom_segment(aes(x = 36, y = 94, xend = 106, yend = 98), size = .8, color = 4)+
geom_segment(aes(x = 106, y = 98, xend = 145, yend = 99), size = .8, color = 4)+
geom_segment(aes(x = 145, y = 99, xend = 429, yend = 100), size = .8, color = 4)+
xlab('Number of Milk Cows') +
ylab('Milk per Milk Cow (lbs)') +
ggtitle('Efficient Frontier of Milk per Milk Cow (lbs)')+
labs(colour = "Farm ID")

  
  
ggplot(data, aes(X__Milk_Cows, X__Fat, colour = Farm_ID))+ 
geom_point(size = .7)+
ylim(1, 5.5)+  
geom_segment(aes(x = 25, y = 3.6, xend = 25, yend = 4.0), size = .8, color = 4)+
geom_segment(aes(x = 25, y = 4.0, xend = 28, yend = 4.4), size = .8, color = 4)+
geom_segment(aes(x = 28, y = 4.4, xend = 31, yend = 4.5), size = .8, color = 4)+
geom_segment(aes(x = 31, y = 4.5, xend = 69, yend = 5.4), size = .8, color = 4)+
xlab('Number of Milk Cows') +
ylab('Fat (%)') +
ggtitle('Efficient Frontier of Fat (%)')+
labs(colour = "Farm ID")


ggplot(data, aes(X__Milk_Cows, X__Pro, colour = Farm_ID))+ 
geom_point(size = .7)+
ylim(2, 3.6)+  
geom_segment(aes(x = 25, y = 2.9, xend = 25, yend = 3.2), size = .8, color = 4)+
geom_segment(aes(x = 25, y = 3.2, xend = 26, yend = 3.3), size = .8, color = 4)+
geom_segment(aes(x = 26, y = 3.3, xend = 41, yend = 3.6), size = .8, color = 4)+
xlab('Number of Milk Cows') +
ylab('Protein (%)') +
ggtitle('Efficient Frontier of Protein (%)')+
labs(colour = "Farm ID")


ggplot(data, aes(X__Milk_Cows, ME_305_Fat, colour = Farm_ID))+ 
  geom_point(size = .8)+
  geom_segment(aes(x = 25, y = 806, xend = 25, yend = 992), size = .9, color = 1)+
  geom_segment(aes(x = 25, y = 992, xend = 42, yend = 1077), size = .9, color = 1)+
  geom_segment(aes(x = 42, y = 1077, xend = 43, yend = 1079), size = .9, color = 1)+
  geom_segment(aes(x = 43, y = 1079, xend = 106, yend = 1188), size = .9, color = 1)+
  geom_segment(aes(x = 106, y = 1188, xend = 122, yend = 1193), size = .9, color = 1)+
  xlab('Number of Milk Cows') +
  ylab('ME_305_Fat') +
  ggtitle('Efficient Frontier of ME_305_Fat')+
  labs(colour = "Farm ID")


ggplot(data, aes(X__Milk_Cows, ME_305_Pro, colour = Farm_ID))+ 
  geom_point(size = .8)+
  geom_segment(aes(x = 25, y = 635, xend = 25, yend = 742), size = .9, color = 4)+
  geom_segment(aes(x = 25, y = 742, xend = 27, yend = 823), size = .9, color = 4)+
  geom_segment(aes(x = 27, y = 823, xend = 43, yend = 905), size = .9, color = 4)+
  geom_segment(aes(x = 43, y = 905, xend = 63, yend = 927), size = .9, color = 4)+
  geom_segment(aes(x = 63, y = 927, xend = 115, yend = 930), size = .9, color = 4)+
  geom_segment(aes(x = 115, y = 930, xend = 950, yend = 932), size = .9, color = 4)+
  xlab('Number of Milk Cows') +
  ylab("ME_305_Pro ") +
  ggtitle('Efficient Frontier of ME_305_Pro')+
  labs(colour = "Farm ID")



##################Merge Yearly Data########################
data_year = read.xlsx(file = "Annual.xlsx", 3)
data_year$Farm_ID = factor(data_year$Farm_ID)
data_new = merge(data, data_year, by = c("Farm_ID", "year"), all.x = T)
write.xlsx(data_new, file = "data_new.xlsx")
sum(data_new$Farm_ID == data$Farm_ID)


#Check Missing Values
#Check Missing Value
missing_fn = function(col_index){
  return(sum(is.na(data_new[,col_index])))
}
result = sapply(1:dim(data_new)[2], function(x) missing_fn(x))
missing_value_new = as.data.frame(result)
missing_value_new$Var = colnames(data_new)
colnames(missing_value_new)[1] = "Number_of_Missing_Values"
missing_value_new = missing_value_new[, c(2, 1)]
knitr::kable(missing_value_new, caption = "A table produced by printr.")

data_new_no_missing = data_new[!(is.na(data_new$Pur_Feed_Calc) |
                                  is.na(data_new$Corn_silage) |
                                  is.na(data_new$Crop_Cost_Calc)), ] 



dea_four_input = dea(cbind(data_new_no_missing$X__Milk_Cows, 
                           data_new_no_missing$Pur_Feed_Calc,
                           data_new_no_missing$Corn_silage, 
                           data_new_no_missing$Crop_Cost_Calc), 
                     matrix(data_new_no_missing$Milk_per_Milk_Cow))
summary(dea_four_input)
dea.plot(cbind(data_new_no_missing$X__Milk_Cows, 
               data_new_no_missing$Pur_Feed_Calc,
               data_new_no_missing$Corn_silage, 
               data_new_no_missing$Crop_Cost_Calc), 
         matrix(data_new_no_missing$Milk_per_Milk_Cow))
eff_four_input = data_new_no_missing[dea_four_input$eff == 1, ]
eff_four_input[, c("X__Milk_Cows", "Pur_Feed_Calc", "Corn_silage", "Crop_Cost_Calc", "Milk_per_Milk_Cow")]



x = data_new_no_missing$X__Milk_Cows +
    data_new_no_missing$Pur_Feed_Calc +
    data_new_no_missing$Corn_silage +
    data_new_no_missing$Crop_Cost_Calc

x_eff = eff_four_input$X__Milk_Cows+
        eff_four_input$Pur_Feed_Calc+
        eff_four_input$Corn_silage+
        eff_four_input$Crop_Cost_Calc

ggplot(data_new_no_missing, aes(x = x, y = Milk_per_Milk_Cow, colour = Farm_ID))+
  geom_point(size = .5)+
  ylim(0, 100)+
  geom_point(data = eff_four_input, aes(x = x_eff, y = Milk_per_Milk_Cow),  size = 1, shape = 3, stroke = 1)





######Group Data###3
library(dplyr)
#-------
#by_Farm_ID = group_by(data, Farm_ID)
#by_Farm_ID_ordered = arrange(by_Farm_ID, DHIDate)



#
#------


### chaining ###
by_Farm_ID_ordered1 = 
  data %>%
    group_by(Farm_ID) %>%
      arrange(DHIDate)

print_sample_number = function(some_data){
  data.frame(some_data$Sample_Number[!is.na(some_data$Sample_Number)])
}

do(by_Farm_ID_ordered1, print_sample_number(.))

#Better chaining###
S_N = 
data %>%
  group_by(Farm_ID) %>%
  arrange(DHIDate) %>%
  do({data.frame(.$Sample_Number[!is.na(.$Sample_Number)])
  })
S_N
