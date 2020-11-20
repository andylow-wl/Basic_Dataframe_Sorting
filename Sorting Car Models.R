## Andy Low Wei Liang, 

rm(list=ls())
autoswithout = read.csv("autoswithout.csv")
model_brand = read.csv("model_and_brand.csv")


## Data Preprocessing
model_brand$brand = as.character(model_brand$brand)
print(paste("There are ",sum(is.na(model_brand$brand))," observation(s) of NA in brands."))
print(paste("There are ",length(unique(model_brand$brand))," unique brand(s) including NA."))
model_brand$model = as.character(model_brand$model)
print(paste("There are ",sum(is.na(model_brand$model))," observation(s) of NA in models."))
print(paste("There are ",length(unique(model_brand$model))," unique model(s) including NA."))
autoswithout$name = as.character(autoswithout$name)
print(paste("There are ",length(autoswithout$name)," observations of names."))
print(paste("There are ",sum(is.na(autoswithout$name))," observation(s) of NA in names"))

# Taking unique brands for loop
unique_brands = unique(model_brand['brand'])
unique_brands = unique_brands[!is.na(unique_brands)] # removing NA's 
print(paste("There are ",length(unique_brands)," unique brands excluding NA"))
unique_brands #observing brands


## Creating a dataframe to store number of counts of model
unique_models =unique(model_brand['model'])
unique_models= unique_models[!is.na(unique_models)]
count = rep(0,length(unique_models))
unique_models_df= data.frame(unique_models,count)
colnames(unique_models_df)= c("model","Count")


## Removing na values
model_brand =na.omit(model_brand)


## Counting the number of times each model appears in the model_brand.csv
for (i in 1:nrow(model_brand)){
  unique_models_df[unique_models_df$model==model_brand$model[i],2] = unique_models_df[unique_models_df$model==model_brand$model[i],2]+1
}

## Remove rows where same model appears more than 1 times
unique_models_df_check = unique_models_df[unique_models_df$Count<2,]


## Inner join the df_model with the original data to get the column "brand"
library(dplyr)
unique_models_df_final = inner_join(unique_models_df_check,model_brand,by="model")



## This will be our predictions column
assigned_brand = c()


## To track unassigned names
unassigned_name =c()

print(paste("There are ",length(autoswithout$name)," observations in our test set."))
for(names in autoswithout$name){
  check_brand = FALSE  #boolean check for brand
  check_model = FALSE  #boolean check for model
  
  ## checking for brand name inside autoswithout$name
  for(brands in unique_brands){
    if (grepl(brands,names,ignore.case = TRUE)){
      check_brand = TRUE 
      if(brands == "merce"){
        assigned_brand = c(assigned_brand,"mercedes_benz")
        break
      }else{
        assigned_brand = c(assigned_brand,brands)
        break
      }
    }
  }
  
  if(check_brand ==FALSE){
    if (grepl("Mercedes",names,ignore.case = TRUE)){
      assigned_brand = c(assigned_brand,"mercedes_benz")
      check_brand= TRUE
    }else if(grepl("VW",names,ignore.case = TRUE)){
      assigned_brand = c(assigned_brand,"volkswagen")
      check_brand= TRUE
    }
  }
    
  
  
  ## checking for model name inside autoswithout$name only if brand not inside name
  if(check_brand == FALSE){
    for(i in 1:length(unique_models_df_final$model)){
      if (grepl(unique_models_df_final$model[i],names,ignore.case = TRUE)){
        assigned_brand = c(assigned_brand,unique_models_df_final$brand[i])
        check_model = TRUE
        break
      }
    }
  }
  
  
  ## if both brand and model check does not work,assign NA as brand 
  if(check_brand == FALSE && check_model ==FALSE){
    assigned_brand = c(assigned_brand,NA)
    unassigned_name = c(unassigned_name,names) ## keeping to create unassigned dataframe
  }
}


########################Creating dataframe with unassigned names##############


unassigned = data.frame(unassigned_name)
colnames(unassigned) = c("name")
unassigned$name = as.character(unassigned$name)
returnNA = inner_join(unassigned,autoswithout,by="name")




      
