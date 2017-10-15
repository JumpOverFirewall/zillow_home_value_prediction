library(data.table)
library(ggplot2)
library(dummies)
library(xgboost)
library(Matrix)

#############################
# Check submission format
sampleSubmission <- fread(paste0('~/Desktop/zillow_home_value_prediction/',
                                 'data/sample_submission.csv'), header = T)
summary(sampleSubmission)
dim(sampleSubmission)

#############################
# train_2016_v2
train2016 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                          'train_2016_v2.csv'), header = T)
train2016$transactiondate <- as.Date(train2016$transactiondate)
train2016$parcelid <- factor(train2016$parcelid)

# check if there's any property sold multiple times within 31 days
cleaning_response <- function(data) {
    data$row_id <- 1:nrow(data)
    
    dup_parcel_id <- unique(data$parcelid[duplicated(data$parcelid)])
    dup_parcel_data <- data[data$parcelid %in% dup_parcel_id,]
    dup_parcel_data <- 
        dup_parcel_data[order(dup_parcel_data$transactiondate),]
    dup_parcel_data <- 
        dup_parcel_data[order(dup_parcel_data$parcelid),]
    dup_parcel_data$date_diff <-
        c(dup_parcel_data$transactiondate[2:nrow(dup_parcel_data)], 
          dup_parcel_data$transactiondate[nrow(dup_parcel_data)]) -
        dup_parcel_data$transactiondate
    # find rows that are oldest records of parcel that will be sold again within
    # 31 days
    rows_to_be_excluded <- 
        dup_parcel_data[duplicated(dup_parcel_data$parcelid, fromLast = T) &
                            dup_parcel_data$date_diff <= 31 &
                            dup_parcel_data$date_diff >= 0, ]
    
    cat('Parcels that will have oldest records excluded:\n')
    print(dup_parcel_data[dup_parcel_data$parcelid %in% 
                              rows_to_be_excluded$parcelid,])
    
    clean_data <- 
        data[!(data$row_id %in% rows_to_be_excluded$row_id),
             c('parcelid', 'logerror', 'transactiondate')]
    
    return(clean_data)
}

train2016_clean <- cleaning_response(train2016)
# 2 cases to be excluded
#    parcelid logerror transactiondate row_id date_diff
# 1: 12584665   0.2829      2016-06-15  45629  -84 days
# 2: 12584665   0.0478      2016-06-22  45630    7 days
# 3: 14367791   2.0550      2016-09-29  80674   36 days
# 4: 14367791   2.1240      2016-09-30  80675    1 days

#############################
# train_2017
train2017 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                          'train_2017.csv'), header = T)
train2017$transactiondate <- as.Date(train2017$transactiondate)
train2017$parcelid <- factor(train2017$parcelid)

train2017_clean <- cleaning_response(train2017)

#############################
# combine train2016_clean and train2017_clean
train_clean <- rbind(train2016_clean, train2017_clean)

# wk <- function(x) as.numeric(format(x, "%U")) 
# wk_of_month <- function(x) {
#     wk(x) - wk(as.Date(cut(x, "month"))) + 1
# }
vars <- c("year", "month")
funs <- c("%Y", "%m")
for(i in 1:2){
    train_clean[, vars[i] := factor(format(transactiondate, funs[i]))]
}
# train_clean[, "week_of_month" := wk_of_month(transactiondate)]

saveRDS(train_clean, file = paste0('~/Desktop/zillow_home_value_prediction/',
                                   'data/train_clean.rds'))

# summaryfun <- function(x){
#     list(N = length(x),
#          Mean = mean(x),
#          Median = median(x),
#          Min = min(x),
#          Max = max(x))
# }
# agg_by_date <- train_clean[, summaryfun(logerror), by = transactiondate]
# agg_by_month_year <- train_clean[, summaryfun(logerror), by = list(month, year)]
# agg_by_month_year_m <- melt(agg_by_month_year,
#                             id.vars = c("month", "year"),
#                             measure.vars = c("Mean", "Median", "Min", "Max"))
# summary(agg_by_month_year_m)
# 
# ggplot(agg_by_date, aes(transactiondate)) + 
#     geom_line(aes(y = Mean, colour = "Mean")) + 
#     geom_line(aes(y = Median, colour = "Median")) +
#     geom_line(aes(y = Min, colour = "Min")) + 
#     geom_line(aes(y = Max, colour = "Max")) +
#     scale_colour_manual(values=c("black", "orange", "green", "blue"))
# 
# ggplot(agg_by_month_year_m, aes(x = month, y = value, colour = year, 
#                                 shape = variable, 
#                                 group = interaction(variable, year))) + 
#     geom_point() + geom_line()
# 
# dim(train_clean)


####################################
#######################
# properties 2016
properties2016 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                               'properties_2016.csv'), header = T)
properties2016[, "year" := 2016] 

# summary(properties2016)

#######################
# properties 2017
properties2017 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                               'properties_2017.csv'), header = T)
properties2017[, "year" := 2017] 
# summary(properties2017)

properties <- rbindlist(list(properties2016, properties2017))

properties[, c("censustractandblock",
               "storytypeid",
               "typeconstructiontypeid",
               "pooltypeid10",
               "pooltypeid2",
               "pooltypeid7",
               "propertyzoningdesc",
               "propertycountylandusecode",
               "rawcensustractandblock",
               "regionidneighborhood",
               "regionidzip",
               "latitude",
               "longitude"):=NULL]
properties$hashottuborspa[properties$hashottuborspa == ''] <- NA

num_pred <- c("basementsqft", 
              "bathroomcnt",
              "bedroomcnt",
              "calculatedbathnbr",
              "finishedfloor1squarefeet",
              "calculatedfinishedsquarefeet",
              "finishedsquarefeet12",
              "finishedsquarefeet13",
              "finishedsquarefeet15",
              "finishedsquarefeet50",
              "finishedsquarefeet6",
              "fireplacecnt",
              "fullbathcnt",
              "garagecarcnt",
              "garagetotalsqft",
              "lotsizesquarefeet",
              "poolcnt",
              "poolsizesum",
              "roomcnt",
              "threequarterbathnbr",
              "unitcnt",
              "yardbuildingsqft17",
              "yardbuildingsqft26",
              "numberofstories",
              "structuretaxvaluedollarcnt",
              "taxvaluedollarcnt",
              "landtaxvaluedollarcnt",
              "taxamount",
              "taxdelinquencyyear",
              "yearbuilt")

cat_pred <- c("year",
              "airconditioningtypeid",
              "architecturalstyletypeid", 
              "buildingclasstypeid",
              "buildingqualitytypeid",
              "decktypeid",
              "fips",
              "hashottuborspa",
              "heatingorsystemtypeid",
              "propertylandusetypeid",
              "regionidcity",
              "regionidcounty",
              "fireplaceflag",
              "assessmentyear",
              "taxdelinquencyflag")


# for (col in cat_pred) {
#     cat(col, " : ", length(unique(properties[[col]])), " \n ")
# }


for (col in cat_pred) {
    properties[, (col) := factor(as.character(properties[[col]]))]
}

properties$decktypeid <- as.character(properties$decktypeid)
properties$decktypeid[is.na(properties$decktypeid)] <- 'other'
properties$decktypeid <- factor(properties$decktypeid)


properties$hashottuborspa <- as.character(properties$hashottuborspa)
properties$hashottuborspa[is.na(properties$hashottuborspa)] <- 'other'
properties$hashottuborspa <- factor(properties$hashottuborspa)


# dim(properties)
# summary(properties)

saveRDS(properties, file = paste0('~/Desktop/zillow_home_value_prediction/',
                                  'data/properties.rds'))

#####################################
# combine properties with train_clean
# train_clean <- readRDS(paste0('~/Desktop/zillow_home_value_prediction/',
#                               'data/train_clean.rds'))
# properties <- readRDS(paste0('~/Desktop/zillow_home_value_prediction/',
#                              'data/properties.rds'))

train_clean[, parcelid_year := paste0(parcelid, "_", year)]
properties[, parcelid_year := paste0(parcelid, "_", year)]

train_clean[, c("parcelid",
                "transactiondate",
                "year"):=NULL]
properties[, c("parcelid"):=NULL]

final_train <- merge(train_clean, properties, by = "parcelid_year", all.x = T)
final_train[, c("parcelid_year"):=NULL]

options(na.action='na.pass')
dense_matrix <- model.matrix(logerror ~ . - 1, data = final_train )
saveRDS(dense_matrix, paste0('~/Desktop/zillow_home_value_prediction/',
                             'data/dense_matrix.rds'))

###################
# run model
model <- xgb.cv(data = dense_matrix, label = final_train$logerror, 
                max_depth = 5,
                eta = 0.1,
                nrounds = 1500,
                objective = "reg:linear",
                eval_metric = "mae",
                nfold=5,
                stratified=F,
                print_every_n=10)

head(model$evaluation_log)
tree <- which.min(model$evaluation_log[[4]])

DMatrix <- xgb.DMatrix(data=dense_matrix, label=final_train$logerror)
trainid <- sample(c(rep(T, 117508),rep(F, 50361)),nrow(DMatrix))
dtrain <- slice(DMatrix, which(trainid))
dtest <- slice(DMatrix, which(!trainid))

watchlist <- list(train=dtrain, test=dtest)

finalmodel <- xgb.train(data = dtrain, 
                        watchlist = watchlist,
                        max_depth = 5,
                        eta = 0.1,
                        save_period = NULL,
                        nrounds = 75,
                        objective = "reg:linear",
                        eval_metric = "mae",
                        print_every_n=10)

xgb.save(finalmodel, 
         "/Users/yueshengu/Desktop/zillow_home_value_prediction/finalmodel.model")

finalmodel <- xgb.load("/Users/yueshengu/Desktop/zillow_home_value_prediction/finalmodel")


#############################
# Prepare data for prediction


pred_data_10 <- pred_data_11 <- pred_data_12 <- properties
pred_data_10$logerror <- pred_data_11$logerror <- pred_data_12$logerror <- 0
pred_data_10$month <- factor('10', levels = c('01', '02', '03', '04', '05', '06',
                                              '07', '08', '09', '10', '11', '12'))
pred_data_11$month <- factor('11', levels = c('01', '02', '03', '04', '05', '06',
                                              '07', '08', '09', '10', '11', '12'))
pred_data_12$month <- factor('12', levels = c('01', '02', '03', '04', '05', '06',
                                              '07', '08', '09', '10', '11', '12'))

pred_data_10[, c("parcelid_year"):=NULL]
pred_data_11[, c("parcelid_year"):=NULL]
pred_data_12[, c("parcelid_year"):=NULL]

pred_data_10_16 <- pred_data_10[pred_data_10$year == '2016',]
pred_data_10_17 <- pred_data_10[pred_data_10$year == '2017',]
pred_data_11_16 <- pred_data_11[pred_data_11$year == '2016',]
pred_data_11_17 <- pred_data_11[pred_data_11$year == '2017',]
pred_data_12_16 <- pred_data_12[pred_data_12$year == '2016',]
pred_data_12_17 <- pred_data_12[pred_data_12$year == '2017',]


options(na.action='na.pass')
dense_matrix_10_16 <- model.matrix(logerror ~ . - 1, data = pred_data_10_16)
predict_10_16 <- predict(object = finalmodel, newdata = dense_matrix_10_16, 
                         ntreelimit = 75)
rm(dense_matrix_10_16)
results <- data.frame(ParcelId = properties$parcelid[properties$year == '2016',],
                      '201610' = predict_10_16)
saveRDS(results, "result.rds")

dense_matrix_11_16 <- model.matrix(logerror ~ . - 1, data = pred_data_11_16)
predict_11_16 <- predict(object = finalmodel, newdata = dense_matrix_11_16, 
                         ntreelimit = 75)
rm(dense_matrix_11_16)
results <- data.frame(results, '201611' = predict_11_16)
saveRDS(results, "result.rds")

dense_matrix_12_16 <- model.matrix(logerror ~ . - 1, data = pred_data_12_16)
predict_12_16 <- predict(object = finalmodel, newdata = dense_matrix_12_16, 
                         ntreelimit = 75)
rm(dense_matrix_12_16)
results <- data.frame(results, '201612' = predict_12_16)
saveRDS(results, "result.rds")


dense_matrix_10_17 <- model.matrix(logerror ~ . - 1, data = pred_data_10_17)
predict_10_17 <- predict(object = finalmodel, newdata = dense_matrix_10_17, 
                         ntreelimit = 75)
rm(dense_matrix_10_17)
results <- data.frame(results, '201710' = predict_10_17)
saveRDS(results, "result.rds")


dense_matrix_11_17 <- model.matrix(logerror ~ . - 1, data = pred_data_11_17)
predict_11_17 <- predict(object = finalmodel, newdata = dense_matrix_11_17, 
                         ntreelimit = 75)
rm(dense_matrix_11_17)
results <- data.frame(results, '201711' = predict_11_17)
saveRDS(results, "result.rds")


dense_matrix_12_17 <- model.matrix(logerror ~ . - 1, data = pred_data_12_17)
predict_12_17 <- predict(object = finalmodel, newdata = dense_matrix_12_17, 
                         ntreelimit = 75)
rm(dense_matrix_12_17)
results <- data.frame(results, '201712' = predict_12_17)
write.csv(results, "result.csv", row.names = F)











