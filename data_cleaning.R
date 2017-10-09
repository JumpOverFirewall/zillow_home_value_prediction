library(data.table)

#############################
# Check submission format
sampleSubmission <- fread(paste0('~/Desktop/zillow_home_value_prediction/',
                                 'data/sample_submission.csv'), header = T)
summary(sampleSubmission)


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

train2017_clean <- 
    train2017[!(train2017$row_id %in% c(44962, 9593, 4418, 22576)),]


summary(train2017)
#######################
# properties 2016
properties2016 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                               'properties_2016.csv'), header = T)
summary(properties2016)








