library(data.table)
library(ggplot2)

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


#############################
# combine train2016_clean and train2017_clean
train_clean <- rbind(train2016_clean, train2017_clean)

wk <- function(x) as.numeric(format(x, "%U")) 
wk_of_month <- function(x) {
    wk(x) - wk(as.Date(cut(x, "month"))) + 1
}
vars <- c("year", "month")
funs <- c("%Y", "%m")
for(i in 1:2){
    train_clean[, vars[i] := factor(format(transactiondate, funs[i]))] 
}
train_clean[, "week_of_month" := wk_of_month(transactiondate)] 

summaryfun <- function(x){
    list(N = length(x),
         Mean = mean(x),
         Median = median(x),
         Min = min(x),
         Max = max(x))
}
agg_by_date <- train_clean[, summaryfun(logerror), by = transactiondate]
agg_by_month_year <- train_clean[, summaryfun(logerror), by = list(month, year)]
agg_by_month_year_m <- melt(agg_by_month_year,
                            id.vars = c("month", "year"),
                            measure.vars = c("Mean", "Median", "Min", "Max"))
summary(agg_by_month_year_m)

ggplot(agg_by_date, aes(transactiondate)) + 
    geom_line(aes(y = Mean, colour = "Mean")) + 
    geom_line(aes(y = Median, colour = "Median")) +
    geom_line(aes(y = Min, colour = "Min")) + 
    geom_line(aes(y = Max, colour = "Max")) +
    scale_colour_manual(values=c("black", "orange", "green", "blue"))

ggplot(agg_by_month_year_m, aes(x = month, y = value, colour = year, 
                                shape = variable, 
                                group = interaction(variable, year))) + 
    geom_point() + geom_line()

summary(train2017)
#######################
# properties 2016
properties2016 <- fread(paste0('~/Desktop/zillow_home_value_prediction/data/',
                               'properties_2016.csv'), header = T)
summary(properties2016)








