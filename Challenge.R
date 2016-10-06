### Totvs Challenge - Data Scientist 
### FABIO SILVA
#######################################

library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)
library (jsonlite)

# PARSE AND EXTRACT DATA ####

setwd("/Users/fabiosilva/Documents/Totvs")
json_file <- "/Users/fabiosilva/Documents/Totvs/sample.txt"
data.json <- fromJSON(json_file,flatten = TRUE)

data <- data.json
names(data.json)
dim(data.json)

#Get List of All Items
all.items.purcharsed <- data.frame()
for (i in 1:length(data.json$dets)) {
  all.items.purcharsed <- rbind(all.items.purcharsed , cbind( data.json$dets[[i]],i ))
}
colnames(all.items.purcharsed)[8] <- "Nota_Fiscal_ID"


# GET INSIGHTS FROM DATA ####

#Average Spent by customers
avg_spent_by_customer <- mean(data$complemento.valorTotal)

#Average spent broken down by product
avg_spent_by_product <- 
  aggregate(all.items.purcharsed$prod.vProd, by = list(all.items.purcharsed$prod.xProd), FUN = mean  )
colnames(avg_spent_by_product) <- c("Product","AvgSpent")
avg_spent_by_product <- avg_spent_by_product[order(-avg_spent_by_product$AvgSpent),]

#% of customers who bought each product apart from buffet
product_bought_apart_buffet <- table(all.items.purcharsed[all.items.purcharsed$prod.xProd != "BUFFET",]$prod.xProd
) /nrow(data)

attach(all.items.purcharsed)
plot.products <- 
subset(all.items.purcharsed[all.items.purcharsed$prod.xProd != "BUFFET",], select = c(prod.xProd,prod.vProd) ) %>%
group_by(prod.xProd) %>%
summarize(count = n())

#% Product Mix apart from Buffet
plot_ly(plot.products, labels = prod.xProd, values = count, type = "pie", hole = 0.6, showlegend = T) %>%
  layout(title = "Product Mix (Excluding Buffet) All Time")

#total spent by timestamp
total_spent_by_timestamp <- 
  as.data.frame( cbind(  
    data.json$`ide.dhEmi.$date`,
    data.json$complemento.valorTotal
) ) 

total_spent_by_timestamp[,1] <- gsub("T", " ",total_spent_by_timestamp[,1] )
colnames(total_spent_by_timestamp) <- c("datetime","valorTotal")
total_spent_by_timestamp[,1] <- as.POSIXct(total_spent_by_timestamp[,1], format = "%Y-%m-%d %H:%M:%S")


#avg spent by time of day
time <- format(total_spent_by_timestamp[,1],"%H:00:00")
total_spent_by_timestamp["time"] <- time
total_spent_by_time_of_day <- as.data.frame( 
aggregate(as.numeric(total_spent_by_timestamp$valorTotal), by = list(total_spent_by_timestamp$time), FUN = mean )
)
names(total_spent_by_time_of_day) <- c("Time","AvgSpent")
ggplot(total_spent_by_time_of_day, aes(total_spent_by_time_of_day$Time,total_spent_by_time_of_day$AvgSpent)) + 
    geom_point(color = "blue") + 
    geom_line(group = 1) + 
    labs(title = "Revenue By Hour", x = "Hour", y = "Revenue")


#avg spent by day of the week
avg_spent_by_weekday <- aggregate(as.numeric(total_spent_by_timestamp$valorTotal),
          by = list(weekdays(as.Date(total_spent_by_timestamp$datetime))), FUN = mean)

ggplot(avg_spent_by_weekday, 
       aes( x = factor( avg_spent_by_weekday$Group.1 ),
            y = avg_spent_by_weekday$x) ) + 
  geom_bar(stat = "identity", color="black", fill = "deepskyblue3") + 
  labs(title = "Average Revenue By Weekday", x = "Hour", y = "Average Revenue")
  

#products most sold by hour
temp <- data.frame(time,1:nrow(data))
colnames(temp) <- c("time", "Nota_Fiscal_ID")
res <- merge(temp,all.items.purcharsed[,c("prod.xProd","Nota_Fiscal_ID")],by="Nota_Fiscal_ID")

grp_cols <- names(res)[-1]
dots <- lapply(grp_cols, as.symbol)

products_by_hour <-  res %>%
    group_by_(.dots=dots) %>%
    summarise(n = n())

products_by_hour <- products_by_hour[with(products_by_hour,order(time,-n)),]


# % of product bought against buffet
buffet_by_hour <- products_by_hour[products_by_hour$prod.xProd == "BUFFET",]
temp <- merge(x = products_by_hour, y = buffet_by_hour, by = "time", all.x = TRUE)
temp <- data.frame(temp,temp$n.x / temp$n.y)
temp$temp.n.x.temp.n.y <- temp$temp.n.x.temp.n.y*100

ggplot(temp[temp$prod.xProd.x != "BUFFET",],aes(x=factor(prod.xProd.x), y = temp.n.x.temp.n.y)) + 
  facet_wrap(~time) +
  geom_bar(stat="identity", aes(fill = factor(prod.xProd.x))) +
  theme(axis.text.x = element_blank()) +
  labs(title = "% of Meals with Each Kind of Product by Hour", x = "Product", y = "Percentage") +
  scale_fill_discrete(name="Product")


# BUILD FORECAST MODEL ####
hist(data.json$complemento.valorTotal, breaks = 200, main = "Distribution of Valor Total", xlab = "Valor Total by Customer") 

#check  unit price
ggplot(all.items.purcharsed) + 
  geom_bar(aes(all.items.purcharsed$prod.xProd,all.items.purcharsed$prod.vUnCom, fill = as.factor(all.items.purcharsed$prod.xProd)), 
           position = "dodge", stat = "summary", fun.y = "mean") +
  labs(title = "Unit Prices", x = "Product", y = "Price") +
  scale_fill_discrete(name="Product") +
  theme(axis.text.x = element_blank()) 

#build datasets
temp <- total_spent_by_timestamp[,1:2]
date_norm <- format(temp[,1],"%Y-%m-%d %H:00:00")
temp <- data.frame(date_norm,temp$valorTotal,1:nrow(temp))
colnames(temp) <- c("date_purchase", "TOTAL_SPENT","Nota_Fiscal_ID")
Data_Y <- aggregate(as.double(temp$TOTAL_SPENT), by = list(temp$date_purchase), FUN = sum)
res_products <- merge(temp,all.items.purcharsed[,c("prod.xProd","Nota_Fiscal_ID","prod.vProd")],by="Nota_Fiscal_ID")

#total buffet by datetime
buffet<- res_products[res_products$prod.xProd=="BUFFET",]
buffet <- buffet[,c(2,4)] 
buffet <- as.data.frame(table(buffet))
buffet <- buffet[,c(1,3)]
names(buffet) <- c("date_purchase","buffet_count")

#total refrigerante + agua by datetime
refri_agua<- res_products[(res_products$prod.xProd=="REFRIGERATE" | res_products$prod.xProd=="AGUA" ),]
refri_agua <- refri_agua[,c(2,4)] 
refri_agua <- as.data.frame(table(refri_agua))
refri_agua <- refri_agua[,c(1,3)]
names(refri_agua) <- c("date_purchase","refri_agua_count")

#total others by datetime
others<- subset(res_products,!(prod.xProd %in% c("AGUA","REFRIGERANTE","BUFFET")))
others$prod.xProd <- "Others"
others <- others[,c(2,4)]
others <- as.data.frame(table(others))
others <- others[,c(1,3)]
names(others) <- c("date_purchase","others_count")

#merge in one single dataset
temp_2 <- merge(x = buffet, y = refri_agua, by = "date_purchase", all.x = TRUE)
temp_2[is.na(temp_2)] <- 0
temp_2 <- merge(x = temp_2, y = others, by = "date_purchase", all.x = TRUE)
temp_2[is.na(temp_2)] <- 0
temp_2 <- data.frame(temp_2,weekdays(as.Date(temp_2$date_purchase)), Data_Y$x)
temp_2[is.na(temp_2)] <- 0
time <- format(as.POSIXct(temp_2 [,1]),"%H:00:00")
data.model <- data.frame(temp_2[,2:6],time)
colnames(data.model) <- c("buffet_count", "refri_agua_count", "others_count","Weekday","Total_Spent","Time")


# Multiple Linear Regression  
train.data =  data.frame()
test.data =  data.frame()
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")

#make sure to get a sample from each day of week
for (i in 1:length(weekday))
{

  temp_weekday <- data.model[data.model$Weekday == weekday[i],]
  row_stop <- as.integer(nrow(temp_weekday)/4) #training data will get 75% of the dataset
  row_start <- row_stop + 1
  
  temp_weekday.test <- temp_weekday[1:row_stop,]
  test.data <- rbind(test.data,temp_weekday.test)
  
  temp_weekday.train <- temp_weekday[row_start:nrow(temp_weekday),]
  train.data <- rbind(train.data,temp_weekday.train)
  
}

#model
fit <- lm( train.data$Total_Spent~., data=train.data )
summary(fit) 

#### forecast next 7 days ###
#inputs: avg number of buffet sold on <weekday> at <time> for past data 
#inputs: avg number of refri+agua sold on <weekday>  at  <time> for past data 
#inputs: avg number of others sold on <weekday> at  <time> for past data 
#inputs: <weekday> 
#IMPORTAT: test inputs were NOT used for training, so the results are not biased.

test.data <- test.data[,c(1:4,6)] #removing response

#monday
monday_input <- test.data[test.data$Weekday=="Monday",] 
monday <- mean(predict(fit,monday_input))

#tuesday
tuesday_input <- test.data[test.data$Weekday=="Tuesday",]
predict(fit,tuesday_input)
tuesday <- mean(predict(fit,tuesday_input))

#wednesday
wednesday_input <- test.data[test.data$Weekday=="Wednesday",]
wednesday <- mean(predict(fit,wednesday_input))

#thursday
thursday_input <- test.data[test.data$Weekday=="Thursday",]
thursday <- mean(predict(fit,thursday_input))

#friday
friday_input <- test.data[test.data$Weekday=="Friday",]
friday <- mean(predict(fit,friday_input))

#saturday
saturday_input <- test.data[test.data$Weekday=="Saturday",]
saturday <- mean(predict(fit,saturday_input))


#WEEK TOTAL:
#sum and multiply by average working hours for each weekday
working_hours = c(8,8,8,8,4,4)

SALES_NEXT_WEEK <- 
  sum(monday*working_hours[1],
    tuesday*working_hours[2],
    wednesday*working_hours[3],
    thursday*working_hours[4],
    friday*working_hours[5],
    saturday*working_hours[6])

# OUTPUT PREDICTION ####
cat("Forecasted Revenue from Sales for Next Week: R$",SALES_NEXT_WEEK)

