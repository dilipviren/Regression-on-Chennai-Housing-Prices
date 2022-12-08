# Importing cleaned data
data<-read.csv('/Users/viren/Desktop/regression_miniproject/chennai_housing_data_cleaned.csv')
head(data)
names(data)

# Dropping useless columns
vars<-c('X.1','X','PRT_ID','DATE_SALE','DATE_BUILD')
drop_vars<-names(data) %in% vars
data<-data[!drop_vars]
head(data)

# Visualising Data

options(scipen = 999)

boxplot(SALES_PRICE~AREA,data=data,xlab='Area',ylab='Price of house',main='Prices by Area')
# 'Clearly, the area of the house has an effect on price, as seen from the box plot'

plot(data$DIST_MAINROAD,data$SALES_PRICE,main='Price vs Distance from main road',
     xlab='Distance from the nearest main road',
     ylab='Price of the house',pch=2)
# 'There seems to be no clear linear relationship, thus the variable may be ignored'

boxplot(SALES_PRICE~N_BEDROOM,data=data,xlab='Number of Bedrooms',
        ylab='Price of house',main='Price by Number of bedrooms')
# 'As expected, there appears to be a linear relationship between the number of bedrooms and the price of a house'

boxplot(SALES_PRICE~N_BATHROOM,data=data,xlab='Number of Bathrooms',
        ylab='Price of house',main='Price by Number of bathrooms')
# 'In this case, there is a small effect, but the number of outliers in "1" and the closeness of the two means seems to show that,
# while there is a relationship, it is very small or negligible'

boxplot(SALES_PRICE~N_ROOM,data=data,xlab='Number of Rooms',
        ylab='Price of house',main='Price by Rooms')
# "Here, there seems to be a clear effect on the price due the number of rooms, as common sense would dictate"
# However, there seems to be almost no difference in prices of houses with 5 or 6 bedrooms

boxplot(SALES_PRICE~SALE_COND,data=data,xlab='Sale condition of the house',
        ylab='Price of house',main='Price by Sale condition')
# There seems to be only a very minute relationship between the sale condition and the price,meaning that it is not relevant in predicting price

boxplot(SALES_PRICE~PARK_FACIL,data=data,xlab='Whether parking facility is present',
        ylab='Price of house',main='Price by parking facility')
# There seems to be some small difference in prices, when parking is present or absent 

boxplot(SALES_PRICE~BUILDTYPE,data=data,xlab='Type of the building',
        ylab='Price of house',main='Price by type of building')
# There is clear difference in the average price of commercial and housing type buildings
# But there is a very small difference overall between housing type buildings and any other types

boxplot(SALES_PRICE~UTILITY_AVAIL,data=data,xlab='Types of utilities available',
        ylab='Price of house',main='Price by available utilities')
# There seems to be no difference in price of the building between the different types of utilities available

boxplot(SALES_PRICE~STREET,data=data,xlab='Type of street',
        ylab='Price of house',main='Price by type of street')
# The difference in average price and the spread do not seem to be varying by the type of street

build<-data.frame(price=data$SALES_PRICE,built=data$YEAR_BUILD)
head(build)
plot(x=build$built,y=build$price,xlab = 'Year built',ylab = 'Price')
# There seems to be a relationship between the year of building and the price, but it seems very small, almost negligible

unique(data$YEAR_SOLD)
mean_price<-aggregate(data$SALES_PRICE, list(data$YEAR_SOLD), FUN=mean)
plot(mean_price$Group.1,mean_price$x,type='o',col='red',
     xlab='Year Sold',ylab='Average price in the year',
     main='Average price by year sold')
# Clearly, the year of sale has an effect on price 

# Some variables, such as COMMIS and INT_SQFT, will almost certainly have an effect on the price (common sense dictates that the relationship would be almost direct)
# Thus, there is no reason to visualise such variables




