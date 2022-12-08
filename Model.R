'Now, we use an Ordinary Least Square model to estimate parameters'
'We will use a no intercept model, since it is not sensible for a building to have 0 price'


'MODEL BUILDING'
# Importing the data 
data<-read.csv('/Users/viren/Documents/MSC stats Loyola/regression_miniproject/chennai_housing_data_cleaned.csv')
data<-subset(data,select=-c(X.1,X,DATE_SALE,DATE_BUILD,PRT_ID))
head(data)
names(data)

# Creating the dummy variables
install.packages('fastDummies')
library('fastDummies')
dummies<-dummy_cols(data,remove_most_frequent_dummy = TRUE)
head(dummies)

new_data<-subset(dummies,select=-c(AREA,SALE_COND,PARK_FACIL,
                                   BUILDTYPE,UTILITY_AVAIL,STREET))

head(new_data)
names(new_data)

# Making a small adjustment to column names

colnames(new_data)[12]<-'AREA_AnnaNagar'
colnames(new_data)[14]<-'AREA_KKnagar'
colnames(new_data)[19]<-'SALE_COND_NormalSale'
names(new_data)

# Building the model

OLS_model<-lm(SALES_PRICE~INT_SQFT+DIST_MAINROAD+N_BEDROOM+N_BATHROOM+N_ROOM+REG_FEE+
              COMMIS+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
              AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
              SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
              BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
              STREET_Gravel+STREET_NoAccess+0,data=new_data)

summary(OLS_model)

# write.csv(new_data,'/Users/viren/Desktop/regression_miniproject/final_model_data.csv')

# Now, we perform forward and backward selection on the model with 5% level of significance
install.packages('olsrr')
library('olsrr')
ols_step_forward_p(OLS_model,details=TRUE,penter=0.05)

ols_step_backward_p(OLS_model,details=T,prem=0.05)

# It is clear that 'DIST_MAINROAD' is not a significant variable in either forward or backward algorithms
# Thus, we build the model without it 

updated_data<-subset(new_data,select=-c(DIST_MAINROAD))

updated_model<-lm(SALES_PRICE~INT_SQFT+N_BEDROOM+N_BATHROOM+N_ROOM+REG_FEE+
                COMMIS+SALES_PRICE+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
                AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
                SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
                BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
                STREET_Gravel+STREET_NoAccess+0,data=updated_data)

summary(updated_model) # Contains the values needed for model accuracy checking

# From the summary, it is clear that apart from UTILITY_AVAILABLE_ELO and
# N_BEDROOM which are significant, all variables are highly significant 

confint(updated_model)


'CHECKING MODEL ASSUMPTIONS'
# Now, we move on the residual plots

res<-resid(updated_model)
options(scipen=999)
plot(fitted(updated_model),res)
abline(0,0)
# There seems to be no significant model defect

qqnorm(res)
qqline(res)

# The QQ plot shows that there is a positive skew in the residuals as there is a 
# significant upward deviation from the central line at the top

# We now calculate the studentized residuals
library('MASS')

stud<-studres(updated_model)
predictions<-updated_model$fitted.values

for (i in 1:length(stud)){
  if (stud[i]>=4){
    print(stud[i])
  }
}

# Now we perform Durbin-Watson test to check whether the residuals are auto-correlated
library('carData')
library('car')

durbinWatsonTest(updated_model)

# As p-value is greater than 0.05, we accept null hypothesis that there is no auto-correlation present

# Checking for interaction effect between variables:

inter_model<-lm(SALES_PRICE~(INT_SQFT+N_BEDROOM+N_BATHROOM+N_ROOM+REG_FEE+
                    COMMIS+SALES_PRICE+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
                    AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
                    SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
                    BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
                    STREET_Gravel+STREET_NoAccess+0)^2,data=updated_data)

summary(inter_model)

'MULTICOLINEARITY'

library('psych')
in_vars<-subset(updated_data,select=-(SALES_PRICE))
names(in_vars)    

corPlot(in_vars)

# It seems that the variables INT_SQFT and N_ROOM have high correlation with other variables
# So we build a new model without them

updated_data<-subset(updated_data,select=-c(N_ROOM,INT_SQFT))

updated_model<-lm(SALES_PRICE~(N_BEDROOM+N_BATHROOM+REG_FEE+
                          COMMIS+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
                          AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
                          SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
                          BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
                          STREET_Gravel+STREET_NoAccess+0),data=updated_data)
summary(updated_model)

# Now, the model loses very little R-squared but the N_BATHROOM variable is now insignificant
# So, we build a model without it 
updated_data<-subset(updated_data,select=-c(N_BATHROOM))

updated_model<-lm(SALES_PRICE~(N_BEDROOM+REG_FEE+
                                 COMMIS+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
                                 AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
                                 SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
                                 BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
                                 STREET_Gravel+STREET_NoAccess+0),data=updated_data)
summary(updated_model)

# Now, all remaining variables are significant with only negligible loss in predicting power 

'MODEL VALIDATION'

df<-updated_data
library(dplyr)

set.seed(1)

df$id <- 1:nrow(df)

train <- df %>% dplyr::sample_frac(0.70) # 70% of data used as training dataset
test  <- dplyr::anti_join(df, train, by = 'id')

dim(train)
dim(test)
dim(updated_data)

model_train<-lm(SALES_PRICE~(N_BEDROOM+REG_FEE+
                               COMMIS+SALES_PRICE+YEAR_BUILD+YEAR_SOLD+AREA_Adyar+AREA_AnnaNagar+
                               AREA_Karapakkam+AREA_KKnagar+AREA_TNagar+AREA_Velachery+SALE_COND_Abnormal+
                               SALE_COND_Family+SALE_COND_NormalSale+SALE_COND_Partial+PARK_FACIL_No+
                               BUILDTYPE_Commercial+BUILDTYPE_Other+UTILITY_AVAIL_AllPub+UTILITY_AVAIL_ELO+
                               STREET_Gravel+STREET_NoAccess+0),data=train)
summary(model_train) # Model is almost as good as the total

predictions<-predict(model_train,newdata = test)
y<-test$SALES_PRICE

residuals<-predictions-y

summary(residuals)




    