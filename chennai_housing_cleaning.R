# Importing the data 
data<-read.csv('/Users/viren/Desktop/regression_miniproject/chennai_housing_data.csv')
head(data)

# Exploring the data 
names(data)
summary(data)

# Dropping ZONE variable as it is meaningless while AREA is available
drop_var<-names(data)%in%c('MZZONE')
data<-data[!drop_var]
names(data)

unique(data$AREA)

# Some of the area names are duplicated with errors, so they must be replaced 
for (i in 1:(length(data$AREA))){
  area=data$AREA[i]
  if (area %in% c('Chrompt','Chrmpet','Chormpet','Chrompet')){
    data$AREA[i]<-'Chromepet'
  }
  else if (area %in% c('Adyr')){
    data$AREA[i]<-'Adyar'
  }
  else if (area %in% c('T Nagar')){
    data$AREA[i]<-'TNagar'
  }
  else if (area %in% c('Karapakam')){
    data$AREA[i]<-'Karapakkam'
  }
  else if (area %in% c('Velchery')){
    data$AREA[i]<-'Velachery'
  }
  else if (area %in% c('Ana Nagar','Ann Nagar')){
    data$AREA[i]<-'Anna Nagar'
  }
  else if (area %in% c('KKNagar')){
    data$AREA[i]<-'KK Nagar'
  }
  else {next}
}

head(data)

unique(data$AREA)

# All the build and sale dates are mentioned in full, it is easier and more sensible to include only the year for both
# Therefore we create a new column for each 
str(data$DATE_BUILD)

data$YEAR_BUILD<-substr(data$DATE_BUILD,7,10)
str(data$YEAR_BUILD)

data$YEAR_SOLD<-substr(data$DATE_SALE,7,10)
str(data$YEAR_SOLD)

head(data)

# BUILD_TYPE contains misspelled duplicates, so we remove them
unique(data$BUILDTYPE)

for (i in 1:length(data$BUILDTYPE)){
  type<-data$BUILDTYPE[i]
  if (type %in% c('Comercial')){
    data$BUILDTYPE[i]<-'Commercial'
  }
  else if (type %in% c('Others')){
    data$BUILDTYPE[i]<-'Other'
  }
  else if (type %in% c('House')){
    data$BUILDTYPE[i]<-'Housing'
  }
  else {next}
}

unique(data$BUILDTYPE)

# STREET also contains misspelled duplicates, so we remove them
unique(data$STREET)

for (i in 1:length(data$STREET)){
  street<-data$STREET[i]
  if (street %in% c('No Access')){
    data$STREET[i]<-'NoAccess'
  }
  else if (street %in% c('Pavd')){
    data$STREET[i]<-'Paved'
  }
  else {next}
}

unique(data$STREET)

head(data)

# SALE_COND also contains misspelled duplicates, so we remove them
unique(data$SALE_COND)

for (i in 1:(length(data$SALE_COND))){
  area=data$SALE_COND[i]
  if (area %in% c('Partiall','PartiaLl')){
    data$SALE_COND[i]<-'Partial'
  }
  else if (area %in% c('AbNormal','Ab Normal')){
    data$SALE_COND[i]<-'Abnormal'
  }
  else if (area %in% c('AdjLand')){
    data$SALE_COND[i]<-'Adj Land'
  }
  else {next}
}

unique(data$SALE_COND)

# UTILITY_AVAIL also contains similar misspellings 
unique(data$UTILITY_AVAIL)

for (i in 1:(length(data$UTILITY_AVAIL))){
  facil=data$UTILITY_AVAIL[i]
  if (facil %in% c('All Pub')){
    data$UTILITY_AVAIL[i]<-'AllPub'
  }
  else if (facil %in% c('NoSewr ','NoSeWa')){
    data$UTILITY_AVAIL[i]<-'No_Sew_Water'
  }
  else {next}
}

unique(data$UTILITY_AVAIL)

head(data)

for (i in 1:length(data$PARK_FACIL)){
  facil=data$PARK_FACIL[i]
  if (facil %in% c('Noo')){
    data$PARK_FACIL[i]<-'No'
  }
  else {next}
}

unique(data$PARK_FACIL)

# Use this line below to make a copy of cleaned data 
# write.csv(data,'/Users/viren/Desktop/regression_miniproject/chennai_housing_data_cleaned.csv')



