data<-read.csv('/Users/viren/Desktop/regression_miniproject/Chennai houseing sale.csv')
View(data)

names(data)

vars<-c("QS_ROOMS","QS_BATHROOM","QS_BEDROOM","QS_OVERALL")
drop_vars<-names(data) %in% vars 
data<-data[!drop_vars]
View(data)

summary(data)
unique(data$AREA)
