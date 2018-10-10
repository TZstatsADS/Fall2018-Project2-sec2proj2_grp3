filter_data <- read.csv("../output/filter_data_used.csv", as.is = T)
table_display <- filter_data[,c(1:21,31,38,30,32:35,39,37),]

colnames(table_display)[1] <- "Zipcode"
colnames(table_display)[2] <- "People/American"
colnames(table_display)[3] <- "People/Asian"
colnames(table_display)[4] <- "People/Chinese"
colnames(table_display)[5] <- "People/Dessert"
colnames(table_display)[6] <- "People/European"
colnames(table_display)[7] <- "People/Italian"
colnames(table_display)[8] <- "People/Mexiacan"
colnames(table_display)[9] <- "People/Others"
colnames(table_display)[10] <- "People/QuickMeal"
colnames(table_display)[11] <- "People/Seafood"

colnames(table_display)[12] <- "American Proportion"
colnames(table_display)[13] <- "Asian Proportion"
colnames(table_display)[14] <- "Chinese Proportion"
colnames(table_display)[15] <- "Dessert Proportion"
colnames(table_display)[16] <- "European Proportion"
colnames(table_display)[17] <- "Italian Proportion"
colnames(table_display)[18] <- "Mexiacan Proportion"
colnames(table_display)[19] <- "Others Proportion"
colnames(table_display)[20] <- "QuickMeal Proportion"
colnames(table_display)[21] <- "Seafood Proportion"

colnames(table_display)[22] <- "Population"
colnames(table_display)[23] <- "Median Income"
colnames(table_display)[24] <- "Median Age"
colnames(table_display)[25] <- "Female Percentage"
colnames(table_display)[26] <- "Transportation"
colnames(table_display)[27] <- "Markets"
colnames(table_display)[28] <- "Crimes"
colnames(table_display)[30] <- "Yelp Star"

write.csv(table_display, file = "../output/table_display.csv", row.names = FALSE)
