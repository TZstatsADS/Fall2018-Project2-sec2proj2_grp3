filter_data <- read.csv("../output/filter_data_used.csv", as.is = T)
table_display <- filter_data[,c(1:21,31,38,30,33:35,39,37),]

colnames(table_display)[2] <- "Pop.American"
colnames(table_display)[3] <- "Pop.Asian"
colnames(table_display)[4] <- "Pop.Chinese"
colnames(table_display)[5] <- "Pop.Dessert"
colnames(table_display)[6] <- "Pop.European"
colnames(table_display)[7] <- "Pop.Italian"
colnames(table_display)[8] <- "Pop.Mexiacan"
colnames(table_display)[9] <- "Pop.Others"
colnames(table_display)[10] <- "Pop.QuickMeal"
colnames(table_display)[11] <- "Pop.Seafood"

colnames(table_display)[12] <- "Prop.American"
colnames(table_display)[13] <- "Prop.Asian"
colnames(table_display)[14] <- "Prop.Chinese"
colnames(table_display)[15] <- "Prop.Dessert"
colnames(table_display)[16] <- "Prop.European"
colnames(table_display)[17] <- "Prop.Italian"
colnames(table_display)[18] <- "Prop.Mexiacan"
colnames(table_display)[19] <- "Prop.Others"
colnames(table_display)[20] <- "Prop.QuickMeal"
colnames(table_display)[21] <- "Prop.Seafood"

colnames(table_display)[22] <- "Population"
colnames(table_display)[23] <- "Median Income"
colnames(table_display)[24] <- "Median Age"
colnames(table_display)[25] <- "Transportation"
colnames(table_display)[26] <- "Markets"
colnames(table_display)[27] <- "Crimes"
colnames(table_display)[29] <- "Yelp Star"

write.csv(table_display, file = "../output/table_display.csv", row.names = FALSE)
