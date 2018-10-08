setwd("C:/Users/Oded/Google Drive/Columbia Statistics/Courses/Fall 2018/GR5243 - Applied Data Science/Project 2/data and code")
rest <- read.csv("restaurant_new.csv")
head(rest)

###################################################
# zip.prop - proportion of types in each zip code #
###################################################

zip.types <- matrix(NA, length(unique(rest$ZIP)),length(unique(rest$TYPE)))
zip.prop <- matrix(NA, length(unique(rest$ZIP)),length(unique(rest$TYPE)))
for(i in 1:length(unique(rest$ZIP))){
  zip.types[i,] <- table(rest$TYPE[rest$ZIP==unique(rest$ZIP)[i]])
}

zip.prop <- zip.types/(rowSums(zip.types))
zip.prop <- as.vector(t(zip.prop))
Type <- rep(names(table(rest$TYPE)),196)
ZIP <- unique(rest$ZIP)
zip.vector <- c()
for(i in 1:length(ZIP)){
  zip.vector <- c(zip.vector,rep(ZIP[i],11))  
}

length(zip.vector)
zip.prop <- matrix(c(as.numeric(zip.vector),as.factor(Type),as.numeric(round(zip.prop,3))),2156,3)

colnames(zip.prop) <- c("ZIP","Type", "Percentage")
head(zip.prop)

###################################################
# zip.rate - average yelp rating in each zip code #
###################################################

zip.rate <- rep(NA, length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  zip.rate[i] <- mean(rest$YELP.STAR[rest$ZIP==unique(rest$ZIP)[i]])
}
zip.rate <- as.matrix(round(zip.rate,2))
zip.rate <- cbind(unique(rest$ZIP),zip.rate)
colnames(zip.rate) <- c("ZIP","Average Yelp star")
head(zip.rate)

###########################################################
# zip.grade  - average inspection grade in each zipe code #
###########################################################

inspec <- read.csv("inspect score.csv", header = T)
names(inspec) <- c("ZIP", "SCORE","GRADE")

zip.score <- rep(NA, length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  zip.score[i] <- mean(inspec$SCORE[inspec$ZIP==unique(rest$ZIP)[i]], na.rm = T)
}

head(zip.score)

zip.grade <- ifelse(zip.score<14, "A", ifelse(zip.score<28, "B", "C"))
head(zip.grade)
table(zip.grade)

###############################################################
# zip.income  - median income per household in each zipe code #
###############################################################

household <- read.csv("income.csv", header = T)
names(household) <- c("ZIP", "zip.income")

income.ind <- rep(NA,length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  income.ind[i] <- ifelse(length(which(household$ZIP==unique(rest$ZIP)[i]))>0,which(household$ZIP==unique(rest$ZIP)[i]),NA)
}
zip.income <- household$zip.income[income.ind]
head(zip.income)

##############################################################
# zip.class  - median income per household in each zipe code #
##############################################################

income.quartiles <- quantile(as.numeric(as.character(zip.income)), na.rm = T)
zip.class <- c()
for(i in 1:length(unique(rest$ZIP))){
  zip.class[i] <- ifelse(is.na(zip.income[i]),NA, ifelse(as.numeric(as.character(zip.income))[i]<=as.numeric(income.quartiles[2]), "Low", ifelse(as.numeric(as.character(zip.income))[i]<=as.numeric(income.quartiles[3]), "Below Median", ifelse(as.numeric(as.character(zip.income))[i]<=as.numeric(income.quartiles[4]),"Above median", "High"))))
}

zip.class <- matrix(c(unique(rest$ZIP),zip.class),196,2)

########################################################
# zip.female  - Percentage of females in each zip code #
########################################################

demog <- read.csv("age_sex - new.csv", header = T)
names(demog) <- c("ZIP", "total.pop", "female.per","below5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65above","median")

zip.female <- rep(NA, length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  zip.female[i] <- ifelse(is.na(demog$female.per),NA,demog$female.per[which(demog$ZIP==unique(rest$ZIP)[i])])
}
zip.female <- round(zip.female,2)
head(zip.female)

large.female.threshold <- 0.45
zip.gender <- ifelse(is.na(zip.female),NA,ifelse(zip.female<large.female.threshold,"Large male proportion",ifelse(zip.female>(1-large.female.threshold), "Large female proportion", "Equal gender proportion")))
head(zip.gender)
table(zip.gender)

##########################################
# zip.age  - age groups in each zip code #
##########################################

zip.age <- matrix(NA, length(unique(rest$ZIP)),8)

for(i in 1:length(unique(rest$ZIP))){
  zip.age[i,1] <- ifelse(is.na(demog$below5[i]),NA,demog$below5[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,2] <- ifelse(is.na(demog$`5-14`[i]),NA,demog$`5-14`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,3] <- ifelse(is.na(demog$`15-24`[i]),NA,demog$`15-24`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,4] <- ifelse(is.na(demog$`25-34`[i]),NA,demog$`25-34`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,5] <- ifelse(is.na(demog$`35-44`[i]),NA,demog$`35-44`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,6] <- ifelse(is.na(demog$`45-54`[i]),NA,demog$`45-54`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,7] <- ifelse(is.na(demog$`55-64`[i]),NA,demog$`55-64`[which(demog$ZIP==unique(rest$ZIP)[i])])
  zip.age[i,8] <- ifelse(is.na(demog$`65above`[i]),NA,demog$`65above`[which(demog$ZIP==unique(rest$ZIP)[i])])
}

zip.age <- as.vector(t(zip.age))
age.group <- rep(names(demog[,4:11]),196)
zip.vector.new <- c()
for(i in 1:length(ZIP)){
  zip.vector.new <- c(zip.vector.new,rep(ZIP[i],8))  
}

length(zip.vector.new)
zip.age <- matrix(c(as.numeric(zip.vector.new),as.factor(age.group),as.numeric(zip.age)),1568,3)

colnames(zip.age) <- c("ZIP","Type", "Percentage")
head(zip.age)

##########################################
# zip.pop  - population in each zip code #
##########################################

zip.pop <- rep(NA, length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  zip.pop[i] <- ifelse(is.na(demog$total.pop),NA,demog$total.pop[which(demog$ZIP==unique(rest$ZIP)[i])])
}

head(zip.pop)

##################################################################
# zip.type.pop  - type of restaurant per person in each zip code #
##################################################################

zip.type.pop <- rep(NA, length(unique(rest$ZIP)))
zip.type.pop <- zip.types/zip.pop

rownames(zip.type.pop) <- unique(rest$ZIP)
colnames(zip.type.pop) <- names(table(rest$TYPE))

head(zip.type.pop)

##################################################################
# zip.crime  - type of restaurant per person in each zip code #
##################################################################

crime <- read.csv("crime_data.csv")
crime <- crime[,c(3,7)]
zip.crime <- rep(NA, length(unique(rest$ZIP)))
for(i in 1:length(unique(rest$ZIP))){
  zip.crime[i] <- length(which(crime$Zipcode==unique(rest$ZIP)[i]))
}

zip.crime <- cbind(unique(rest$ZIP), zip.crime)
head(zip.crime)
