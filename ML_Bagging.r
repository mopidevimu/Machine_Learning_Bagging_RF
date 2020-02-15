# =================================================================================================================
#                                  Bagging Algorithm Applied to Census Dataset
# =================================================================================================================



# =================================================================================================================
#                                           Package dependencies
# =================================================================================================================
install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("Amelia") # used to find the missing values and visualize them
install.packages("inTrees")


# =================================================================================================================
#                                       Invoking respective Libraries
# =================================================================================================================
library(dplyr)
library(ggplot2)
theme_set(theme_gray())
library(randomForest)
library(Amelia)
library(inTrees)

# =================================================================================================================
#                                  Reading the Census Dataset and viewing it
# =================================================================================================================

census = read.csv('H:\\Documents\\R\\WORKING\\census.csv', header = TRUE)

#Basic Analysis Command , reading  the dataset
str(census)
summary(census)
glimpse(census)
plot(census)

# =================================================================================================================
#                                           Data cleaning
# =================================================================================================================

#===========  1.Rename the column to improve readability and  reflect the factor levels ==============
names(census)[names(census)=="nativecountry"] <- "region"
names(census)[names(census)=="hoursperweek"] <- "hr_per_week"
names(census)[names(census)=="over50k"] <- "income"
names(census)[names(census)=="maritalstatus"] <- "marital"
names(census)[names(census)=="workclass"] <- "employer_type"
names(census)[names(census)=="sex"] <- "gender"
str(census)

any(is.na(census))

#================ 2.converting Category type data in the type category for better handling ============================
# Factorizing the columns if needed
# in our case its already factorized but we re factor after combining few categories into one


#======================================================================================================
# Combining the two smallest groups into a  single group called "Unemployed" by converting these objects into character data types (as.character() and then using sapply with a custom function.
# Combine State and Local gov jobs into a category called SL-gov 
# Combine self-employed jobs into a category called self-emp
#=========================================================================================================
table(census$employer_type)
group_emp<-function(job){
    job<-as.character(job)
    if(job==' Never-worked'|job==' Without-pay'){
        return('Unemployed')
    }
    else if(job==' State-gov'|job==' Local-gov'){
        return('Govt')
    }
    else if(job==' Self-emp-inc'|job==' Self-emp-not-inc'){
        return('self_emp')
    }
    else{
        return(job)
    }
}

census$employer_type<-sapply(census$employer_type,group_emp)
table(census$employer_type)

#=========================================================================================================
# Look at the marital data column an Reduce this to three groups:Married Not-Married Never-Married
#=========================================================================================================
levels(census$marital)

group_marital<-function(mar){
    mar<-as.character(mar)
    if(mar==' Separated'|mar==' Widowed'|mar==' Divorced'){
        return('Not-Married')
    }
    else if (mar== ' Never-married'){
        return('Never-married')
        
    }
    else{
        return('Married')
    }
}


census$marital<-sapply(census$marital,group_marital)
table(census$marital)
#=========================================================================================================
#Problem with country column(too many categories and will ruin learning)
# Going ahead to group the countries by continent
#=========================================================================================================
Asia<-c(' China',' Hong',' India',' Iran',' Cambodia',' Japan', ' Laos',
        ' Philippines' ,' Vietnam' ,' Taiwan', ' Thailand')
North.America <- c(' Canada',' United-States',' Puerto-Rico' )

Europe <- c(' England' ,' France', ' Germany' ,' Greece',' Holand-Netherlands',' Hungary',
            ' Ireland',' Italy',' Poland',' Portugal',' Scotland',' Yugoslavia')

Latin.and.South.America <- c(' Columbia',' Cuba',' Dominican-Republic',' Ecuador',
                             ' El-Salvador',' Guatemala',' Haiti',' Honduras',
                             ' Mexico',' Nicaragua',' Outlying-US(Guam-USVI-etc)',' Peru',
                             ' Jamaica',' Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
    if (ctry %in% Asia){
        return('Asia')
    }else if (ctry %in% North.America){
        return('North.America')
    }else if (ctry %in% Europe){
        return('Europe')
    }else if (ctry %in% Latin.and.South.America){
        return('Latin.and.South.America')
    }else{
        return('Other')      
    }
}

census$region <- sapply(census$region,group_country)
table(census$region)

#============== 3.Checking for the NA and special Charecters and converting them to NA    =====================

census[census == ' ?'] <- NA
census$employer_type <- sapply(census$employer_type,factor)
census$region <- sapply(census$region,factor)
census$marital <- sapply(census$marital,factor)
census$occupation <- sapply(census$occupation,factor)

# I am going to use missmap to see the missing values
missmap(census)
missmap(census,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#omit NA values if any(backup contingency)
census <- na.omit(census)
ggplot(census,aes(age)) + geom_histogram(aes(fill=census$income),color='black',binwidth=1) + theme_bw()

#Recheck for missing values
missmap(census)
missmap(census,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


#Create a barplot of region with the fill color defined by income class.
ggplot(census,aes(region)) + geom_bar(aes(fill=census$income),color='black')+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Region wise income")


levels(census$education)[levels(census$education)==c(" Preschool"," 1st-4th"," 5th-6th"," 7th-8th"," 9th"," 10th"," HS-grad"," 11th"," 12th",
                                                     " Assoc-acdm"," Assoc-voc"," Prof-school"," Some-college",
                                                     " Bachelors"," Masters"," Doctorate")]<- c("Preschool","1st-4th","5th-6th",
                                                                                                "7th-8th","9th","10th","HS-grad",
                                                                                                "11th","12th","Assoc-acdm","Assoc-voc",
                                                                                                "Prof-school","Some-college","Bachelors","Masters","Doctorate")


str(census)

# =================================================================================================================
#                                             Univariate Analysis Visualization
# =================================================================================================================
# barplot(table(census$employer_type))
# barplot(table(census$education))
# barplot(table(census$marital))
# barplot(table(census$occupation))
# barplot(table(census$relationship))
# barplot(table(census$race))
# barplot(table(census$gender))
# barplot(table(census$region))
# barplot(table(census$income))


# ggplot(diamonds)  # if only the dataset is known.
# ggplot(diamonds, aes(x=carat))  # if only X-axis is known. The Y-axis can be specified in respective geoms.
# ggplot(diamonds, aes(x=carat, y=price))  # if both X and Y axes are fixed for all layers.
# ggplot(diamonds, aes(x=carat, color=cut))  # Each category of the 'cut' variable will now have a distinct  color, once a geom is added.
ggplot(census, aes(x=census$gender)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Gender Stats")+ labs(y="Count(in thousand)", x = "Gender")
ggplot(census, aes(x=census$employer_type)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Working Class")+ labs(y="Count(in thousand)", x = "Working Class")
ggplot(census, aes(x=census$education)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education Level")+ labs(y="Count(in thousand)", x = "Educational Qualification")
ggplot(census, aes(x=census$marital)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Individuals Martial Status")+ labs(y="Count(in thousand)", x = "Marital Status")
ggplot(census, aes(x=census$occupation)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Individuals Profession")+ labs(y="Count(in thousand)", x = "Profession")
ggplot(census, aes(x=census$race)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Ethnic diversity")+ labs(y="Count(in thousand)", x = "Ethnicity")
ggplot(census, aes(x=census$income)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Income")+ labs(y="Count(in thousand)", x = "Income bracket")


# =================================================================================================================
#                                              Bivariate Analysis Visualization
# =================================================================================================================

# table(census$employer_type,census$income)
# table(census$education,census$income)
# table(census$occupation,census$income)
# table(census$employer_type,census$race)
# table(census$gender,census$income)
# table(census$employer_type,census$occupation)

ggplot(census, aes(x= census$employer_type)) + geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Income breakage for diff employer type")+ labs(y="No of Individual", x = "Employer type",fill = "Income")
ggplot(census, aes(x= census$education)) +  geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Income based on Educational Qualification")+ labs(y="No of Individual", x = "Educational Qualification",fill = "Income") + coord_flip()
ggplot(census, aes(x= census$occupation)) + geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Income based on Profession")+ labs(y="income", x = "Profession",fill = "Income") + coord_flip()
ggplot(census, aes(x= census$gender)) + geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Gender Based Income breakage")+ labs(y="income", x = "Gender",fill = "Income") + coord_flip()
ggplot(census, aes(x= census$marital)) + geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Income breakage based on Marital Status")+ labs(y="No of Individual", x = "Marital Status",fill = "Income")
ggplot(census, aes(x= census$marital)) + geom_bar(aes(fill = factor(gender)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Marital Status V/S Gender")+ labs(y="No of Individual", x = "Marital Status",fill = "Gender")


ggplot(census, aes(x= census$employer_type)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + geom_bar(aes(fill = factor(race)))+ ggtitle("Ethnicity V/S Employer type")+ labs(y="No of Individual", x = "Employer type",fill = "Ethnicity")
ggplot(census, aes(x= census$education)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + geom_bar(aes(fill = factor(occupation))) + ggtitle("Education V/S Occupation")+ labs(y="No of Individual", x = "Education",fill = "Occupation")

ggplot(census, aes(x= census$age)) + geom_bar() + facet_grid(~ census$income)+ theme(axis.text.x = element_text(angle = 90)) + ggtitle("Age V/S Income")+ labs(y="No of Individuals", x = "Age")
#ggplot(census, aes(x= census$gender)) + geom_bar() + facet_grid(~ census$income)+ theme(axis.text.x = element_text(angle = 90)) 
ggplot(census, aes(x= census$occupation)) + geom_bar() + facet_grid(~ census$income)+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Occupation V/S Income Earned")+ labs(y="No of Individuals", x = "Occupation")
ggplot(census, aes(x= census$occupation)) + geom_bar() + facet_grid(~ census$employer_type)+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Occupation V/S employer type")+ labs(y="No of Individuals", x = "Occupation")
ggplot(census, aes(x= census$employer_type)) + geom_bar() + facet_grid(~ census$marital)+ theme(axis.text.x = element_text(angle = 90))+ ggtitle("Marital Status & preference for Employer type")+ labs(y="No of Individuals", x = "Employer type")
# =================================================================================================================
#                                       Pie Charts & Bar Plots
# =================================================================================================================

pct <- (table(census$income)/sum(table(census$income)))*100
pct
pie(table(census$income), labels = paste(c("below 50K", "above 50K"), round(pct,2), "%"),col = c("pink","brown"),main = 'Income % share')


# -----------------------------------------------------------------------------------------------
pct <- (table(census$gender)/sum(table(census$gender)))*100
pct
pie(table(census$gender), labels = paste(c("Female", "Male"), round(pct,2), "%"),col = c("pink","brown"),main = 'gender % share')

# -----------------------------------------------------------------------------------------------
pct <- (table(census$marital)/sum(table(census$marital)))*100
pct
pie(table(census$marital), labels = paste(c("Never-married","Married","Not-Married" ), round(pct,2), "%"),main = 'Marital status % share')


# -----------------------------------------------------------------------------------------------
pct <- (table(census$relationship)/sum(table(census$relationship)))*100
pct
pie(table(census$relationship), labels = paste(c(" Husband"," Not-in-family"," Other-relative"," Own-child"," Unmarried"," Wife"), round(pct,2), "%"),main = 'Relationship status % share')



# Few R Feature checks
table(census$gender,census$education,census$income)
ftable(census$gender,census$education,census$income )
census[,2]   #column access
census[2,]    # row access
census[c(2,3),]   #printing 2 records
census[,c(2,4,5)]  #printing selected column


# =================================================================================================================
#                       Derived Attribute from Age As  age is significant factor when checing income
#                                   Function to define the Age Groups
#                                   <30 , 30-45, 45-60, 60-75, above 75
# =================================================================================================================


extractAgeGroup <- function(age)
{
    if (age <= 30) return("Young")
    else if (age <= 45) return("Middle")
    else if (age <= 60) return("Senior")
    else if (age <= 75) return("Top-Senior")
    else return ("Retired")
}

extractAgeGroup(77)
AgeGroups <-NULL


for (i in 1:nrow(census))
{
    AgeGroups <- c(AgeGroups,extractAgeGroup(census$age[i]))
}
census$AgeGroups = as.factor(AgeGroups)
census$AgeGroups

p<-barplot(table(census$income,census$AgeGroups), beside = TRUE, legend.text = c("below 50K", "above 50K"),col = c("pink","brown"),las=2,border = 0)
title('Marital Status Breakage')
ggplot(census, aes(x=census$AgeGroups)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Experience Breakage")+ labs(y="Count(in thousand)", x = "Experience Bracket")
ggplot(census, aes(x= census$AgeGroups)) + geom_bar(aes(fill = factor(income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Experience V/S Income")+ labs(y="Count(in thousand)", x = "Experience", fill = "Income")

# =================================================================================================================
#                         Rather than using no of work hours and grouping them into regular/average, ovretime etc
#                                 Function to define the Work Hours
# =================================================================================================================

extractworkhour <- function(hour)
{
    if (hour <= 25) return("Part-time")
    else if (hour <= 45) return("Full-time")
    else return ("Overtime")
}


WorkHour <-NULL
for (i in 1:nrow(census))
{
    WorkHour <- c(WorkHour,extractworkhour(census$hr_per_week[i]))
}
census$WorkHour = as.factor(WorkHour)



# New variables/Attributes
p<-barplot(table(census$income,census$WorkHour), beside = TRUE, legend.text = c("below 50K", "above 50K"),col = c("pink","brown"),las=2,border = 0)
title('Marital Status Breakage')


ggplot(census, aes(x=census$WorkHour)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))+ ggtitle("Working Hours Breakage")+ labs(y="No Of Individuals", x = "Working conditions")
ggplot(census, aes(x= census$WorkHour)) + geom_bar(aes(fill = factor(census$income)),position = 'dodge2') + theme(axis.text.x = element_text(angle = 90))  + ggtitle("Working Hours Breakdown")+ labs(y="Count(in thousand)", x = "Working conditions", fill = "Income")


# =================================================================================================================
#                                   Single runs of Algorithm 
# =================================================================================================================

#     ---------------------- Splitting Data into test And Train -------------------------      #

data1 <-sample(2,nrow(census),prob = c(0.7,0.3), replace = T) 
census_train <- census[data1==1,]
census_test <- census[data1==2,]


#     -------------------------- creating random forest model ----------------------------      #

formulae = income ~ employer_type + education + marital + occupation + relationship + race + gender + region + AgeGroups + WorkHour
rf<- randomForest(formulae, data = census_train, importance =T)
rf

#     ----------------------------------- Testing model --------------------------------      #
pred<- predict(rf, newdata = census_test)
table(pred,census_test$income)


#     --------------------------------- Extracting the Tree --------------------------------      #
treevisual<-getTree(rf,1,labelVar = T)
t<-grow(rf,1)
t
treevisual

# ============================================================================================
# Professors code for extracting rule from tree
# ============================================================================================

#Extract a tree

getTree(rf,2, labelVar=T)

#Functions we need

#This function checks if the right part contains some variable

containsThisVariable <- function(v, myRule)
{
    if (length(grep(v,myRule))>0)
        return(TRUE)
    else
        return(FALSE)
}

containsTheseVariables <- function(lv, myRule)
{
    for (num in 1:length(lv))
    {
        if (containsThisVariable(lv[num], myRule)==FALSE)
            return(FALSE)
    }
    return(TRUE)
}


#Extracting rules from the forest

#Trnasform the RF into a list

treeList <- RF2List(rf)
treeList
head(census_train)
X <- census_train[,c(2,3,4,5,6,7,8,12,14,15)]
ruleExec <- extractRules(treeList,X,ntree=rf$ntree)
ruleExec <- unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,census_train$income)
varnames <- c("employer_type","education","martialstatus","Occupation","relationship","race","gender","region","AgeGroups","WorkHour")
readableRules <- presentRules(ruleMetric, varnames)
nbRules <- dim(readableRules)[1]
cat(nbRules," rules extracted.","\n")


minFreq <- 0.1
maxErr <- 0.2
#Put in this table the variables you want to have in the rules
wantedVariables=c("gender")

nr<- 0
for(numRule in 1:nbRules)
{
    if ((as.numeric(readableRules[numRule,2])>=minFreq)&(as.numeric(readableRules[numRule,3])<maxErr)
        &(containsTheseVariables(wantedVariables,readableRules[numRule,4])==TRUE))
    {
        nr<- nr+1
        cat(nr,":",readableRules[numRule,4]," --> ",readableRules[numRule,5],"\n")
        print("==============================================================================================")
    }
}
# ============================================================================================
# Professors code finishes here
# ============================================================================================



#     ------------------------------- Additional visualizations ------------------------------      #
plot(rf, main = "Random Forest errors for treees") #Plot the error rates or MSE of a randomForest objec
plot(margin(rf,census_test$income))
rf$importance
varImpPlot(rf, main = 'Variable Importance')
## Using different symbols for the classes:
MDSplot(rf, census_train$income,k=2, palette=rep(1, 3), pch=as.numeric(census_train$income))

hist(treesize(rf)) #Size of trees (number of nodes) in and ensemble

# =================================================================================================================
#                        Fixing  the problem of randomness by using setseed
# =================================================================================================================




accOOB <- c() 
accTest <- c()
nbiter <- 10


results <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Accuracy_OOB", "Accuracy_Test")
colnames(results) <- x
results[c('Accuracy_OOB' ,'Accuracy_Test')] 


for (num in {1:nbiter}) 
{
    set.seed(num)
    data1 <-sample(2,nrow(census),prob = c(0.7,0.3), replace = T) 
    census_train <- census[data1==1,]
    census_test <- census[data1==2,]
    
    
    rf<- randomForest(formulae, data = census_train,importance =T)
    confTr<-rf$confusion
    accOOB =  (confTr[1,1] + confTr[2,2]) / (confTr[1,1] + confTr[1,2] + confTr[2,1]+ confTr[2,2])
    results[num,'Accuracy_OOB']<- accOOB
    
    pred<- predict(rf, newdata = census_test)
    confTest<-table(pred,census_test$income)
    accTest = (confTest[1,1] + confTest[2,2]) / (confTest[1,1] + confTest[1,2] + confTest[2,1]+ confTest[2,2])
    results[num,'Accuracy_Test']<- accTest
    #cat(accOOB," :: ", accTest ,"\n")
    
} 
results
size<-length(results$Accuracy_OOB)
g<- ggplot(results, aes(x =1:size,y=results$Accuracy_OOB )) + geom_point(size = 2) + geom_point(aes(y = results$Accuracy_Test),size =2, colour = "green")
g<- g + scale_x_continuous(name="no of run", limits=c(0, nbiter),(breaks = seq(0, nbiter, by = 1)))
g<- g + scale_y_continuous(name="accuracy", limits=c(0, 1), (breaks = seq(0, 1, by = 0.2)))
g + ggtitle("OOB VS Test Accuracy")+ labs(y="Accuracy", x = "no of run")
# ======================================================================================================
#                       Multiple variation of algortithm by varying ntree & mtry
# ======================================================================================================




results <- data.frame(matrix(ncol = 8, nrow = 0))
x <- c("ntree","mtry","Mean Acc OOB", "Mean Acc Test","Precision","Recall","Specificity","F1 Score")
colnames(results) <- x
results[c('ntree','mtry','Mean Acc OOB' ,'Mean Acc Test','Precision','Recall','Specificity','F1 Score')] 

numbiter <-seq(10,100,by = 10 )

for (i in 1:length(numbiter) )
{
    accTest <-c() 
    accOOB <-c()
    precision <-c() 
    recalls <-c() 
    specificity <-c() 
    f1score <-c() 
    for(j in {1:7})
    {
        
        data1 <-sample(2,nrow(census),prob = c(0.7,0.3), replace = T) 
        census_train <- census[data1==1,]
        census_test <- census[data1==2,]
        
        rf<- randomForest(formulae, data = census_train, ntree=numbiter[i], mtry=j, importance =T)
        confTr<-rf$confusion
        accOOB =  c(accOOB,(confTr[1,1] + confTr[2,2]) / (confTr[1,1] + confTr[1,2] + confTr[2,1]+ confTr[2,2]))
        
        #- Accuracy = (TP+TN)/(TP+FP+FN+TN)
        # How many individual were labeled correctly inthe respective classes(earnings below 50K or above 50k)
        pred<- predict(rf, newdata = census_test)
        confTest<-table(pred,census_test$income)
        accTest= c(accTest,(confTest[1,1] + confTest[2,2]) / (confTest[1,1] + confTest[1,2] + confTest[2,1]+ confTest[2,2]))
        
        #- Precision = TP/(TP+FP)
        # How many individuals earning above 50K were correctly identified to be high earners
        precision = c(precision,(confTest[1,1]) / (confTest[1,1] + confTest[1,2]))
        
        #- Recall(aka Sensitivity) = TP/(TP+FN)
        # Of all the people who are High Earners, how many of those we correctly predict?
        recalls = c(recalls,(confTest[1,1]) / (confTest[1,1] + confTest[2,1]))
        
        #- Specificity = TN/(TN+FP)
        # Of all the people who are healthy, how many of those did we correctly predict?
        specificity = c(specificity,(confTest[2,2]) / (confTest[1,2] + confTest[2,2]))
        
        #- F1-score (aka F-Score / F-Measure) = 2*(Recall * Precision) / (Recall + Precision)
        # some sort of balance between precision (p) & recall (r) in the system.
        f1score = c(f1score,2*(recalls * precision) / (recalls + precision))
        
    }
    
    results[i,'ntree']<- numbiter[i]
    results[i,'mtry']<- j
    results[i,'Mean Acc OOB']<- mean(accOOB)
    results[i,'Mean Acc Test']<- mean(accTest)
    results[i,'Precision']<- mean(precision)
    results[i,'Recall']<- mean(recalls)
    results[i,'Specificity']<- mean(specificity)
    results[i,'F1 Score']<- mean(f1score)
    
    
}


results
print(" The end Solution of my Model for a single run and different ntree and mtry is: ")
cat("Model Random Forest is generating Accuracy OOB ", mean(results[,'Mean Acc OOB']),"Accuracy Test",mean(results[,'Mean Acc Test']),
    "Standard Deviation OOB",sd(results[,'Mean Acc OOB']),"Standard Deviation Acc Test",sd(results[,'Mean Acc OOB']))


plot(results$ntree, results$`Mean Acc OOB`, type = "n", xlim=c(0,100),ylim=c(0,1), xlab = "ntree", ylab = "Error")
points(results$ntree, results$`Mean Acc OOB`, col = "red")
points(results$ntree, results$`Mean Acc Test`, col = "green")
title("Accuracy OOB V/S Test")

g<- ggplot(results, aes(x =results$ntree,y=results$`Mean Acc OOB` )) + geom_point(size = 2) + geom_point(aes(y = results$`Mean Acc Test`),size =2, colour = "green")
g<- g + scale_x_continuous(name="no of run", limits=c(0, max(numbiter)),(breaks = seq(0, max(numbiter), by = 10)))
g<- g + scale_y_continuous(name="accuracy", limits=c(0, 1), (breaks = seq(0, 1, by = 0.2)))
g + ggtitle("OOB VS Test Accuracy")+ labs(y="Accuracy", x = "no of trees")
# =================================================================================================================
#                     Multiple runs of Algorithm with variations of ntrees and mtry 
#                       Fixing  the problem of randomness by using setseed
# =================================================================================================================




results <- data.frame(matrix(ncol = 8, nrow = 0))
x <- c("ntree","mtry","Acc OOB", "Acc Test","Precision","Recall","Specificity","F1 Score")
colnames(results) <- x
results[c('ntree','mtry','Acc OOB' ,'Acc Test','Precision','Recall','Specificity','F1 Score')] 

numbtree <-seq(10,100,by = 10 )
mtrys <- seq(1,7)

for (i in 1:length(numbtree))
{
    accTest <-c() 
    accOOB <-c()
    precision <-c() 
    recalls <-c() 
    specificity <-c() 
    f1score <-c() 
    for(j in 1:length(mtrys))
    {
        
        accTest1 <-c() 
        accOOB1 <-c()
        precision1 <-c() 
        recalls1 <-c() 
        specificity1 <-c() 
        f1score1 <-c() 
        
        for(num in 1:10)
        { 
            
            set.seed(num)
            data1 <-sample(2,nrow(census),prob = c(0.7,0.3), replace = T) 
            census_train <- census[data1==1,]
            census_test <- census[data1==2,]
            
            rf<- randomForest(formulae, data = census_train, ntree=numbtree[i], mtry=mtrys[j], importance =T)
            confTr<-rf$confusion
            accOOB1 =  c(accOOB1,(confTr[1,1] + confTr[2,2]) / (confTr[1,1] + confTr[1,2] + confTr[2,1]+ confTr[2,2]))
            
            #- Accuracy = (TP+TN)/(TP+FP+FN+TN)
            # How many individual were labeled correctly inthe respective classes(earnings below 50K or above 50k)
            pred<- predict(rf, newdata = census_test)
            confTest<-table(pred,census_test$income)
            accTest1= c(accTest1,(confTest[1,1] + confTest[2,2]) / (confTest[1,1] + confTest[1,2] + confTest[2,1]+ confTest[2,2]))
            
            #- Precision = TP/(TP+FP)
            # How many individuals earning above 50K were correctly identified to be high earners
            precision1 = c(precision1,(confTest[1,1]) / (confTest[1,1] + confTest[1,2]))
            
            #- Recall(aka Sensitivity) = TP/(TP+FN)
            # Of all the people who are High Earners, how many of those we correctly predict?
            recalls1 = c(recalls1,(confTest[1,1]) / (confTest[1,1] + confTest[2,1]))
            
            #- Specificity = TN/(TN+FP)
            # Of all the people who are earning, how many of those did we correctly predict?
            specificity1 = c(specificity1,(confTest[2,2]) / (confTest[1,2] + confTest[2,2]))
            
            #- F1-score (aka F-Score / F-Measure) = 2*(Recall * Precision) / (Recall + Precision)
            # some sort of balance between precision (p) & recall (r) in the system.
            f1score1 = c(f1score1,2*(recalls1 * precision1) / (recalls1 + precision1))
        }
        accOOB = c(accOOB,mean(accOOB1))
        accTest = c(accTest,mean(accTest1))
        precision <-c(precision,mean(precision1)) 
        recalls <-c(recalls,mean(recalls1)) 
        specificity <-c(specificity,mean(specificity1)) 
        f1score <-c(f1score,mean(f1score1)) 
        
        sd_Test = sd(accOOB1)
        sd_OOB = sd(accTest1)
        cat(i,".",j,": ntree = ",numbtree[i]," mtry = ",mtrys[j],", errors = ",max(accOOB),",",max(accTest),"\n")
        
        
    }
    results[i,'ntree']<- numbtree[i]
    results[i,'mtry']<- mtrys[j]
    results[i,'Acc OOB']<- max(accOOB)
    results[i,'Acc Test']<- max(accTest)
    results[i,'Precision']<- max(precision)
    results[i,'Recall']<- max(recalls)
    results[i,'Specificity']<- max(specificity)
    results[i,'F1 Score']<- max(f1score)
    
}

results


plot(results$ntree, results$`Mean Acc OOB`, type = "n", xlim=c(0,100),ylim=c(0,1), xlab = "ntree", ylab = "Error")
points(results$ntree, results$`Mean Acc OOB`, col = "red",pch = 18)
points(results$ntree, results$`Mean Acc Test`, col = "green",pch =19)
title("Accuracy OOB V/S Test")

g<- ggplot(results, aes(x =results$ntree,y=results$`Mean Acc OOB` )) + geom_point(size = 2) + geom_point(aes(y = results$`Mean Acc Test`),size =2, colour = "green")
g<- g + scale_x_continuous(name="no of run", limits=c(0, max(numbtree)),(breaks = seq(0, max(numbtree), by = (max(numbtree)/length(numbtree)) )))
g + scale_y_continuous(name="accuracy", limits=c(0, 1), (breaks = seq(0, 1, by = 0.2)))
g + ggtitle("OOB VS Test Accuracy")+ labs(y="Accuracy", x = "no of trees")


#=========================================================================================================

#=========================================================================================================

#=========================================================================================================

#Defining the training controls for multiple models
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(formulae, data=census_train, method="treebag", metric=metric, trControl=control)
fit.treebag
# Random Forest
set.seed(seed)
fit.rf <- train(formulae, data=census_train, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)


