#loading the data (train.csv) and assigning it a name (df)

> df <- read.csv('/Users/lolafeiger/data_science/GADS4/data/kaggle_salary/train.csv')
> nrow(df)

#rooting around in train.csv to see what I'm working with
> names(df)

> unique(df$"ContractTime")

> unique(df$"ContractType")

> summary(df$Category)

> table(df$ContractType, df$ContractTime)

#creating a new variable of the top ten professions represented in the data, as well as a separate level to which all the NAs are assigned

> category.counts <-summary(df$Category)
> top.categories <- names(category.counts[order(category.counts, decreasing= T)][1:10])
> df$NewCategoryFeature <- factor(df$Category, levels=top.categories)
> addNA(df$NewCategoryFeature)
> unique(df$NewCategoryFeature)

#I saw here that there were still only 10 levels, so I viewed the data to see if there were still NAs listed, and there were

> View(df$NewCategoryFeature)

> length(levels(df$NewCategoryFeature))
[1] 10

#so I tried to fix this
> df$NewCategoryFeature <- addNA(df$NewCategoryFeature)

> length(levels(df$NewCategoryFeature))
[1] 11

#creating a new variable of the top ten locations (LocationNormalized) represented in the data, as well as a separate level to which all the NAs are assigned

> locations.counts <-summary(df$LocationNormalized)
> top.locations <- names(locations.counts[order(locations.counts, decreasing= T)][1:10])
> df$NewLocationFeature <- factor(df$LocationNormalized, levels=top.locations)
> df$NewLocationFeature <- addNA(df$NewLocationFeature)

> length(levels(df$NewLocationFeature))
[1] 11

#splitting the original training file (df) into a training set (train.data) and a test set (test.data), and checking the number of rows to see that it actually worked
> nrow(df)

> N <- nrow(df)

> train.pct <- .7

> train.idx <- sample(1:N, train.pct* N)

> train.data <- df[train.idx, ]

> test.data <- df[-train.idx, ]

> nrow(train.data)

>nrow (test.data)

#creating a model and seeing how significant the variables are as well as its overall R squared 

> model <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime + ContractType, data=train.data)
> summary(model)
#writing the function for mean absolute error with which to assess the quality/validity of the model
> mae <- function(x,y)
  + {
    +     sum( abs(x-y) ) /length(x)
    + }
#testing the model on the test set
> model <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime + ContractType, data=train.data)
> predictions <- predict(model)

> plot(train.data$SalaryNormalized, predictions)

> test.predict <- predict(model, test.data)
> mae(test.predict, test.data$SalaryNormalized)
[1] 10205.45
#trying a new model to see how it compares in both R squared and MAE
> model1 <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime * ContractType, data=train.data)
> summary(model1)
> test.predict1 <- predict(model1, test.data)
> mae(test.predict1, test.data$SalaryNormalized)
[1] 10196.83
> mae(predict(model1, test.data), test.data$SalaryNormalized)
[1] 10196.83
#attempting to merge the location_tree file
> location_info <- read.table('~/data_science/GADS4/data/kaggle_salary/Location_Tree_reformatted.csv', sep=",", fill=T, col.names=c('Country', 'City', 'City Detailed', 'More Detailed', 'Town', 'Neighborhood','extra'))
> View(location_info)
> nrow(location_info)
[1] 14614
#putting the location_tree info aside for now...
# Finally, we need to work with the actual test data
> realtest <- read.csv('/Users/lolafeiger/data_science/GADS4/data/kaggle_salary/test.csv')

# We should train our final model with all the training data
> finalmodel <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime * ContractType, data=df)

predictions <- predict(finalmodel, realtest)
Error in eval(expr, envir, enclos) : 
  object 'NewCategoryFeature' not found

#need to create NewCategoryFeature and NewLocationFeature in the test data!

> category.counts1 <-summary(realtest$Category)
> top.categories1 <- names(category.counts1[order(category.counts1, decreasing= T)][1:10])
> realtest$NewCategoryFeature <- factor(realtest$Category, levels=top.categories1)
>  realtest$NewCategoryFeature <- addNA(realtest$NewCategoryFeature)

> length(levels(realtest$NewCategoryFeature))


> locations.counts1 <-summary(realtest$LocationNormalized)
> top.locations1 <- names(locations.counts1[order(locations.counts1, decreasing= T)][1:10])
> realtest$NewLocationFeature <- factor(realtest$LocationNormalized, levels=top.locations1)
>  realtest$NewLocationFeature <- addNA(realtest$NewLocationFeature)

> length(levels(realtest$NewLocationFeature))

> finalmodel <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime * ContractType, data=df)

> predictions <- predict(finalmodel, realtest)

Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
  factor 'NewCategoryFeature' has new level(s) PR, Advertising & Marketing Jobs

#turns out I think I need to formalize the definition of these new features in the test set (realtest), since the top ten levels are not the same across training and test files.

> unique(df$NewCategoryFeature)
[1] Engineering Jobs            HR & Recruitment Jobs       Accounting & Finance Jobs  
[4] Healthcare & Nursing Jobs   Other/General Jobs          Hospitality & Catering Jobs
[7] IT Jobs                     Customer Services Jobs      <NA>                       
  [10] Sales Jobs                  Teaching Jobs              
11 Levels: Healthcare & Nursing Jobs IT Jobs Engineering Jobs ... <NA>
  
  > unique(df$NewLocationFeature)
[1] <NA>       Surrey     UK         Manchester Leeds      London     Birmingham The City   Sheffield 
[10] Belfast   
Levels: (Other) UK London Manchester Leeds Belfast Birmingham The City Surrey Sheffield <NA>
  
  realtest$NewCategoryFeature <- factor(realtest$NewCategoryFeature, levels=c(levels(realtest$NewCategoryFeature), "Engineering Jobs ", "HR & Recruitment Jobs", " Accounting & Finance Jobs ", "Healthcare & Nursing Jobs", "Other/General Jobs", "Hospitality & Catering Jobs", "IT Jobs", " Customer Services Jobs ", "  <NA>  ", " Sales Jobs  ", " Teaching Jobs"))

realtest$NewLocationFeature <- factor(realtest$NewLocationFeature, levels=c(levels(realtest$NewCategoryFeature), "Engineering Jobs ", "HR & Recruitment Jobs", " Accounting & Finance Jobs ", "Healthcare & Nursing Jobs", "Other/General Jobs", "Hospitality & Catering Jobs", "IT Jobs", " Customer Services Jobs ", "  <NA>  ", " Sales Jobs  ", " Teaching Jobs"))

#that didn't work. tried something else.

> realtest$NewCategoryFeature <- factor(realtest$NewCategoryFeature, levels=levels(df$NewCategoryFeature))
> unique(realtest$NewCategoryFeature)

[1] Engineering Jobs            IT Jobs                     <NA>                       
  [4] Healthcare & Nursing Jobs   Sales Jobs                  HR & Recruitment Jobs      
[7] Other/General Jobs          Hospitality & Catering Jobs Teaching Jobs              
[10] Accounting & Finance Jobs  
10 Levels: Healthcare & Nursing Jobs IT Jobs Engineering Jobs ... Other/General Jobs

> realtest$NewCategoryFeature <- addNA(realtest$NewCategoryFeature)
> unique(realtest$NewCategoryFeature)

[1] Engineering Jobs            IT Jobs                     <NA>                       
  [4] Healthcare & Nursing Jobs   Sales Jobs                  HR & Recruitment Jobs      
[7] Other/General Jobs          Hospitality & Catering Jobs Teaching Jobs              
[10] Accounting & Finance Jobs  
11 Levels: Healthcare & Nursing Jobs IT Jobs Engineering Jobs ... <NA>
  
  > realtest$NewLocationFeature <- factor(realtest$NewLocationFeature, levels=levels(df$NewLocationFeature))

> realtest$NewLocationFeature <- addNA(realtest$NewLocationFeature)

> unique(realtest$NewLocationFeature)
[1] <NA>       UK         London     The City   Birmingham Manchester Leeds     
Levels: (Other) UK London Manchester Leeds Belfast Birmingham The City Surrey Sheffield <NA>
  
  > finalmodel <- lm(SalaryNormalized ~ NewCategoryFeature + NewLocationFeature + ContractTime * ContractType, data=df)

> predictions <- predict(finalmodel, realtest)
# What are these predictions going to be?

# Put the submission together and write it to a file
