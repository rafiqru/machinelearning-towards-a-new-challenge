introduction-to-machine-learning-with-r/
##chpa-1-Machine Learning: What's the challenge?
Acquainting yourself with the data
As a first step, you want to find out some properties of the dataset with which you'll be working. More specifically, you want to know more about the dataset's number of observations and variables.

In this exercise, you'll explore the iris dataset. If you want to learn more about it, you can click on it or type ?iris in the console.

Your job is to extract the number of observations and variables from iris. This dataset is readily available in R (in the datasets package that's loaded by default).
# iris is available from the datasets package
> 
> # Reveal number of observations and variables in two different ways
> 
> dim(iris)
[1] 150   5
> str(iris)
'data.frame':	150 obs. of  5 variables:
 $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
 $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
 $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
 $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
 $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
> 
> # Show first and last observations in the iris data set
> head(iris)
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
> tail(iris)
    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
145          6.7         3.3          5.7         2.5 virginica
146          6.7         3.0          5.2         2.3 virginica
147          6.3         2.5          5.0         1.9 virginica
148          6.5         3.0          5.2         2.0 virginica
149          6.2         3.4          5.4         2.3 virginica
150          5.9         3.0          5.1         1.8 virginica
> 
> 
> # Summarize the iris data set
> summary(iris)
  Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
 Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
 Median :5.800   Median :3.000   Median :4.350   Median :1.300  
 Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
 Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
       Species  
 setosa    :50  
 versicolor:50  
 virginica :50
> Basic prediction model
Let's get down to a bit of coding! Your task is to examine this course's first prediction model. You'll be working with the Wage dataset. It contains the wage and some general information for workers in the mid-Atlantic region of the US.

As we briefly discussed in the video, there could be a relationship between a worker's age and his wage. Older workers tend to have more experience on average than their younger counterparts, hence you could expect an increasing trend in wage as workers age. So we built a linear regression model for you, using lm(): lm_wage. This model predicts the wage of a worker based only on the worker's age.

With this linear model lm_wage, which is built with data that contain information on workers' age and their corresponding wage, you can predict the wage of a worker given the age of that worker. For example, suppose you want to predict the wage of a 60 year old worker. You can use the predict() function for this. This generic function takes a model as the first argument. The second argument should be some unseen observations as a data frame. predict() is then able to predict outcomes for these observations.

Note: At this point, the workings of lm() are not important, you'll get a more comprehensive overview of regression in chapter 4.
> # The Wage dataset is available
> 
> # Build Linear Model: lm_wage (coded already)
> lm_wage <- lm(wage ~ age, data = Wage)
> 
> # Define data.frame: unseen (coded already)
> unseen <- data.frame(age = 60)
> 
> # Predict the wage for a 60-year old worker
> predict(lm_wage,unseen)
       1 
124.1413
> Classification: Filtering spam
Filtering spam from relevant emails is a typical machine learning task. Information such as word frequency, character frequency and the amount of capital letters can indicate whether an email is spam or not.

In the following exercise you'll work with the dataset emails, which is loaded in your workspace (Source: UCI Machine Learning Repository). Here, several emails have been labeled by humans as spam (1) or not spam (0) and the results are found in the column spam. The considered feature in emails to predict whether it was spam or not is avg_capital_seq. It is the average amount of sequential capital letters found in each email.

In the code, you'll find a crude spam filter we built for you, spam_classifier() that uses avg_capital_seq to predict whether an email is spam or not. In the function definition, it's important to realize that x refers to avg_capital_seq. So where the avg_capital_seq is greater than 4, spam_classifier() predicts the email is spam (1), if avg_capital_seq is inclusively between 3 and 4, it predicts not spam (0), and so on. This classifier's methodology of predicting whether an email is spam or not seems pretty random, but let's see how it does anyways!

Your job is to inspect the emails dataset, apply spam_classifier to it, and compare the predicted labels with the true labels. If you want to play some more with the emails dataset, you can download it here. And if you want to learn more about writing functions, consider taking the Writing Functions in R course taught by Hadley and Charlotte Wickham.
> # The emails dataset is already loaded into your workspace
> 
> # Show the dimensions of emails
> dim(emails)
[1] 13  2
> 
> # Inspect definition of spam_classifier()
> spam_classifier <- function(x){
    prediction <- rep(NA, length(x)) # initialize prediction vector
    prediction[x > 4] <- 1
    prediction[x >= 3 & x <= 4] <- 0
    prediction[x >= 2.2 & x < 3] <- 1
    prediction[x >= 1.4 & x < 2.2] <- 0
    prediction[x > 1.25 & x < 1.4] <- 1
    prediction[x <= 1.25] <- 0
    return(prediction) # prediction is either 0 or 1
  }
> 
> # Apply the classifier to the avg_capital_seq column: spam_pred
> spam_pred<-spam_classifier(emails$avg_capital_seq)
> 
> # Compare spam_pred to emails$spam. Use ==
> spam_pred==emails$spam
 [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
> Regression: LinkedIn views for the next 3 days
It's time for you to make another prediction with regression! More precisely, you'll analyze the number of views of your LinkedIn profile. With your growing network and your data science skills improving daily, you wonder if you can predict how often your profile will be visited in the future based on the number of days it's been since you created your LinkedIn account.

The instructions will help you predict the number of profile views for the next 3 days, based on the views for the past 3 weeks. The linkedin vector, which contains this information, is already available in your workspace.
> # linkedin is already available in your workspace
> 
> # Create the days vector
> 
> days<-c(1:21)
> # Fit a linear model called on the linkedin views per day: linkedin_lm
> linkedin_lm<-lm(linkedin~days)
> 
> # Predict the number of views for the next three days: linkedin_pred
> future_days <- data.frame(days = 22:24)
> linkedin_pred <-predict(linkedin_lm,future_days )
> 
> # Plot historical data and predictions
> plot(linkedin ~ days, xlim = c(1, 24))
> points(22:24, linkedin_pred, col = "green")
> Clustering: Separating the iris species
Last but not least, there's clustering. This technique tries to group your objects. It does this without any prior knowledge of what these groups could or should look like. For clustering, the concepts of prior knowledge and unseen observations are less meaningful than for classification and regression.

In this exercise, you'll group irises in 3 distinct clusters, based on several flower characteristics in the iris dataset. It has already been chopped up in a data frame my_iris and a vector species, as shown in the sample code on the right.

The clustering itself will be done with the kmeans() function. How the algorithm actually works, will be explained in the last chapter. For now, just try it out to gain some intuition!

Note: In problems that have a random aspect (like this problem with kmeans()), the set.seed() function will be used to enforce reproducibility. If you fix the seed, the random numbers that are generated (e.g. in kmeans()) are always the same.
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Chop up iris in my_iris and species
> my_iris <- iris[-5]
> species <- iris$Species
> 
> # Perform k-means clustering on my_iris: kmeans_iris
> kmeans_iris<-kmeans(my_iris,3)
> 
> # Compare the actual Species to the clustering using table()
> table(species,kmeans_iris$cluster)
            
species       1  2  3
  setosa     50  0  0
  versicolor  0  2 48
  virginica   0 36 14
> 
> # Plot Petal.Width against Petal.Length, coloring by cluster
> plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)
> Getting practical with supervised learning
Previously, you used kmeans() to perform clustering on the iris dataset. Remember that you created your own copy of the dataset, and dropped the Species attribute? That's right, you removed the labels of the observations.

In this exercise, you will use the same dataset. But instead of dropping the Species labels, you will use them do some supervised learning using recursive partitioning! Don't worry if you don't know what that is yet. Recursive partitioning (a.k.a. decision trees) will be explained in Chapter 3.
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Take a look at the iris dataset
> str(iris)
'data.frame':	150 obs. of  5 variables:
 $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
 $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
 $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
 $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
 $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
> summary(iris)
  Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
 Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
 Median :5.800   Median :3.000   Median :4.350   Median :1.300  
 Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
 Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
       Species  
 setosa    :50  
 versicolor:50  
 virginica :50
> 
> # A decision tree model has been built for you
> tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                data = iris, method = "class")
> 
> # A dataframe containing unseen observations
> unseen <- data.frame(Sepal.Length = c(5.3, 7.2),
                       Sepal.Width = c(2.9, 3.9),
                       Petal.Length = c(1.7, 5.4),
                       Petal.Width = c(0.8, 2.3))
> 
> # Predict the label of the unseen observations. Print out the result.
> predict(tree,unseen,type="class")
        1         2 
   setosa virginica 
Levels: setosa versicolor virginica
> How to do unsupervised learning (1)
In this exercise, you will group cars based on their horsepower and their weight. You can find the types of car and corresponding attributes in the cars data frame, which has been derived from the mtcars dataset. It's available in your workspace.

To cluster the different observations, you will once again use kmeans().

In short, your job is to cluster the cars in 2 groups, but don't forget to explore the dataset first!
> # The cars data frame is pre-loaded
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Explore the cars dataset
> str(cars)
'data.frame':	32 obs. of  2 variables:
 $ wt: num  2.62 2.88 2.32 3.21 3.44 ...
 $ hp: num  110 110 93 110 175 105 245 62 95 123 ...
> summary(cars)
       wt              hp       
 Min.   :1.513   Min.   : 52.0  
 1st Qu.:2.581   1st Qu.: 96.5  
 Median :3.325   Median :123.0  
 Mean   :3.217   Mean   :146.7  
 3rd Qu.:3.610   3rd Qu.:180.0  
 Max.   :5.424   Max.   :335.0
> 
> # Group the dataset into two clusters: km_cars
> km_cars<-kmeans(cars,2)
> 
> # Print out the contents of each cluster
> km_cars$cluster
          Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
                  1                   1                   1                   1 
  Hornet Sportabout             Valiant          Duster 360           Merc 240D 
                  2                   1                   2                   1 
           Merc 230            Merc 280           Merc 280C          Merc 450SE 
                  1                   1                   1                   2 
         Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
                  2                   2                   2                   2 
  Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
                  2                   1                   1                   1 
      Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
                  1                   1                   1                   2 
   Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
                  2                   1                   1                   1 
     Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
                  2                   2                   2                   1
> How to do unsupervised learning (2)
In the previous exercise, you grouped the cars based on their horsepower and their weight. Now let's have a look at the outcome!

An important part in machine learning is understanding your results. In the case of clustering, visualization is key to interpretation! One way to achieve this is by plotting the features of the cars and coloring the points based on their corresponding cluster.

In this exercise you'll summarize your results in a comprehensive figure. The dataset cars is already available in your workspace; the code to perform the clustering is already available.
> # The cars data frame is pre-loaded
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Group the dataset into two clusters: km_cars
> km_cars <- kmeans(cars, 2)
> 
> # Add code: color the points in the plot based on the clusters
> plot(cars,col=km_cars$cluster)
> 
> # Print out the cluster centroids
> km_cars$centers
        wt        hp
1 2.692000  99.47368
2 3.984923 215.69231
> 
> # Replace the ___ part: add the centroids to the plot
> points(km_cars$centers, pch = 22, bg = c(1, 2), cex = 2)
> ##chap-2-Measuring model performance or error
The Confusion Matrix
Have you ever wondered if you would have survived the Titanic disaster in 1912? Our friends from Kaggle have some historical data on this event. The titanic dataset is already available in your workspace.

In this exercise, a decision tree is learned on this dataset. The tree aims to predict whether a person would have survived the accident based on the variables Age, Sex and Pclass (travel class). The decision the tree makes can be deemed correct or incorrect if we know what the person's true outcome was. That is, if it's a supervised learning problem.

Since the true fate of the passengers, Survived, is also provided in titanic, you can compare it to the prediction made by the tree. As you've seen in the video, the results can be summarized in a confusion matrix. In R, you can use the table() function for this.

In this exercise, you will only focus on assessing the performance of the decision tree. In chapter 3, you will learn how to actually build a decision tree yourself.

Note: As in the previous chapter, there are functions that have a random aspect. The set.seed() function is used to enforce reproducibility. Don't worry about it, just don't remove it!
> # The titanic dataset is already loaded into your workspace
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Have a look at the structure of titanic
> str(titanic)
'data.frame':	714 obs. of  4 variables:
 $ Survived: Factor w/ 2 levels "1","0": 2 1 1 1 2 2 2 1 1 1 ...
 $ Pclass  : int  3 1 3 1 3 1 3 3 2 3 ...
 $ Sex     : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 1 1 1 ...
 $ Age     : num  22 38 26 35 35 54 2 27 14 4 ...
> 
> # A decision tree classification model is built on the data
> tree <- rpart(Survived ~ ., data = titanic, method = "class")
> 
> # Use the predict() method to make predictions, assign to pred
> pred<-predict(tree,titanic, type="class")
> 
> # Use the table() method to make the confusion matrix
> table(titanic$Survived,pred)
   pred
      1   0
  1 212  78
  0  53 371
> Deriving ratios from the Confusion Matrix
The confusion matrix from the last exercise provides you with the raw performance of the decision tree:

The survivors correctly predicted to have survived: true positives (TP)
The deceased who were wrongly predicted to have survived: false positives (FP)
The survivors who were wrongly predicted to have perished: false negatives (FN)
The deceased who were correctly predicted to have perished: true negatives (TN)
pnPTPFPNFNTN
The confusion matrix is called conf, try this in the console for its specific values:

> conf

      1   0
  1 212  78
  0  53 371
In the video, you saw that these values can be used to estimate comprehensive ratios to asses the performance of a classification algorithm. An example is the accuracy, which in this case represents the percentage of correctly predicted fates of the passengers.

Accuracy=TP+TNTP+FN+FP+TN.
Apart from accuracy, precision and recall are also key metrics to assess the results of a classification algorithm:

Precision=TPTP+FP
Recall=TPTP+FN
The confusion matrix you've calculated in the previous exercise is available in your workspace as conf.
> # The confusion matrix is available in your workspace as conf
> 
> # Assign TP, FN, FP and TN using conf
> TP <- conf[1, 1] # this will be 212
> FN <- conf[1, 2] # this will be 78
> FP <- conf[2, 1] # fill in
> TN <- conf[2, 2] # fill in
> 
> # Calculate and print the accuracy: acc
> 
> acc<-(TP+TN)/(TP+TN+FP+FN)
> acc
[1] 0.8165266
> # Calculate and print out the precision: prec
> 
> prec<-TP/(TP+FP)
> prec
[1] 0.8
> # Calculate and print out the recall: rec
> rec<-TP/(TP+FN)
> rec
[1] 0.7310345
> The quality of a regression
Imagine this: you're working at NASA and your team measured the sound pressure produced by an airplane's wing under different settings. These settings are the frequency of the wind, the angle of the wing, and several more. The results of this experiment are listed in the air dataset (Source: UCIMLR).

Your team wants to build a model that's able to predict the sound pressure based on these settings, instead of having to do those tedious experiments every time.

A colleague has prepared a multivariable linear regression model, fit. It takes as input the predictors: wind frequency (freq), wing's angle (angle), and chord's length (ch_length). The response is the sound pressure (dec). All these variables can be found in air.

Now, your job is to assess the quality of your colleague's model by calculating the RMSE:

RMSE=1N∑i=1N(yi−y^i)2−−−−−−−−−−−−−⎷
For example: if truth$colwas a column with true values of a variable and pred is the prediction of that variable, the formula could be calculated in R as follows:

sqrt((1/nrow(truth)) * sum( (truth$col - pred) ^ 2))
> # The air dataset is already loaded into your workspace
> 
> # Take a look at the structure of air
> str(air)
'data.frame':	1503 obs. of  6 variables:
 $ freq     : int  800 1000 1250 1600 2000 2500 3150 4000 5000 6300 ...
 $ angle    : num  0 0 0 0 0 0 0 0 0 0 ...
 $ ch_length: num  0.305 0.305 0.305 0.305 0.305 ...
 $ velocity : num  71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 71.3 ...
 $ thickness: num  0.00266 0.00266 0.00266 0.00266 0.00266 ...
 $ dec      : num  126 125 126 128 127 ...
> 
> # Inspect your colleague's code to build the model
> fit <- lm(dec ~ freq + angle + ch_length, data = air)
> 
> # Use the model to predict for all values: pred
> pred<-predict(fit,air)
> 
> # Use air$dec and pred to calculate the RMSE
> rmse<-sqrt(mean((air$dec-pred)^2))
> rmse<-sqrt((1/nrow(air))*sum((air$dec-pred)^2))
> # Print out rmse
> rmse
[1] 5.215778
> Adding complexity to increase quality
In the last exercise, your team's model had 3 predictors (input variables), but what if you included more predictors? You have the measurements on free-stream velocity, velocity and suction side displacement thickness, thickness available for use in the air dataset as well!

Adding the new variables will definitely increase the complexity of your model, but will it increase the performance? To find out, we'll take the RMSE from the new, more complex model and compare it to that of the original model.

A colleague took your code from the previous exercise and added code that builds a new extended model, fit2! It's your job to once again assess the performance by calculating the RMSE.
> # The air dataset is already loaded into your workspace
> 
> # Previous model
> fit <- lm(dec ~ freq + angle + ch_length, data = air)
> pred <- predict(fit)
> rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))
> rmse
[1] 5.215778
> 
> # Your colleague's more complex model
> fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)
> 
> # Use the model to predict for all values: pred2
> pred2<-predict(fit2)
> 
> # Calculate rmse2
> rmse2 <- sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))
> 
> # Print out rmse2
> rmse2
[1] 4.799244
> Let's do some clustering!
In the dataset seeds you can find various metrics such as area, perimeter and compactness for 210 seeds. (Source: UCIMLR). However, the seeds' labels were lost. Hence, we don't know which metrics belong to which type of seed. What we do know, is that there were three types of seeds.

The code on the right groups the seeds into three clusters (km_seeds), but is it likely that these three clusters represent our seed types? Let's find out.

There are two initial steps you could take:

Visualize the distribution of cluster assignments among two variables, for example length and compactness.
Verify if the clusters are well separated and compact. To do this, you can calculate the between and within cluster sum of squares respectively.
> # The seeds dataset is already loaded into your workspace
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Explore the structure of the dataset
> str(seeds)
'data.frame':	210 obs. of  7 variables:
 $ area         : num  15.3 14.9 14.3 13.8 16.1 ...
 $ perimeter    : num  14.8 14.6 14.1 13.9 15 ...
 $ compactness  : num  0.871 0.881 0.905 0.895 0.903 ...
 $ length       : num  5.76 5.55 5.29 5.32 5.66 ...
 $ width        : num  3.31 3.33 3.34 3.38 3.56 ...
 $ asymmetry    : num  2.22 1.02 2.7 2.26 1.35 ...
 $ groove_length: num  5.22 4.96 4.83 4.8 5.17 ...
> 
> # Group the seeds in three clusters
> km_seeds <- kmeans(seeds, 3)
> 
> # Color the points in the plot based on the clusters
> plot(length ~ compactness, data = seeds,col=km_seeds$cluster)
> 
> # Print out the ratio of the WSS to the BSS
> km_seeds$tot.withinss/km_seeds$betweenss
[1] 0.2762846
> Split the sets
Let's return to the titanic dataset for which we set up a decision tree. In exercises 2 and 3 you calculated a confusion matrix to assess the tree's performance. However, the tree was built using the entire set of observations. Therefore, the confusion matrix doesn't assess the predictive power of the tree. The training set and the test set were one and the same thing: this can be improved!

First, you'll want to split the dataset into train and test sets. You'll notice that the titanic dataset is sorted on titanic$Survived , so you'll need to first shuffle the dataset in order to have a fair distribution of the output variable in each set.

For example, you could use the following commands to shuffle a data frame df and divide it into training and test sets with a 60/40 split between the two.

n <- nrow(df)
shuffled_df <- df[sample(n), ]
train_indices <- 1:round(0.6 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.6 * n) + 1):n
test <- shuffled_df[test_indices, ]
Watch out, this is an example of how to do a 60/40 split! In the exercise you have to do a 70/30 split. However, you can use the same commands, just change the numbers!
> # The titanic dataset is already loaded into your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Shuffle the dataset, call the result shuffled
> n <- nrow(titanic)
> shuffled <- titanic[sample(n),]
> 
> # Split the data in train and test
> train<-shuffled[1:round(0.7*n),]
> test<-shuffled[(round(0.7*n)+1):n,]
> 
> # Print the structure of train and test
> str(train)
'data.frame':	500 obs. of  4 variables:
 $ Survived: Factor w/ 2 levels "1","0": 2 2 2 1 2 1 1 1 1 2 ...
 $ Pclass  : int  3 3 2 1 3 1 2 3 2 3 ...
 $ Sex     : Factor w/ 2 levels "female","male": 2 2 1 2 2 2 1 2 1 2 ...
 $ Age     : num  32 19 44 27 7 56 48 9 29 26 ...
> str(test)
'data.frame':	214 obs. of  4 variables:
 $ Survived: Factor w/ 2 levels "1","0": 1 2 2 1 2 2 2 2 2 2 ...
 $ Pclass  : int  2 3 2 2 1 1 3 3 2 3 ...
 $ Sex     : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 2 2 2 1 ...
 $ Age     : num  18 16 36 45 61 31 40.5 28 30 2 ...
> First you train, then you test
Time to redo the model training from before. The titanic data frame is again available in your workspace. This time, however, you'll want to build a decision tree on the training set, and next assess its predictive power on a set that has not been used for training: the test set.

On the right, the code that splits titanic up in train and test has already been included. Also, the old code that builds a decision tree on the entire set is included. Up to you to correct it and connect the dots to get a good estimate of the model's predictive ability.
> # The titanic dataset is already loaded into your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Shuffle the dataset; build train and test
> n <- nrow(titanic)
> shuffled <- titanic[sample(n),]
> train <- shuffled[1:round(0.7 * n),]
> test <- shuffled[(round(0.7 * n) + 1):n,]
> 
> # Fill in the model that has been learned.
> tree <- rpart(Survived ~ ., train, method = "class")
> 
> # Predict the outcome on the test set with tree: pred
> pred<-predict(tree,test,type="class")
> 
> # Calculate the confusion matrix: conf
> conf<-table(test$Survived,pred)
> 
> # Print this confusion matrix
> conf
   pred
      1   0
  1  58  31
  0  23 102
> Using Cross Validation
You already did a great job in assessing the predictive performance, but let's take it a step further: cross validation.

In this exercise, you will fold the dataset 6 times and calculate the accuracy for each fold. The mean of these accuracies forms a more robust estimation of the model's true accuracy of predicting unseen data, because it is less dependent on the choice of training and test sets.

Note: Other performance measures, such as recall or precision, could also be used here.
> # The shuffled dataset is already loaded into your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Initialize the accs vector
> accs <- rep(0,6)
> 
> for (i in 1:6) {
    # These indices indicate the interval of the test set
    indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
    
    # Exclude them from the train set
    train <- shuffled[-indices,]
    
    # Include them in the test set
    test <- shuffled[indices,]
    
    # A model is learned using each training set
    tree <- rpart(Survived ~ ., train, method = "class")
    
    # Make a prediction on the test set using tree
  pred<-predict(tree,test,type="class")
    
    # Assign the confusion matrix to conf
  conf<-table(test$Survived,pred)
    
    # Assign the accuracy of this model to the ith index in accs
    accs[i] <- sum(diag(conf))/sum(conf)
  }
> 
> # Print out the mean of accs
> mean(accs)
[1] 0.8011204
> Overfitting the spam!
Do you remember the crude spam filter, spam_classifier(), from chapter 1? It filters spam based on the average sequential use of capital letters (avg_capital_seq) to decide whether an email was spam (1) or not (0).

You may recall that we cheated and it perfectly filtered the spam. However, the set (emails_small) you used to test your classifier was only a small fraction of the entire dataset emails_full (Source: UCIMLR).

Your job is to verify whether the spam_classifier() that was built for you generalizes to the entire set of emails. The accuracy for the set emails_small was equal to 1. Is the accuracy for the entire set emails_full substantially lower?
> # The spam filter that has been 'learned' for you
> spam_classifier <- function(x){
    prediction <- rep(NA, length(x)) # initialize prediction vector
    prediction[x > 4] <- 1 
    prediction[x >= 3 & x <= 4] <- 0
    prediction[x >= 2.2 & x < 3] <- 1
    prediction[x >= 1.4 & x < 2.2] <- 0
    prediction[x > 1.25 & x < 1.4] <- 1
    prediction[x <= 1.25] <- 0
    return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
  }
> 
> # Apply spam_classifier to emails_full: pred_full
> 
> 
> # Build confusion matrix for emails_full: conf_full
> 
> 
> # Calculate the accuracy with conf_full: acc_full
> 
> 
> # Print acc_full
> Increasing the bias
It's official now, the spam_classifier() from chapter 1 is bogus. It simply overfits on the emails_small set and, as a result, doesn't generalize to larger datasets such as emails_full.

So let's try something else. On average, emails with a high frequency of sequential capital letters are spam. What if you simply filtered spam based on one threshold for avg_capital_seq?

For example, you could filter all emails with avg_capital_seq > 4 as spam. By doing this, you increase the interpretability of the classifier and restrict its complexity. However, this increases the bias, i.e. the error due to restricting your model.

Your job is to simplify the rules of spam_classifier and calculate the accuracy for the full set emails_full. Next, compare it to that of the small set emails_small, which is coded for you. Does the model generalize now?

Instructions
> # The all-knowing classifier that has been learned for you
> # You should change the code of the classifier, simplifying it
> spam_classifier <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 4] <- 1
    prediction[x <= 4]<-0
      return(factor(prediction, levels = c("1", "0")))
  }
> 
> # conf_small and acc_small have been calculated for you
> conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
> acc_small <- sum(diag(conf_small)) / sum(conf_small)
> acc_small
[1] 0.7692308
> 
> # Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
> pred_full <- spam_classifier(emails_full$avg_capital_seq)
> conf_full<-table(emails_full$spam,pred_full)
> # Calculate acc_full
> acc_full <- sum(diag(conf_full)) / sum(conf_full)
> 
> # Print acc_full
> acc_full
[1] 0.7259291
> ## chap-4-Decision trees
Learn a decision tree
As a big fan of shipwrecks, you decide to go to your local library and look up data about Titanic passengers. You find a data set of 714 passengers, and store it in the titanic data frame (Source: Kaggle). Each passenger has a set of features - Pclass, Sex and Age - and is labeled as survived (1) or perished (0) in the Survived column.

To test your classification skills, you can build a decision tree that uses a person's age, gender, and travel class to predict whether or not they survived the Titanic. The titanic data frame has already been divided into training and test sets (named train and test).

In this exercise, you'll need train to build a decision tree. You can use the rpart() function of the rpart package for this. Behind the scenes, it performs the steps that Vincent explained in the video: coming up with possible feature tests and building a tree with the best of these tests.

Finally, a fancy plot can help you interpret the tree. You will need the rattle, rpart.plot, and RColorBrewer packages to display this.

Note: In problems that have a random aspect, the set.seed() function is used to enforce reproducibility. Don't worry about it, just don't remove it!
> # The train and test set are loaded into your workspace.
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Load the rpart, rattle, rpart.plot and RColorBrewer package
> library(rpart)
> library(rattle)
Rattle: A free graphical interface for data mining with R.
Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
Type 'rattle()' to shake, rattle, and roll your data.
> library(rpart.plot)
> library(RColorBrewer)
> 
> # Fill in the ___, build a tree model: tree
> tree <- rpart(Survived~., train, method="class")
> 
> # Draw the decision tree
> fancyRpartPlot(tree)
> Classify with the decision tree
The previous learning step involved proposing different tests on which to split nodes and then to select the best tests using an appropriate splitting criterion. You were spared from all the implementation hassles that come with that: the rpart() function did all of that for you.

Now you are going to classify the instances that are in the test set. As before, the data frames titanic, train and test are available in your workspace. You'll only want to work with the test set, though.
> # The train and test set are loaded into your workspace.
> 
> # Code from previous exercise
> set.seed(1)
> library(rpart)
> tree <- rpart(Survived ~ ., train, method = "class")
> 
> # Predict the values of the test set: pred
> pred<-predict(tree,test,type="class")
> 
> # Construct the confusion matrix: conf
> conf<-table(test$Survived,pred)
> 
> # Print out the accuracy
> sum(diag(conf))/sum(conf)
[1] 0.7990654
> Pruning the tree
A like-minded shipwreck fanatic is also doing some Titanic predictions. He passes you some code that builds another decision tree model. The resulting model, tree, seems to work well but it's pretty hard to interpret. Generally speaking, the harder it is to interpret the model, the more likely you're overfitting on the training data.

You decide to prune his tree. This implies that you're increasing the bias, as you're restricting the amount of detail your tree can model. Finally, you'll plot this pruned tree to compare it to the previous one.
> # All packages are pre-loaded, as is the data
> 
> # Calculation of a complex tree
> set.seed(1)
> tree <- rpart(Survived ~ ., train, method = "class", control = rpart.control(cp=0.00001))
> 
> # Draw the complex tree
> fancyRpartPlot(tree)
> 
> # Prune the tree: pruned
> pruned<-prune(tree, cp=0.01)
> 
> # Draw pruned
> fancyRpartPlot(pruned)
> Splitting criterion
Do you remember the spam filters we built and tested in chapter 1 and 2? Well, it's time to make the filter more serious! We added some relevant data for every email that will help filter the spam, such as word and character frequencies. All of these can be found in the emails dataset, which is loaded in your workspace. Also, a training and test set have already been built from it: train and test.

In this exercise, you'll build two decision trees based on different splitting criteria. In the video you've learned about information gain: the higher the gain when you split, the better. However, the standard splitting criterion of rpart() is the Gini impurity.

It is up to you now to compare the information gain criterion with the Gini impurity criterion: how do the accuracy and resulting trees differ?
> # All packages, emails, train, and test have been pre-loaded
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Train and test tree with gini criterion
> tree_g <- rpart(spam ~ ., train, method = "class")
> pred_g <- predict(tree_g, test, type = "class")
> conf_g <- table(test$spam, pred_g)
> acc_g <- sum(diag(conf_g)) / sum(conf_g)
> 
> # Change the first line of code to use information gain as splitting criterion
> tree_i <- rpart(spam ~ ., train, method = "class", parms = list(split = "information"))
> pred_i <- predict(tree_i, test, type = "class")
> conf_i <- table(test$spam, pred_i)
> acc_i <- sum(diag(conf_i)) / sum(conf_i)
> 
> # Draw a fancy plot of both tree_g and tree_i
> fancyRpartPlot(tree_g)
> fancyRpartPlot(tree_i)
> 
> # Print out acc_g and acc_i
> acc_g
[1] 0.8905797
> acc_i
[1] 0.8963768
> Preprocess the data
Let's return to the tragic titanic dataset. This time, you'll classify its observations differently, with k-Nearest Neighbors (k-NN). However, there is one problem you'll have to tackle first: scale.

As you've seen in the video, the scale of your input variables may have a great influence on the outcome of the k-NN algorithm. In your case, the Age is on an entirely different scale than Sex and Pclass, hence it's best to rescale first!

For example, to normalize a vector x, you could do the following:

x−min(x)max(x)−min(x)
Head over to the instructions to normalize Age and Pclass for both the training and the test set.
> # train and test are pre-loaded
> 
> # Store the Survived column of train and test in train_labels and test_labels
> train_labels<-train$Survived
> test_labels<-test$Survived
> 
> # Copy train and test to knn_train and knn_test
> 
> knn_train<-train
> knn_test<-test
> # Drop Survived column for knn_train and knn_test
> knn_train$Survived<-NULL
> knn_test$Survived<-NULL
> 
> # Normalize Pclass
> min_class <- min(knn_train$Pclass)
> max_class <- max(knn_train$Pclass)
> knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
> knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)
> 
> # Normalize Age
> min_age <- min(knn_train$Age)
> max_age <- max(knn_train$Age)
> knn_train$Age <- (knn_train$Age-min_age)/(max_age-min_age)
> knn_test$Age <- (knn_test$Age-min_age)/(max_age-min_age)
> The knn() function
Now that you have your preprocessed data - available in your workspace as knn_train, knn_test, train_labels and test_labels - you are ready to start with actually classifying some instances with k-Nearest Neighbors.

To do this, you can use the knn() function which is available from the class package.

> # knn_train, knn_test, train_labels and test_labels are pre-loaded
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Load the class package
> library(class)
> 
> # Fill in the ___, make predictions using knn: pred
> pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)
> 
> # Construct the confusion matrix: conf
> conf<-table(test_labels,pred)
> 
> # Print out the confusion matrix
> conf
           pred
test_labels   1   0
          1  61  24
          0  17 112
> 
Let's try it out and see what the result looks like.
K's choice
A big issue with k-Nearest Neighbors is the choice of a suitable k. How many neighbors should you use to decide on the label of a new observation? Let's have R answer this question for us and assess the performance of k-Nearest Neighbor classification for increasing values of k.

Again, knn_train, knn_test, train_labels and test_labels that you've created before are available in your workspace.
> # knn_train, knn_test, train_labels and test_labels are pre-loaded
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Load the class package, define range and accs
> library(class)
> range <- 1:round(0.2 * nrow(knn_train))
> accs <- rep(0, length(range))
> 
> for (k in range) {
  
    # Fill in the ___, make predictions using knn: pred
    pred <- knn(knn_train, knn_test, train_labels, k = k)
  
    # Fill in the ___, construct the confusion matrix: conf
    conf <- table(test_labels, pred)
  
    # Fill in the ___, calculate the accuracy and store it in accs[k]
    accs[k] <-sum(diag(conf))/sum(conf)
  }
> 
> # Plot the accuracies. Title of x-axis is "k".
> plot(range, accs, xlab = "k")
> 
> # Calculate the best k
>  which.max(accs)
[1] 73
> Creating the ROC curve (1)
In this exercise you will work with a medium sized dataset about the income of people given a set of features like education, race, sex, and so on. Each observation is labeled with 1 or 0: 1 means the observation has annual income equal or above $50,000, 0 means the observation has an annual income lower than $50,000 (Source: UCIMLR). This label information is stored in the income variable.

A tree model, tree, is learned for you on a training set and tries to predict income based on all other variables in the dataset.

In previous exercises, you used this tree to make class predictions, by setting the type argument in predict() to "class".

To build an ROC curve, however, you need the probabilities that the observations are positive. In this case, you'll want to to predict the probability of each observation in the test set (already available) having an annual income equal to or above $50,000. Now, you'll have to set the type argument of predict() to "prob".
# train and test are pre-loaded
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Build a tree on the training set: tree
> tree <- rpart(income ~ ., train, method = "class")
> 
> # Predict probability values using the model: all_probs
> 
> all_probs<-predict(tree,test,type="prob")
> # Print out all_probs
> #all_probs
> 
> # Select second column of all_probs: probs
> 
> probs<-all_probs[,2]
> # train and test are pre-loaded
> 
> # Set random seed. Don't remove this line
> set.seed(1)
> 
> # Build a tree on the training set: tree
> tree <- rpart(income ~ ., train, method = "class")
> 
> # Predict probability values using the model: all_probs
> 
> all_probs<-predict(tree,test,type="prob")
> # Print out all_probs
> #all_probs
> 
> # Select second column of all_probs: probs
> 
> probs<-all_probs[,2]
> Creating the ROC curve (2)
Now that you have the probabilities of every observation in the test set belonging to the positive class (annual income equal or above $50,000), you can build the ROC curve.

You'll use the ROCR package for this. First, you have to build a prediction object with prediction(). Next, you can use performance() with the appropriate arguments to build the actual ROC data and plot it.

probs, which you had to calculate in the previous exercise, is already coded for you.
> # train and test are pre-loaded
> 
> # Code of previous exercise
> set.seed(1)
> tree <- rpart(income ~ ., train, method = "class")
> probs <- predict(tree, test, type = "prob")[,2]
> 
> # Load the ROCR library
> library(ROCR)
> 
> # Make a prediction object: pred
> pred<- prediction(probs,test$income)
> 
> # Make a performance object: perf
> perf<- performance(pred,"tpr","fpr")
> 
> # Plot this curve
> plot(perf)
> he area under the curve
The same package you used for constructing the ROC curve can be used to quantify the area under the curve, or AUC. The same tree model is loaded into your workspace, and the test set's probabilities have again been calculated for you.

Again using the ROCR package, you can calculate the AUC. The use of prediction() is identical to before. However, the performance() function needs some tweaking.
# test and train are loaded into your workspace
> 
> # Build tree and predict probability values for the test set
> set.seed(1)
> tree <- rpart(income ~ ., train, method = "class")
> probs <- predict(tree, test, type = "prob")[,2]
> 
> # Load the ROCR library
> library(ROCR)
> 
> # Make a prediction object: pred
> pred <- prediction(probs, test$income)
> 
> # Make a performance object: perf
> perf <- performance(pred, "auc")
> 
> # Print out the AUC
> print(perf@y.values[[1]])
[1] 0.8463732
> Comparing the methods
In this exercise you're going to assess two models: a decision tree model and a k-Nearest Neighbor model. You will compare the ROC curves of these models to draw your conclusions.

You finished the previous chapter by building a spam filter. This time, we have some predictions from two spam filters! These spam filters calculated the probabilities of unseen observations in the test set being spam. The real spam labels of the test set can be found in test$spam.

It is your job to use your knowledge about the ROCR package to plot two ROC curves, one for each classifier. The assigned probabilities for the observations in the test set are loaded into your workspace: probs_t for the decision tree model, probs_k for k-Nearest Neighbors.

The test set is loaded into your workspace as test. It's a subset of the emails dataset.
> # Load the ROCR library
> library(ROCR)
Loading required package: gplots

Attaching package: 'gplots'
The following object is masked from 'package:stats':

    lowess
> 
> # Make the prediction objects for both models: pred_t, pred_k
> pred_t<-prediction(probs_t,test$spam)
> pred_k<-prediction(probs_k,test$spam)
> 
> # Make the performance objects for both models: perf_t, perf_k
> perf_t<-performance(pred_t,"tpr","fpr")
> perf_k<-performance(pred_k,"tpr","fpr")
> 
> 
> # Draw the ROC lines using draw_roc_lines(perf_t,perf_k)
> draw_roc_lines(perf_t,perf_k)
> ##chap-4-Regression: simple and linear
Simple linear regression: your first step!
In your first exercise, you'll familiarize yourself with the concept of simple linear regression. You are given measures of grey kangaroos' nose width and length (Source). You can find the data in kang_nose, which is loaded in your workspace. It has two columns: nose_width and nose_length.

Your job is to describe the linear relationship between the grey kangaroo's nose width (mm) and nose length (mm). Make use of the lm() function as shown in the video and consult the help file if necessary. Remember to explore your data first!

We caught Skippy and measured its nose width, nose_width_new, but it escaped before we measured its nose length, can you help?
> # The kang_nose dataset and nose_width_new are already loaded in your workspace.
> 
> # Plot nose length as function of nose width.
> plot(kang_nose, xlab = "nose width", ylab = "nose length")
> 
> # Fill in the ___, describe the linear relationship between the two variables: lm_kang
> lm_kang <- lm(nose_length ~ nose_width, data = kang_nose)
> 
> # Print the coefficients of lm_kang
> lm_kang$coefficients
(Intercept)  nose_width 
  27.893058    2.701175
> 
> # Predict and print the nose length of the escaped kangoroo
> predict(lm_kang,nose_width_new)
       1 
703.1869
> Performance measure: RMSE
Now that you've got a grasp on the concept of simple linear regression, let's move on to assessing the performance. Let's stick to the Kangaroo example. The dataset, kang_nose, as well as the linear model you built, lm_kang, are available so you can start right away.

In this exercise, you'll plot the regression line through the data points. Next, you'll calculate the residuals,

resi=yi−y^i
These are the errors you made by fitting a line through the data points.

Lastly, you'll determine the Root-Mean-Square-Error:

RMSE=1N∑i=1Nres2i−−−−−−−−−⎷
This estimates the standard deviation of the model. Remember, the smaller the RMSE, the better your fit!
> # kang_nose is pre-loaded in your workspace
> 
> # Build model and make plot
> lm_kang <- lm(nose_length ~ nose_width, data=kang_nose)
> plot(kang_nose, xlab = "nose width", ylab = "nose length")
> abline(lm_kang$coefficients, col = "red")
> 
> # Apply predict() to lm_kang: nose_length_est
> nose_length_est<-predict(lm_kang)
> 
> # Calculate difference between the predicted and the true values: res
> res<-kang_nose$nose_length - nose_length_est
> 
> # Calculate RMSE, assign it to rmse and print it
> rmse<-sqrt(mean((res)^2))
> rmse
[1] 43.26288
> erformance measures: R-squared
You've correctly calculated the RMSE in the last exercise, but were you able to interpret it? You can compare the RMSE to the total variance of your response by calculating the R2, which is unitless! The closer R2 to 1, the greater the degree of linear association is between the predictor and the response variable.

R calculates these performance measures for you. You can display them by applying summary() to your linear model object. Your job is to now manually calculate R2 and compare your value to the value that R calculated automatically.

R2=1−SSresSStot
Here, SSres is the residual sum of squares,

SSres=∑i=1nres2i=∑i=1n(yi−y^i)2
whereas SStot is the total sum of squares,

SStot=∑i=1n(yi−y¯)2
with y¯ the sample mean of the response. res, kang_nose and lm_kang are already available in your workspace.
> # kang_nose, lm_kang and res are already loaded in your workspace
> 
> # Calculate the residual sum of squares: ss_res
> ss_res<-sum(res^2)
> 
> # Determine the total sum of squares: ss_tot
> ss_tot<-sum((kang_nose$nose_length - mean(kang_nose$nose_length))^2)
> 
> # Calculate R-squared and assign it to r_sq. Also print it.
> r_sq<-1-ss_res/ss_tot
> 
> r_sq
[1] 0.7768914
> # Apply summary() to lm_kang
> summary(lm_kang)

Call:
lm(formula = nose_length ~ nose_width, data = kang_nose)

Residuals:
    Min      1Q  Median      3Q     Max 
-69.876 -32.912  -4.855  30.227  86.307 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  27.8931    54.2991   0.514     0.61    
nose_width    2.7012     0.2207  12.236 1.34e-15 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 44.26 on 43 degrees of freedom
Multiple R-squared:  0.7769,	Adjusted R-squared:  0.7717 
F-statistic: 149.7 on 1 and 43 DF,  p-value: 1.342e-15
> Another take at regression: be critical
You are given data on GDP per capita and its relation to the percentage of urban population for several UN countries, measured in the year 2014 (Source: The World Bank). This dataset is stored in a data frame world_bank_train and has two variables: cgdp and urb_pop.

Have a look at the data, do you think a relationship between the two is plausible? Try to set up a linear model for the percentage of urban population based on the GDP per capita.

Afghanistan has a GDP per capita of 413 USD, stored in cgdp_afg, but its urban population in 2014 is not known yet. Can you predict the outcome using your model?
> # world_bank_train and cgdp_afg is available for you to work with
> 
> # Plot urb_pop as function of cgdp
> plot(world_bank_train$cgdp,world_bank_train$urb_pop)
> 
> # Set up a linear model between the two variables: lm_wb
> lm_wb<-lm(urb_pop~cgdp,world_bank_train)
> 
> # Add a red regression line to your scatter plot
> abline(lm_wb$coefficients, col = "red")
> 
> # Summarize lm_wb and select R-squared
> summary(lm_wb)

Call:
lm(formula = urb_pop ~ cgdp, data = world_bank_train)

Residuals:
    Min      1Q  Median      3Q     Max 
-38.273 -12.364   0.548  12.574  37.714 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 4.470e+01  1.727e+00  25.881  < 2e-16 ***
cgdp        7.578e-04  8.143e-05   9.307 2.46e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 16.91 on 140 degrees of freedom
Multiple R-squared:  0.3822,	Adjusted R-squared:  0.3778 
F-statistic: 86.61 on 1 and 140 DF,  p-value: 2.461e-16
> summary(lm_wb)$r.squared
[1] 0.3822067
> # Predict the urban population of afghanistan based on cgdp_afg
> predict(lm_wb,cgdp_afg)
       1 
45.01204
> Non-linear, but still linear?
In the last exercise, your scatter plot didn't show a strong linear relationship. You confirmed this with the regression line and R2.

To improve your model, take a step back and study the nature of the data. The predictor variable is numerical, while the response variable is expressed in percentiles. It would make more sense if there were a linear relationship between the percentile changes of the GDP / capita and the changes in the response.

To obtain an estimation of percentile changes, take the natural logarithm of the GDP / capita and use this as your new predictor variable. A model solution to the previous exercise is included in the editor; up to you to make some changes.
> # world_bank_train and cgdp_afg is available for you to work with
> 
> # Plot: change the formula and xlab
> plot(urb_pop ~ log(cgdp), data = world_bank_train,
       xlab = "log(GDP per Capita)",
       ylab = "Percentage of urban population")
> 
> # Linear model: change the formula
> lm_wb <- lm(urb_pop ~ log(cgdp), data = world_bank_train)
> 
> # Add a red regression line to your scatter plot
> abline(lm_wb$coefficients, col = "red")
> 
> # Summarize lm_wb and select R-squared
> summary(lm_wb)

Call:
lm(formula = urb_pop ~ log(cgdp), data = world_bank_train)

Residuals:
    Min      1Q  Median      3Q     Max 
-41.696  -7.338  -0.548  10.119  32.881 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -42.6013     7.0531   -6.04 1.32e-08 ***
log(cgdp)    11.3671     0.8196   13.87  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 13.97 on 140 degrees of freedom
Multiple R-squared:  0.5788,	Adjusted R-squared:  0.5757 
F-statistic: 192.4 on 1 and 140 DF,  p-value: < 2.2e-16
> summary(lm_wb)$r.squared
[1] 0.5787588
> 
> # Predict the urban population of afghanistan based on cgdp_afg
> predict(lm_wb,cgdp_afg)
       1 
25.86759
> Going all-in with predictors!
In the video, Vincent showed you a multi-linear model for net sales based on advertisement and competition. The dataset is available in your workspace as shop_data.

In this exercise you'll add even more predictors: inventory (inv), the size of the district (size_dist) and the shop size (sq_ft).

Your job is to set up this model, verify if the fit is good and finally measure the accuracy. Make sure you interpret the results at every step!
> # shop_data has been loaded in your workspace
> 
> # Add a plot: sales as a function of inventory. Is linearity plausible?
> plot(sales ~ sq_ft, shop_data)
> plot(sales ~ size_dist, shop_data)
> plot(sales ~ inv, shop_data)
> 
> # Build a linear model for net sales based on all other variables: lm_shop
> lm_shop<-lm(sales~.,shop_data)
> 
> # Summarize lm_shop
> summary(lm_shop)

Call:
lm(formula = sales ~ ., data = shop_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-26.338  -9.699  -4.496   4.040  41.139 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -18.85941   30.15023  -0.626 0.538372    
sq_ft        16.20157    3.54444   4.571 0.000166 ***
inv           0.17464    0.05761   3.032 0.006347 ** 
ads          11.52627    2.53210   4.552 0.000174 ***
size_dist    13.58031    1.77046   7.671 1.61e-07 ***
comp         -5.31097    1.70543  -3.114 0.005249 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 17.65 on 21 degrees of freedom
Multiple R-squared:  0.9932,	Adjusted R-squared:  0.9916 
F-statistic: 611.6 on 5 and 21 DF,  p-value: < 2.2e-16
> Are all predictors relevant?
To further analyze the performance, take a look at the p-values of every predictor. Are they all relevant? Remember that you should verify the assumptions on the error variable before interpreting these results.

There is a shop owner that didn't participate in the questionnaire, who has caught wind of your amazing model! He asked us to estimate his net sales based on the data he provided. Can you help him out? The data can be found in shop_new. shop_data and lm_shop are also available in your workspace.
> # shop_data, shop_new and lm_shop have been loaded in your workspace
> 
> # Plot the residuals in function of your fitted observations
> plot(lm_shop$fitted.values, lm_shop$residuals)
> 
> # Make a Q-Q plot of your residual quantiles
> qqnorm(lm_shop$residuals,ylab="Residual Quantiles")
> 
> # Summarize your model, are there any irrelevant predictors?
> summary(lm_shop)

Call:
lm(formula = sales ~ ., data = shop_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-26.338  -9.699  -4.496   4.040  41.139 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -18.85941   30.15023  -0.626 0.538372    
sq_ft        16.20157    3.54444   4.571 0.000166 ***
inv           0.17464    0.05761   3.032 0.006347 ** 
ads          11.52627    2.53210   4.552 0.000174 ***
size_dist    13.58031    1.77046   7.671 1.61e-07 ***
comp         -5.31097    1.70543  -3.114 0.005249 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 17.65 on 21 degrees of freedom
Multiple R-squared:  0.9932,	Adjusted R-squared:  0.9916 
F-statistic: 611.6 on 5 and 21 DF,  p-value: < 2.2e-16
> 
> # Predict the net sales based on shop_new.
> predict(lm_shop,shop_new)
       1 
262.5006
> Are all predictors relevant? Take 2!
Let's take a different dataset. In 2002 Brisbane did a study on different chocolate bars and measured its energy per 100 gram, percentage of proteins, percentage of fat and the total size. You're wondering whether energy is related to the other variables. You can find the results in choco_data.

Your job is to build a multi linear model for the energy based on all other variables and judge its performance.
> # choco_data has been loaded in your workspace
> 
> # Add a plot:  energy/100g as function of total size. Linearity plausible?
> plot(energy ~ protein, choco_data)
> plot(energy ~ fat, choco_data)
> plot(energy ~ size, choco_data)
> 
> # Build a linear model for the energy based on all other variables: lm_choco
> lm_choco<-lm(energy ~ ., choco_data)
> 
> # Plot the residuals in function of your fitted observations
> plot(lm_choco$fitted.values,lm_choco$residuals)
> 
> # Make a Q-Q plot of your residual quantiles
> qqnorm(lm_choco$residuals)
> 
> # Summarize lm_choco
> summary(lm_choco)

Call:
lm(formula = energy ~ ., data = choco_data)

Residuals:
     Min       1Q   Median       3Q      Max 
-107.084  -35.756   -8.323   36.100  104.660 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1338.3334    40.0928  33.381  < 2e-16 ***
protein       23.0019     3.6636   6.279 6.97e-08 ***
fat           24.4662     1.6885  14.490  < 2e-16 ***
size          -0.8183     0.6035  -1.356    0.181    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 52.2 on 52 degrees of freedom
Multiple R-squared:  0.9019,	Adjusted R-squared:  0.8962 
F-statistic: 159.3 on 3 and 52 DF,  p-value: < 2.2e-16
> Does your model generalize?
Remember the dataset on GDP per capita and percentage of urban development? You were advised to take the logarithm of GDP per capita. Both the regression line and R2 showed a better result for the log-linear model, lm_wb_log, than for the simple linear model, lm_wb.

You might be wondering whether we were misguiding you and had you produce an overfit? Your workspace contains world_bank_train that you've used before to build the log-linear model. Now, however, also world_bank_test is available. You can use this dataset to test if your model generalizes well.

You'll be comparing the RMSE for both sets:

RMSE=1n∑i=1n(resi)2−−−−−−−−−−√
> # world_bank_train, world_bank_test and lm_wb_log are pre-loaded
> 
> # Build the log-linear model
> lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train)
> 
> # Calculate rmse_train
> rmse_train <- sqrt(mean(lm_wb_log$residuals ^ 2))
> 
> # The real percentage of urban population in the test set, the ground truth
> world_bank_test_truth <- world_bank_test$urb_pop
> 
> # The predictions of the percentage of urban population in the test set
> world_bank_test_input <- data.frame(cgdp = world_bank_test$cgdp)
> world_bank_test_output <- predict(lm_wb_log, world_bank_test_input)
> 
> # The residuals: the difference between the ground truth and the predictions
> res_test <- world_bank_test_output - world_bank_test_truth
> 
> 
> # Use res_test to calculate rmse_test
> rmse_test<-sqrt(mean((res_test)^2))
> 
> # Print the ratio of the test RMSE over the training RMSE
> rmse_test/rmse_train
[1] 1.08308
> 
our own k-NN algorithm!
In the video, Gilles shortly showed you how to set up your own k-NN algorithm. Now it's time to inspect up close how it works.

We went ahead and defined a function my_knn that contains a k-NN algorithm. Its arguments are:

x_pred: predictor values of the new observations (this will be the cgdp column of world_bank_test),
x: predictor values of the training set (the cgdp column of world_bank_train),
y: corresponding response values of the training set (the urb_pop column of world_bank_train),
k: the number of neighbors (this will be 30).
The function returns the predicted values for your new observations (predict_knn).

You'll apply a k-NN algorithm to the GDP / capita of the countries in world_bank_test to predict their percentage of urban population
> ###
> # You don't have to change this!
> # The algorithm is already coded for you;
> # inspect it and try to understand how it works!
> my_knn <- function(x_pred, x, y, k){
    m <- length(x_pred)
    predict_knn <- rep(0, m)
    for (i in 1:m) {
  
      # Calculate the absolute distance between x_pred[i] and x
      dist <- abs(x_pred[i] - x)
  
      # Apply order() to dist, sort_index will contain
      # the indices of elements in the dist vector, in
      # ascending order. This means sort_index[1:k] will
      # return the indices of the k-nearest neighbors.
      sort_index <- order(dist)
  
      # Apply mean() to the responses of the k-nearest neighbors
      predict_knn[i] <- mean(y[sort_index[1:k]])
  
    }
    return(predict_knn)
  }
> ###
> 
> # world_bank_train and world_bank_test are pre-loaded
> 
> # Apply your algorithm on the test set: test_output
> test_output<-my_knn(world_bank_test$cgdp,world_bank_train$cgdp,world_bank_train$urb_pop,30)
> 
> # Have a look at the plot of the output
> plot(world_bank_train,
       xlab = "GDP per Capita",
       ylab = "Percentage Urban Population")
> points(world_bank_test$cgdp, test_output, col = "green")
> Parametric vs non-parametric!
So now you've built three different models for the same data:

a simple linear model, lm_wb,
a log-linear model, lm_wb_log and
a non-parametric k-NN model. This k-NN model is actually simply a function that takes test and training data and predicts response variables on the fly: my_knn().
These objects are all stored in your workspace, as are world_bank_train and world_bank_test.

Have a look at the sample code on the right, which shows the first steps for building a fancy plot. In the end, three lines should be plotted that represent the predicted responses for the test set, together with the true responses.

You'll also calculate the RMSE of the test set for the simple linear, log-linear and k-NN regression. Have a look at the results, which regression approach performs the best?
> # world_bank_train and world_bank_test are pre-loaded
> # lm_wb and lm_wb_log have been trained on world_bank_train
> # The my_knn() function is available
> 
> # Define ranks to order the predictor variables in the test set
> ranks <- order(world_bank_test$cgdp)
> 
> # Scatter plot of test set
> plot(world_bank_test,
       xlab = "GDP per Capita", ylab = "Percentage Urban Population")
> 
> # Predict with simple linear model and add line
> test_output_lm <- predict(lm_wb, data.frame(cgdp = world_bank_test$cgdp))
> lines(world_bank_test$cgdp[ranks], test_output_lm[ranks], lwd = 2, col = "blue")
> 
> # Predict with log-linear model and add line
> test_output_lm_log <- predict(lm_wb_log, data.frame(cgdp = world_bank_test$cgdp))
> lines(world_bank_test$cgdp[ranks], test_output_log[ranks], lwd = 2, col = "red")
Error: object 'test_output_log' not found
> 
> # Predict with k-NN and add line
> test_output_knn <- my_knn(world_bank_test$cgdp,world_bank_train$cgdp,world_bank_train$urb_pop,30)
> lines(world_bank_test$cgdp[ranks], test_output_knn[ranks], lwd = 2, col = "green")
> 
> 
> # Calculate RMSE on the test set for simple linear model
> sqrt(mean((test_output_lm - world_bank_test$urb_pop) ^ 2))
[1] 17.41897
> 
> # Calculate RMSE on the test set for log-linear model
> sqrt(mean((test_output_lm_log - world_bank_test$urb_pop) ^ 2))
[1] 15.01911
> 
> # Calculate RMSE on the test set for k-NN technique
> sqrt(mean((test_output_knn - world_bank_test$urb_pop) ^ 2))
[1] 16.10026
> ##chap-5-Clustering with k-means
k-means: how well did you do earlier?
Remember the seeds dataset from Chapter 2 which you clustered using the k-means method? Well, we've found the labels of the seeds. They are stored in the vector seeds_type; there were indeed three types of seeds!

While clusters are made without the use of true labels, if you happen to have them, it is simply interesting to see how well the clusters you made correspond to these true labels.

It's up to you now to cluster the instances in seeds and compare the resulting clusters with seeds_type. Both objects are available in your workspace.
> # seeds and seeds_type are pre-loaded in your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(100)
> 
> # Do k-means clustering with three clusters, repeat 20 times: seeds_km
> seeds_km<-kmeans(seeds,3,nstart=20)
> 
> # Print out seeds_km
> seeds_km
K-means clustering with 3 clusters of sizes 77, 72, 61

Cluster means:
      area perimeter compactness   length    width asymmetry groove_length
1 11.96442  13.27481   0.8522000 5.229286 2.872922  4.759740      5.088519
2 14.64847  14.46042   0.8791667 5.563778 3.277903  2.648933      5.192319
3 18.72180  16.29738   0.8850869 6.208934 3.722672  3.603590      6.066098

Clustering vector:
  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2
 [38] 3 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 3 3 3 3
 [75] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3
[112] 3 3 3 3 3 3 3 3 3 3 3 2 3 2 3 3 3 3 3 3 3 2 2 2 2 3 2 2 2 1 1 1 1 1 1 1 1
[149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1
[186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1

Within cluster sum of squares by cluster:
[1] 195.7453 207.4648 184.1086
 (between_SS / total_SS =  78.4 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
[6] "betweenss"    "size"         "iter"         "ifault"
> 
> # Compare clusters with actual seed types. Set k-means clusters as rows
> table(seeds_km$cluster,seeds_type)
   seeds_type
     1  2  3
  1  9  0 68
  2 60 10  2
  3  1 60  0
> 
> # Plot the length as function of width. Color by cluster
> plot(seeds$width,seeds$length,col=seeds_km$cluster)
> The influence of starting centroids
If you call kmeans() without specifying your centroids, R will randomly assign them for you. In this exercise, you will see the influence of these starting centroids yourself using the seeds dataset.

To compare the clusters of two cluster models, you can again use table(). If every row and every column has one value, the resulting clusters completely overlap. If this is not the case, some objects are placed in different clusters.
> # seeds is pre-loaded in your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(100)
> 
> # Apply kmeans to seeds twice: seeds_km_1 and seeds_km_2
> seeds_km_1<-kmeans(seeds,5,nstart=1)
> seeds_km_2<-kmeans(seeds,5,nstart=1)
> 
> 
> # Return the ratio of the within cluster sum of squares
> seeds_km_1$tot.withinss/seeds_km_2$tot.withinss
[1] 1.006146
> 
> # Compare the resulting clusters
> table(seeds_km_1$cluster,seeds_km_2$cluster)
   
     1  2  3  4  5
  1  0 16  0  0 18
  2  0 56  0  0  0
  3  0  0 12  0  0
  4  0  0  0 18 41
  5 33  0  3 13  0
> Making a scree plot!
Let's move on to some new data: school results! You're given a dataset school_result containing school level data recording reading and arithmetic scores for each school's 4th and 6th graders. (Source: cluster.datasets package). We're wondering if it's possible to define distinct groups of students based on their scores and if so how many groups should we consider?

Your job is to cluster the schools based on their scores with k-means, for different values of k. On each run, you'll record the ratio of the within cluster sum of squares to the total sum of squares. The scree plot will tell you which k is optimal!
> # The dataset school_result is pre-loaded
> 
> # Set random seed. Don't remove this line.
> set.seed(100)
> 
> # Explore the structure of your data
> str(school_result)
'data.frame':	25 obs. of  4 variables:
 $ reading.4   : num  2.7 3.9 4.8 3.1 3.4 3.1 4.6 3.1 3.8 5.2 ...
 $ arithmetic.4: num  3.2 3.8 4.1 3.5 3.7 3.4 4.4 3.3 3.7 4.9 ...
 $ reading.6   : num  4.5 5.9 6.8 4.3 5.1 4.1 6.6 4 4.7 8.2 ...
 $ arithmetic.6: num  4.8 6.2 5.5 4.6 5.6 4.7 6.1 4.9 4.9 6.9 ...
> 
> # Initialise ratio_ss
> ratio_ss<-rep(0,7)
> 
> # Finish the for-loop.
> for (k in 1:7) {
    
    # Apply k-means to school_result: school_km
  school_km<-kmeans(school_result,k,nstart=20)
    
    # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k]<-(school_km$tot.withinss)/(school_km$totss)
    
  }
> 
> # Make a scree plot with type "b" and xlab "k"
> plot(ratio_ss,type="b",xlab="k")
> Standardized vs non-standardized clustering (1)
You agreed that the run_record data should be standardized prior to clustering, but is it truly that important?

In this exercise you'll cluster the countries based on their unstandardized records and calculate Dunn's index. Take your time to check out the dataset, you'll be using it in the upcoming exercises.

In the instructions that follow, you'll also create a scatter plot of two variables of the run_record dataset. Note that the clustering is performed based on all the data, not only these two columns!
> # The dataset run_record has been loaded in your workspace
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Explore your data with str() and summary()
> str(run_record)
'data.frame':	54 obs. of  8 variables:
 $ X100m   : num  10.23 9.93 10.15 10.14 10.27 ...
 $ X200m   : num  20.4 20.1 20.4 20.2 20.3 ...
 $ X400m   : num  46.2 44.4 45.8 45 45.3 ...
 $ X800m   : num  106 104 106 104 107 ...
 $ X1500m  : num  221 212 215 214 222 ...
 $ X5000m  : num  800 776 796 770 878 ...
 $ X10000m : num  1659 1652 1663 1612 1829 ...
 $ marathon: num  7774 7651 7933 7632 8782 ...
> summary(run_record)
     X100m           X200m           X400m           X800m      
 Min.   : 9.78   Min.   :19.32   Min.   :43.18   Min.   :101.4  
 1st Qu.:10.10   1st Qu.:20.17   1st Qu.:44.91   1st Qu.:103.8  
 Median :10.20   Median :20.43   Median :45.58   Median :105.6  
 Mean   :10.22   Mean   :20.54   Mean   :45.83   Mean   :106.1  
 3rd Qu.:10.32   3rd Qu.:20.84   3rd Qu.:46.32   3rd Qu.:108.0  
 Max.   :10.97   Max.   :22.46   Max.   :51.40   Max.   :116.4  
     X1500m          X5000m          X10000m        marathon    
 Min.   :206.4   Min.   : 759.6   Min.   :1588   Min.   : 7473  
 1st Qu.:213.0   1st Qu.: 788.9   1st Qu.:1653   1st Qu.: 7701  
 Median :216.6   Median : 805.2   Median :1675   Median : 7819  
 Mean   :219.2   Mean   : 817.1   Mean   :1712   Mean   : 8009  
 3rd Qu.:224.2   3rd Qu.: 834.5   3rd Qu.:1739   3rd Qu.: 8050  
 Max.   :254.4   Max.   :1002.0   Max.   :2123   Max.   :10276
> # Cluster run_record using k-means: run_km. 5 clusters, repeat 20 times
> run_km<-kmeans(run_record,5,nstart=20)
> 
> # Plot the 100m as function of the marathon. Color using clusters
> plot(run_record$marathon,run_record$X100m,col=run_km$cluster,xlab="marathon",ylab="X100m")
> 
> # Calculate Dunn's index: dunn_km. Print it.
> dunn_km<-dunn(clusters = run_km$cluster,Data=run_record)
> dunn_km
[1] 0.05651773
> Standardized vs non-standardized clustering (2)
You expected it already, the unstandardized clusters don't produce satisfying results. Let's see if standardizing helps!

Your job is the standardize the run_record dataset and apply the k-means algorithm again. Calculate Dunn's index and compare the results
# The dataset run_record as well as run_km are available
> 
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Standardize run_record, transform to a dataframe: run_record_sc
> run_record_sc <- as.data.frame(scale(run_record))
> 
> # Cluster run_record_sc using k-means: run_km_sc. 5 groups, let R start over 20 times
> run_km_sc <- kmeans(run_record_sc, 5, nstart = 20)
> 
> # Plot records on 100m as function of the marathon. Color using the clusters in run_km_sc
> plot(run_record$marathon, run_record$X100m, col = run_km_sc$cluster,
       xlab = "marathon", ylab ="100m", main = "Run Records")
> 
> # Compare the resulting clusters in a nice table
> table(run_km$cluster, run_km_sc$cluster)
   
     1  2  3  4  5
  1  0  6  0  0  0
  2  7  0 10  0  1
  3  1  0 10  0 13
  4  2  2  0  0  0
  5  0  0  0  2  0
> 
> # Calculate Dunn's index: dunn_km_sc. Print it.
> dunn_km_sc <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)
> dunn_km_sc
[1] 0.1453556
> Single Hierarchical Clustering
Let's return to the Olympic records example. You've already clustered the countries using the k-means algorithm, but this gave you a fixed amount of clusters. We're interested in more!

In this exercise, you'll apply the hierarchical method to cluster the countries. Of course, you'll be working with the standardized data. Make sure to visualize your results!
> # The dataset run_record_sc has been loaded in your workspace
> 
> # Apply dist() to run_record_sc: run_dist
> run_dist<-dist(run_record_sc)
> 
> # Apply hclust() to run_dist: run_single
> run_single<-hclust(run_dist,method="single")
> 
> # Apply cutree() to run_single: memb_single
> memb_single<-cutree(run_single,k=5)
> 
> # Apply plot() on run_single to draw the dendrogram
> 
> plot(run_single)
> # Apply rect.hclust() on run_single to draw the boxes
> rect.hclust(run_single,k=5,border=2:6)
> Complete Hierarchical Clustering
The clusters of the last exercise weren't truly satisfying. The single-linkage method appears to be placing each outlier in its own cluster. Let's see if complete-linkage agrees with this clustering!

In this exercise, you'll repeat some steps from the last exercise, but this time for the complete-linkage method. Visualize your results and compare with the single-linkage results. A model solution to the previous exercise is already available to inspire you. It's up to you to add code for complete-linkage.
> # run_record_sc is pre-loaded
> 
> # Code for single-linkage
> run_dist <- dist(run_record_sc, method = "euclidean")
> run_single <- hclust(run_dist, method = "single")
> memb_single <- cutree(run_single, 5)
> plot(run_single)
> rect.hclust(run_single, k = 5, border = 2:6)
> 
> # Apply hclust() to run_dist: run_complete
> run_complete <- hclust(run_dist, method = "complete")
> 
> # Apply cutree() to run_complete: memb_complete
> memb_complete <- cutree(run_complete, 5)
> 
> # Apply plot() on run_complete to draw the dendrogram
> 
> plot(run_complete)
> # Apply rect.hclust() on run_complete to draw the boxes
> rect.hclust(run_complete, k = 5, border = 2:6)
> 
> # table() the clusters memb_single and memb_complete. Put memb_single in the rows
> table(memb_single,memb_complete)
           memb_complete
memb_single  1  2  3  4  5
          1 27  7 14  0  1
          2  0  0  0  1  0
          3  0  0  0  0  1
          4  0  0  0  0  2
          5  0  0  0  1  0
> Hierarchical vs k-means
So you've clustered the countries based on their Olympic run performances using three different methods: k-means clustering, hierarchical clustering with single linkage and hierarchical clustering with complete linkage. You can ask yourself: which method returns the best separated and the most compact clusters?

Let's use Dunn's index. Remember, it returns the ratio between the minimum intercluster distance to the maximum intracluster diameter. The dunn() function in R, requires the argument clusters, indicating the cluster partitioning, the Data and a method to determine the distance. In this case, that's "euclidean", which is the default.

Your job is to calculate Dunn's index for all three clusterings and compare the clusters to each other. The R objects you calculated in the previous exercises are already available in your workspace.
> # run_record_sc, run_km_sc, memb_single and memb_complete are pre-calculated
> 
> # Set random seed. Don't remove this line.
> set.seed(100)
> 
> # Dunn's index for k-means: dunn_km
> dunn_km<-dunn(clusters = run_km_sc$cluster, Data=run_record_sc)
> 
> # Dunn's index for single-linkage: dunn_single
> dunn_single<-dunn(clusters = memb_single, Data=run_record_sc)
> 
> # Dunn's index for complete-linkage: dunn_complete
> dunn_complete<-dunn(clusters = memb_complete, Data=run_record_sc)
> 
> # Compare k-means with single-linkage
> table(run_km_sc$cluster,memb_single)
   memb_single
     1  2  3  4  5
  1  6  0  0  2  0
  2  9  0  1  0  0
  3  0  1  0  0  1
  4 14  0  0  0  0
  5 20  0  0  0  0
> 
> # Compare k-means with complete-linkage
> table(run_km_sc$cluster,memb_complete)
   memb_complete
     1  2  3  4  5
  1  0  0  6  0  2
  2  0  0  8  0  2
  3  0  0  0  2  0
  4  7  7  0  0  0
  5 20  0  0  0  0
> Clustering US states based on criminal activity
You've seen that different clustering methods can return entirely different clusters, each with their own interpretation and uses. It's time to put your skills, both the programming and the interpretation, to the test!

Your client has provided you with a dataset, crime_data, containing info on the crimes committed in each of the 50 US states and the percentage of urban population (Source: Edureka). He'd like you to group the states in 4 clusters. He didn't specify which similarity to use, but the euclidean distance seems acceptable, don't you agree?

You decide to try out two techniques: k-means and single-linkage hierarchical clustering. You then want to compare the results by calculating the Dunn's indices to make a conclusion. Which clustering will you deliver to your client?
> # Set random seed. Don't remove this line.
> set.seed(1)
> 
> # Scale the dataset: crime_data_sc
> crime_data_sc<-scale(crime_data)
> 
> # Perform k-means clustering: crime_km
> crime_km<-kmeans(crime_data_sc,centers=4,nstart=20)
> 
> # Perform single-linkage hierarchical clustering
> ## Calculate the distance matrix: dist_matrix
> dist_matrix<-dist(crime_data_sc)
> 
> ## Calculate the clusters using hclust(): crime_single
> crime_single<-hclust(dist_matrix,method = "single")
> 
> ## Cut the clusters using cutree: memb_single
> memb_single<-cutree(crime_single,k=4)
> 
> # Calculate the Dunn's index for both clusterings: dunn_km, dunn_single
> dunn_km<-dunn(clusters=crime_km$cluster,Data=crime_data_sc)
> dunn_single<-dunn(clusters=memb_single,Data=crime_data_sc)
> 
> # Print out the results
> dunn_km;dunn_single
[1] 0.1604403
[1] 0.2438734
> 
