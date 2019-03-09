Homework 4: Bags, Forests, Boosts, oh my
================
Liang Lu
2/28/2019

### Load Packages

Problem 1
---------

Problem 7 from Chapter 8 in the text. To be specific, please use a sequence of `ntree` from 25 to 500 in steps of 25 and `mtry` from 3 to 9 for by 1.

``` r
set.seed(1234)
df <- tbl_df(Boston)

for (k in 1:20){
  inTraining <- createDataPartition(df$medv, p = .75, list = F)
  training <- df[inTraining, ]
  testing <- df[-inTraining, ]
  mtry <- c(3:9)
  ntree <- seq(25, 500, len = 20)
  results <- tibble(trial = rep(NA, 140),
  mtry = rep(NA, 140),
  ntree = rep(NA, 140),
  mse = rep(NA, 140)) 
  for(i in 1:7){
    cat(sprintf('Trial: %s, mtry: %s --- %s\n', k, mtry[i], Sys.time()))
    for(j in 1:20){ 
      rf_train <- randomForest(medv ~ .,
                               data = training,
                               mtry = mtry[i],
                               ntree = ntree[j])
      mse <- mean((predict(rf_train, newdata = testing) - testing$medv)^2)
      results[(i-1)*20 + j, ] <- c(k, mtry[i], ntree[j], mse)
    }
  }
  if(exists("results_total")){
  results_total <- bind_rows(results_total, results)
  }
  else(
  results_total <- results
  )
}
```

    Trial: 1, mtry: 3 --- 2019-03-09 13:49:04
    Trial: 1, mtry: 4 --- 2019-03-09 13:49:09
    Trial: 1, mtry: 5 --- 2019-03-09 13:49:15
    Trial: 1, mtry: 6 --- 2019-03-09 13:49:21
    Trial: 1, mtry: 7 --- 2019-03-09 13:49:29
    Trial: 1, mtry: 8 --- 2019-03-09 13:49:37
    Trial: 1, mtry: 9 --- 2019-03-09 13:49:47
    Trial: 2, mtry: 3 --- 2019-03-09 13:49:57
    Trial: 2, mtry: 4 --- 2019-03-09 13:50:02
    Trial: 2, mtry: 5 --- 2019-03-09 13:50:07
    Trial: 2, mtry: 6 --- 2019-03-09 13:50:14
    Trial: 2, mtry: 7 --- 2019-03-09 13:50:22
    Trial: 2, mtry: 8 --- 2019-03-09 13:50:30
    Trial: 2, mtry: 9 --- 2019-03-09 13:50:39
    Trial: 3, mtry: 3 --- 2019-03-09 13:50:49
    Trial: 3, mtry: 4 --- 2019-03-09 13:50:54
    Trial: 3, mtry: 5 --- 2019-03-09 13:51:00
    Trial: 3, mtry: 6 --- 2019-03-09 13:51:06
    Trial: 3, mtry: 7 --- 2019-03-09 13:51:14
    Trial: 3, mtry: 8 --- 2019-03-09 13:51:22
    Trial: 3, mtry: 9 --- 2019-03-09 13:51:31
    Trial: 4, mtry: 3 --- 2019-03-09 13:51:41
    Trial: 4, mtry: 4 --- 2019-03-09 13:51:46
    Trial: 4, mtry: 5 --- 2019-03-09 13:51:52
    Trial: 4, mtry: 6 --- 2019-03-09 13:51:58
    Trial: 4, mtry: 7 --- 2019-03-09 13:52:06
    Trial: 4, mtry: 8 --- 2019-03-09 13:52:15
    Trial: 4, mtry: 9 --- 2019-03-09 13:52:24
    Trial: 5, mtry: 3 --- 2019-03-09 13:52:34
    Trial: 5, mtry: 4 --- 2019-03-09 13:52:39
    Trial: 5, mtry: 5 --- 2019-03-09 13:52:45
    Trial: 5, mtry: 6 --- 2019-03-09 13:52:51
    Trial: 5, mtry: 7 --- 2019-03-09 13:52:59
    Trial: 5, mtry: 8 --- 2019-03-09 13:53:07
    Trial: 5, mtry: 9 --- 2019-03-09 13:53:16
    Trial: 6, mtry: 3 --- 2019-03-09 13:53:27
    Trial: 6, mtry: 4 --- 2019-03-09 13:53:31
    Trial: 6, mtry: 5 --- 2019-03-09 13:53:37
    Trial: 6, mtry: 6 --- 2019-03-09 13:53:43
    Trial: 6, mtry: 7 --- 2019-03-09 13:53:51
    Trial: 6, mtry: 8 --- 2019-03-09 13:53:59
    Trial: 6, mtry: 9 --- 2019-03-09 13:54:09
    Trial: 7, mtry: 3 --- 2019-03-09 13:54:19
    Trial: 7, mtry: 4 --- 2019-03-09 13:54:24
    Trial: 7, mtry: 5 --- 2019-03-09 13:54:29
    Trial: 7, mtry: 6 --- 2019-03-09 13:54:36
    Trial: 7, mtry: 7 --- 2019-03-09 13:54:43
    Trial: 7, mtry: 8 --- 2019-03-09 13:54:52
    Trial: 7, mtry: 9 --- 2019-03-09 13:55:01
    Trial: 8, mtry: 3 --- 2019-03-09 13:55:11
    Trial: 8, mtry: 4 --- 2019-03-09 13:55:16
    Trial: 8, mtry: 5 --- 2019-03-09 13:55:22
    Trial: 8, mtry: 6 --- 2019-03-09 13:55:28
    Trial: 8, mtry: 7 --- 2019-03-09 13:55:36
    Trial: 8, mtry: 8 --- 2019-03-09 13:55:44
    Trial: 8, mtry: 9 --- 2019-03-09 13:55:54
    Trial: 9, mtry: 3 --- 2019-03-09 13:56:04
    Trial: 9, mtry: 4 --- 2019-03-09 13:56:09
    Trial: 9, mtry: 5 --- 2019-03-09 13:56:14
    Trial: 9, mtry: 6 --- 2019-03-09 13:56:21
    Trial: 9, mtry: 7 --- 2019-03-09 13:56:28
    Trial: 9, mtry: 8 --- 2019-03-09 13:56:36
    Trial: 9, mtry: 9 --- 2019-03-09 13:56:45
    Trial: 10, mtry: 3 --- 2019-03-09 13:56:55
    Trial: 10, mtry: 4 --- 2019-03-09 13:57:00
    Trial: 10, mtry: 5 --- 2019-03-09 13:57:06
    Trial: 10, mtry: 6 --- 2019-03-09 13:57:12
    Trial: 10, mtry: 7 --- 2019-03-09 13:57:20
    Trial: 10, mtry: 8 --- 2019-03-09 13:57:28
    Trial: 10, mtry: 9 --- 2019-03-09 13:57:38
    Trial: 11, mtry: 3 --- 2019-03-09 13:57:48
    Trial: 11, mtry: 4 --- 2019-03-09 13:57:52
    Trial: 11, mtry: 5 --- 2019-03-09 13:57:58
    Trial: 11, mtry: 6 --- 2019-03-09 13:58:05
    Trial: 11, mtry: 7 --- 2019-03-09 13:58:12
    Trial: 11, mtry: 8 --- 2019-03-09 13:58:21
    Trial: 11, mtry: 9 --- 2019-03-09 13:58:31
    Trial: 12, mtry: 3 --- 2019-03-09 13:58:41
    Trial: 12, mtry: 4 --- 2019-03-09 13:58:46
    Trial: 12, mtry: 5 --- 2019-03-09 13:58:51
    Trial: 12, mtry: 6 --- 2019-03-09 13:58:58
    Trial: 12, mtry: 7 --- 2019-03-09 13:59:05
    Trial: 12, mtry: 8 --- 2019-03-09 13:59:14
    Trial: 12, mtry: 9 --- 2019-03-09 13:59:23
    Trial: 13, mtry: 3 --- 2019-03-09 13:59:33
    Trial: 13, mtry: 4 --- 2019-03-09 13:59:38
    Trial: 13, mtry: 5 --- 2019-03-09 13:59:44
    Trial: 13, mtry: 6 --- 2019-03-09 13:59:50
    Trial: 13, mtry: 7 --- 2019-03-09 13:59:57
    Trial: 13, mtry: 8 --- 2019-03-09 14:00:06
    Trial: 13, mtry: 9 --- 2019-03-09 14:00:15
    Trial: 14, mtry: 3 --- 2019-03-09 14:00:25
    Trial: 14, mtry: 4 --- 2019-03-09 14:00:30
    Trial: 14, mtry: 5 --- 2019-03-09 14:00:35
    Trial: 14, mtry: 6 --- 2019-03-09 14:00:42
    Trial: 14, mtry: 7 --- 2019-03-09 14:00:49
    Trial: 14, mtry: 8 --- 2019-03-09 14:00:58
    Trial: 14, mtry: 9 --- 2019-03-09 14:01:07
    Trial: 15, mtry: 3 --- 2019-03-09 14:01:17
    Trial: 15, mtry: 4 --- 2019-03-09 14:01:22
    Trial: 15, mtry: 5 --- 2019-03-09 14:01:28
    Trial: 15, mtry: 6 --- 2019-03-09 14:01:34
    Trial: 15, mtry: 7 --- 2019-03-09 14:01:41
    Trial: 15, mtry: 8 --- 2019-03-09 14:01:50
    Trial: 15, mtry: 9 --- 2019-03-09 14:01:59
    Trial: 16, mtry: 3 --- 2019-03-09 14:02:09
    Trial: 16, mtry: 4 --- 2019-03-09 14:02:14
    Trial: 16, mtry: 5 --- 2019-03-09 14:02:19
    Trial: 16, mtry: 6 --- 2019-03-09 14:02:26
    Trial: 16, mtry: 7 --- 2019-03-09 14:02:34
    Trial: 16, mtry: 8 --- 2019-03-09 14:02:42
    Trial: 16, mtry: 9 --- 2019-03-09 14:02:52
    Trial: 17, mtry: 3 --- 2019-03-09 14:03:02
    Trial: 17, mtry: 4 --- 2019-03-09 14:03:07
    Trial: 17, mtry: 5 --- 2019-03-09 14:03:12
    Trial: 17, mtry: 6 --- 2019-03-09 14:03:19
    Trial: 17, mtry: 7 --- 2019-03-09 14:03:26
    Trial: 17, mtry: 8 --- 2019-03-09 14:03:35
    Trial: 17, mtry: 9 --- 2019-03-09 14:03:44
    Trial: 18, mtry: 3 --- 2019-03-09 14:03:54
    Trial: 18, mtry: 4 --- 2019-03-09 14:03:59
    Trial: 18, mtry: 5 --- 2019-03-09 14:04:04
    Trial: 18, mtry: 6 --- 2019-03-09 14:04:11
    Trial: 18, mtry: 7 --- 2019-03-09 14:04:18
    Trial: 18, mtry: 8 --- 2019-03-09 14:04:27
    Trial: 18, mtry: 9 --- 2019-03-09 14:04:36
    Trial: 19, mtry: 3 --- 2019-03-09 14:04:47
    Trial: 19, mtry: 4 --- 2019-03-09 14:04:52
    Trial: 19, mtry: 5 --- 2019-03-09 14:04:57
    Trial: 19, mtry: 6 --- 2019-03-09 14:05:04
    Trial: 19, mtry: 7 --- 2019-03-09 14:05:11
    Trial: 19, mtry: 8 --- 2019-03-09 14:05:19
    Trial: 19, mtry: 9 --- 2019-03-09 14:05:29
    Trial: 20, mtry: 3 --- 2019-03-09 14:05:39
    Trial: 20, mtry: 4 --- 2019-03-09 14:05:44
    Trial: 20, mtry: 5 --- 2019-03-09 14:05:49
    Trial: 20, mtry: 6 --- 2019-03-09 14:05:56
    Trial: 20, mtry: 7 --- 2019-03-09 14:06:03
    Trial: 20, mtry: 8 --- 2019-03-09 14:06:12
    Trial: 20, mtry: 9 --- 2019-03-09 14:06:21

``` r
results_total$mtry <- as.factor(results_total$mtry)
p <- ggplot(results, aes(x=ntree, y=mse, group=mtry, colour=mtry))
p + geom_line()
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

Problem 2
---------

Problem 8 from Chapter 8 in the text. Set your seed with 9823 and split into train/test using 50% of your data in each split. In addition to parts (a) - (e), do the following

``` r
set.seed(9823)
df <- tbl_df(Carseats)
```

1.  Split the data into testing and training datasets.

``` r
inTraining <- createDataPartition(df$Sales, p = .5, list = F)
training <- df[inTraining, ]
testing  <- df[-inTraining, ]
```

1.  Fit a regression tree to the training dataset.

``` r
sales_tree <- rpart::rpart(Sales ~ ., 
                      data = training,
                      control = rpart.control(minsplit = 50))
sales_tree
```

    n= 201 

    node), split, n, deviance, yval
          * denotes terminal node

     1) root 201 1493.06900  7.465721  
       2) ShelveLoc=Bad,Medium 162  931.48560  6.822222  
         4) ShelveLoc=Bad 49  240.27190  5.210204 *
         5) ShelveLoc=Medium 113  508.66760  7.521239  
          10) Price>=105.5 73  240.23530  6.706301  
            20) Advertising< 11.5 54  140.78540  6.281667  
              40) CompPrice< 121.5 18   15.18209  5.420556 *
              41) CompPrice>=121.5 36  105.58240  6.712222 *
            21) Advertising>=11.5 19   62.03941  7.913158 *
          11) Price< 105.5 40  131.47350  9.008500 *
       3) ShelveLoc=Good 39  215.84900 10.138720 *

``` r
prp(sales_tree)
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
plot(as.party(sales_tree))
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
test_preds <- predict(sales_tree, newdata = testing)
sales_test_df <- testing %>%
  mutate(y_hat = test_preds,
         sq_err = (y_hat - Sales)^2)
mean(sales_test_df$sq_err)
```

    [1] 5.428695

``` r
sqrt(mean(sales_test_df$sq_err))
```

    [1] 2.329956

1.  use cross-validation

``` r
set.seed(1982)

rf_sales_cv <- train(Sales ~ ., 
                      data = training,
                      method = "rf",
                      ntree = 100,
                      importance = T,
                      tuneGrid = data.frame(mtry = 1:11))
rf_sales_cv
```

    Random Forest 

    201 samples
     10 predictor

    No pre-processing
    Resampling: Bootstrapped (25 reps) 
    Summary of sample sizes: 201, 201, 201, 201, 201, 201, ... 
    Resampling results across tuning parameters:

      mtry  RMSE      Rsquared   MAE     
       1    2.327522  0.4835063  1.880982
       2    2.064376  0.5825965  1.655484
       3    1.957037  0.6000690  1.553626
       4    1.892942  0.6165645  1.503834
       5    1.856871  0.6170286  1.469395
       6    1.840347  0.6177505  1.461519
       7    1.838569  0.6110534  1.457821
       8    1.836504  0.6068632  1.458830
       9    1.836190  0.6048121  1.451467
      10    1.841215  0.5962499  1.460827
      11    1.844077  0.5941374  1.460404

    RMSE was used to select the optimal model using the smallest value.
    The final value used for the model was mtry = 9.

``` r
plot(rf_sales_cv)
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
p <- ggplot(data = rf_sales_cv$results,
            aes(x = mtry, y = RMSE))
p + geom_point() +
  geom_line()
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
rf_sales_9 <- randomForest(Sales ~ ., 
                            data = training,
                            mtry = 9)
rf_sales_9
```


    Call:
     randomForest(formula = Sales ~ ., data = training, mtry = 9) 
                   Type of random forest: regression
                         Number of trees: 500
    No. of variables tried at each split: 9

              Mean of squared residuals: 2.812059
                        % Var explained: 62.14

And the test MSE is:

``` r
test_preds <- predict(rf_sales_9, newdata = testing)
sales_test_df <- sales_test_df %>%
  mutate(y_hat_rf_9 = test_preds,
         sq_err_rf_9 = (y_hat_rf_9 - Sales)^2)
mean(sales_test_df$sq_err_rf_9)
```

    [1] 3.043777

The test MSE droped from 5.43 to 3.04.

1.  Use bagging to analyze the data.

``` r
set.seed(10982)
bag_sales <- randomForest(Sales ~ ., data = training, mtry = 9)
bag_sales
```


    Call:
     randomForest(formula = Sales ~ ., data = training, mtry = 9) 
                   Type of random forest: regression
                         Number of trees: 500
    No. of variables tried at each split: 9

              Mean of squared residuals: 2.847362
                        % Var explained: 61.67

Compute the test MSE:

``` r
test_preds <- predict(bag_sales, newdata = testing)
sales_test_df <- testing %>%
  mutate(y_hat_bags = test_preds,
         sq_err_bags = (y_hat_bags - Sales)^2)
mean(sales_test_df$sq_err_bags)
```

    [1] 3.036256

``` r
imp <- varImp(rf_sales_cv)$importance
rn <- row.names(imp)
imp_df <- data_frame(variable = rn, 
                     importance = imp$Overall) %>%
  arrange(desc(-importance)) %>%
  mutate(variable = factor(variable, variable))
p <- ggplot(data = imp_df,
            aes(variable, importance))
p + geom_col(fill = "#6e0000") +
  coord_flip()
```

<img src="homework-4_files/figure-markdown_github/unnamed-chunk-17-1.png" style="display: block; margin: auto;" /> ShelveLocGood is the most important factor and price the next.

1.  Fit a gradient-boosted tree to the training data and report the estimated test MSE.

``` r
set.seed(99)
grid <- expand.grid(interaction.depth = c(1, 3), 
                    n.trees = seq(0, 2000, by = 100),
                    shrinkage = c(.01, 0.001),
                    n.minobsinnode = 10)
trainControl <- trainControl(method = "cv", number = 5)
gbm_boston <- train(Sales ~ ., 
                    data = training, 
                    distribution = "gaussian", 
                    method = "gbm",
                    trControl = trainControl, 
                    tuneGrid = grid,
                    verbose = FALSE)
gbm_boston
```

    Stochastic Gradient Boosting 

    201 samples
     10 predictor

    No pre-processing
    Resampling: Cross-Validated (5 fold) 
    Summary of sample sizes: 161, 161, 160, 161, 161 
    Resampling results across tuning parameters:

      shrinkage  interaction.depth  n.trees  RMSE      Rsquared   MAE     
      0.001      1                     0     2.720901        NaN  2.200036
      0.001      1                   100     2.668562  0.3119799  2.160981
      0.001      1                   200     2.623310  0.3349800  2.127949
      0.001      1                   300     2.581385  0.3523965  2.098310
      0.001      1                   400     2.545754  0.3666920  2.070786
      0.001      1                   500     2.512543  0.3809969  2.044273
      0.001      1                   600     2.481666  0.3939578  2.018036
      0.001      1                   700     2.452927  0.4088655  1.991425
      0.001      1                   800     2.424873  0.4152267  1.967778
      0.001      1                   900     2.398995  0.4237763  1.945800
      0.001      1                  1000     2.375672  0.4330498  1.925372
      0.001      1                  1100     2.353309  0.4388542  1.906005
      0.001      1                  1200     2.332440  0.4465898  1.888161
      0.001      1                  1300     2.312408  0.4536939  1.870766
      0.001      1                  1400     2.292222  0.4628786  1.852540
      0.001      1                  1500     2.273802  0.4697011  1.836495
      0.001      1                  1600     2.254344  0.4787806  1.819180
      0.001      1                  1700     2.236924  0.4871407  1.803926
      0.001      1                  1800     2.219330  0.4937408  1.788802
      0.001      1                  1900     2.203591  0.5003083  1.775720
      0.001      1                  2000     2.186885  0.5074642  1.761472
      0.001      3                     0     2.720901        NaN  2.200036
      0.001      3                   100     2.627912  0.4930030  2.125919
      0.001      3                   200     2.545606  0.5231462  2.062052
      0.001      3                   300     2.472038  0.5402485  2.008076
      0.001      3                   400     2.404860  0.5582945  1.955853
      0.001      3                   500     2.344762  0.5709705  1.908088
      0.001      3                   600     2.291123  0.5857111  1.863954
      0.001      3                   700     2.240250  0.5974413  1.823374
      0.001      3                   800     2.193501  0.6093420  1.784524
      0.001      3                   900     2.151463  0.6210834  1.748863
      0.001      3                  1000     2.110661  0.6302962  1.713870
      0.001      3                  1100     2.072971  0.6390587  1.681379
      0.001      3                  1200     2.038592  0.6478095  1.651424
      0.001      3                  1300     2.004567  0.6562643  1.621982
      0.001      3                  1400     1.972555  0.6633622  1.594111
      0.001      3                  1500     1.941382  0.6697797  1.567538
      0.001      3                  1600     1.912028  0.6757876  1.542360
      0.001      3                  1700     1.884786  0.6813646  1.519568
      0.001      3                  1800     1.858085  0.6867272  1.496456
      0.001      3                  1900     1.832781  0.6924661  1.475548
      0.001      3                  2000     1.808477  0.6968629  1.455993
      0.010      1                     0     2.720901        NaN  2.200036
      0.010      1                   100     2.374804  0.4466603  1.924292
      0.010      1                   200     2.181338  0.5071241  1.755472
      0.010      1                   300     2.041953  0.5755361  1.641019
      0.010      1                   400     1.930099  0.6200132  1.550259
      0.010      1                   500     1.833595  0.6526090  1.478075
      0.010      1                   600     1.757034  0.6766546  1.420474
      0.010      1                   700     1.689164  0.6953642  1.370305
      0.010      1                   800     1.627573  0.7126489  1.319968
      0.010      1                   900     1.579361  0.7229535  1.282176
      0.010      1                  1000     1.534387  0.7332620  1.246525
      0.010      1                  1100     1.501189  0.7400117  1.222383
      0.010      1                  1200     1.473748  0.7446795  1.199906
      0.010      1                  1300     1.448553  0.7486352  1.179520
      0.010      1                  1400     1.427970  0.7520601  1.164064
      0.010      1                  1500     1.415243  0.7540405  1.154835
      0.010      1                  1600     1.405889  0.7546474  1.146331
      0.010      1                  1700     1.395891  0.7558224  1.137152
      0.010      1                  1800     1.385397  0.7579965  1.128786
      0.010      1                  1900     1.380008  0.7588813  1.123819
      0.010      1                  2000     1.377614  0.7586327  1.122812
      0.010      3                     0     2.720901        NaN  2.200036
      0.010      3                   100     2.110003  0.6197359  1.714046
      0.010      3                   200     1.807931  0.6927165  1.453151
      0.010      3                   300     1.635790  0.7226935  1.315696
      0.010      3                   400     1.528886  0.7403189  1.236027
      0.010      3                   500     1.464104  0.7479038  1.190877
      0.010      3                   600     1.425665  0.7528153  1.164063
      0.010      3                   700     1.401137  0.7564725  1.145385
      0.010      3                   800     1.386705  0.7584579  1.130402
      0.010      3                   900     1.380775  0.7592703  1.122429
      0.010      3                  1000     1.374289  0.7603748  1.113818
      0.010      3                  1100     1.369702  0.7616736  1.110900
      0.010      3                  1200     1.366161  0.7622422  1.109732
      0.010      3                  1300     1.366409  0.7621843  1.109369
      0.010      3                  1400     1.363737  0.7629975  1.106400
      0.010      3                  1500     1.359933  0.7644042  1.103096
      0.010      3                  1600     1.359682  0.7637238  1.101714
      0.010      3                  1700     1.364285  0.7622454  1.105059
      0.010      3                  1800     1.365983  0.7619572  1.106019
      0.010      3                  1900     1.364915  0.7623998  1.105743
      0.010      3                  2000     1.365814  0.7618629  1.106612

    Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    RMSE was used to select the optimal model using the smallest value.
    The final values used for the model were n.trees = 1600,
     interaction.depth = 3, shrinkage = 0.01 and n.minobsinnode = 10.

``` r
test_preds <- predict(gbm_boston, newdata = testing)
sales_test_df <- sales_test_df %>%
  mutate(y_hat_gbm = test_preds,
         sq_err_gbm = (y_hat_gbm - Sales)^2)
mean(sales_test_df$sq_err_gbm)
```

    [1] 1.805444

1.  Fit a multiple regression model to the training data and report the estimated test MSE

``` r
sales_lm <- lm(Sales ~ ., data = df)
summary(sales_lm)
```


    Call:
    lm(formula = Sales ~ ., data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.8692 -0.6908  0.0211  0.6636  3.4115 

    Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
    (Intercept)      5.6606231  0.6034487   9.380  < 2e-16 ***
    CompPrice        0.0928153  0.0041477  22.378  < 2e-16 ***
    Income           0.0158028  0.0018451   8.565 2.58e-16 ***
    Advertising      0.1230951  0.0111237  11.066  < 2e-16 ***
    Population       0.0002079  0.0003705   0.561    0.575    
    Price           -0.0953579  0.0026711 -35.700  < 2e-16 ***
    ShelveLocGood    4.8501827  0.1531100  31.678  < 2e-16 ***
    ShelveLocMedium  1.9567148  0.1261056  15.516  < 2e-16 ***
    Age             -0.0460452  0.0031817 -14.472  < 2e-16 ***
    Education       -0.0211018  0.0197205  -1.070    0.285    
    UrbanYes         0.1228864  0.1129761   1.088    0.277    
    USYes           -0.1840928  0.1498423  -1.229    0.220    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 1.019 on 388 degrees of freedom
    Multiple R-squared:  0.8734,    Adjusted R-squared:  0.8698 
    F-statistic: 243.4 on 11 and 388 DF,  p-value: < 2.2e-16

``` r
test_preds <- predict(sales_lm, newdata = testing)
sales_test_df <- sales_test_df %>%
  mutate(y_hat_gbm = test_preds,
         sq_err_gbm = (y_hat_gbm - Sales)^2)
mean(sales_test_df$sq_err_gbm)
```

    [1] 0.9635997

1.  Summarize your results.
