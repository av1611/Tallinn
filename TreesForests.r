#The dataset is called tv. You will need to install the packages party and Hmisc first.

library(party)

#Section 3.5. Grow an individual tree.

tv.cit <- ctree(Form ~ ., data = tv) 
#equivalent to: tv.cit <- ctree(Form ~ ., data = tv, controls = ctree_control(teststat = "quad", testtype = "Bonferroni", mincriterion = 0.95, minsplit = 20, minbucket = 7)) 

#Figure 5
plot(tv.cit)  

#Figure 6
plot(tv.cit, inner_panel = node_barplot)

#Obtain the proportions of ty and vy in a given final node: example
prop.table(table(tv$Form[tv$Rel_Circle %in% c("Fri", "Fam", "Rom")]))

#       ty        vy 
#0.8888889 0.1111111 

prop.table(table(tv$Form[tv.rus$Rel_Circle %in% c("Str", "Acq", "Work")&tv.rus$H_Age %in% c("Middle", "Old")]))

#       ty        vy 
#0.2136752 0.7863248 

#Evaluate the goodness of fit of the tree.

#1. Compute the classification accuracy:

pred.cit <- predict(tv.cit)

table(pred.cit, tv$Form[complete.cases(tv)])

#pred.ctree  ty  vy
#        ty  75  15
#        vy  33 105

(75 + 105)/228
#[1] 0.7894737

#2. Compute the C-index:

prob.cit <- unlist(predict(tv.cit, type = "prob"))[c(FALSE, TRUE)]
library(Hmisc)
somers2(prob.cit, as.numeric(tv$Form) - 1)
#          C         Dxy           n     Missing 
#  0.8092593   0.6185185 228.0000000   0.0000000 

#Section 3.6. Grow a conditional random forest

set.seed(2450)#if you want to reproduce the results shown in the chapter
#tv.crf <- cforest(Form ~., data = tv, controls = cforest_unbiased(mtry = 4, ntree = 1000))
tv.crf1 <- cforest(Form ~., data = tv, controls = cforest_control(mtry = 4, ntree = 1000))

accvalues_max_teststat_replace <- numeric()
for (i in 1:16){
  set.seed(51)
  crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = TRUE, teststat = "max", testtype = "Teststatistic", mincriterion = 0.9))  
pred.oob <- predict(crf, OOB = TRUE)
crosstab <- table(pred.oob, tv$Form)
accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
accvalues[i] <- accuracy
print (i)
}

accvalues_quad_teststat_replace <- numeric()
for (i in 1:16){
  set.seed(70)
  crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = TRUE, teststat = "quad", testtype = "Teststatistic", mincriterion = 0.9))  
  pred.oob <- predict(crf, OOB = TRUE)
  crosstab <- table(pred.oob, tv$Form)
  accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
  accvalues_quad_teststat_replace[i] <- accuracy
  print (i)
}


accvalues_max_teststat_noreplace <- numeric()
for (i in 1:16){
  set.seed(89)
  crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = FALSE, teststat = "max", testtype = "Teststatistic", mincriterion = 0.9))  
  pred.oob <- predict(crf, OOB = TRUE)
  crosstab <- table(pred.oob, tv$Form)
  accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
  accvalues_max_teststat_noreplace[i] <- accuracy
  print (i)
}

accvalues_quad_teststat_noreplace <- numeric()
for (i in 1:16){
  set.seed(36)
  crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = FALSE, teststat = "quad", testtype = "Teststatistic", mincriterion = 0.9))  
  pred.oob <- predict(crf, OOB = TRUE)
  crosstab <- table(pred.oob, tv$Form)
  accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
  accvalues_quad_teststat_noreplace[i] <- accuracy
  print (i)
}

accvalues_quad_bonf_replace <- numeric()
for (i in 1:16){
    set.seed(24)
    crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = TRUE, teststat = "quad", testtype = "Bonferroni", mincriterion = 0.9))  
  pred.oob <- predict(crf, OOB = TRUE)
  crosstab <- table(pred.oob, tv$Form)
  accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
  accvalues_quad_bonf_replace[i] <- accuracy
  print (i)
}

accvalues_quad_bonf_noreplace <- numeric()
for (i in 1:16){
  set.seed(15)  
  crf <- cforest(Form ~., data = tv, controls = cforest_control(mtry = i, ntree = 1000, replace = FALSE, teststat = "quad", testtype = "Bonferroni", mincriterion = 0.9))  
  pred.oob <- predict(crf, OOB = TRUE)
  crosstab <- table(pred.oob, tv$Form)
  accuracy <- (crosstab[1,1] + crosstab[2, 2])/length(tv$Form)
  accvalues_quad_bonf_noreplace[i] <- accuracy
  print (i)
}

plot(1:16, accvalues_quad_bonf_replace, type = "l", col = "red", ylim = c(0.6, 0.85), ylab = "OOB accuracy", xlab = "mtry")
lines(1:16, accvalues_quad_bonf_noreplace, type = "l", col = "red", lty = 2)
lines(1:16, accvalues_quad_teststat_replace, type = "l", col = "blue")
lines(1:16, accvalues_quad_teststat_noreplace, type = "l", col = "blue", lty = 2)
lines(1:16, accvalues_max_teststat_replace, type = "l", col = "black")
lines(1:16, accvalues_max_teststat_noreplace, type = "l", col = "black", lty = 2)
grid()

#The function cforest_unbiased() provides the settings suggested for the construction of unbiased random forests by Strobl et al. (2007). The settings are as follows: teststat = "quad", testtype = "Univ", replace = FALSE. This means that for each individual tree that is grown for this forest, the algorithm uses the quadratic statistic for variable selection (Hothorn et al. 2006), no Bonferroni correction is made, and the random sample used for constructing an individual tree is created without replacement. This has been the default option since party 0.9-90. In order to use other settings, one should use cforest_control(), which has other default parameters (see ?cforest_control).
#The argument mtry determines the number of variables that are selected for splits in an individual tree. The default value is 5 (for some technical reasons). If mtry is equal to the number of predictors, the method is called bagging. Note that in that case, you will lose the advantage of weakening the competition for highly specialized predictors. As a result, the trees will be less diverse than if non-bagging, and the prediction will be less accurate. According to a rule of thumb, the recommended value of mtry is the square root of the number of predictors. Since we have sixteen predictors, four is an appropriate number. 
#Finally, the argument ntree tells R to grow 1,000 trees (by default, 500 are grown). 
#Other possible tuning parameters, which are not modified here, are minsplit (i.e. the minimum number of observations to be considered for splitting; 20 by default), mincriterion (1 – p-value, which must be exceeded in order to implement a split; 0.95 by default) and minbucket (the minimum number of data points in a branch after a split; 7 by default). For small samples, for example, it may be useful to reduce minsplit and decrease mincriterion (Strobl et al. 2009).

#Compute and plot the conditional variable importance scores.

set.seed(32) #if you want to reproduce the results shown in the chapter
tv.varimp <- varimp(tv.crf, conditional = T) #this may take a while, depending on the number of trees and the size of the dataset
par(mar = c(5, 6, 5, 2)) #to make sufficient space for plotting the vertical axis labels on the bar plot
barplot(sort(tv.varimp), horiz = T, las = 2, col = "gray")
abline(v = abs(min(tv.varimp)), lwd = 2, col = "blue")

#Evaluate the goodness of fit of the forest.

#1a. Compute the classification accuracy based on OOB samples:

pred.crf.oob <- predict(tv.crf, OOB = TRUE)

table(pred.crf.oob, tv$Form)

#pred.rf ty vy
#ty 79 23
#vy 29 97

(79 + 97)/228
#[1] 0.7719298 #OOB classification accuracy

       
#1b. Classification accuracy based on training samples

pred.crf.train <- predict(tv.crf, OOB = FALSE)
table(pred.crf.train, tv$Form)
#pred.rf  ty  vy
#     ty  91  11
#     vy  17 109

(91 + 109)/228
#[1] 0.877193 

#2a. Compute the C-index based on OOB samples.

prob.crf.oob <- unlist(predict(tv.crf, type = "prob", OOB = TRUE))[c(FALSE, TRUE)]
somers2(prob.crf.oob, as.numeric(tv$Form) - 1)
#C         Dxy           n     Missing 
#0.8656636   0.7313272 228.0000000   0.0000000

#2b. For comparison: compute the C-index based on training samples.

prob.crf.train <- unlist(predict(tv.crf, type = "prob"))[c(FALSE, TRUE)]
somers2(prob.crf.train, as.numeric(tv$Form) - 1)
#C         Dxy           n     Missing 
#0.9510031   0.9020062 228.0000000   0.0000000


#Section 3.7. Create partial dependence plots

library(pdp)
#Figure 8a
pdp_circle <- partial(tv.crf, "Rel_Circle", prob = TRUE)
plotPartial(pdp_circle, main = "Rel_Circle")
#Figure 8b
pdp_office <- partial(tv.crf, "Office", prob = TRUE)
plotPartial(pdp_office, main = "Office")