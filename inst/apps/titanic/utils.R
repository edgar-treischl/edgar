#library(ggplot2)
#library(titanic)
#library(ggbeeswarm)
#library(viridis)
#library(thematic)
#library(broom)
#library(tidyverse)
#library(ggalluvial)
#library(caret)
#library(ggmosaic)
#library(precrec)

#Data Prep#############
train_df<- titanic::titanic_train

train_df$Survived <- factor(train_df$Survived, 
                            levels = c(0, 1),
                            labels = c("Not survived", "Survived")) 

train_df$Pclass <- factor(train_df$Pclass, 
                          levels = c(1, 2 , 3),
                          labels = c("First class", "Second class", "Third class")) 

glm_fit <- glm(Survived ~ Sex + Age + Pclass , family = binomial(link = 'logit'), data = train_df)




#Titanic data prep for alluvial#######
tita <- titanic::titanic_train


tita <- tita |> 
  dplyr::select(Age, Sex, Survived, Pclass) |> 
  dplyr::mutate(
    Adults = dplyr::case_when(
      Age >= 0 & Age < 14    ~ "Kids",
      Age >= 14 & Age < 100   ~ "Adult",
      TRUE                   ~ "NA")
  ) |> 
  dplyr::mutate(Adults = dplyr::na_if(Adults, "NA"))



tita<- tita |> 
  dplyr::group_by(Sex, Survived, Pclass, Adults) |> 
  tidyr::drop_na() |> 
  dplyr::count(Sex) 


tita$Survived <- factor(tita$Survived, 
                        levels = c(0, 1),
                        labels = c("Not survived", "Survived")) 

tita$Pclass <- factor(tita$Pclass, 
                      levels = c(1, 2 , 3),
                      labels = c("1. Class", "2. Class", "3. class"))

library(ggalluvial)

make_alluvial <- function(var1, var2 = NULL, var3 = NULL) {
  
  ggplot2::ggplot(data = tita,
                  ggplot2::aes(axis1 = {{var1}}, 
                               axis2 = {{var2}},
                               axis3 = {{var3}},
                               y = n)) +
    ggplot2::scale_x_discrete(limits = c("Sex", "Adults", "Class"), 
                              expand = c(.2, .05)) +
    ggplot2::xlab("Group") +
    ggalluvial::geom_alluvium(aes(fill = Survived), alpha = 0.75) +
    ggalluvial::geom_stratum() +
    ggplot2::geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
                       size = rel(5)) +
    ggplot2::theme_minimal(base_size = 16)+
    ggplot2::scale_fill_manual(values = c("#009E73","#E69F00"))+
    ggplot2::theme(legend.position="bottom")
  
}

#make_alluvial(Sex, Adults, Pclass)


#plots
# all_adults <- ggplot2::ggplot(data = tita,
#                               ggplot2::aes(axis1 = Sex, axis2 = Adults,
#            y = n)) +
#   ggplot2::scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
#   ggplot2::xlab("Group") +
#   ggalluvial::geom_alluvium(aes(fill = Survived), alpha = 0.75) +
#   ggalluvial::geom_stratum() +
#   ggplot2::geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
#   ggplot2::theme_minimal(base_size = 16)+
#   ggplot2::scale_fill_manual(values = c("#009E73","#E69F00"))+
#   ggplot2::theme(legend.position="bottom")


#Calculate Model#######

# require(stats)
# centre <- function(x, type) {
#   switch(type,
#          mean = mean(x),
#          median = median(x),
#          trimmed = mean(x, trim = .1))
# }
# x <- rcauchy(10)
# centre(x, "mean")
# centre(x, "median")
# centre(x, "trimmed")

model_call <- function(type) {
  switch(type,
         m1 = glm(Survived ~ Sex , family = binomial(link = 'logit'), data = train_df),
         m2 = glm(Survived ~ Pclass , family = binomial(link = 'logit'), data = train_df),
         m3 = glm(Survived ~ Age , family = binomial(link = 'logit'), data = train_df),
         m4 = glm(Survived ~ Sex + Pclass , family = binomial(link = 'logit'), data = train_df),
         m5 = glm(Survived ~ Sex + Pclass + Age, family = binomial(link = 'logit'), data = train_df),
         m6 = glm(Survived ~ Sex + Age , family = binomial(link = 'logit'), data = train_df),
         m7 = glm(Survived ~ Pclass + Age , family = binomial(link = 'logit'), data = train_df)
         )
}


plotOR <- function(model) {
  broom::tidy(model_call(model)) |> 
    dplyr::mutate(oddsRatio = exp(estimate)) |> 
    dplyr::select(term, estimate, oddsRatio) |> 
    dplyr::mutate(term=replace(term, term=="(Intercept)", NA)) |>  
    tidyr::drop_na() |> 
    ggplot2::ggplot(ggplot2::aes(x = term, y= oddsRatio)) + 
    ggplot2::geom_bar(stat="identity")+
    ggplot2::geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
    ggplot2::ylab("Odds Ratio")+
    ggplot2::xlab("Coefficient")+
    ggplot2::theme_bw(base_size = 16)+
    ggplot2::expand_limits(y=c(0,2))+
    ggplot2::scale_color_manual(values = c("#2C3E50"))
  
}

#plotOR("m2")


#OR Barplots Vorlage

# or1 <- tidy(model_call("m1"))%>% 
#   mutate(oddsRatio = exp(estimate))%>%
#   select(term, estimate, oddsRatio)%>%
#   mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
#   drop_na()%>% 
#   ggplot2::ggplot(ggplot2::aes(x = term, y= oddsRatio)) + 
#   ggplot2::geom_bar(stat="identity")+
#   ggplot2::geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
#   ggplot2::ylab("Odds Ratio")+
#   ggplot2::xlab("Coefficient")+
#   ggplot2::theme_bw(base_size = 16)+
#   ggplot2::expand_limits(y=c(0,2))+
#   ggplot2::scale_color_manual(values = c("#2C3E50"))






#Prediction plots: Sensitivity##########
glm_fit <- glm(Survived ~ Sex + Age + as.numeric(Pclass) , family = binomial(link = 'logit'), data = train_df)

pred_surv <- (predict(glm_fit, train_df, type = 'response') > 0.5) * 1

#diese Prognose speichern wir zusammen mit der PassengerID
df_pred <- data.frame('PassengerId' = train_df$PassengerId, 'True' =  train_df$Survived, 
                      'Predicted' = pred_surv) |> tidyr::drop_na()


df_pred$Predicted <- factor(df_pred$Predicted, 
                            levels = c(0, 1),
                            labels = c("Not survived", "Survived")) 

cmMatrix<- caret::confusionMatrix(df_pred$Predicted, df_pred$True,  positive = "Survived")



label_df <- df_pred |> 
  dplyr::group_by(True, Predicted) |> 
  dplyr::count() |> 
  dplyr::arrange(True)

fcukit <- data.frame(
  x = c(0.32, 0.32, 0.82, 0.82),
  y = c(0.1, 0.9, 0.1, 0.9),
  text = label_df$n,
  text2 = c("True negative", "False positive", "False negative", "True positive")
)



sample_size <- df_pred |> dplyr::group_by(True) |>  dplyr::summarize(num=dplyr::n())

df_pred <- df_pred |> 
  dplyr::left_join(sample_size) |> 
  dplyr::mutate(True = paste0(True, "\n", "n=", num))


library(ggmosaic)


#spineplot(df_pred$Predicted ~ df_pred$True)

#head(df_pred)

pred_plot <- ggplot2::ggplot(df_pred) + 
  ggmosaic::geom_mosaic(ggplot2::aes(x = product(True, Predicted), 
                                     fill = Predicted), 
                        alpha = 1)+
  ggplot2::scale_fill_manual(values=c("#E69F00","#009E73"))+
  ggplot2::geom_label(data = fcukit, 
             aes(x = x, y = y, 
                 label = paste0(text2, ":", text)),
             size = 5)+
  ggplot2::ylab("Predicted")+
  ggplot2::xlab("Observed")+
  ggplot2::theme_minimal(base_size = 18)+
  ggplot2::labs(caption = paste0("Sensitivity:", round(cmMatrix$byClass[1], 2),
                        ", Specificity:", round(cmMatrix$byClass[2], 2)))+
  ggplot2::theme(plot.caption = element_text(color = "black",
                                    size = 18,
                                    hjust = 0,
                                    face = "bold"))+
  ggplot2::theme(legend.position = "none")


#ROC###############

pred_roc <- (predict(glm_fit, train_df, type = 'response') > 0.5) * 1
df_roc <- data.frame('Survived' = train_df$Survived, 'predict' =  pred_roc)
precrec_obj <- precrec::evalmod(scores = df_roc$predict, labels = df_roc$Survived)

p <-  ggplot2::autoplot(precrec_obj, "ROC")

roc_curve <-  p +
  ggplot2::theme_minimal(base_size = 18)+
  ggplot2::geom_line(size = 1, color="#E69F00")+
  ggplot2::geom_vline(xintercept = 0, size=1, color="black") + 
  ggplot2::geom_hline(yintercept = 1, size=1, color="black") + 
  ggplot2::geom_abline(size = 0.8, linetype = "dashed", color="darkgray")+
  ggplot2::theme(legend.position = "none")+
  ggplot2::ggtitle("")



