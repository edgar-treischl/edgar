#Confounder/Colider Functions###################
#library(tidyverse)

confounder <- function(n, r) {
  set.seed(5)
  z <- stats::rnorm(n, 1, 1)
  x <- stats::rnorm(n, 1, 1) + r*z
  y <- stats::rnorm(n, 1, 1) + r*z
  data.frame(x, y, z)
  
}

#confounder(n= 500, r = 0.2)



collider <- function(n, rx, ry) {
  set.seed(5)
  x <- stats::rnorm(n, 5, 3)
  y <- stats::rnorm(n, 10, 1) 
  #y <- rnorm(n, 10, 1) + 1*x
  collider <-  rx*x + ry*y  + stats::rnorm(n, 1, 1)
  data.frame(x, y, collider)
  
}

#df_coll <- collider(n = 1000, rx = 0.5, ry = .5)





mediator <- function(n, m, w) {
  set.seed(5)
  x <-  stats::rnorm(n, 10, 3)
  random1<- stats::runif(n, min=min(x),max=max(x))
  #Important step 1: The mediator = 0,5*x * 0,5*random variable
  mediator <- x*m+ random1*(1-m)
  
  random2<- stats::runif(n, min=min(mediator), max=max(mediator))
  #Important step 2: y = 0,5*random variable + 0,5*mediator
  y <- m*mediator + (1-m)* random2 + w*x
  
  data.frame(x, y, mediator)
  
}

#df_mediator <- mediator(100, 0.5)

mediator_new <- function(fallzahl, mediation, xeffect) {
  set.seed(5)
  x <-  stats::rnorm(fallzahl, 10, 3)
  random1<- stats::runif(fallzahl, min=min(x),max=max(x))
  #Important step 1: The mediator = 0,5*x * 0,5*random variable
  mediator <- x*mediation+ random1*(1-mediation)
  
  random2<- stats::runif(fallzahl, min=min(mediator), max=max(mediator))
  
  #Important step 2: y = 0,5*random variable + 0,5*mediator
  y <- .5*mediator + .5* random2
  
  data.frame(x, y, mediator)
  
}





#df <- mediator(n = 2000, m = .5)

#fm <- lm(y ~ x, df)
#summary(fm)

#fm <- lm(y ~ x + mediator, df)
#summary(fm)

#ggplot(data=df, aes(x=x, y=y)) +
#geom_smooth()+
# geom_point()

#Shoesexexample#######################
var_confounder <- function(n, y, sd, bonus) {
  set.seed(12334)
  sex <- stats::rbinom(n, 1, .5)
  schoesize <- stats::rnorm(n, 36, 4) + sex*stats::rnorm(n, 4, 2)
  bonus <- sex*stats::rnorm(n, bonus, sd)
  income <- stats::rnorm(n, y, sd) + sex*stats::rnorm(n, bonus, sd)
  sex <- as.factor(sex)
  sex <- factor(sex,
                levels = c(0, 1),
                labels = c("Female", "Male"))
  df <- data.frame(sex, schoesize, income)
}


simulated_data <- function(n = 500, y, sd = 333, bonus) {
  set.seed(12334)
  sex <- stats::rbinom(n, 1, .5)
  bonus <- sex*stats::rnorm(n, bonus, sd)
  income <- stats::rnorm(n, y, sd) + sex*stats::rnorm(n, bonus, sd)
  sex <- as.factor(sex)
  sex <- factor(sex,
                levels = c(0, 1),
                labels = c("Female", "Male"))
  df <- data.frame(sex, income)
}

