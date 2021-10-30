# 次の行を入れないとdevtoolsが動かない
#install.packages("backports")
#install.packages('devtools',dependencies=TRUE)

# 多分いらないんだろうな。
library(devtools)
#install_url('https://cran.r-project.org/src/contrib/Archive/DPpackage/DPpackage_1.1-7.tar.gz')

#install.packages('tidyverse',dependencies=TRUE)
#install.packages('broom',dependencies=TRUE)
library(tidyverse)
library(broom.mixed)
library(purrr)
library(lme4)
library(mice)
library(tidyr)

data("toenail", package = "DPpackage")

data <- tidyr::complete(toenail, ID, visit) %>%
  tidyr::fill(treatment) %>%
  dplyr::select(-month)
table(data$outcome, useNA = "always")

pred <- make.predictorMatrix(data)
pred["outcome", "ID"] <- -2
imp <- mice(data, method = "2l.bin", pred = pred, seed = 12102,
            maxit = 1, m = 5, print = FALSE)
table(mice::complete(imp)$outcome, useNA = "always")


mice::complete(imp, "all") %>%
  purrr::map(lme4::glmer,
             formula = outcome ~ treatment * visit + (1 | ID),
             family = binomial) %>%
  pool() %>%
  summary()

