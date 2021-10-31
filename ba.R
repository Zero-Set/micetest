#install.packages('mitml',dependencies=TRUE)
library('mice')
library('miceadds')
library(lme4)
library(mitml)

data("brandsma", package = "mice")
dat <- brandsma[, c("sch", "pup", "lpo",
                    "iqv", "ses", "ssi")]
d <- brandsma[, c("sch", "lpo")]
pred <- make.predictorMatrix(d)
pred["lpo", "sch"] <- -2
imp <- mice(d, pred = pred, meth = "2l.pmm", m = 10, maxit = 1,
            print = FALSE, seed = 152)
# 空のモデルは入力されたデータセットに適合され、推定値は次のようにプールされます。

fit <- with(imp, lmer(lpo ~ (1 | sch), REML = FALSE))
summary(pool(fit))

#次のサンプルコードはwarningを出すので、このようにする。
#Warning message:
#In .checkDeprecated(extra.pars, arg.list = dots, name = "var.comp") :
#  The 'var.comp' argument is deprecated. Please use 'extra.pars' instead.

testEstimates(as.mitml.result(fit), var.comp = TRUE)$extra.pars


#7.2
d <- brandsma[,c("sch","lpo","iqv")]
pred <- make.predictorMatrix(d)
pred["lpo",]<- c(-2,0,3)
pred["iqv",]<- c(-2,3,0)
imp <- mice(d, pred = pred, meth = "2l.pmm", seed = 919,
            m = 10, print = FALSE)
d$lpo <- as.vector(scale(d$lpo, scale = FALSE))


res <- mice::complete(imp, "long") %>%
  group_by(sch, .imp) %>%
  mutate(iqm = mean(iqv)) %>%
  group_by(.imp) %>%
  do(model = lmer(lpo ~ iqv + iqm + (1 | sch),
                  REML = FALSE, data = .)) %>%
  as.list() %>% .[[-1]]
summary(pool(res))

# 7.10.4 ランダム切片、レベル2の欠落
d <- brandsma[, c("sch", "lpo", "iqv", "den")]
meth <- make.method(d)
meth[c("lpo", "iqv", "den")] <- c("2l.pmm", "2l.pmm",
                                  "2lonly.pmm")
pred <- make.predictorMatrix(d)
pred["lpo", ] <- c(-2, 0, 3, 1)
pred["iqv", ] <- c(-2, 3, 0, 1)
pred["den", ] <- c(-2, 1, 1, 0)
imp <- mice(d, pred = pred, meth = meth, seed = 418,
            m = 10, print = FALSE)
fit <- with(imp, lmer(lpo ~ 1 + iqv + as.factor(den)
                      + (1 | sch), REML = FALSE))
summary(pool(fit))
testEstimates(as.mitml.result(fit), var.comp = TRUE)$extra.pars


# 7.10.5
d <- brandsma[, c("sch", "lpo", "iqv", "sex", "den")]
d <- data.frame(d, lpm = NA, iqm = NA, sxm = NA,
                iqd = NA, lpd = NA,
                iqd.sex = NA, lpd.sex = NA, iqd.lpd = NA,
                iqd.den = NA, sex.den = NA, lpd.den = NA,
                iqm.den = NA, sxm.den = NA, lpm.den = NA)
# level-1 variables
meth <- make.method(d)
meth[c("lpo", "iqv", "sex")] <- "2l.pmm"

pred <- make.predictorMatrix(d)
pred[,] <- 0
pred[, "sch"] <- -2
codes <- c(3, 3, rep(1, 6))
pred["lpo", c("iqv", "sex", "iqd.sex", "sex.den", "iqd.den",
              "den", "iqm.den", "sxm.den")] <- codes
pred["iqv", c("lpo", "sex", "lpd.sex", "sex.den", "lpd.den",
              "den", "lpm.den", "sxm.den")] <- codes
pred["sex", c("lpo", "iqv", "iqd.lpd", "lpd.den", "iqd.den",
              "den", "iqm.den", "lpm.den")] <- codes

# level-2 variables
meth["den"] <- "2lonly.pmm"
pred["den", c("lpo", "iqv", "sex",
              "iqd.sex", "lpd.sex", "iqd.lpd")] <- 1

# derive group means
meth[c("iqm", "sxm", "lpm")] <- "2l.groupmean"
pred[c("iqm", "sxm", "lpm"), c("iqv", "sex", "lpo")] <- diag(3)

# derive deviations from cluster mean
meth["iqd"] <- "~ I(iqv - iqm)"
meth["lpd"] <- "~ I(lpo - lpm)"

# derive interactions
meth["iqd.sex"] <- "~ I(iqd * sex)"
meth["lpd.sex"] <- "~ I(lpd * sex)"
meth["iqd.lpd"] <- "~ I(iqd * lpd)"
meth["iqd.den"] <- "~ I(iqd * den)"
meth["sex.den"] <- "~ I(sex * den)"
meth["lpd.den"] <- "~ I(lpd * den)"
meth["iqm.den"] <- "~ I(iqm * den)"
meth["sxm.den"] <- "~ I(sxm * den)"
meth["lpm.den"] <- "~ I(lpm * den)"

visit <- c("lpo", "lpm", "lpd",
           "lpd.sex", "iqd.lpd", "lpd.den", "lpm.den",
           "iqv", "iqm", "iqd",
           "iqd.sex", "iqd.lpd", "iqd.den", "iqm.den",
           "sex", "sxm",
           "iqd.sex", "lpd.sex", "sex.den", "sxm.den",
           "den", "iqd.den", "sex.den", "lpd.den",
           "iqm.den", "sxm.den", "lpm.den")

imp <- mice(d, pred = pred, meth = meth, seed = 188,
            visit = visit, m = 10, print = FALSE,
            allow.na = TRUE)

long <- mice::complete(imp, "long", include = TRUE)
long$den <- as.factor(long$den)
imp2 <- as.mids(long)
fit <- with(imp2, lmer(lpo ~ 1 + iqv*sex + iqm*den + sex*den
                       + (1 | sch), REML = FALSE))
summary(pool(fit))