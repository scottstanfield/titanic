# Royal Mail Steamer Titanic
# https://cfss.uchicago.edu/stat003_logistic_regression.html

source('utils.r')
dt1 <- load.csv('train.csv')
dt2 <- load.csv('test.csv')

dt2[, survived := NA]
dt <- rbind(dt1, dt2)
put.first(dt, c('survived'))
dt

# Quick check
summary(dt)
cat('')     # cls

# Won't need their primary key
drop.cols(dt, 'passengerid')
str(dt)

# Column type changes
dt[, name     := as.character(name)]
dt[, survived := as.logical(survived)]
dt[, pclass   := as.factor(pclass)]
summary(dt)
colnames(dt)
str(dt)

# How many NAs are we dealing with?
check.missing <- function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(dt, check.missing))     # dt vs dt1
data.frame(sapply(dt1, check.missing))     # dt vs dt1

# Ignore cabin: too many NAs
dt$cabin %>% unique     # unpack cabins and calculate distance to lifeboats?
dt[, cabin := NULL]
colnames(dt)

# Handle NA embarked      <-- use library(txtplot) for histogram
dt$embarked %>% summary
dt[is.na(embarked)]
plot(dt$embarked)
dt[is.na(embarked), embarked := 'S']

# Handle NA age (mean impute)
dt1$age %>% summary
dt[is.na(age)]
m_age <- dt[sex == 'male' & !is.na(age) , mean(age)]
f_age <- dt[sex == 'female' & !is.na(age) , mean(age)]
m_age
f_age
dt[sex == 'male' & is.na(age), age := m_age]
dt[sex == 'female' & is.na(age), age := f_age]

# Handle fare amount ($ or pounds)
summary(dt$fare)
boxplot(dt$fare, notch=T)
dt$fare <- log1p(dt$fare)
boxplot(dt$fare)

########################
# Interesting passengers
########################

# How'd the Astor's do?
dt[name %like% "Astor"]
dt[ticket == 'PC 17757']
dt[name %like% "Spedden"]     # page 37
dt[ticket=='16966']           # Ms Elizabeth Burns is Mstr. Robert's nanny

# Ms. Mabel Francatelli (Sec. of Lady Duff Gordon). Cabin was E deck, 20' above water
dt[name %like% 'Duff']
dt[ticket == 'PC 17485']
dt[ticket==1601]

# Babies < 1 yr old
dt[age < 1]

##
## Plots
## Try exploratory
##

plot(rnorm(50))
cols <- cc('survived sex age sibsp fare embarked')
pairs(dt[1:100, cols, with=F])

# Sample plots
with(dt, table(cut(age, quantile(age, na.rm=T)), survived))
with(dt, xtabs(survived ~ cut(age, quantile(age,na.rm=T)) + sex))

##
## Feature Engineering
##

dt[, nchar(name), by=name][order(V1)]
dt[, tstrsplit(name, '[,]')]
dt[, tstrsplit(name, '[.,]')]
dt[, tstrsplit(name, '[.,]')[2]] %>% unique
dt[, title := tstrsplit(name, '[.,]')[2]]

# Strip leading space
dt[, title := substr(title, 2, 200)]
dt$title %>% unique

# Map obscure titles to canoncial ones
dt[, .N, by=title]
dt[title %in% cc('Mme Ms Mlle'), title := 'Miss']
dt[title %notin% cc('Mr Mrs Miss Master'), title := 'Special']
dt[, .N, by=title]
dt[, title := as.factor(title)]
summary(dt)

table(dt$title) %>% data.table
dt[, .(sex, title)]
dt[, .(sex, title)] %>% table
table(dt$survived, dt$sex)


###################
# Linear Regression
###################
set.seed(42)

dt[, name := NULL]
dt[, ticket := NULL]

str(dt)
dt.train <- dt[!is.na(survived)]
dt.test  <- dt[is.na(survived)]

# consider glmnet
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

foo <- glm(survived ~ age, data=dt.train, family=binomial)
summary(foo)

colnames(dt.train)
rms_fit <- glm(survived ~ ., data=dt.train, family='binomial')
summary(rms_fit) %>% head()
twoup()
plot(rms_fit)
oneup()

library(ROCR)
yhat <- predict(rms_fit, dt.test, type='response')


##
## H2O
##
library(h2o)
h2o.init(ip='localhost')

dt.train <- dt[!is.na(survived)]
dt.test <- dt[is.na(survived)]
dt.test[, survived := NULL]

dt.hex <- as.h2o(dt.train, destination_frame = 'rms.all')
splits <- h2o.splitFrame(data=dt.hex, ratios=c(0.8), seed=42)
train  <- h2o.assign(splits[[1]], 'train.hex')
valid  <- h2o.assign(splits[[2]], 'valid.hex')
test   <- as.h2o(dt.test, destination_frame = 'test.hex')

y <- 'survived'
x <- setdiff(names(dt.train), c(y, 'name', 'ticket'))  # interest rate uncorrelated

##
## One-off GBM
##

gbm1 <- h2o.gbm(x=x,
            y=y,
            training_frame = train,
            validation_frame = valid,
            model_id = "gbm1",
            ntrees = 500,
            max_depth = 6,
            learn_rate = 0.1)

gbm1

# How do we know which features contributed to this particular model?
h2o.varimp(gbm1)

yhat <- h2o.predict(gbm1, test)
h2o.performance(gbm1, train)
h2o.confusionMatrix(gbm1, train)

yhat.dt <- as.data.table(yhat)

cbind(dt.test, yhat.dt)[4:10]

dim(yhat)
cbind(xx, yhat)

# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      score_tree_interval =  10,
                      hyper_params = gbm_params1)


# Get the grid results, sorted by AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)


# https://cfss.uchicago.edu/stat003_logistic_regression.html


