library(xts)
library(PerformanceAnalytics)

data("sample_matrix")
mydata <- sample_matrix

head(mydata, 4)
str(mydata)

# demo xts - get monthly data
mydata_mthly <- apply.monthly(mydata,tail,1 )

# import dataframe and transfer to ts object
ts <- read.table("data/ts.csv",header=TRUE,sep=",",as.is=TRUE)
head(ts,2)

ts_xts <- xts(ts[,-1], 
              order.by = as.Date(ts[,1], format= "%Y-%m-%d"))
head(ts_xts)
class(ts_xts)
index(ts_xts)

# calculate liner return
ts.ret.lin <- sample_matrix[-1,]/sample_matrix[-nrow(sample_matrix),] -1

# calculate log return
ts.ret.log <- diff(log(sample_matrix))

# Simulate return series
my.returns <- rnorm(252,mean=0.1/252,sd=.16/sqrt(252))
plot(my.returns,type="l",col="blue",ylab="Daily Return",
      xlab="Day",main="Daily Returns")

# load edhec data
data(edhec)
colnames(edhec)
colnames(edhec) = c("CA","CTA","DS","EM","EMN","ED","FIA",
                     "GM","LS","MA","RV","SS","FoF")

cor(edhec)[1:4,1:4]

# summary
coreData <- coredata(edhec[,1:3])
summary(coreData)

#
# regression example
#
library(quantmod)
getSymbols("^GSPC", src = "yahoo",from="1996-12-31",to="2009-08-31")

# get ret
spx.dat = apply.monthly(GSPC[,6],tail,1)
spx.ret = (exp(diff(log(spx.dat)))-1)[-1,]

# bind data and transfer to ts
my.df = cbind(coredata(spx.ret), coredata(edhec))
my.data.xdets = xts(my.df,order.by=as.Date(index(spx.ret)))

regression <- lm(my.data.xts[,2] ~ my.data.xts[,1])
beta <- regression$coefficients[2]

# loop beta
betas <- rep(0,ncol(edhec))
 for (i in 1:ncol(edhec)) {
  betas[i] = lm(my.data.xts[,(i+1)] ~
                    + my.data.xts[,1])$coef[[2]]
 }

# try tseries package
library(tseries)
GSPC <- get.hist.quote("^GSPC","2007-12-31","2014-12-31",quote = "Close",
                        provider="yahoo",retclass="zoo")


# test Garch model
library(tseries)
garchFit <- garch(ts.ret.log[,1], trace = FALSE)
coef(garchFit)

# load stock data
stk_data <- read.csv(file = "data/stock_data.csv",
                     stringsAsFactors = FALSE)

stk_data_xts <- xts(stk_data[,-1], as.Date(stk_data[,1],
                                           format = "%Y-%m-%d"))
plot.zoo(stk_data_xts, type = "l", col = 1:5)

# objects
die <- c(1:6)
text <- c("R", "Workshop")
logicals <- c(TRUE, FALSE,FALSE)

mtx <- matrix(die, nrow = 2)
ary <- array(mtx, dim=c(2,3,3))

list <- list(die, mtx, ary)

df <- data.frame(face = c("ace","king","queen"),
                 suit = c("heart","spades","diamonds"),
                 value = c(1,13,12))

allB <- c("B","BB","BB")
barCase1 <- c("B","BB","BB")
barCase2 <- c("DD","0","B")

n <- 1000
myPrizeHist <- vector()
for(i in 1:n){
  myPrizeHist[i] <- play()
}

# load tidyverse
library(tidyverse)
#library(dplyr)

load("data/data_df.RData")

# select
tic_sec <- data_df %>% 
  select(c(Ticker, Sector)) %>% 
  filter(Sector == "Financials")

# mutate
data_df_new <-data_df %>% 
  mutate(combV = (DivYield + ROE)/2)

data_df_new <- data_df
data_df_new$combV <- (data_df_new$DivYield + data_df_new$ROE)/2

# filter

data_df_fin <- data_df %>% 
  filter(Sector == "Financials")

# arrange
data_df_betaorder <- data_df %>% 
  arrange(desc(Beta))

# summarize
sec_mean_pe <- data_df %>% 
  group_by(Sector) %>% 
  summarise(pe_m = mean(PE))

# task

# mean ROE group by sector & mom(pos,neg flag)
# observation numbers >= 3
# order mean ROE in desc

mean_roe <- data_df %>%
  mutate(mom = ifelse(Return12m >0, "Positive","Negative")) %>% 
  group_by(Sector, mom) %>% 
  filter(n() >= 3) %>% 
  summarise(ROE_m = mean(ROE, na.rm = TRUE)) %>% 
  arrange(desc(ROE_m))
  

# ggplot

myplot <- data_df %>%
   group_by(Sector) %>%
   summarise_at(vars(contains("Return")), funs(mean)) %>%
   tidyr::gather(Variable, Return, -Sector) %>%
   ggplot(aes(x = Variable, y = Return, fill = Variable)) +
   geom_bar(stat = "identity") + ylab("") +
   facet_wrap(~ Sector, ncol = 4)

# Modelr
library(modelr)
ggplot(sim1, aes(x,y))+
  geom_point()

models <- tibble(
   a1 = runif(250,-20,20),
   a2 = runif(250,-5,5)
   )

ggplot(sim1, aes(x,y)) +
   geom_abline(
   aes(intercept = a1, slope = a2),
   data = models) +
   geom_point()

model1 <- function(a,data){
  a[1] + data$x *a[2]
}

model1(c(8,2), sim1)

meansure_dist <- function(mod, data){
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff^2))
  }

meansure_dist(c(8,2), sim1)

#sim1_dist <- function(a1,a2){

sim1_dist <- function(a1,a2){
  meansure_dist(c(a1,a2), sim1)
  }

models <- models %>%
mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist))

ggplot(sim1, aes(x,y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
  aes(intercept = a1, slope = a2, color = -dist),
  data = filter(models, rank(dist) <= 10)
  )

# to visulize a1 and a2 
ggplot(models, aes(a1,a2)) +
   geom_point(
     data = filter(models, rank(dist) <= 10),
     size = 4, color = "red"
     ) +
   geom_point(
     aes(color = -dist)
     )

grid <- expand.grid(
  a1 = seq(1,10, length = 25),
  a2 = seq(0,2.5, length = 25)) %>%
  mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist))


grid %>%
  ggplot(aes(a1,a2))+
  geom_point(
    data = filter(grid, rank(dist) <=10),
    size = 4, color = "red"
   ) +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x,y))+
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
    )

ggplot(sim1,aes(x,y))+
  geom_point(size = 2, color ="grey30") +
  geom_abline(intercept = best_guess$par[1],
                 slope = best_guess$par[2])
# lm
sim1_mod <- lm(y ~x , data = sim1)
coef(sim1_mod)

# diamonds data

ggplot(diamonds, aes(clarity, price)) +
   geom_boxplot()

ggplot(diamonds, aes(clarity, carat))+
  geom_boxplot()

# get residual of carat to price

mod_diamond <- lm(price ~ carat, data = diamonds)
diamonds2 <- diamonds %>%
   add_residuals(mod_diamond, "resid")
ggplot(diamonds2, aes(clarity, resid))+
  geom_boxplot()


# creat a dataframe

test_df <- data.frame(
  cbind(
    c("A","B","C"),
c(3,5,6),
c("A","A","B"),
c("F","M","O"),
c(20,19,24)))

colnames(test_df) <- c("name","rn","grade","gender","age")

test_df$name
test_df[,2]
# creat a list

test_list <- list(
  name = c("A","B","C"),
  rm = c(3,5,7),
  grade = c(20,19,24,4949,"erere")
)

test_list$name
test_list[[2]][2]
