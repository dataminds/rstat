if(!require(Hmisc)) install.packages("Hmisc")
if(!require(dplyr)) install.packages("dplyr")

###########################
### creating correlation table
###########################

names(iris)
str(iris)
df <- iris
var <- 1:4
dcor.df <- df[var]
names(dcor.df)

# hide upper 
cor.r <- cor(dcor.df)
cor2.r<- round(cor.r, 2)

upper <- cor2.r 
upper[upper.tri(upper)] <- ""
upper
class(upper)
upper <- apply(upper, 2, as.numeric)

colnames(upper) <- 1:length(upper[1,])

upper

upper.df <- as.data.frame(upper)
dmean.df <- apply(dcor.df,2, mean)
dmean.df <- round(dmean.df, 2)
dsd.df <- apply(dcor.df,2, sd)
dsd.df <- round(dsd.df, 2)

upper.df <- rbind(upper.df, dmean.df)
upper.df <- rbind(upper.df, dsd.df)

rownames(upper.df) <- c(
  "꽃받침 길이", 
  "꽃받침 넓이", 
  "꽃잎 길이", 
  "꽃잎 넓이",
  "평균", 
  "표준편차" )

upper.df
coef.df <- upper.df

### significance data frame
library(dplyr)
dcor.r <- dcor.df %>% as.matrix %>% Hmisc::rcorr(.)
options(digits = 3)
dcor.r$P
sig.m <- as.matrix(dcor.r$P)

# hide upper
upper <- sig.m
upper[upper.tri(upper)] <- ""
upper
sig.m <- upper

sig.df <- as.data.frame(sig.m, stringsAsFactors = F)
sig.df <- as.data.frame(apply(sig.df, 2, as.numeric))
str(sig.df)
names(sig.df)

sig.df <- round(sig.df, 3)
colnames(sig.df) <- paste0("p", 1:ncol(sig.df))

# coef.df + sig.df
empty.df <- matrix( rep(NA, ncol(sig.df)*2), 
       nrow = 2, ncol = ncol(sig.df) , byrow = T ) %>% as.data.frame()
colnames(empty.df) <- paste0("p", 1:ncol(sig.df))
sig.df <- rbind(sig.df, empty.df)
cor2.df <- cbind(coef.df, sig.df )
names(cor2.df)

# convert dataframe to vector
df <- data.frame(v1 = 1:ncol(sig.df), v2 = 1:ncol(sig.df)+ncol(sig.df))
var <- c()
for(i in 1:ncol(sig.df)){
  var <- c(var, as.numeric(df[i,]) )
}
var

names(cor2.df[var])

cor2.df <- cor2.df[var]
cor2.df





