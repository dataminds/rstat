# rstat
Statistical Analysis

### 복수의 t검정 결과를 데이터프레임으로 만들어 정열
변수가 여러개 있는 데이터를 사전분석할 때 sex등과 같은 변수가 미치는 영향을 사전 검토할 때 유용

```
names(df)
options(digits = 3)

DV <- df[1:27] # 종속변수 선택 
IV <- df$sex #독립변수
IVcol <- "sex"

v1 <- levels(IV)[1] #독립변수의 속성1
v2 <- levels(IV)[2] #독립변수의 속성2

t.df <- data.frame()
for (i in 1:ncol(DV)) {
  s.t <- t.test(DV[, i] ~ IV)
  coef.df <- data.frame( v1 = DV[which(df[,IVcol]== v1), i] %>% mean,
                         v2 = DV[which(df[,IVcol]== v2), i] %>% mean,
                         t = s.t$statistic,
                         p = s.t$p.value )
  colnames(coef.df) <- c(v1, v2, "t", "p")
  rownames(coef.df) <- names(dfN)[i]
  t.df <- rbind(t.df, coef.df)
} ; t.df[order(t.df$p), ]

t.df

```

### 복수의 wilcox검정 결과를 데이터프레임으로 만들어 정열
```
names(df)
DV <- df[1:27] # 종속변수 선택 
IV <- df$status #독립변수
IVcol <- "status"

v1 <- levels(IV)[1] #독립변수의 속성1
v2 <- levels(IV)[2] #독립변수의 속성2

t.df <- data.frame()
for (i in 1:ncol(DV)) {
  s.t <- wilcox.test(DV[, i] ~ IV)
  coef.df <- data.frame( v1m = DV[which(df[,IVcol]== v1), i] %>% mean,
                         v1d = DV[which(df[,IVcol]== v1), i] %>% median,
                         v1s = DV[which(df[,IVcol]== v1), i] %>% sd,
                         v2m = DV[which(df[,IVcol]== v2), i] %>% mean,
                         v2d = DV[which(df[,IVcol]== v2), i] %>% median,
                         v2s = DV[which(df[,IVcol]== v2), i] %>% sd,
                         w = s.t$statistic,
                         p = s.t$p.value )
  colnames(coef.df) <- c(paste0(v1, "me"), paste0(v1, "md"), paste0(v1, "sd"), 
                         paste0(v2, "me"), paste0(v2, "md"), paste0(v2, "sd"), 
                         "w", "p")
  rownames(coef.df) <- names(dfN)[i]
  t.df <- rbind(t.df, coef.df)
} ; t.df[order(t.df$p), ]

t.df
```



### 복수의 회귀분석 결과를 하나의 데이터프레임 생성
```
attach(df)
DV <- wbN
IV1 <- year
IV2 <- boeeC
detach(df)

# regression
fit1 <- lm(DV ~ IV1) %>% summary
fit2 <- lm(DV ~ IV1 + IV2) %>% summary

# object2string 
o2s <- function(x) { 
    n <- deparse(substitute(x)); print(n) 
    }

# combining results data
fit.df <- do.call( "rbind", 
  list(
    fit1$coef[, 3:4] %>% round(., 3) %>% as.data.frame() %>% mutate(var = rownames(.), fit = o2s(fit1)),
    fit2$coef[, 3:4] %>% round(., 3) %>% as.data.frame() %>% mutate(var = rownames(.), fit = o2s(fit2))
  )
) 

fit.df <- fit.df[c(4, 3, 1, 2)]
fit.df
fit.df[order(fit.df$var),]
```

### 회귀분석 결과 APA형식에 맞게 출력
```
if(!require(QuantPsyc)) install.packages("QuantPsyc")
if(!require(dplyr)) install.packages("dpyr")
library(dplyr)
library(QuantPsyc)

# 분석 데이터
df <- iris
# 투입할 변수
DV <- df[,1]
IV1 <- df[,2]
IV2 <- df[,3]

lm(DV ~ IV1 + IV2, data = df)
lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = df)

coef.df <- (lm(DV ~ IV1 + IV2, data = df) %>% summary)$coef %>% round(.,3)
beta.df <- (lm(scale(DV) ~ scale(IV1) + scale(IV2), data = df) %>% summary)$coef[, 1] %>% 
  as.data.frame() %>% round(.,3) 
confint.df <- lm(DV ~ IV1 + IV2, data = df) %>% confint %>% as.data.frame() %>% round(.,3)

t.df <- cbind(coef.df, confint.df)
t.df <- cbind(t.df, beta.df)

colnames(t.df) <- c("B", "SE", "t", "p", "95%CI LL", "95%CI UL", "b")
t.df

dplyr::select(t.df, "B", "b", "SE", "t", "p", "95%CI LL", "95%CI UL")
```

### 객체명을 문자로 
```
object2string <- function(x) {
  n <- deparse(substitute(x))
  print(n)
  }

object2string(a)
```

### 문자를 객체명으로
```
varName <- names(iris)[1] 
as.name(varName)
```

### Centering

```
# look into function
rep(1, nrow(p1.df))
t(colMeans(p1.df))
head(rep(1, nrow(p1.df)) %*% t(colMeans(p1.df)))

# function for mean centering
center_mean <- function(x) {
  ones = rep(1, nrow(x))
  x_mean = ones %*% t(colMeans(x))
  x - x_mean
}

# source: http://gastonsanchez.com/blog/how-to/2014/01/15/Center-data-in-R.html
```

