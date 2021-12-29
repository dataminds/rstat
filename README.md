# rstat
Statistical Analysis

### 데이터프레임 factor의 level 값 변경
#### tidyverse forcats패키지의 fct_recode 이용
```
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
dat <- data.frame(x = factor("A"), y = 1)
mutate(dat, x = fct_recode(x, B = "A"))
```

### 결측값 평균값 대체
#### 데이터프레임 각 변수 결측값 Simple (nonstochastic) imputation 
```
colSums(is.na(dataset)) #결측값포함된 열 확인
data_filled <- data.frame( sapply(dataset, 
                                  function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), 
                                  x)) )
```
### 결측값 k최근 인접 분류 
https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/knnImputation
https://thebook.io/006723/ch09/02/02/
```
if(!require(DMwR)) install.packages("DMwR")
library(DMwR)
data(algae)
cleanAlgae <- knnImputation(algae)
summary(cleanAlgae)
```

### 변수 만들 때 여러 행의 값 더하고 나누기 & 알파 
```
if(!require(dplyr)) install.packages("dplr")
if(!require(psych)) install.packages("psych")
library(dplyr)
library(psych)

a1 <- c(1, 2, 3, 4)
b1 <- c(1, 2, 3, 4)
c1 <- c(1, 2, 3, 4)
df <- data.frame(a1, b1, c1)

df$var1 <- rowSums(select(df, a1:c1))/ncol(select(df, a1:c1))

s1 <- "sol_001.x"
s2 <- "sol_009.x"
sel <- select(df, s1:s2)
wb <- rowSums(sel) / ncol(sel)
alpha(sel)

```



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
### Running Multiple Linear Regression Models in for-Loop
https://statisticsglobe.com/r-multiple-regressions-in-for-loop
```
mod_summaries <- list()                  # Create empty list
for(i in 2:ncol(data)) {                 # Head of for-loop
  predictors_i <- colnames(data)[2:i]    # Create vector of predictor names
  mod_summaries[[i - 1]] <- summary(     # Store regression model summary in list
    lm(y ~ ., data[ , c("y", predictors_i)]))
}
```

### 회귀분석 결과 APA형식에 맞게 출력
```
## regresssion table
coef.df <- (fit %>% summary)$coef %>% round(.,3)
confint.df <- fit %>% confint %>% as.data.frame() %>% round(.,3)
beta.df <- (fit.s %>% summary)$coef[, 1] %>% as.data.frame() %>% round(.,3) 

t.df <- cbind(coef.df, confint.df)
t.df <- cbind(t.df, beta.df)

colnames(t.df) <- c("B", "SE", "t", "p", "95%CI LL", "95%CI UL", "b")
t.df

t.df <- dplyr::select(t.df, "B", "b", "SE", "t", "p", "95%CI LL", "95%CI UL")

## table note 
n <- nrow(df)

# F통계
numdf <- (fit %>% summary)$fstatistic[2]
dendf <- (fit %>% summary)$fstatistic[3]
value <- (fit %>% summary)$fstatistic[1] %>% round(.,2)

# R제곱
r2 <- (fit %>% summary)$r.squared %>% round(.,2)
r2adj <- (fit %>% summary)$adj.r.squared %>% round(.,2)

fstat <- paste0("F(", numdf, ",", dendf, ") = ", value)
r.v <- paste0("R2 = ", r2, " , 수정 R2 = ", r2adj)
nn <- paste0("N = ", n)
cf <- "SE = 표준오차; CI = 신뢰구간; LL = 하한계; UL = 상한계."

## 
n.v <- c(fstat, r.v, nn, cf)

write.csv(t.df, "table_regression.csv")
write.csv(n.v, "table_regression_note.csv", row.names = F)

```

### 문자를 객체명으로
```
varName <- names(iris)[1] 
as.name(varName)
```

### 객체명을 문자로 
```
object2string <- function(x) {
  n <- deparse(substitute(x))
  print(n)
  }
object2string(a)

# Or use all.vars() all.names()
# Return a character vector containing all the names which occur in an expression or call
modelformula <- mpg ~ cyl + disp + hp + drat + qsec

all.vars(modelformula)
 [1] "mpg"  "cyl"  "disp" "hp"   "drat" "qsec"

```
### 복수의 변수를 한번에 표준화
```
modelformula <- A ~ B + C
mycars <- lapply(mtcars[, all.vars(modelformula)], scale) 

```


### Centering

```
# look into line by line
rep(1, nrow(df))
t(apply(df, 2, median))
rep(1, nrow(df)) %*% t(apply(df, 2, median)) # 행렬 곱하기

head(rep(1, nrow(df)) %*% t(apply(df, 2, median)), 3)

# function for median centering
center_median <- function(x) {
  ones = rep(1, nrow(x))
  x_median = ones %*% t(apply(x, 2, median))
  x - x_median
}

```

### 문자형인 table을 데이터프레임으로 변경
```
table() %>% as.data.frame.matrix() -> df

bind_cols(
  tibble(변수명 = rownames(df)),
  df) -> df 
```

### 데이터프레임 복수의 열에서 숫자형만 찾아 계산
```
# 행방향 합산
df %>% summarise(across(where(is.numeric), sum)) %>%
    add_column(변수명 = "Total", .before = "열이름")
# 열방향 합산
  mutate(
    Total = rowSums(across(where(is.numeric) ))
  ) 
 ``` 

