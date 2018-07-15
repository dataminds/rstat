# rstat
Statistical Analysis


### 상관관계 표 만들기
- R에서 분석한 상관관계를 계수와 P값이 함께 나오게 표로 만들기 [R파일](https://github.com/dataminds/rtip/blob/master/correlatonMatrix_table.R)

### 복수의 t검정 결과를 데이터프레임으로 만들어 정열
변수가 여러개 있는 데이터를 사전분석할 때 sex등과 같은 변수가 미치는 영향을 사전 검토할 때 유용

```
names(df)
options(digits = 3)
IV <- df$sex #독립변수
v1 <- levels(IV)[1] #독립변수의 속성1
v2 <- levels(IV)[2] #독립변수의 속성2
t.df <- data.frame()
for (i in 1:ncol(dfN)) {
  s.t <- t.test(df[, i] ~ IV)
  coef.df <- data.frame( v1 = s.t$estimate[1],
                         v2 = s.t$estimate[2],
                         t = s.t$statistic,
                         p = s.t$p.value )
  colnames(coef.df) <- c(v1, v2, "t", "p")
  rownames(coef.df) <- names(dfN)[i]
  t.df <- rbind(t.df, coef.df)
} ; t.df[order(t.df$p), ]
```
