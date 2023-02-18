## 0. 라이브러리 설치 ##

install.packages("ggplot2")
library(ggplot2)
#install.packages("GGally")
library(GGally)
#install.packages("CCA")
library(CCA)
#install.packages("CCP")
library(CCP)
#install.packages("MVN")
library(nscancor)
library("MVN")

## 1. 데이터 불러오기 ##

path <- "C:/Users/User/Desktop/대학교/[2021 4-1] 다변량해석"
setwd(path)

df <- read.csv("mmreg.csv")

str(df)
summary(df)

# 변수명 인덱싱 
colnames(df) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(df)

## 3. 데이터 전처리 (결측치 확인) ##
colSums(is.na(df))


## 4. 정준상관분석 ##

# 변수군 분리 ##
psych <- df[, 1:3] # 심리적 변수
acad <- df[, 4:7] # 학문적 변수

# 변수군 상관관계 확인 #
ggpairs(psych)
ggpairs(acad)

# 상관관계 시각화 #
correl <- matcor(psych, acad)
img.matcor(correl, type = 2)

# 정준상관분석 실시 ##
cc1 <- cc(psych, acad)
cc1

# 정준상관계수 #
cc1$cor  

# 선형결합계수 -> 정준변수 #
cc1[3:4]

# 정준변수들간의 상관관계 확인 #
u=as.matrix(psych)%*%as.matrix(cc1$xcoef)
v=as.matrix(acad)%*%as.matrix(cc1$ycoef)
cor(u,v)

# 제1정준변수들의 산점도 #
u1 = cc1$scores$xscores[,1]
v1 = cc1$scores$yscores[,1]
plot(u1,v1)



### 4.4 두 변수 집단에 대한 독립성 검정 ###
## 정규성 확인 ##
library(MVN)
psych_mvn <- mvn(psych, multivariatePlot = "qq")
mvn(acad , multivariatePlot = "qq")

# F 분포를 이용한 유의성 검정 #
# tests of canonical dimensions

rho <- cc1$cor;rho # 정준상관계수
n <- dim(psych)[1];n
p <- length(psych);p
q <- length(acad);q

p.asym(rho, n, p, q, tstat = "Wilks")
# p.asym(rho, n, p, q, tstat = "Hotelling")
# p.asym(rho, n, p, q, tstat = "Pillai")
# p.asym(rho, n, p, q, tstat = "Roy").

### 4.5 결과 해석 ##
# 결과해석을 위해 표준화
# 표준화 된 표준 계수는 표준화 된 회귀 계수를 해석하는 것과 유사한 방식으로 해석

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% cc1$xcoef

# standardized acad canonical coefficients diagonal matrix of acad sd's
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% cc1$ycoef


## 시각화 코드 ##
plt.cc(cc1,var.label=T)

#install.packages("yacca")
library("yacca")
rs3 <- cca(psych, acad)
rs3

plot(rs3)
