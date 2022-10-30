#환율
csv.data <-read.csv(file="c:/Rdata/ch_1.csv",header=F) 
csv.data <- csv.data[-c(1,3),-c(1:5)]
rownames(csv.data) <- NULL
colnames(csv.data) <- NULL
A <- as.numeric(gsub(",","",csv.data))

csv.datae <-read.csv(file="c:/Rdata/nx.csv",header=F) 
csv.datae <- csv.datae[-c(1,3),-c(1:4)]
rownames(csv.datae) <- NULL
colnames(csv.datae) <- NULL
B <- as.numeric(gsub(",","",csv.datae))

ls.result <- lsfit(x=B,y=A)
ls.print(ls.result)


plot(x=B, y=A, xlab="NX", ylab="EX",main="NX-EX")
abline(ls.result)  #abline 표본회귀선 



#경제학원론 실업률과 고용률의 관계 상관관계 분석석
principle <-read.csv(file="c:/principle_6.csv",header=F) 
principle <- principle[-1,-c(1,2,3,4,5)]
principle_1 <- t(principle)
principle_df <- ts(principle,frequency=12)
df.pr <- data.frame(data=principle_1)
plot(x=df.pr$data.2, y=df.pr$data.3, xlab="u", ylab="ER", main="u-ER")





ls.result <- lsfit(x=df.pr$data.2, y=df.pr$data.3)
ls.print(ls.result)

plot(x=df.pr$data.2, y=df.pr$data.3, xlab="실업률", ylab="고용률", main="실업률과 고용률의 관계")
abline(ls.result)  #abline 표본회귀선 


f <- function(x){x^(1/2)}
x <- 1:900
plot(x, f(x), type='l')

