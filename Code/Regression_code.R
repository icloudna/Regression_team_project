# 2020년 6월 8일 (월) 시도

mosq<-read.csv("mosquito0.csv", header=T)
wea2015<-read.csv("weather_2015.csv", header=T)
wea2016<-read.csv("weather_2016.csv", header=T)
wea2017<-read.csv("weather_2017.csv", header=T)
wea2018<-read.csv("weather_2018.csv", header=T)
wea2019<-read.csv("weather_2019.csv", header=T)
wea2015<-wea2015[,-1]
wea2016<-wea2016[,-1]
wea<-rbind(wea2015,wea2016,wea2017,wea2018,wea2019)
dat0<-cbind(mosq,wea)
attach(dat0)
data1<-dat0[-which(mogi>=10000),]
attach(data1)
d<-with(data1,dummy*average_ground_temp)
data1<-cbind(data1,d)
d2<-with(data1,dummy*total_daylight)
data1<-cbind(data1,d2)

nnn<-lm(log(mogi)~dummy+average_wind+average_humidity+total_daylight+average_ground_temp+d+d2)

mog<-log(dat0$mogi)
dat1<-cbind(mog,dat1)>k<-which(dat1$mogi>10000)
dat2<-dat1[-k,]
kk<-which(dat2$total_daylight<6)
dat3<-dat2[-kk,]
dat4<-dat3[,c(1,3,8,9,11,12,15)]
summary(dat4)

# 2020년 6월 10일 (화) 시도

mosq<-read.csv("mosquito0.csv", header=T)
wea2015<-read.csv("weather_2015.csv", header=T)
wea2016<-read.csv("weather_2016.csv", header=T)
wea2017<-read.csv("weather_2017.csv", header=T)
wea2018<-read.csv("weather_2018.csv", header=T)
wea2019<-read.csv("weather_2019.csv", header=T)
wea2015<-wea2015[,-1]
wea2016<-wea2016[,-1]
wea<-rbind(wea2015,wea2016,wea2017,wea2018,wea2019)
dat0<-cbind(mosq,wea)
k<-which(dat0$mogi>10000)
dat0<-dat0[-k,]
d<-with(dat0,dummy*average_ground_temp)
dat1<-cbind(dat0,d)
mog<-log(dat0$mogi)
dat1<-cbind(mog,dat1)
data1<-dat1[,c(1,3,6,8,9,11,12,15)]
model0<-lm(mog~., data=data1)
summary(model0)
d2<-with(data1,dummy*total_daylight)
data4<-cbind(data1,d2)
modell<-lm(mog~., data=data4)
summary(modell)
kk<-which(dat0$precipitation==0)
data2<-data4[kk,]
data5<-data2[,-3]
modelk<-lm(mog~., data=data5)
summary(modelk)
data10<-data5[,-8]
modela<-lm(mog~., data=data10)
summary(modela)

# 2020년 6월 12일 (목) 시도

mosq<-read.csv("Desktop/mosquito0.csv", header=T)
wea2015<-read.csv("Desktop/2015기상.csv", header=T)
wea2016<-read.csv("Desktop/2016기상1.csv", header=T)
wea2017<-read.csv("Desktop/2017기상.csv", header=T)
wea2018<-read.csv("Desktop/2018기상1.csv", header=T)
wea2019<-read.csv("Desktop/2019기상.csv", header=T)
wea2015<-wea2015[,-1]
wea2016<-wea2016[,-1]
wea<-rbind(wea2015,wea2016,wea2017,wea2018,wea2019)
dat0<-cbind(mosq,wea)

k<-which(dat0$mogi>10000)
dat0<-dat0[-k,]
d<-with(dat0,dummy*average_ground_temp)
dat1<-cbind(dat0,d)
mog<-log(dat0$mogi)
dat1<-cbind(mog,dat1)
data1<-dat1[,c(1,3,6,8,9,11,12,15)]

data1$rainn<-ifelse(dat0$precipitation==0,0,1)

data10<-data1[,-3]
modela<-lm(mog~., data=data10)
summary(modela)

a<-(rstandard(modelg)<(-2))
aa<-which(a==1)
b<-(rstandard(modelg)>2)
bb<-which(b==1)
data12<-data11[-c(aa,bb),]
modell<-lm(mog~., dat=data12)
summary(modell)

# 강수량=0일때 만을 본 데이터 선택.  
# (데이터 개수가 많고, Vif 좋은데 Qudratic 형태 보여서 잔차제거 시도)


# 강수량=0일 때 온도 변수(low_temp+average_지면온도) 끼리 더해서 ondo라는 변수로 적합시,
# log(mogi)~log(ondo)+dummy+average_wind+average_humidity+d): 설명력 0.6607 (데이터 갯수도 609개)


