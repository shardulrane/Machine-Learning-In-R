dat=read.csv("wbcd.csv",stringsAsFactors = FALSE)
dat1=dat
names(dat)
str(dat)
dat <- dat[-1]
table(dat$diagnosis)
dat$diagnosis=ifelse(dat$diagnosis=="Beningn",0,1)
dat$diagnosis=as.factor(dat$diagnosis)
summary(dat)
#values dekh kar lag raha hai ki area ka impact jyada hoga
# distance calculation ke liye normalize karna padega 
#agar hum har value ko min se subtract and max aur min se subtract karenge toh data scale ho jayega apne hisaab se
norm_data <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
nn=colnames(dat[2:31])
{
  main_l= dat[2:31]
  for (i in 2:31) {
    dem=dat[,i]
    for (j in length(dat[,i])) {
      dem[j]=norm_data(dem[j])
      
    }
    main_l[nn[i]]=dem
    dem=c()
  }}
# or we can apply lapply() function
norm_dat <- as.data.frame(lapply(dat[2:31], norm_data))
norm_dat1=norm_dat
norm_dat1['diagnosis']=dat$diagnosis
summary(norm_dat$area_mean)
#train test split karna padega so data divide karte hai 80-20 mein
train_data <- norm_dat1[1:455,]
test_data <- norm_dat1[456:569,]

m1=naive_bayes(diagnosis~.,data = train_data)
plot(m1)
y1=predict(m1,test_data)
table(test_data$diagnosis)
table(y1)
plot(test_data$diagnosis)
lines(y1)