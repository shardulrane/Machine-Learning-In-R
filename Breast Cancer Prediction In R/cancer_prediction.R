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
summary(norm_dat$area_mean)
#train test split karna padega so data divide karte hai 80-20 mein
train_data <- norm_dat[1:455,]
test_data <- norm_dat[456:569,]
train_labels <- dat[1:455,1]
test_labels <- dat[456:569,1]

wbcd_test_pred <- knn(train = train_data,test = wbcd_test,
                      cl=wbcd_train_labels,k=21)