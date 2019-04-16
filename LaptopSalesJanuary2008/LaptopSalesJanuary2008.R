#第一題
laptop.df <- read.csv('C:\\Users\\USER\\Desktop\\kay\\BusinessAnalysis\\LaptopSalesJanuary2008Sub.csv')
str(laptop.df)
str(laptop.df)
dim(laptop.df)
head(laptop.df)
tail(laptop.df)
install.packages("psych")
library(psych)
describe(laptop.df[,c(2,5:9,11,13)])

#第二題:我們定義的actually selling是平均下來一般人大概在某規格下的某個價格
# table(laptop.df$Integrated.Wireless.)
#   #Yes>No,但是只多一點
# table(laptop.df$Bundled.Applications.)
#   #Yes>No,但是只多一點
# Condi = which(laptop.df$Integrated.Wireless.=="Yes" & laptop.df$Bundled.Applications.=="Yes")
# ConPrice=laptop.df$Retail.Price[Condi]
# mean(ConPrice)
# mean(laptop.df$Configuration[Condi])
# mean(laptop.df$Screen.Size..Inches.[Condi])
# mean(laptop.df$Battery.Life..Hours.[Condi])
# mean(laptop.df$RAM..GB.[Condi])
# mean(laptop.df$Processor.Speeds..GHz.[Condi])
# mean(laptop.df$HD.Size..GB.[Condi])
# mean(laptop.df$CustomerStoreDistance[Condi])
#平均離店家這個距離的顧客會消費在上述各種平均規格條件下的電腦價格
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
maxConfigQuan<-which(CountCon$Freq==max(CountCon$Freq))
maxConfig<-CountCon$Var1[maxConfigQuan]
maxConfigPrice<-which(laptop.df$Configuration==maxConfig)
table(laptop.df$Retail.Price[maxConfigPrice])




#第三題
price.mean <- aggregate(Retail.Price~Store.Postcode,data=laptop.df, mean)
library(lattice)
barchart(Retail.Price~Store.Postcode, data=price.mean,ylab='Retail Price Mean', xlab='Store PostCode',col="lightblue")

#第四題
sd(price.mean$Retail.Price)
IQR(price.mean$Retail.Price)


#第五題
boxplot(Retail.Price~Store.Postcode, data=laptop.df, yaxt='n', ylab='價格 ($k)')
a<-which(laptop.df$Store.Postcode=='W4 3PH'| laptop.df$Store.Postcode=='N17 6QA') 
#選出價格平均最大和最小的兩家店位置
b<-laptop.df$Retail.Price[a]#找出最大最小的兩店價格分布
c<-laptop.df$Store.Postcode[a]
two <- factor(c)#打斷結構，不要出現剩下14個沒有用到的店家
minmax<-data.frame( "Store"=two, "Price"=b)#只比較最大和最小兩家店
boxplot(Price~Store, data=minmax, col=c(5,6),xlab='Store Postcode', ylab='價格 ($k)')#最大和最小的box plot
#Q1min<- quantile(minPrice,1/4) 
#Q2min <- quantile(minPrice, 2 / 4) 
# Q3min <- quantile(minPrice, 3 / 4)
# Q1max<- quantile(maxPrice,1/4) 
# Q2max <- quantile(maxPrice, 2 / 4) 
# Q3max <- quantile(maxPrice, 3 / 4)
# IQR(minPrice)
# IQR(maxPrice)
# range(maxPrice)[2] - range(maxPrice)[1]
# range(minPrice)[2] - range(minPrice)[1]

#第六題
plot(sort(table(laptop.df$Store.Postcode),decreasing = FALSE))
table(laptop.df$Store.Postcode)

#第七題
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
cor(x=Config,y=CountCon$Freq)
aggregate(Retail.Price~Configuration, data=laptop.df, mean)

#第八題
sum(laptop.df$Retail.Price)
Storesum = aggregate(laptop.df$Retail.Price,
                        by=list(Country=laptop.df$Store.Postcode), sum)
library(lattice)
barchart(x~Country, data=Storesum, col="lightblue")


seg.aov.own <- aov(Retail.Price ~ Store.Postcode, data=laptop.df) 

anova(seg.aov.own)














