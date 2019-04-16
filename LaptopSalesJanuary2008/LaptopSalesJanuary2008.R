#�Ĥ@�D
laptop.df <- read.csv('C:\\Users\\USER\\Desktop\\kay\\BusinessAnalysis\\LaptopSalesJanuary2008Sub.csv')
str(laptop.df)
str(laptop.df)
dim(laptop.df)
head(laptop.df)
tail(laptop.df)
install.packages("psych")
library(psych)
describe(laptop.df[,c(2,5:9,11,13)])

#�ĤG�D:�ڭ̩w�q��actually selling�O�����U�Ӥ@��H�j���b�Y�W��U���Y�ӻ���
# table(laptop.df$Integrated.Wireless.)
#   #Yes>No,���O�u�h�@�I
# table(laptop.df$Bundled.Applications.)
#   #Yes>No,���O�u�h�@�I
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
#���������a�o�ӶZ�����U�ȷ|���O�b�W�z�U�إ����W�����U���q������
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
maxConfigQuan<-which(CountCon$Freq==max(CountCon$Freq))
maxConfig<-CountCon$Var1[maxConfigQuan]
maxConfigPrice<-which(laptop.df$Configuration==maxConfig)
table(laptop.df$Retail.Price[maxConfigPrice])




#�ĤT�D
price.mean <- aggregate(Retail.Price~Store.Postcode,data=laptop.df, mean)
library(lattice)
barchart(Retail.Price~Store.Postcode, data=price.mean,ylab='Retail Price Mean', xlab='Store PostCode',col="lightblue")

#�ĥ|�D
sd(price.mean$Retail.Price)
IQR(price.mean$Retail.Price)


#�Ĥ��D
boxplot(Retail.Price~Store.Postcode, data=laptop.df, yaxt='n', ylab='���� ($k)')
a<-which(laptop.df$Store.Postcode=='W4 3PH'| laptop.df$Store.Postcode=='N17 6QA') 
#��X���業���̤j�M�̤p����a����m
b<-laptop.df$Retail.Price[a]#��X�̤j�̤p���⩱�������
c<-laptop.df$Store.Postcode[a]
two <- factor(c)#���_���c�A���n�X�{�ѤU14�ӨS���Ψ쪺���a
minmax<-data.frame( "Store"=two, "Price"=b)#�u����̤j�M�̤p��a��
boxplot(Price~Store, data=minmax, col=c(5,6),xlab='Store Postcode', ylab='���� ($k)')#�̤j�M�̤p��box plot
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

#�Ĥ��D
plot(sort(table(laptop.df$Store.Postcode),decreasing = FALSE))
table(laptop.df$Store.Postcode)

#�ĤC�D
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
cor(x=Config,y=CountCon$Freq)
aggregate(Retail.Price~Configuration, data=laptop.df, mean)

#�ĤK�D
sum(laptop.df$Retail.Price)
Storesum = aggregate(laptop.df$Retail.Price,
                        by=list(Country=laptop.df$Store.Postcode), sum)
library(lattice)
barchart(x~Country, data=Storesum, col="lightblue")


seg.aov.own <- aov(Retail.Price ~ Store.Postcode, data=laptop.df) 

anova(seg.aov.own)













