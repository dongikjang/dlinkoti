# sk data

library(data.table)
data02 <- fread("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201702.txt", sep="|",
	colClasses = c("character", "character", "character", "integer", "integer"))
data02
save(data02, file="/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201702.RData")
rm(data02)
gc(rese=TRUE)
############
data03 <- fread("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201703.txt", sep="|",
	colClasses = c("character", "character", "character", "integer", "integer"))
data03
save(data03, file="/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201703.RData")
rm(data03)
gc(rese=TRUE)
############
data04 <- fread("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201704.txt", sep="|",
	colClasses = c("character", "character", "character", "integer", "integer"))
data04
save(data04, file="/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201704.RData")
rm(data04)
gc(rese=TRUE)
############
data05 <- fread("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201705.txt", sep="|",
	colClasses = c("character", "character", "character", "integer", "integer"))
data05
save(data05, file="/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201705.RData")
rm(data05)
gc(rese=TRUE)
############

load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201702.RData")

tmp <- subset(data02, LINK_ID == 1000000301)
library(doBy)
tmp <- orderBy( ~ DATE + HOUR, data = tmp)

par(mfrow=c(2,1))
plot(strptime(paste(tmp$DATE, tmp$HOUR), format = "%Y%m%d %H"), tmp$SPD, type="l",
	xlab="Time", ylab="Speed(km/H)")
plot(strptime(paste(tmp$DATE, tmp$HOUR), format = "%Y%m%d %H"), tmp$VOL, type="l",
	xlab="Time", ylab="Traffic volume")



library(RMySQL)
con <- dbConnect(MySQL(), user="root", password="*Yeoju0917*", dbname="SKTmap", host="localhost")
library(data.table)

load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201702.RData")
dbWriteTable(con, "SKTmap_2017_02_05", data02, overwrite = TRUE, row.names=FALSE,
			field.types=list(DATE = "VARCHAR(8)", HOUR = "VARCHAR(2)", LINK_ID = "VARCHAR(10)", SPD = "INT(3)", VOL = "INT(5)"))
rm(data02)
load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201703.RData")
dbWriteTable(con, "SKTmap_2017_02_05", data03, append=TRUE, row.names=FALSE, overwrite=FALSE)
rm(data03)
load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201704.RData")
dbWriteTable(con, "SKTmap_2017_02_05", data04, append=TRUE, row.names=FALSE, overwrite=FALSE)
rm(data04)
load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201705.RData")
dbWriteTable(con, "SKTmap_2017_02_05", data05, append=TRUE, row.names=FALSE, overwrite=FALSE)
rm(data05)

gc(rese=TRUE)



library(foreign)
linkval <- read.dbf("/Users/jang/Downloads/2017-02-06 전국표준노드링크 3/MOCT_LINK.dbf")
linkid <- as.character(linkval$LINK_ID)

linkidval <- paste(paste("LINK_ID = '", linkid, "'", sep=""), collapse=" OR ")

##########################################################################################################################################################
library(RMySQL)
con <- dbConnect(MySQL(), user="root", password="*Yeoju0917*", dbname="SKTmap", host="localhost")
library(data.table)


res <- dbSendQuery(con, "SELECT distinct LINK_ID FROM SKTmap_2017_02_05")
uid <- dbFetch(res, n = Inf)
dbClearResult(res)
uid <- uid$LINK_ID

linkid[linkid %in% uid]

res <- dbSendQuery(con, paste("SELECT * FROM SKTmap_2017_02_05 WHERE",  linkidval))
val <- dbFetch(res, n = Inf)
dbClearResult(res)

save(val, file="/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/종합운동장주변.Rdata")


library(reshape2)
val$TIME <- paste(val$DATE, val$HOUR, sep="")
tmp2 <- dcast(val, LINK_ID ~ TIME, value.var="SPD")
length(unique(val$LINK_ID))
rownames(tmp2) <- tmp2$LINK_ID
tmp2$LINK_ID <- NULL

library(fields)
png("종합운동장.png", width= 2000, height=800)
image.plot(list(x = 1:ncol(tmp2), y = 1:nrow(tmp2), z=t(tmp2)), xlab="TIME", ylab="Link ID")
dev.off()

library(fields)
tmp3 <- tmp2[, 1:(24*7)]
png("종합운동장_7일.png", width= 2000, height=2000)
image.plot(list(x = 1:ncol(tmp3), y = 1:nrow(tmp3), z=t(tmp3)), xlab="TIME", ylab="Link ID")
dev.off()


library(fields)
tmp3 <- tmp2[, substring(colnames(tmp2), 1, 8) == "20170331"]
png("종합운동장_0331.png", width= 1000, height=10000)
image.plot(list(x = 1:ncol(tmp3), y = 1:nrow(tmp3), z=t(tmp3)), xlab="TIME", ylab="Link ID")
dev.off()


library(rgdal)
library(sp)
library(maptools)
proj4val <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs"
nc <- readShapeLines("/Users/jang/Downloads/2017-02-06 전국표준노드링크 3/MOCT_LINK.shp", proj4string=CRS(proj4val))


library(dplyr)
tmp3 <- dcast(val, LINK_ID ~ TIME, value.var="SPD")

ncdata <- left_join(nc@data, tmp3, by="LINK_ID")
sum(nc@data$LINK_ID != ncdata$LINK_ID)
dim(ncdata)
nc@data <- data.frame(nc@data, ncdata[, (ncol(nc@data) + 1) : ncol(ncdata)])

source("/Volumes/PromisePegasus/GitHub/NaverMap/val2col.R")
#zlim <- range(tmp3[,-1], na.rm=TRUE)
zlim <- range(tmp3[,substring(colnames(tmp2), 1, 8) == "20170401"], na.rm=TRUE)
for(ii in 0:23){
	timeval <- ifelse(ii < 10, paste("0", ii, sep=""), ii)
	eval(parse(text = paste("out <- nc@data$X20170401", timeval, sep="")))
	out[out > 60] <- 60
	png(paste("종합운동장_0401", timeval, ".png", sep=""), width= 1000, height=600)
	par(mar=c(0,0,0,0))
	plot(c(200500, 212500), c(542000, 550000), type="n", axes=FALSE, xlab="", ylab="")
	plot(nc, col= val2col(out, zlim =  zlim, col= rev(tim.colors(64))), add=TRUE)
	legend("topright", legend=paste("2017-04-01", timeval), cex=3, text.font=2, x.intersp = 0, box.col=NA)
	dev.off()
}



library(fields)
tmp2 <- dcast(val, LINK_ID ~ TIME, value.var="VOL")

approx2 <- function(시간, 전체평균속도, xout){
  approx(시간, 전체평균속도, xout= xout,
           yleft = mean(전체평균속도, na.rm=TRUE), 
           yright = mean(전체평균속도, na.rm=TRUE) )$y
}

x <- 2:ncol(tmp2) - 1
for(ii in 1:nrow(tmp2)){
	y <- as.vector(unlist(tmp2[ii, 2:ncol(tmp2)]))
	if(sum(is.na(y)) > 0){
		yhat <- approx2(x, y, x)
		tmp2[ii, 2:ncol(tmp2)] <- yhat
	}
}

#tmp3 <- tmp2[, substring(colnames(tmp2), 1, 8) == "20170401" | substring(colnames(tmp2), 1, 8) == "20170331"]
tmp3 <- tmp2[, colnames(tmp2)!="LINK_ID"]
#tmp3[tmp3 > 80] <- 80


png("종합운동장_0401.png", width= 1000, height=10000)
image.plot(list(x = 1:ncol(tmp3), y = 1:nrow(tmp3), z=t(tmp3)), xlab="TIME", ylab="Link ID")
dev.off()


library(fields)
#tmp3 <- tmp2[, substring(colnames(tmp2), 1, 8) == "20170401" | substring(colnames(tmp2), 1, 8) == "20170331"]
tmp3 <- tmp2[, colnames(tmp2)!="LINK_ID"][, 1:(24*14)]
#tmp3[tmp3 > 80] <- 80
png("종합운동장_0401_2.png", width= 1000, height=600)
matplot(t(tmp3), xlab="TIME", type="l")
dev.off()


library(funFEM)

tmp4 <- t(apply(tmp3, 1, function(x) {
	dval <- diff(range(x, na.rm=TRUE))
	if(dval > 0){
		#diff((x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE)) )
		(x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE))
	} else{
		rep(0.5, length(x))
		#rep(0, length(x) - 1)
	}
}))


basis <- create.fourier.basis(c(0, 336), nbasis=25)

tmp3 <- tmp3[which(apply(tmp3,1, sd) > 0), ]
fdobj <- smooth.basis(1:336, t(tmp3),basis)$fd
d2fdobj <- deriv.fd(fdobj , 2)
plot(d2fdobj)

rmind <- which(apply(d2fdobj$coefs, 2, sd) < 0.1)
tmp3 <- tmp3[-rmind, ]
fdobj <- smooth.basis(1:336, t(tmp3),basis)$fd
d2fdobj <- deriv.fd(fdobj , 2)


# Clustrering with FunFEM
#res = funFEM(fdobj, K=2:20, model="all", init="kmeans", lambda=.1, disp=TRUE)
debugonce(funFEM)
res = funFEM(d2fdobj, K=5, model="AkjBk", init="kmeans", lambda=.1, disp=TRUE)

#The best model is AkjBk with K = 20 ( bic = 33919.73 )


fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans, col=1:res$K, xaxt='n',lwd=2)
axis(1, at=seq(1,336, 24), labels=colnames(tmp3)[seq(1,336,24)], las=2)


matplot(t(tmp3[res$cls == 6, ]), type="l", col=6, ylim=c(0, 1000))
matlines(t(tmp3[res$cls == 5, ]), type="l", col=5)
matlines(t(tmp3[res$cls == 4, ]), type="l", col=4)
matlines(t(tmp3[res$cls == 3, ]), type="l", col=3)
matlines(t(tmp3[res$cls == 2, ]), type="l", col=2)
matlines(t(tmp3[res$cls == 1, ]), type="l", col=1)

library(data.table)
load("/Volumes/PromisePegasus/Preject/2017/NIER/교통량/SK/TI_HIST_KS_1H_201702_05/TI_HIST_KS_1H_201702.RData")




ulinkid <- unique(data02$LINK_ID)

tmp <- subset(data02, LINK_ID %in% ulinkid[1:30])
tmp$TIME <- paste(tmp$DATE, tmp$HOUR, sep="")

library(reshape2)
tmp2 <- dcast(tmp, LINK_ID ~ TIME, value.var="SPD")

rownames(tmp2) <- tmp2$LINK_ID
tmp2$LINK_ID <- NULL

x <- strptime(colnames(tmp2), format = "%Y%m%d%H")
y <- seq(min(tmp2, na.rm=TRUE), max(tmp2, na.rm=TRUE),,ncol(tmp2))
plot(x,y, type="n")
matlines(matrix(as.numeric(strptime(colnames(tmp2), format = "%Y%m%d%H")), ncol=1), t(tmp2[1:10,]), type="l")


library(rnn)
slicef <- function(xx,  lstm_num_timesteps = 5){
	#lstm_num_timesteps <- 5
	trend_train <- xx
	xout <- 1:length(trend_train)

	nmiss <- length(sum(is.na(trend_train)))
	if(nmiss > 0){
		missind <- which(is.na(trend_train))
		trend_train[missind] <- approx(xout, trend_train, xout)$y[missind]
	}
	return(list(X = t(sapply(1:(length(trend_train) - lstm_num_timesteps), function(x) trend_train[x:(x + lstm_num_timesteps - 1)])),
				Y = sapply((lstm_num_timesteps + 1):(length(trend_train)), function(x) trend_train[x])))


}
dim(tmp2)

val <- slicef(as.vector(unlist(tmp2[1,])))
X_train <- array(NA, c(nrow(val), ncol(val), nrow(tmp2)))
for(ii in 2:nrow(tmp2)){

	X_train[,,ii] <- slicef(as.vector(unlist(tmp2[ii,])))
}


