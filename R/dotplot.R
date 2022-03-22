#' @title dotplot
#' @description This function allows you to draw data distribution geographically with EEZ map Function from a numeric vector.
#' @param lona Input the longitude.
#' @param lata Input the latitude.
#' @param map default is "ALL", Other possible options is "PAC", "IND" and "ATL".
#' @param eez default is TRUE, when TRUE show the EEZ map.
#' @param grid default is FALSE, when TRUE show the 5 degree grid.
#' @param color default is "#FF0000", define the color of points.
#' @param size default is 1, define the size of points.
#' @param shape default is 16, define the shape of points.
#' @return the plot of \code{lona} and \code{lata}.
#' @export
#' @import sf
#' @import sp
#' @import rgdal
#' @import broom
#' @import rgeos
#' @import ggplot2
#' @import maps
#' @examples
#' dotplot(141,23)

dotplot <- function(lona,lata,map="ALL",eez=TRUE,grid=FALSE,color="#FF0000", size=1 ,shape=16){
long <- lat <- group <- NULL
file<-data.frame(lona=lona,lata=lata)
eezdata <- get("eez_rg")
world.data<-switch(map, PAC=map_data("world2"), IND=map_data("world2"),ALL=map_data("world2"), ATL=map_data("world"))
latseq <- switch(map, PAC=seq(-50, 50, by=10), IND=seq(-50, 20, by=10), ATL=seq(-50, 50, by=10), ALL=seq(-50, 50, by=10))
latseq2 <- as.character(latseq)
lonseq <- switch(map, PAC=seq(100, 290, by=20), IND=seq(20, 120, by=10), ATL=seq(-70, 20, by=10), ALL=seq(0,360 , by=20))
lonseq2 <- as.character(lonseq)
latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
lonseq2[lonseq < 0] <- paste( abs(lonseq[lonseq < 0]),"W",sep="")
lonseq2[lonseq < 180 & lonseq > 0] <- paste(lonseq[lonseq < 180 & lonseq > 0],"E",sep="")
lonseq2[lonseq > 180] <- paste( abs(360-as.numeric(lonseq[lonseq > 180])), "W",sep="")
lonseq2[lonseq < 0] <- paste( abs(as.numeric(lonseq[lonseq < 0])), "W",sep="")
a0 <- ifelse(grid,1,0)
a1 <- ifelse(eez,1,0)
a2 <- ifelse(eez,"grey",NA)

if(map=="PAC" | map=="ALL"){
 file$lona <- ifelse(file$lona>=0,file$lona,file$lona+360)
 uk_eez_simple <- SpatialPolygonsDataFrame(gSimplify(eezdata, tol = 0.01, topologyPreserve = TRUE), data = eezdata@data)
 uk_eez_df <- tidy(uk_eez_simple)
 uk_eez_df$long<-ifelse(uk_eez_df$long<0,uk_eez_df$long+360,uk_eez_df$long)
 uk_eez_df$long<-ifelse(uk_eez_df$long<1|uk_eez_df$long>359 ,NA,uk_eez_df$long)
 }else{
 uk_eez_df <- eezdata
 }
 plotall<-ggplot()+
          geom_vline(xintercept = seq(min(lonseq),max(lonseq),5),lwd=0.3,col="grey",lty=a0)+
          geom_hline(yintercept = seq(min(latseq),max(latseq),5),lwd=0.3,col="grey",lty=a0)+
		  geom_polygon(aes(x = long, y = lat, group = group), alpha=a1 ,fill="#FFFFBB", color=a2, data=uk_eez_df)+
	      geom_polygon(data=world.data,aes(x=long, y=lat, group=group),fill = "#000000" , colour="#000000", size= 1.2)+
	      geom_polygon(data=world.data,aes(x=long, y=lat, group=group),fill = "#73C000" , colour="#73C000")+
          geom_point(aes(x=lona, y=lata), colour=color, size=size, data=file, pch=shape)+
          coord_cartesian(xlim=c(min(lonseq),max(lonseq)),ylim=c(min(latseq),max(latseq)),expand=0) +
          scale_y_continuous( "", breaks=latseq, labels=latseq2) +
          scale_x_continuous( "", breaks=lonseq, labels=lonseq2) +
          theme(
                axis.text.x=element_text(colour="black", size = 14),
                axis.text.y=element_text(colour="black", size = 14),
                panel.border = element_rect(linetype = "solid", fill = NA),
				panel.background = element_blank()
				)
 return(plotall)
}

