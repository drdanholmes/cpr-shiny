#    cp-R: A Graphical User Interface to R for Clinical Chemists
#    Copyright (C) 2014  Daniel T. Holmes, MD
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#Deming Regression Script
#add packages if not already installed
list.of.packages <- c("car","boot","ggplot2","plotly","htmlwidgets")

options(menu.graphics=FALSE)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='http://cran.us.r-project.org')
library("car")
library("boot")
library("ggplot2")
library("plotly")
library("htmlwidgets")

args=(commandArgs(TRUE))
tmpdir=eval(parse(text=args))


#Deming regession function from: Linnet, Statistics in Medicine, VOL 9, 1463-73, 1990
Deming.reg<-function(data,delta,alpha,indices=1:nrow(data)){
    #The data is selected by the set of indices that is passed
    d<-data[indices,]
    #The data for the Deming function is then taken from the resampled set, d
    x<-d[,1]
    y<-d[,2]
    w<-d[,3]
    #Linnet denotes the variance ratio var(x)/var(y) as lambda. Others denote the variance ratio var(y)/var(x) as delta
	lambda<-1/delta
    n<-length(x)
	xbarw<-as.numeric(w%*%x/sum(w))
	ybarw<-as.numeric(w%*%y/sum(w))
	uw<-sum(w*(x-xbarw)^2)
	pw<-sum(w*(x-xbarw)*(y-ybarw))
	qw<-sum(w*(y-ybarw)^2)
	b<-((lambda*qw-uw)+sqrt((uw-lambda*qw)^2+4*lambda*pw^2))/(2*lambda*pw)
	#this variable that I denote "a" is denoted "a0" in Linnet's paper
	a<-ybarw-b*xbarw
	return(c(a,b))
}

#Calculate the weights and fitted values
weight.calculator<-function(data,delta,weighting){
	x<-data$x
	y<-data$y
	w.new<-rep(1,length(x))
	#Linnet denotes the variance ratio var(x)/var(y) as lambda. Others denote the variance ratio var(y)/var(x) as delta
	lambda<-1/delta
	n<-length(x)
	max.ratio<-1
	if(weighting==TRUE){
		while (max.ratio>10^(-5)){
			w.old<-w.new
			dem.coeff<-Deming.reg(data=data.frame(x,y,w=w.new),delta=delta,indices=1:n)
			a<-dem.coeff[1]
			b<-dem.coeff[2]
			dist<-y-(a+b*x)
			Xhat<-x+lambda*b*dist/(1+lambda*b^2)
			Yhat<-y-dist/(1+lambda*b^2)
			w.new<-1/((Xhat+Yhat)/2)^2
			max.ratio<-abs(max((w.old-w.new)/w.old))
			}
	}else{
		dem.coeff<-Deming.reg(data=data.frame(x,y,w=w.new),delta=delta,indices=1:n)
		a<-dem.coeff[1]
		b<-dem.coeff[2]
		dist<-y-(a+b*x)
		Xhat<-x+lambda*b*dist/(1+lambda*b^2)
		Yhat<-y-dist/(1+lambda*b^2)
	}
	weights<-data.frame(cbind(w.new,Xhat,Yhat))
	names(weights)<-c("w","Xhat","Yhat")
	return(weights)
}

#Determine bootstrapped CIs for the intercept and slope
Deming.boot<-function(data,delta,R,n.fit,alpha,xmin,xmax){
    boot.results<-boot(data=data,delta=delta,alpha=alpha,statistic=Deming.reg, R=R)
    valid<-complete.cases(boot.results$t)
    boot.results$t<-boot.results$t[valid,,drop=FALSE]
    boot.results$R<-sum(valid)
    a.ci<-boot.ci(boot.results, type="bca",index=1) # intercept
    b.ci<-boot.ci(boot.results, type="bca",index=2) # slope
    a.vector<-boot.results$t[,1]
    b.vector<-boot.results$t[,2]
    x.points<-seq(xmin-0.1*abs(xmax-xmin),xmax+0.1*abs(xmax-xmin),length.out=n.fit)
    y.points<-array(dim=R)
    reg.CI.data<-matrix(data="NA",nrow=n.fit,ncol=3)
    for (i in 1:n.fit){
            for (j in 1:R){
    #Determine the intersections of all bootstrapped regression lines with vertical line x=a
            y.points[j]<-a.vector[j]+b.vector[j]*x.points[i]
            }
    #Determine the central (1-alpha)/2 quantiles
    lower.y<-quantile(y.points,probs=alpha/2,na.rm=TRUE)
    upper.y<-quantile(y.points,probs=(1-alpha/2),na.rm=TRUE)
    reg.CI.data[i,1:3]<-c(x.points[i],lower.y,upper.y)
    }
    return(list(reg.CI.data=reg.CI.data,a.ci=a.ci,b.ci=b.ci))
}

# A couple of functions for converting percent opacity to colour hex codes
addazero<-function(rgbvalue){
    if(nchar(rgbvalue)==1){
    rgbvalue<-sprintf("0%s",rgbvalue)  
    }
    return(rgbvalue)    
}

col2hex<-function(colorname){
    rgbcol<-col2rgb(colorname)
    r<-rgbcol[1]
    class(r)<-"hexmode"
    b<-rgbcol[2]
    class(b)<-"hexmode"
    g<-rgbcol[3]
    class(g)<-"hexmode"
    rval<-addazero(as.character(r))
    bval<-addazero(as.character(b))
    gval<-addazero(as.character(g))
    hexcolstring<-paste("#",rval,bval,gval,sep="")
    return(hexcolstring)
}

#Draw the regression and the Bland Altman plot
plot.graph<-function(data,R,n.fit,alpha,pp){
    #Perform the regression
    reg<-Deming.reg(data=data,delta=pp$delta,alpha=alpha)
    a<-reg[1]
    b<-reg[2]
    x<-data[,1]
    y<-data[,2]
    w<-data[,3]
    Xhat<-data[,4]
    Yhat<-data[,5]
    
    plot(x,y,pch=pp$my_pch,cex=pp$my_cex,col=as.character(pp$my_point_col),bg=as.character(pp$my_bg),lty=pp$my_lty,lwd=pp$my_lwd,
    xlim=c(pp$xmin,pp$xmax),ylim=c(pp$ymin,pp$ymax),xlab=as.character(pp$my_xlab),ylab=as.character(pp$my_ylab),
    main=as.character(pp$my_main))

    #Perform bootstrapping to determine the confidence intervals of regression coefficients
    boot.results<-Deming.boot(data=data,delta=pp$delta,R=R,n.fit=n.fit,alpha=alpha,xmin=pp$xmin,xmax=pp$xmax)
    reg.CI.data<-boot.results$reg.CI.data
    a.ci<-boot.results$a.ci
    b.ci<-boot.results$b.ci
    #In Linnet's paper, lambda is 1/delta, that is, lambda = var(x)/vary(y)
    lambda<-1/pp$delta
    resid<-sign(y-Yhat)*sqrt(w*(x-Xhat)^2+w*lambda*(y-Yhat)^2)
    resid.raw<-sign(y-Yhat)*sqrt((x-Xhat)^2+lambda*(y-Yhat)^2)
    fitted.values<-as.data.frame(cbind(Xhat,Yhat))
    reg.list<-list(intercept=reg[1],slope=reg[2],CI.intercept=a.ci,CI.slope=b.ci,resid=resid,resid.raw=resid.raw,fitted=fitted.values,reg.CI.data=reg.CI.data)
    #Create a flag to handle a bug in ps and pdf display
    transp_flag<-(pp$confidence_transp>90)
    #Convert opacity percent to 0-255 scale
    confidence_transp<-round((pp$confidence_transp*2.55),0)
    #Convert opacity to hexidecimal
    class(confidence_transp)<-"hexmode"
    #Handle hex values that have a leading 0 chopped off
    cftrans<-addazero(as.character(confidence_transp))
    cfcol<-col2hex(as.character(pp$confidence_fill_col))
    cffill<-paste(cfcol,cftrans,sep="")

    #Plot the confidence band if requested
    if(pp$plot_confidence){
        #Colour in the confidence band with polygons.
        for (i in 1:n.fit){
            if (i<n.fit) {
                    vx<-c(reg.CI.data[i,1],reg.CI.data[i,1],reg.CI.data[i+1,1],reg.CI.data[i+1,1])
                    vy<-c(reg.CI.data[i,2],reg.CI.data[i,3],reg.CI.data[i+1,3],reg.CI.data[i+1,2])
                    #This partially handles a little bug in polygon going over to ps and pdf
                    #Some levels of opacity are either going to have holes or stripes...no escapin' it
                    if((pp$plot_type=="ps"|pp$plot_type=="pdf")&transp_flag){
                        polygon(vx,vy,fillOddEven=FALSE,col=cffill,border=cffill)
                    }else{
                        polygon(vx,vy,fillOddEven=FALSE,col=cffill,border=NA)
                    }
            }
        }
        #Outline the confidence hand if requested
        if(pp$plot_conf_outline){
            lines(reg.CI.data[,1],reg.CI.data[,2],col=as.character(pp$conf_col),lwd=pp$my_lwd,lty=2)
            lines(reg.CI.data[,1],reg.CI.data[,3],col=as.character(pp$conf_col),lwd=pp$my_lwd,lty=2)
        }
    }

    #Replot the points because the confidence band covered it over
    points(x,y,pch=pp$my_pch,cex=pp$my_cex,col=as.character(pp$my_point_col),bg=as.character(pp$my_bg), lwd=pp$my_lwd)
    #Plot the regression
    abline(a,b,lty=pp$my_lty,lwd=pp$my_lwd,col=as.character(pp$my_lincol))
    #Plot the line of identity if requested
    if  (pp$plot_identity){
    abline(0,1,lwd=pp$my_lwd,lty=2,col="red")
    }

    #Plot the regression question and coefficent of determination if reqested
    #Round to 2 decimals and make sure that the terminal zeros display on the graph
    intercept<-sprintf("%.2f",round(a,2))
    slope<-sprintf("%.2f",round(b,2))
    if(as.numeric(intercept)>0){
        equation<-paste("y=",slope,"x+",intercept,sep="")
    }else{
        equation<-paste("y=",slope,"x-",abs(as.numeric(intercept)),sep="")
    }
    rho<-cor.test(y,x)$estimate
    rsquared.equation=bquote(R^2*"="*.(round(rho^2,digits=4)))
    rsquared.equation=bquote(R^2*"="*.(round(rho^2,digits=4)))
	legend.location.1=c(pp$xmin,pp$ymin+0.98*abs(pp$ymax-pp$ymin))
    legend.location.2=c(pp$xmin,pp$ymin+0.92*abs(pp$ymax-pp$ymin))
    legend.location.3=c(pp$xmin,pp$ymin+0.86*abs(pp$ymax-pp$ymin))
	if (pp$plot_regression&pp$plot_rsquared&pp$plot_method){
		text(legend.location.1[1],legend.location.1[2],equation,adj=c(0,0))
        text(legend.location.2[1],legend.location.2[2],rsquared.equation,adj=c(0,0))
        if (pp$weighting==TRUE){
			text(legend.location.3[1],legend.location.3[2],"Method: Deming, weighted",adj=c(0,0))
		}else{
			text(legend.location.3[1],legend.location.3[2],"Method: Deming",adj=c(0,0))		
		}
    }else if(pp$plot_regression&pp$plot_rsquared){
        text(legend.location.1[1],legend.location.1[2],equation,adj=c(0,0))
        text(legend.location.2[1],legend.location.2[2],rsquared.equation,adj=c(0,0))
    }else if (pp$plot_regression&pp$plot_method){
        text(legend.location.1[1],legend.location.1[2],equation,adj=c(0,0))
		if (pp$weighting==TRUE){
			text(legend.location.2[1],legend.location.2[2],"Method: Deming, weighted",adj=c(0,0))
		}else{
			text(legend.location.2[1],legend.location.2[2],"Method: Deming",adj=c(0,0))
		}
    }else if (pp$plot_rsquared&pp$plot_method){
        text(legend.location.1[1],legend.location.1[2],rsquared.equation,adj=c(0,0))
		if (pp$weighting==TRUE){        
			text(legend.location.2[1],legend.location.2[2],"Method: Deming, weighted",adj=c(0,0))
		}else{
			text(legend.location.2[1],legend.location.2[2],"Method: Deming",adj=c(0,0))
		}
	}else if (pp$plot_rsquared){
        text(legend.location.1[1],legend.location.1[2],rsquared.equation,adj=c(0,0))
    }else if(pp$plot_regression){
        text(legend.location.1[1],legend.location.1[2],equation,adj=c(0,0))
 	}else if(pp$plot_method){
		if (pp$weighting==TRUE){     
			text(legend.location.1[1],legend.location.1[2],"Method: Deming,weighted",adj=c(0,0))
		}else{
			text(legend.location.2[1],legend.location.2[2],"Method: Deming",adj=c(0,0))		
		}
    }
    return(reg.list)
}

plot.resid<-function(fitted,resid,pp){
    #convert tranparency percent to 0-255 scale
    confidence_transp<-round((pp$confidence_transp*2.55),0)
    #convert transparency to hexidecimal
    class(confidence_transp)<-"hexmode"
    cftrans<-addazero(as.character(confidence_transp))
    cfcol<-col2hex(as.character(pp$confidence_fill_col))
    cffill<-paste(cfcol,cftrans,sep="")
    #define the plot region
    par(mfrow=c(1,2))
    resid.bar<-mean(resid)
    resid.sd<-sd(resid)
    z.score<-(resid-resid.bar)/resid.sd
    if(pp$weighting==TRUE){
		title.hist<-"Histogram of Weighted Residuals"
    }else{
		title.hist<-"Histogram of Residuals"
    }
    hist(z.score,breaks=10,main=title.hist,xlab="Standardized Residuals",col=as.character(cffill),prob=TRUE)
    lines(density(z.score),col=as.character(pp$my_lincol),lwd=pp$my_lwd)
    if(pp$weighting==TRUE){
		title.resid<-"Weighted Residuals Plot"
    }else{
		title.resid<-"Residuals Plot"
    }  
    plot(fitted$Xhat,z.score,ylim=c(-3*sd(z.score),3*sd(z.score)),xlab="Fitted Values",
    ylab="Standardized Residuals",main=title.resid,pch=pp$my_pch,bg=as.character(pp$my_bg),
    col=as.character(pp$my_point_col),cex=pp$my_cex)
    abline(h=mean(z.score),col=as.character(pp$my_lincol),lwd=as.character(pp$my_lwd))
    if(pp$plot_confidence){
        vx<-c(-2,-2,2,2)*max(abs(fitted$Xhat))
        vy<-c(-1.96,1.96,1.96,-1.96)*sd(z.score)
        polygon(vx,vy,fillOddEven=FALSE,col=cffill,border=NA)
        points(fitted$Xhat,z.score,ylim=c(-max(abs(z.score)),max(abs(z.score))),lwd=pp$my_lwd,pch=pp$my_pch,
        bg=as.character(pp$my_bg),col=as.character(pp$my_point_col),cex=pp$my_cex)
    }
    if(pp$plot_conf_outline){
    abline(h=(mean(z.score)+1.96*sd(z.score)),lty=2,col=as.character(pp$conf_col),lwd=as.character(pp$my_lwd))
    abline(h=(mean(z.score)-1.96*sd(z.score)),lty=2,col=as.character(pp$conf_col),lwd=as.character(pp$my_lwd))
    }
    par(mfrow=c(1,1))
}

plot.qq<-function(resid,pp){
        #this is a bit of a work-around to make something that fits the aesthetic theme of the plots
        qqdata<-qqnorm(reg$resid,plot.it=FALSE)
		if(pp$weighting==TRUE){
			title.qq<-"QQ Plot of Weighted Residuals"
		}else{
			title.qq<-"QQ Plot of Residuals"
		}  
        qqPlot(reg$resid,main=title.qq,ylab="Residuals",xlab="Normal Quantiles",
        col=as.character(pp$my_point_col),col.lines=as.character(pp$my_lincol),pch=pp$my_pch,cex=pp$my_cex,lwd=pp$my_lwd)
        points(qqdata$x,qqdata$y,col=as.character(pp$my_point_col),pch=pp$my_pch,cex=pp$my_cex,bg=as.character(pp$my_bg),lwd=pp$my_lwd)
        qqline(reg$resid,col=as.character(pp$my_lincol),lwd=pp$my_lwd)
}

#-----Program starts here-----#

#Read in the regression data
my.data<-read.csv(file=paste(tmpdir,"/Rdata/regression_data.csv",sep=""),header=FALSE,sep="\t")
names(my.data)<-c("x","y")

#Convert non-numeric results to NA
my.data$x<-as.numeric(as.character(my.data$x))
my.data$y<-as.numeric(as.character(my.data$y))

#Get rid of rows with NAs completely
raw.data <- my.data
my.data <- my.data[complete.cases(my.data),]

#Read in the plot parameters, denoted pp
pp<-read.csv(file=paste(tmpdir,"/Rdata/plot_parameters.csv",sep=""),header=TRUE,sep="\t",quote="\"")

#Convert the Python Trues and Falses to R TRUEs and FALSEs
bool_col <- function(x) { if (length(x) == 0 || is.null(x)) FALSE else as.logical(toupper(x)) }
pp$plot_regression=bool_col(pp$plot_regression)
pp$plot_rsquared=bool_col(pp$plot_rsquared)
pp$plot_method=bool_col(pp$plot_method)
pp$plot_identity=bool_col(pp$plot_identity)
pp$plot_confidence=bool_col(pp$plot_confidence)
pp$plot_conf_outline=bool_col(pp$plot_conf_outline)
pp$plot_difference_abs=bool_col(pp$plot_difference_abs)
pp$weighting=bool_col(pp$weighting)

#Deal with empty x and y plotting limits
if (is.na(pp$xmin)){
    pp$xmin=min(my.data$x,na.rm=TRUE)
}
if (is.na(pp$xmax)){
    pp$xmax=max(my.data$x,na.rm=TRUE)
}
if (is.na(pp$ymin)){
    pp$ymin=min(my.data$y,na.rm=TRUE)
}
if (is.na(pp$ymax)){
    pp$ymax=max(my.data$y,na.rm=TRUE)
}

#Set parameters for bootstrapping
R<-1000
n.fit<-50
alpha<-0.05

#Get the weights to begin with. Fitted values Xhat and Yhat are also determined
w<-weight.calculator(data=my.data,delta=pp$delta,weighting=pp$weighting)

#append the appropriate weights and the values of Xhat and Yhat to the data
my.data<-data.frame(my.data,w)


if(Sys.info()['sysname'] == 'Windows'|Sys.info()['sysname'] == 'Linux'){
	#Create the preview plot(s)
	jpeg(paste(tmpdir,"/previews/A.jpg",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, units="in", res=pp$my_dpi)
	reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
	dev.off()

	jpeg(paste(tmpdir,"/previews/C.jpg",sep=""),width=pp$my_width,height=pp$my_height,
	pointsize=pp$font_mult,units="in", res=pp$my_dpi)
	plot.qq(reg$resid,pp)
	dev.off()

	jpeg(paste(tmpdir,"/previews/D.jpg",sep=""),width=1.5*pp$my_width,height=pp$my_height,
	pointsize=pp$font_mult,units="in", res=pp$my_dpi)
	plot.resid(reg$fitted,reg$resid,pp)
	dev.off()
  
	#Output jpeg
	if(pp$plot_type=="jpg"){
		jpeg(paste(tmpdir,"/plots/plot.jpg",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, units="in", res=pp$my_dpi)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		jpeg(paste(tmpdir,"/plots/qq_plot.jpg",sep=""),width=pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.qq(reg$resid,pp)
		dev.off()


		jpeg(paste(tmpdir,"/plots/hist.jpg",sep=""),width=1.5*pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()

		reg
  
	#Output png
	}else if(pp$plot_type=="png"){
		png(paste(tmpdir,"/plots/plot.png",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, units="in", res=pp$my_dpi)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		png(paste(tmpdir,"/plots/qq_plot.png",sep=""),width=pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.qq(reg$resid,pp)
		dev.off()

		png(paste(tmpdir,"/plots/hist.png",sep=""),width=1.5*pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()

		reg
  
	#Output tiff
	}else if(pp$plot_type=="tiff"){
		tiff(paste(tmpdir,"/plots/plot.tiff",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, units="in", res=pp$my_dpi)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		tiff(paste(tmpdir,"/plots/qq_plot.tiff",sep=""),width=pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.qq(reg$resid,pp)
		dev.off()

		tiff(paste(tmpdir,"/plots/hist.tiff",sep=""),width=1.5*pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()

		reg
  
	#Output bmp
	}else if(pp$plot_type=="bmp"){
		bmp(paste(tmpdir,"/plots/plot.bmp",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, units="in", res=pp$my_dpi)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		bmp(paste(tmpdir,"/plots/qq_plot.bmp",sep=""),width=pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.qq(reg$resid,pp)
		dev.off()

		bmp(paste(tmpdir,"/plots/hist.bmp",sep=""),width=1.5*pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult,units="in", res=pp$my_dpi)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()

		reg
  
	#Output pdf
	}else if(pp$plot_type=="pdf"){
		pdf(paste(tmpdir,"/plots/plot.pdf",sep=""),width=pp$my_width,height=pp$my_height,pointsize=pp$font_mult)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		pdf(paste(tmpdir,"/plots/qq_plot.pdf",sep=""),width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult)
		plot.qq(reg$resid,pp)
		dev.off()

		pdf(paste(tmpdir,"/plots/hist.pdf",sep=""),width=1.5*pp$my_width,height=pp$my_height, pointsize=pp$font_mult)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()

		reg
  
	#Output ps
	}else if(pp$plot_type=="ps"){
		cairo_ps(paste(tmpdir,"/plots/plot.ps",sep=""),width=pp$my_width,height=pp$my_height,pointsize=pp$font_mult)
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		cairo_ps(paste(tmpdir,"/plots/qq_plot.ps",sep=""),width=pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult)
		plot.qq(reg$resid,pp)
		dev.off()

		cairo_ps(paste(tmpdir,"/plots/hist.ps",sep=""),width=1.5*pp$my_width,height=pp$my_height,
		pointsize=pp$font_mult)
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()
		
		reg
	}

}else{
	#It's Mac
	quartz(file=paste(tmpdir,"/previews/A.jpg",sep=""),type="jpg",width=pp$my_width,height=pp$my_height, pointsize=pp$font_mult, dpi=pp$my_dpi)
	reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
	dev.off()

	#By deafault R calculates the unweighted residuals and rse. I will report the weighted values.
	resid<-sqrt(w)*reg$resid

	quartz(file=paste(tmpdir,"/previews/C.jpg",sep=""),type="jpg",width=pp$my_width,height=pp$my_height, 
		 pointsize=pp$font_mult, dpi=pp$my_dpi)    
	plot.qq(reg$resid,pp)
	dev.off()

	quartz(file=paste(tmpdir,"/previews/D.jpg",sep=""),type="jpg",width=1.5*pp$my_width,height=pp$my_height,
		 pointsize=pp$font_mult, dpi=pp$my_dpi,bg="white")    
	plot.resid(reg$fitted,reg$resid,pp)
	dev.off()     
	#create finalized plots
	if(pp$plot_type=="ps"){
        cairo_ps(file=paste(tmpdir,"/plots/plot.ps",sep=""),width=pp$my_width,height=pp$my_height,pointsize=pp$font_mult,bg="white")
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		cairo_ps(file=paste(tmpdir,"/plots/qq_plot.ps",sep=""),width=pp$my_width,height=pp$my_height,pointsize=pp$font_mult,bg="white")
		plot.qq(resid,pp)
		dev.off()

		cairo_ps(file=paste(tmpdir,"/plots/hist.ps",sep=""),width=pp$my_width,height=pp$my_height,pointsize=pp$font_mult,bg="white")
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()		
	}else{
		quartz(file=paste(tmpdir,"/plots/plot.",pp$plot_type,sep=""),type=as.character(pp$plot_type),width=pp$my_width,height=pp$my_height, dpi=pp$my_dpi,pointsize=pp$font_mult,bg="white")
		reg<-plot.graph(data=my.data,R=R,n.fit=n.fit,alpha=alpha,pp=pp)
		dev.off()

		quartz(file=paste(tmpdir,"/plots/qq_plot.",pp$plot_type,sep=""),type=as.character(pp$plot_type),width=pp$my_width,height=pp$my_height, dpi=pp$my_dpi,pointsize=pp$font_mult,bg="white")
		plot.qq(resid,pp)
		dev.off()

		quartz(file=paste(tmpdir,"/plots/hist.",pp$plot_type,sep=""),type=as.character(pp$plot_type),width=pp$my_width,height=pp$my_height, dpi=pp$my_dpi,pointsize=pp$font_mult,bg="white")
		plot.resid(reg$fitted,reg$resid,pp)
		dev.off()
	}
}

#Write Statistical Summary to a text file
n=length(my.data$x)
intercept<-round(as.numeric(reg$intercept),3)
slope<-round(as.numeric(reg$slope),3)
rse.weighted<-round(sqrt((sum(reg$resid^2))/(n-2)),3)
rse.raw<-round(sqrt((sum(reg$resid.raw^2))/(n-2)),3)
CI.int<-reg$CI.intercept
upper.int<-round(CI.int$bca[5],3)
lower.int<-round(CI.int$bca[4],3)
CI.slope<-reg$CI.slope
upper.slope<-round(CI.slope$bca[5],3)
lower.slope<-round(CI.slope$bca[4],3)
r.squared<-round(cor(my.data$x,my.data$y)^2,6)
deg.free<-n-2

if (pp$weighting==TRUE){
	write("Regression Summary\n\nMethod: Deming, weighted",file=paste(tmpdir,"/plots/stats_output.txt",sep=""))
}else{
	write("Regression Summary\n\nMethod: Deming",file=paste(tmpdir,"/plots/stats_output.txt",sep=""))
}
write(paste("Number of complete cases: ",n,"\n",sep=""), file=paste(tmpdir,"/plots/stats_output.txt",sep=""), append=TRUE)
write(paste("Intercept: ",intercept,sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
write(paste("CI Intercept: [",lower.int,",",upper.int,"]\n",sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
write(paste("Slope: ",slope,sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
write(paste("CI Slope: [",lower.slope,",",upper.slope,"]\n",sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
write(paste("Residual Standard Error: ",rse.raw," on ",deg.free," degrees of freedom",sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
if(pp$weighting==TRUE){
	write(paste("(Weighted RSE: ",rse.weighted,")\n",sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)
}
write(paste("R-squared: ",r.squared,sep=""),file=paste(tmpdir,"/plots/stats_output.txt",sep=""),append=TRUE)

#Write the data file for reloading the plot
reg_method<-data.frame("Deming")
names(reg_method)<-"reg_method"
pp<-cbind(pp,reg_method)
t.pp<-t(pp)
row_names<-row.names(t.pp)
t.pp<-as.data.frame(sapply(t.pp,gsub,pattern="TRUE",replacement="True"))
t.pp<-as.data.frame(sapply(t.pp,gsub,pattern="FALSE",replacement="False"),row.names=row_names)

write.table(cbind("nplotparamaters",length(t.pp[,1])),file=paste(tmpdir,file="/plots/data_file.csv",sep=""),col.names=FALSE,row.names=FALSE,sep="\t")
write.table(t.pp,file=paste(tmpdir,file="/plots/data_file.csv",sep=""),col.names=FALSE,row.names=TRUE,sep="\t",append=TRUE)
write.table(cbind("npoints",length(my.data$x)),file=paste(tmpdir,file="/plots/data_file.csv",sep=""),col.names=FALSE,row.names=FALSE,sep="\t",append=TRUE)
write.table(my.data[,1:2],file=paste(tmpdir,file="/plots/data_file.csv",sep=""),col.names=FALSE,row.names=FALSE,sep="\t",append=TRUE)

#---- Interactive plotly widget ----#
dir.create(file.path(tmpdir, "widgets"), showWarnings = FALSE)

a_val <- as.numeric(reg$intercept)
b_val <- as.numeric(reg$slope)

my.data$tip <- paste0("Row: ", rownames(my.data),
                      "<br>x: ", round(my.data$x, 4),
                      "<br>y: ", round(my.data$y, 4))

eq_str <- paste0("y=", sprintf("%.2f", b_val), "x",
                 ifelse(a_val >= 0, paste0("+", sprintf("%.2f", a_val)),
                                    paste0("-", sprintf("%.2f", abs(a_val)))))
annot_parts <- c()
if (pp$plot_regression) annot_parts <- c(annot_parts, eq_str)
if (pp$plot_rsquared)   annot_parts <- c(annot_parts,
                                         paste0("R²=", round(cor(my.data$x, my.data$y)^2, 4)))
if (pp$plot_method)     annot_parts <- c(annot_parts,
                                         if (pp$weighting) "Method: Deming, weighted" else "Method: Deming")
annot_text <- paste(annot_parts, collapse = "\n")

cf_fill <- as.character(pp$confidence_fill_col)
cf_alph <- pp$confidence_transp / 100
cf_bord <- as.character(pp$conf_col)
ln_col  <- as.character(pp$my_lincol)
pt_col  <- as.character(pp$my_point_col)
pt_fill <- as.character(pp$my_bg)
lwd     <- pp$my_lwd * 0.5

g <- ggplot(my.data, aes(x = x, y = y)) +
  coord_cartesian(xlim = c(pp$xmin, pp$xmax), ylim = c(pp$ymin, pp$ymax)) +
  labs(x = as.character(pp$my_xlab), y = as.character(pp$my_ylab),
       title = as.character(pp$my_main)) +
  theme_bw(base_size = pp$font_mult) +
  theme(plot.title = element_text(hjust = 0.5))

if (pp$plot_confidence && !is.null(reg$reg.CI.data)) {
  ci_df <- as.data.frame(reg$reg.CI.data)
  names(ci_df) <- c("x", "lwr", "upr")
  ci_df[] <- lapply(ci_df, as.numeric)
  ci_df <- ci_df[complete.cases(ci_df), ]
  g <- g + geom_ribbon(data = ci_df, aes(x = x, ymin = lwr, ymax = upr),
                       inherit.aes = FALSE, fill = cf_fill, alpha = cf_alph, colour = NA)
  if (pp$plot_conf_outline)
    g <- g +
      geom_line(data = ci_df, aes(x = x, y = lwr), inherit.aes = FALSE,
                colour = cf_bord, linetype = "dashed", linewidth = lwd) +
      geom_line(data = ci_df, aes(x = x, y = upr), inherit.aes = FALSE,
                colour = cf_bord, linetype = "dashed", linewidth = lwd)
}

g <- g + geom_abline(intercept = a_val, slope = b_val,
                     colour = ln_col, linetype = pp$my_lty, linewidth = lwd)

if (pp$plot_identity)
  g <- g + geom_abline(intercept = 0, slope = 1, colour = "red",
                       linetype = "dashed", linewidth = lwd)

g <- g + geom_point(aes(text = tip), shape = pp$my_pch, size = pp$my_cex * 2.5,
                    colour = pt_col, fill = pt_fill, stroke = pp$my_lwd * 0.4)

p <- ggplotly(g, tooltip = "text")
if (nzchar(annot_text))
  p <- p %>% layout(annotations = list(list(
    text = gsub("\n", "<br>", annot_text), x = 0.02, xref = "paper",
    y = 0.97, yref = "paper", xanchor = "left", yanchor = "top",
    showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
    bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1, borderpad = 5,
    font = list(size = 13))))
p <- p %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

saveWidget(p, file = file.path(tmpdir, "widgets", "regression.html"),
           selfcontained = TRUE)
