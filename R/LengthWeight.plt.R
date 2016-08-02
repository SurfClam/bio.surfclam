#//	plots result of LengthWeight.lme	

LengthWeight.plt <- function(htwt.fit,graphic='pdf',ht=7,wd=7,cx=1,lw=2,xl,yl,xlab="Length (mm)",ylab="Weight (g)",fn=''){

	
	if(graphic=='pdf')pdf(file.path(project.datadirectory('offshoreclams'),'figures',paste0("LenWt",fn,".pdf")), width = wd, height = ht, pointsize = 14)
	if(graphic=='R')x11(wd,ht)

	par(mar = c(2,2,0,0), omi = c(0.5, 0.5, 0.1, 0.1))
	
	if(missing(xl))xl<-range(htwt.fit$data$length)
	if(missing(yl))yl<-c(0, max(htwt.fit$data$weight) + 5)
	
	plot(weight ~ length, data = htwt.fit$data, xlim = xl, ylim = yl, col = rgb(0.1,0.1,0.1,0.3), las = 1, mgp = c(0.5, 0.5, 0), xlab ="", ylab = "", tcl = -0.3,cex.axis=cx)
	grid(col = "grey30")
			
	x <- with(htwt.fit$data, seq(min(length, na.rm = T), max(length, na.rm = T), length = 40))
	
	for(i in 1:length(htwt.fit$a)){
		lines(x, x^htwt.fit$b[i] * htwt.fit$a[i], col = rgb(1,0,0,0.2))
	}
	lines(x, x^htwt.fit$B * htwt.fit$A, lwd = lw, col = 'blue')
		
		mtext(xlab, 1, 1, outer = T,cex=cx)
		mtext(ylab, 2, 1, outer = T,cex=cx)
		
	if(graphic!='R')dev.off()
		
}	