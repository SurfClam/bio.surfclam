#' @export
SPMpost.plt <- function(model.out, priors, vector.names, nr=4, nc=2, wd=8, ht=11, post.labs=NULL,name='', graphic='R',xl.type=1, path=file.path( project.datadirectory("bio.surfclam"), "figures")){

	if(graphic=='pdf')pdf(file.path(path,paste0(name,"post_single.pdf")), width = wd, height = ht, pointsize = 16)
	if(graphic=='R')x11(wd, ht)
	par(mfrow = c(nr, nc), mar = c(2, 3, 1, 1), omi = c(0.4, 0.6, 0, 0.2))
	
	dims = lapply(model.out$median,dim)
	mats = which(lapply(dims,length)>1)

	mat.posts = model.out$sims.list[mats]
	other.posts = model.out$sims.list[-mats]
	dims2 = lapply(model.out$median[-mats],dim)
	#mat.posts = model.out$median[mats]
	#other.posts = model.out$median[-mats]

	single.posts = other.posts[which(dims2==1)]
	vector.posts = other.posts[which(dims2>1)]
	

	for (i in 1:(length(single.posts))){
		
		if(names(single.posts)[i]!="deviance"){
			if(names(single.posts)[i]%in%names(priors)){
				if(priors[[names(single.posts)[i]]]$d=="dbeta")xl=c(0,1)
				else if(priors[[names(single.posts)[i]]]$d%in%c("dlnorm","dgamma"))xl<-c(0,max(single.posts[[i]]))
				else xl<-range(single.posts[[i]])
				x<-seq(xl[1], xl[2], l = 500)
				p<-get(priors[[names(single.posts)[i]]]$d)(x,  priors[[names(single.posts)[i]]]$a,  priors[[names(single.posts)[i]]]$b)
			}
			else {
				xl<-range(single.posts[[i]])
				#yl=range(single.posts[[i]])
			}
			
			hist(single.posts[[i]], breaks = 25, main = "", prob = T, ylab = "", las = 1, mgp=c(1,0.4,0), tcl=-0.3, xlab="",cex.axis=1.2,xlim=xl)
			if(names(single.posts)[i]%in%names(priors))lines(x, p, col = 'red')
			mtext(names(single.posts)[i], 1, 2, cex=1)
			if(i%in%(nr*nc*1:5))mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
		}

	}
	mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
	if(graphic!="R")dev.off()	
	
	if(missing(vector.names))vector.names = 1:model.out$data$NJ
	
	if(graphic=='pdf')pdf(file.path(path,paste0(name,"post_vector.pdf")), width = wd, height = ht, pointsize = 12)
	for (i in 1:length(vector.posts)){
		if(graphic=='R')x11(wd, ht)
		par(mfrow = c(ceiling(model.out$data$NJ/ceiling(sqrt(model.out$data$NJ))), ceiling(sqrt(model.out$data$NJ))), mar = c(2, 4, 2, 1), omi = c(0.4, 0.4, 0, 0.2))
	#browser()
		
		for(j in 1:ncol(vector.posts[[i]])){
			if(names(vector.posts)[i]%in%names(priors)&&xl.type==1){
				if(priors[[names(vector.posts)[i]]]$d=="dbeta")xl=c(0,1)
				else if(priors[[names(vector.posts)[i]]]$d%in%c("dlnorm","dgamma"))xl<-c(0,max(vector.posts[[i]]))
				else xl<-range(vector.posts[[i]])
			}
			else {
				if(xl.type==1)xl<-range(vector.posts[[i]])
				if(xl.type==2)xl<-range(vector.posts[[i]][,j])
			}
			x<-seq(xl[1], xl[2], l = 1000)
	#browser()
			
			if(names(vector.posts)[i]%in%names(priors)){
				if(length(priors[[names(vector.posts)[i]]]$a)==1)p<-get(priors[[names(vector.posts)[i]]]$d)(x,  priors[[names(vector.posts)[i]]]$a,  priors[[names(vector.posts)[i]]]$b)
				if(length(priors[[names(vector.posts)[i]]]$a)>1)p<-get(priors[[names(vector.posts)[i]]]$d)(x,  priors[[names(vector.posts)[i]]]$a[j],  priors[[names(vector.posts)[i]]]$b[j])
			}
			
			hist(vector.posts[[i]][,j], breaks = 30, main = as.character(vector.names[j]), prob = T, las = 1,ylab="",xlim= xl)
			if(names(vector.posts)[i]%in%names(priors))lines(x, p, col = 'red')
		}
		mtext("Posterior density", 2, 1, outer = T, adj = 0.5, cex=1.25)
		mtext(names(vector.posts)[i], 1, 1, outer = T, adj = 0.5, cex=1.25)
	}
	if(graphic!="R")dev.off()	



}
	


