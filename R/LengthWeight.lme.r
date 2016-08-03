#' @export
#// Mixed effects model for length - weight relaionlengthip

LengthWeight.lme<-function(wt.dat,random.effect="TowID",verbose=T, GBmodel=F, b.par='estimate'){


	# Estimate weight - length relationlengthips; overall and for each year
	# Linear mixed-effects model
	wt.dat$length <- as.numeric(wt.dat$length)
	wt.dat$weight <- as.numeric(wt.dat$weight)
	wt.dat <- na.omit(wt.dat)

	require(nlme)
	wt.dat$raneff<-wt.dat[,random.effect]
	ran.effects<-unique(wt.dat$raneff)

	wt.gdat <- groupedData(weight ~ length | raneff, data = wt.dat)
#	browser()
	
	if(b.par=='estimate'){
		wt.lme <- lme(fixed = log(weight) ~ log(length), data = wt.gdat, random = ~ log(length) | raneff, method="REML")
		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
		
		a <- exp(fit[,2])
		b <- fit[,3]

		A <- exp(wt.lme$coef$fixed[1])
		B <- wt.lme$coef$fixed[2]
	}
#	browser()
		
	if(b.par!='estimate'){
		wt.gdat$length<-wt.gdat$length^b.par
		wt.lme <- lme(fixed = weight ~ length -1, data = wt.gdat, random = ~ length -1 | raneff, method="REML")
		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(as.numeric(row.names(coef(wt.lme)))),a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),])

		a <- fit
		b <- rep(b.par,length(a))

		A <- wt.lme$coef$fixed[1]
		B <- b.par
	}
	
	names(fit)[1]<-random.effect

	if(verbose) print(summary(wt.lme))
	wt.dat$label<-wt.dat[,random.effect]

	return(list(A=A,B=B,a=a,b=b,data=wt.dat,fit=fit))
	
}
	