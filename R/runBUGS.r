#' @exportrunBUGS <- function(model, input, priors, years, inits=T, n = 60000, burn = 20000, thin = 10, nchains = 2, debug = F, wd=file.path(  project.datadirectory("bio.surfclam"),"inst"), parameters=c(names(priors),'K','P','r'),add.parameters=NULL,add.inits=NULL,pe=F,sw='bugs'){    start<-Sys.time()        data=input      if(!missing(priors)){            # Prepare priors for WinBUGS      for(i in 1:length(priors)){        if(priors[[i]]$d%in%c("dlnorm","dnorm"))priors[[i]]$b<-1/priors[[i]]$b^2      #  if(priors[[i]]$d=="dgamma")priors[[i]]$b<-1/priors[[i]]$b      }      prior.dat<- data.frame(par=names(priors),do.call("rbind",lapply(priors,rbind)))      prior.lst<-list()      for(i in seq(1,nrow(prior.dat)*2,2)){        prior.lst[[i]]<-prior.dat$a[[ceiling(i/2)]]        prior.lst[[i+1]]<-prior.dat$b[[ceiling(i/2)]]      }      names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')               data=c(prior.lst,input)    }    # parameters to save    parameters<-c(parameters, add.parameters)        # initial values    if(inits==T){	    inits<-list()	    prior.dat<-data.frame(do.call("rbind",priors))	    for(i in 1:nchains){	     #browser()	     inits[[i]]<-sapply(1:nrow(prior.dat),function(x){rep(unlist(prior.dat[paste('i',i,sep='')][x,]),prior.dat$l[[x]])})	      #browser()	      if(pe){	        prior.dat$l[prior.dat$l>1]<-input$NY	        inits[[i]]<-sapply(1:nrow(prior.dat),function(x){rep(unlist(prior.dat[paste('i',i,sep='')][x,]),prior.dat$l[[x]])})	      }	      names(inits[[i]])<-names(priors)	      inits[[i]]<-c(inits[[i]],list(P=rep(1,length(years))),add.inits[[i]])	    }	}	else inits = NULL        #browser()    if(missing(years))years=1:input$NY    input$iyr<-years    ## Call to WinBUGS ##    if(sw=='bugs'){      require(R2WinBUGS)      tmp = bugs(data = data, inits, parameters.to.save = parameters, model.file = file.path(wd,"bugs",paste(model,".bug",sep="")), n.chains = nchains, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = file.path("C:","WinBUGS14"), debug = debug)      #out.lst<-list(data=input, sims.list=tmp$sims.list,median=tmp$median,mean=tmp$mean,summary=tmp$summary)       out.lst<-c(data=list(input),tmp)         }    ## Call to JAGS ##    if(sw=='jags'){      require(R2jags)      tmp = jags(data = data, inits, parameters.to.save = parameters, model.file = file.path(wd,"bugs",paste(model,".bug",sep="")), n.chains = nchains, n.iter = n, n.burnin = burn, n.thin = thin )       #browser()      pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(model,"traceplots.pdf")),11,8)      traceplot(tmp,mfrow=c(3,1),ask=F,col=rainbow(nchains,alpha=0.5))      dev.off()      pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(model,"densityplots.pdf")),11,8)      par(mfrow=c(3,2))      densplot(as.mcmc(tmp))      dev.off()           out.lst<-c(data=list(input),tmp$BUGSoutput)      #browser()    }    print(Sys.time()-start)    rm(tmp)    return(out.lst)  } 