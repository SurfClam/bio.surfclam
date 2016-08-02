summarize.gridout = function(grid.out,type=1,scaler=10^6){
	summary.table<-list()
	for (i in 1:length(grid.out$grid.polyData[[type]]) ){
		summary.table[[i]] <- summary(grid.out$grid.polyData[[type]][[i]]$Z/scaler)
	}
	return(do.call("rbind",summary.table))
}