#' qitools
#' 
#' This collection of functions implements selected functions from qcc.
#' 
#' @author Robert Badgett
#' @import httr RJSONIO reshape2
#' @export
home <- function () {
	
	myplot <- plot.new()
	text(0,0,getwd())
	
	#don't return anything
	print(myplot)  
	invisible();
}
