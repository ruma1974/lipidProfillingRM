# _"/media/rune/joachime/RscripterCool/Rprojects/rmpackges/lipidProfillingRMp.R"


#' return ks_test p value between two numeric groups in data frame
#' @param Th (Value)
#' @param DF (data frame)
#' @param colNameControl (string - col name containing group information)
#' @param conGroup (string - control group)
#' @param altGroup (string - alternative group)
#' @param colNameNumeric (string - col name of numeric variable to compare)
#' @param Above (bool - use values above or below)
#' @return double
#' @examples
#' ---
#' @export
ks_pvalues <- function(Th,DF,colNameControl,conGroup,altGroup,colNameNumeric,Above=TRUE){
#
#
if (Above==TRUE){
idxL1=(DF[,colNameControl]=="control" & DF[,colNameNumeric]>Th)
} else {
idxL1=(DF[,colNameControl]=="control" & DF[,colNameNumeric]<Th)	
}
idxL2=which(altGroup==DF[,colNameControl])
res=ks.test(x=DF[,colNameNumeric][idxL1], y=DF[,colNameNumeric][idxL2],alternative = c("two.sided", "less", "greater")[1],exact = NULL)$p.value
invisible(res) # 
}


#' plot KS_test p values
#' @param Range (vector - e.g. 25:70)
#' @param DF (data frame)
#' @param colNameControl (string - col name containing group information)
#' @param conGroup (string - control group)
#' @param altGroup (string - alternative group)
#' @param colNameNumeric (string - col name of numeric variable to compare)
#' @param Above (bool - use values above or below)
#' @return double
#' @examples
#' ---
#' @export
plotKSpvalues <- function(Range,DF,colNameControl,conGroup,altGroup,colNameNumeric,Above,...){
#
#
pvL=-log10(unlist(lapply(Range,ks_pvalues,DF,colNameControl,conGroup,altGroup,colNameNumeric,Above)))
aL=Range
aL[which(pvL==min(pvL))]
pvL[which(pvL==min(pvL))]
x11();plot(Range,pvL,type="o", ylab="-log10(p value)",cex.lab=2,cex.axis=2,cex.main=2,...)
abline(h = -log10(0.01), lty = 3, col = "red", lwd = 1)
invisible(pvl) # 
}

#' plot KS_test p values
#' @param Range (vector - e.g. 25:70)
#' @param DF (data frame)
#' @param colNameControl (string - col name containing group information)
#' @param conGroup (string - control group)
#' @param altGroup (string - alternative group)
#' @param colNameNumeric (string - col name of numeric variable to compare)
#' @return double
#' @examples
#' ---
#' @export
plotKSpvaluesLowHigh <- function(Range,DF,colNameControl,conGroup,altGroup,colNameNumeric,...){
#
#
pvL=-log10(unlist(lapply(Range,ks_pvalues,DF,colNameControl,conGroup,altGroup,colNameNumeric,TRUE)))
pvL_low=-log10(unlist(lapply(Range,ks_pvalues,DF,colNameControl,conGroup,altGroup,colNameNumeric,FALSE)))

aL=Range
aL[which(pvL==min(pvL))]
pvL[which(pvL==min(pvL))]
x11();plot(Range,pvL_low,type="o",col="blue", ylab="-log10(p value)",cex.lab=2,cex.axis=2,cex.main=2,...)
lines(25:70,pvL,type="o",col="green")
abline(h = -log10(0.01), lty = 3, col = "red", lwd = 1)
invisible(pvl) # 
}



#' Plot ROC curve with 95% ci
#' @param obj (object from ROC package)
#' @param legacy (bool - plot order on X axis - legacy equal true correspond to same plot order as legacy software)
#' @return
#' @examples
#' ---
#' @export
RocWith_ci <- function(obj,legacy=TRUE) {
ciobj <- pROC::ci.se(obj, specificities = seq(0, 1, l = 25))
dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),lower = ciobj[, 1],upper = ciobj[, 3])

# plot
if (legacy==FALSE){
  pROC::ggroc(obj) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) + ggplot2::coord_equal() +
    ggplot2::geom_ribbon(
      data = dat.ci,
      ggplot2::aes(x = x, ymin = lower, ymax = upper),
      fill = "steelblue",
      alpha = 0.2
    ) + ggplot2::ggtitle(capture.output(obj$ci))	
} else {
pROC::ggroc(obj,legacy.axes=TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) + ggplot2::coord_equal() +
    ggplot2::geom_ribbon(
      data = dat.ci,
      aes(x = 1-x, ymin = lower, ymax = upper),
      fill = "steelblue",
      alpha = 0.2
    ) + ggplot2::ggtitle(capture.output(obj$ci))	
}
} 

