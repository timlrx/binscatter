
binscatter <- function(formula, key_var, data, bins=20, partial=FALSE){
  
require(lfe)
require(ggplot2)

if(partial==TRUE){
  y <- unlist(strsplit(formula, "~"))[1]
  x <- unlist(strsplit(formula, "~"))[2]
  controls <- gsub(paste("[[:punct:]]*",key_var,"[[:space:]]*[[:punct:]]*",sep=""),
                   "",x)
  
  reg_all <- felm(formula(formula),data=data)
  reg_y <- felm(formula(paste(y, "~", controls, sep="")), data=data)
  reg_x <- felm(formula(paste(key_var, "~", controls, sep="")), data=data)
  resid_all <- resid(reg_all)
  resid_y <- resid(reg_y)
  resid_x <- resid(reg_x)
  df <- data.frame(resid_y, resid_x)
  cluster_grp <- trimws(unlist(strsplit(formula, "\\|"))[4])
  if(is.na(cluster_grp)){
    reg <- felm(resid_y ~ resid_x)  
  } else{
    data$resid_y <- resid_y
    data$resid_x <- resid_x
    reg <- felm(formula(paste("resid_y ~ resid_x | 0 | 0 |",
                              cluster_grp, sep="")), data=data)
  }
  newdata <- df
  colnames(df) <- c(paste("residual",names(df)[1]), paste("residual",names(df)[2]))
  
} else if(partial==FALSE){
  reg <- felm(formula(formula),data=data)
  y <- trimws(unlist(strsplit(formula, "~"))[1])
  df <- data[,c(y,key_var)]
  newdata <- df  # To calculate CI of predicted mean
  #  colnames(df) <- c("resid_y", "resid_x")
}
intercept <- coef(reg)[1]
slope <- coef(reg)[2]

### Use cluster vcov from the correct model, if not available, use robust
if(is.null(reg$clustervcv)){
  vcov <- reg$robustvcv
  se_type <- "robust"
} else {
  vcov <- reg$clustervcv
  se_type <- "cluster"
}

Terms <- terms(reg)
m.mat <- model.matrix(Terms,data=newdata)
fit <- as.vector(m.mat %*% coef(reg))
se.fit <- sqrt(rowSums((m.mat %*% vcov) * m.mat)) 
## Much faster alternative to sqrt(diag(m.mat%*%vcov%*%t(m.mat))) and works fine since
## we only want the diagonal
df$upper_ci <- fit + 1.96*se.fit
df$lower_ci <- fit - 1.96*se.fit

min_x <- min(df[,2])
max_x <- max(df[,2])
min_y <- intercept + min_x*slope
max_y <- intercept + max_x*slope

df_bin <- aggregate(df,by=list(cut(as.matrix(df[,2]),bins)), mean)

ggplot(data=df, aes(x=df[,2], y=df[,1])) +
  geom_point(alpha=0.2) + 
  geom_segment(aes(x = min_x, y = min_y, xend = max_x, yend = max_y),
               color="blue", size=1) +
  geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci),alpha=0.18) +
  geom_point(data=df_bin, aes(x=df_bin[,3], y=df_bin[,2]), color="orange", size=2) +
  labs(caption = paste(" slope = ", signif(slope,2), sep=""),
       x = names(df)[2], y = names(df)[1]) +
  theme_classic()
}