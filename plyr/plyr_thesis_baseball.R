
#######################      {R base}      #######################
baseball <- subset(baseball,ab>=25)
pieces <- split(baseball,list(baseball$id))
df_lst <- lapply(pieces,function(df_in) {df_in$cyear=df_in$year-min(df_in$year)+1;df_in})
require("ggplot2")

# dir_new <- sprintf("C:/Users/Zhou/Documents/%s","baseball_pdf")
# dir.create(dir_new)
# plot_func <- function(df_in){
#   pdf(sprintf("%s/%s.pdf", dir_new, df_in$id[1]), width=8, height=4)
#   qplot(cyear, rbi/ab, data=df_in, geom="line")
#   dev.off()
# }
# lapply(df_lst,plot_func)

model <- function(df_in){
  lm(rbi/ab~cyear,data=df_in)
}
model_lst <- lapply(df_lst,model)
rsq <- function(model_in){
  try(r2 <- summary(model_in)$r.squared,T)
}
bcoefs <- as.data.frame(t(as.data.frame(lapply(model_lst,function(x) c(coef(x),rsquare=rsq(x))))))
names(bcoefs)[1:2] <- c("intercept","slop")

# plot histogram
qplot(x=rsquare,data=bcoefs,geom="histogram",binwidth=0.01)

# plot 
p <- ggplot(data=bcoefs)
p <- p + geom_point(aes(x=slop,y=intercept,size=rsquare),alpha=I(0.5))
p <- p + geom_line(aes(x=x,y=y),data=data.frame(x=c(-0.25,0.25),y=c(0,0)))
p <- p + geom_line(aes(x=x,y=y),data=data.frame(x=c(0,0),y=c(-0.25,0.25)))
p <- p + xlim(c(-0.15,0.15)) + ylim(c(-0.2,0.27))


#######################      {plyr}      #######################
require(c("ggplot2","plyr"))
baseball <- subset(baseball,ab>=25)
# append "cyear"
baseball <- ddply(baseball,c("id"),function(x) {x$cyear=x$year-min(x$year)+1; x})
# split based on "id", apply func "lm", merge to list 
model_lst <- dlply(baseball,c("id"),function(x) lm(rbi/ab ~ cyear,data=x))
# split based on list, apply func "",   merge to df
rsq <- function(x){
  summary(x)$r.squared
}
bcoefs <- ldply(model_lst,function(x) c(coef(x),rsq(x)))
names(bcoefs) <- c("id","intercept","slop","rsquare")
p <- ggplot(data=bcoefs)
p <- p + geom_point(aes(x=slop,y=intercept,size=rsquare),alpha=I(0.5))
p <- p + geom_line(aes(x=x,y=y),data=data.frame(x=c(-0.25,0.25),y=c(0,0)))
p <- p + geom_line(aes(x=x,y=y),data=data.frame(x=c(0,0),y=c(-0.25,0.25)))
p <- p + xlim(c(-0.15,0.15)) + ylim(c(-0.2,0.27))

