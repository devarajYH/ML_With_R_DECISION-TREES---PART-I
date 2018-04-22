# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



setwd("G:/DrVinodsML")

require(xlsx)
require(ggplot2)

cereals<-read.xlsx("cereals_practice.xlsx",sheetIndex = 1)
head(cereals)

str(cereals)
summary(cereals)
dim(cereals)


#Check for any missing values
sum(is.na(cereals))
table(is.na(cereals))



# Remove the NA values from cereal data
cereals<-na.omit(cereals)

library(corrplot)
col_numeric<-sapply(cereals,is.numeric)
numericvars<-cereals[,col_numeric]
cormat<-cor(numericvars)


#Corelation plot
corrplot(cormat,method = 'number')


#Check the boxplot later on


#Create a histogram which for caories per serving


# ggplot(cereals, aes(calories)) + geom_histogram(bandwidth=10)
# + scale_x_continuous("Calories", breaks = seq(0,180,by=10))
# + scale_y_continuous("Count", breaks = seq(0,10,by = 5))+ theme_bw()
# + labs(title="Calories Frequency")
# 
# ggplot(data=cereals, aes(calories)) + 
#   geom_histogram()

#1 Highest calories observed between 100 to 110
hist(cereals$calories)


ggplot(data=cereals, aes(calories)) + 
  geom_histogram(breaks=seq(50, 180, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .8) + 
  labs(title="Histogram for Calories") +
  labs(x="Calories", y="Count") + 
  xlim(c(50,160)) + 
  ylim(c(0,30))


#2 Ratson Purina produce the highest calories
boxplot(cereals$calories~cereals$mfr, xlab ="mfr", ylab = "Calories")

ggplot(cereals, aes(mfr, calories)) + geom_boxplot(fill = "green")+
  scale_y_continuous("Manufacturer", breaks= seq(0,150, by=20))+
  labs( x = "Calories")



#3 Ratings vs manufacturers
ggplot(cereals, aes(mfr,rating)) + geom_boxplot(fill = "yellow")+
  scale_y_continuous("Rating", breaks= seq(0,150, by=20))+
  labs( x = "Manufacturer")
boxplot(cereals$rating~cereals$mfr,xlab ="mfr", ylab = "Ratings")
#Nabisco got the highest rating and Kellogs got one outstanding rating


#4 Calories vs shelf

ggplot(cereals, aes(shelf,calories)) + geom_boxplot(fill = "plum")+
  scale_y_continuous("Calories", breaks= seq(0,150, by=20))+
  labs( x = "Shelf")

boxplot(cereals$calories~cereals$shelf,xlab='Shelf',ylab='Calories')

#5 Ratings vs Calories

ggplot(cereals, aes(calories,rating)) + geom_point(aes(color = calories))
+ scale_x_continuous("Rating", breaks = seq(0,120,20))
+ scale_y_continuous("Calories", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Ratings vs Calories")


##Ratings are high if calories are less

#SCatterplot of various nutrients against calories
#For sodium

par(mfrow=c(2,2))
plot(cereals$sodium,cereals$calories)
plot(cereals$fat,cereals$calories)
plot(cereals$sugars,cereals$calories)
plot(cereals$protein,cereals$calories)


p1<-ggplot(cereals, aes(sodium,calories)) + geom_point(aes(color = calories))
+ scale_x_continuous("Sodium", breaks = seq(0,120,20))
+ scale_y_continuous("Calories", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Calories vs Sodium")


p2<-ggplot(cereals, aes(fat,calories)) + geom_point(aes(color = calories))
+ scale_x_continuous("Fat", breaks = seq(0,120,20))
+ scale_y_continuous("Calories", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Calories vs Fat")


p3<-ggplot(cereals, aes(sugars,calories)) + geom_point(aes(color = calories))
+ scale_x_continuous("Sugars", breaks = seq(0,120,2))
+ scale_y_continuous("Calories", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Calories vs Sugars")

p4<-ggplot(cereals, aes(protein,calories)) + geom_point(aes(color = calories))
+ scale_x_continuous("Protein", breaks = seq(0,120,20))
+ scale_y_continuous("Calories", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Calories vs Protein")

multiplot(p1, p2, p3, p4, cols=2)

##SCatterplot of various nutrients against ratings

p1<-ggplot(cereals, aes(sodium,rating)) + geom_point(aes(color = rating))
+ scale_x_continuous("Sodium", breaks = seq(0,120,20))
+ scale_y_continuous("Ratings", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Ratings vs Sodium")


p2<-ggplot(cereals, aes(fat,rating)) + geom_point(aes(color = rating))
+ scale_x_continuous("Fat", breaks = seq(0,120,20))
+ scale_y_continuous("Ratings", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Ratings vs Fat")


p3<-ggplot(cereals, aes(sugars,rating)) + geom_point(aes(color = rating))
+ scale_x_continuous("Sugars", breaks = seq(0,120,20))
+ scale_y_continuous("Ratings", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Ratings vs Sugars")

p4<-ggplot(cereals, aes(protein,rating)) + geom_point(aes(color = rating))
+ scale_x_continuous("Protein", breaks = seq(0,120,20))
+ scale_y_continuous("Ratings", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Calories vs Protein")

multiplot(p1, p2, p3, p4, cols=2)


#Plot how many cereals are of each type


#Major players in market


#Types of cereals by manufacturers


#Serving size with ratings
ggplot(cereals, aes(cups,rating)) + geom_point(aes(color = rating))
+ scale_x_continuous("Cups", breaks = seq(0,1,0.2))
+ scale_y_continuous("Ratings", breaks = seq(0,180,by = 20))+ theme_bw()
+ labs(title="Serving vs Rating")