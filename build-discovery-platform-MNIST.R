############################
## Creating files for the XAI Discovery Platform
## based on MNIST and a SVM classifier
#############################
library(e1071)
library(DAAG)
library(dplyr)


## First, we will read in the mnist training data set.
## This will can be downloaded from 
## https://www.kaggle.com/oddrationale/mnist-in-csv
##
set.seed(100)
if(!file.exists("archive/mnist_train.csv"))
{
  stop('Please download mnist-in-csv data set.')
}
dat <- read.csv("archive/mnist_train.csv")
dat$case <- 1:nrow(dat)
dat$class <- as.factor(dat$label)##what the data file calls 'label' we are calling 'class'

##sample 10,000 to use for training; ignore the rest.
## for this demo, we don't need a great model, just a reasonably good
## one. 10K cases produces accuracy of almost 90% using an SVM.
use <- sample(1:nrow(dat),10000)
notuse <- rep(T,nrow(dat))
notuse[use] <- F

##get the small training set
dat.small <- dat[use,]


#dat.test <- dat.small #do testing on the same training set.
dat.validate <- dat[notuse,]


## This should take a few minutes (10-20 depending on computer) to train.  Be patient.

s1 <- svm(class~.,data=dat.small,method="class",kernel="linear",scale=F,cost=10,type="nu-classification",
          probability=T)
summary(s1)
##save and load image here after training.
save.image("mnist.RData")
load("mnist.RData")

##test on trained data:
sortorder <- c(1:10)
names(sortorder) <- c(1:9,0)

##this just obtains some sample predictions so we can organize the column order
pred.tmp <- predict(s1,dat.small[1:2,],probability=T)
distn.tmp <- round(attr(pred.tmp,"probabilities"),4)
columnord <- order(sortorder[colnames(distn.tmp)])

#predictions.prob.small <- predict(s1,dat.small, probability=T)
#predictions.small <- as.numeric(as.character(predictions.prob))
#distn.small <- round(attr(predictions.prob.small,"probabilities"),4)[,columnord]

#mean((dat.small$class==predictions.small))
#confusion(dat.small$class,predictions.small)




##test on the validation data to look at accuracy.
## dat.validate has 50,000 cases in it. This takes a while too.
pred2 <- predict(s1,dat.validate,probability=T)
distn.full <- round(attr(pred2,"probabilities"),4)[,columnord]#


probdf <- data.frame(matrix(rep(1:10,nrow(distn.full)),nrow=nrow(distn.full),byrow=T),
                     distn.full)
##mean accuracy for the validation data:
mean((dat.validate$class==pred2))
confusion(dat.validate$class,pred2)



##
#dat.test$case <- 1:nrow(dat.test)
#dat.test$case <- paste("0000000",dat.test$case,sep="")
#dat.test$case <- substr(dat.test$case,nchar(dat.test$case)-7,20)

##Now, we want to create images from each bitmap. This will create a filename that is reasonably formatted.

dat.validate$casetext <- paste0("0000000",dat.validate$case,sep="")
dat.validate$casetext <- substr(dat.validate$casetext,nchar(dat.validate$casetext)-7,20)


##Make a small data set for use in the discovery platform:


dat.validate$label <- pred2  #label is the prediction.
dat.validate$corr <-  dat.validate$class==dat.validate$label

dat.output.full <- dplyr::select(dat.validate,case,classID=class, class=class,
                                 labelID=label,label=label,corr,casetext)



##resample to include 10000; 50% correct and 50% error
corrRows <- sample(which(dat.output.full$corr),size=5000)
incRows <- sample(which(!dat.output.full$corr),size=5000)

rows <- sort(c(corrRows,incRows))
dat.output <- dat.output.full[rows,]

#dat.output[,20:29] <- distn.full[rows,]

## Create images
## This will limit teh number of files per directory.
dat.output$fname<- "NA"
perdirectory <- 1000
olddir <- "X"

for(i in 1:nrow(dat.output))
{

  case <- dat.output[i,]
    print(paste(i,case[1]))
  id <- case$case
  directory <- floor(as.numeric(id)/perdirectory)*perdirectory
  newdir <- directory
  if(newdir != olddir){
    path <- paste("images/D",newdir,"/",sep="")
  }
  olddir <- newdir
  fname<- (paste(path,"img",case$casetext,".png",sep=""))
  dat.output$fname[i] <- (fname)
 
}

#dat.output$id <- 1:nrow(dat.output)
dat.output <- select(dat.output,case=case,classID,class,labelID,label,corr,fname)
write.csv(dat.output,file="main.csv",row.names=F)
write.csv(probdf[rows,],file="probs.csv",row.names=F)
save.image("mnist.RData")
load("mnist.RData")


#if this is set to T, it will create png images for each selected case
# and store them in www/images/
regenImages <- T
if(regenImages)
{
dir.create("www")
dir.create("www/images")
perdirectory <- 1000
olddir <- "X"
for(i in 1:nrow(dat.output))
#for(i in 1:500 )
{
  caseID <- dat.output$case[i]
  case <- dat.output[i,]
  ##reach back to original features and access by case number
  features <- matrix(as.integer(dat[caseID,1+1:28^2]),28)
  directory <- floor(caseID/perdirectory)*perdirectory
  newdir <- directory
  if(newdir != olddir){
    print(newdir)
    path <- paste("./www/images/D",newdir,"/",sep="")
    if(!dir.exists(path))
      dir.create(path)
  }
  olddir <- newdir
    
  fname<- case$fname
  
  print(fname)
  png(fname)
  image(1:28,1:28,features[,28:1],col=grey(255:0/255),xaxt="n",yaxt="n",bty="n",
        xlab="",ylab="")
  dev.off()
}
}

