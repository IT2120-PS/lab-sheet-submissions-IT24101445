#1
setwd("C:\\Users\\it24101445\\Desktop\\IT24101445")

Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = "\t")

#view the file in a separate window
fix(Delivery_Times)

#Rename the variables (column headings) of the data set
names(Delivery_Times)<-c("X1")

#Attach the file into R. So, you can call the variables by their names.
attach(Delivery_Times)

#2
histogram<-hist(X1,main="Histogram for Delivery Times",xlab = "Delivery Time (minutes)",breaks = seq(20,70,length=10), right=FALSE)

#3
#The histogram appears slightly right-skewed, most delivery times are between 30 and 50 minutes.

#4
#creating the frequency distribution into a variable called "breaks"
breaks <- round(histogram$breaks)

#Assign class frequencies into a variable called "freq"
freq <- histogram$counts

#Assign class midpoints into a variable called "mids"
mids <- histogram$mids

#Using "cumsum" command we can get cumulative Frequencies
cum.Freq <- cumsum(freq)

#Creating a null variable called "new"
new <- c()

#Using "for" loop to store Cumulative Frequencies in order to get the ogive
for(i in 1:length(breaks)){
  if(i==1){
    new[i] = 0
  }else{
    new[i] = cum.Freq[i-1]
  }
}

#Draw cumulative frequency polygon in a new plot
plot(breaks, new, type = "l", 
     main="Cumulative Frequency Polygon (Ogive) for Delivery Times", 
     xlab = "Delivery Time (minutes)", ylab = "Cumulative Frequency", ylim = c(0,max(cum.Freq)))

#obtain upper limit of each class along with its Cumulative Frequency in a table
cbind(Upper = breaks, CumFreq = new)