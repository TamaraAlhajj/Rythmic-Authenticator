#data filtered from Part1Section2.R
filteredData = read.csv("Part1DescriptiveStats.csv");

#filter by password scheme
scheme = list(filteredData$Password.Scheme);

###Calculate Means###

##Logins per user
meanNumLogins_total = aggregate(filteredData[2], scheme, mean);
meanNumLogins_successful = aggregate(filteredData[3], scheme, mean);
meanNumLogins_failure = aggregate(filteredData[4], scheme, mean);
#combine to create table
meanNumLogins = data.frame(meanNumLogins_total, meanNumLogins_successful, meanNumLogins_failure);
#rename column and delete un-used columns
colnames(meanNumLogins)[1]<-"Password.Scheme";
meanNumLogins$Group.1.1 <- NULL;
meanNumLogins$Group.1.2 <- NULL;

##Login times
meanLoginTimes_successful = aggregate(filteredData[5], scheme, mean);
meanLoginTimes_failure = aggregate(filteredData[6], scheme, mean, na.rm=TRUE, na.action=NULL);
#combine to create table
meanLoginTimes = data.frame(meanLoginTimes_successful, meanLoginTimes_failure);
#rename column and delete un-used columns
colnames(meanLoginTimes)[1]<-"Password.Scheme";
meanLoginTimes$Group.1.1 <- NULL;
meanLoginTimes$Group.1.2 <- NULL;
#create complete mean table with both data frames
meanTable <- merge(meanNumLogins, meanLoginTimes, by="Password.Scheme");

###Calculate Standard Deviations###

##Logins per user
sdNumLogins_total = aggregate(filteredData[2], scheme, sd);
sdNumLogins_successful =  aggregate(filteredData[3], scheme, sd);
sdNumLogins_failure =  aggregate(filteredData[4], scheme, sd);
#combine to create table
sdNumLogins = data.frame(sdNumLogins_total, sdNumLogins_successful, sdNumLogins_failure);
#rename column and delete un-used columns
colnames(sdNumLogins)[1]<-"Password.Scheme";
sdNumLogins$Group.1.1 <- NULL;
sdNumLogins$Group.1.2 <- NULL;

##Login times
sdLoginTimes_successful = aggregate(filteredData[5], scheme, sd);
sdLoginTimes_failure =  aggregate(filteredData[6], scheme, sd, na.rm=TRUE);
#combine to create table
sdLoginTimes = data.frame(sdLoginTimes_successful, sdLoginTimes_failure);
#rename column and delete un-used columns
colnames(sdLoginTimes)[1]<-"Password.Scheme";
sdLoginTimes$Group.1.1 <- NULL;
sdLoginTimes$Group.1.2 <- NULL;
#create complete standard deviation table with both data frames
sdTable <- merge(sdNumLogins, sdLoginTimes, by="Password.Scheme");

###Calculate Medians###

##Logins per user
medianNumLogins_total = aggregate(filteredData[2], scheme, median);
medianNumLogins_successful =  aggregate(filteredData[3], scheme, median);
medianNumLogins_failure =  aggregate(filteredData[4], scheme, median);
#combine to create table
medianNumLogins = data.frame(medianNumLogins_total, medianNumLogins_successful, medianNumLogins_failure);
#rename column and delete un-used columns
colnames(medianNumLogins)[1]<-"Password.Scheme";
medianNumLogins$Group.1.1 <- NULL;
medianNumLogins$Group.1.2 <- NULL;

##Login times
medianLoginTimes_successful = aggregate(filteredData[5], scheme, median);
medianLoginTimes_failure =  aggregate(filteredData[6], scheme, median, na.rm=TRUE);
#combine to create table
medianLoginTimes = data.frame(medianLoginTimes_successful, medianLoginTimes_failure);
#rename column and delete un-used columns
colnames(medianLoginTimes)[1]<-"Password.Scheme";
medianLoginTimes$Group.1.1 <- NULL;
medianLoginTimes$Group.1.2 <- NULL;
##create standard deviation table with both data frames
medianTable <- merge(medianNumLogins, medianLoginTimes, by="Password.Scheme");

#### Graphs of Stats ###

#text password information
text <- subset(filteredData, Password.Scheme == "testtextrandom")
#image password information
image <- subset(filteredData, Password.Scheme == "testpasstiles")

##Histograms##

##Number of logins

#text
hist(text$Number.of.Logins,
     main="Histogram of Number of Login using Text Password Scheme", 
     xlab="Total Logins Per User", 
     ylab="Frequency",
     border="light blue", 
     col="dark blue");
histNumLogins_text = recordPlot();
plot.new();

#image
hist(image$Number.of.Logins,
     main="Histogram of Number of Login using Image Password Scheme", 
     xlab="Total Logins Per User", 
     ylab="Frequency",
     border="light blue", 
     col="dark blue");
histNumLogins_image = recordPlot();
plot.new();

#total for both
hist(filteredData$Number.of.Logins,
     main="Histogram of Total Number of Login Across Both Schemes", 
     xlab="Total Logins Per User", 
     ylab="Frequency",
     border="light blue", 
     col="dark blue");
histNumLogins_total = recordPlot();
plot.new();

## Number of Successful logins

#text
hist(text$Successful.Logins,
     main="Histogram of Successful of Login using Text Password Scheme", 
     xlab="Successful Logins Per User", 
     ylab="Frequency",
     border="dark green", 
     col="light green");
histSuccessfulLogins_text = recordPlot();
plot.new();

#image
hist(image$Successful.Logins,
     main="Histogram of Successful of Login using Image Password Scheme", 
     xlab="Successful Logins Per User", 
     ylab="Frequency",
     border="dark green", 
     col="light green");
histSuccessfulLogins_image = recordPlot();
plot.new();

# total for both
hist(filteredData$Successful.Logins,
     main="Histogram of Successful of Login Across Both Schemes", 
     xlab="Successful Logins Per User", 
     ylab="Frequency",
     border="dark green", 
     col="light green");
histSuccessfulLogins_total = recordPlot();
plot.new();

## Number Failed logins

#text
hist(text$Failed.Logins,
     main="Histogram of Failed of Login using Text Password Scheme", 
     xlab="Failed Logins Per User", 
     ylab="Frequency",
     border="pink", 
     col="red");
histFailedLogins_text = recordPlot();
plot.new();

#image
hist(image$Failed.Logins,
     main="Histogram of Failed of Login using Image Password Scheme", 
     xlab="Failed Logins Per User", 
     ylab="Frequency",
     border="pink", 
     col="red");
histFailedLogins_image = recordPlot();
plot.new();

#total for both
hist(filteredData$Failed.Logins,
     main="Histogram of Failed of Login Across Both Password Schemes", 
     xlab="Failed Logins Per User", 
     ylab="Frequency",
     border="pink", 
     col="red");
histFailedLogins_total = recordPlot();
plot.new();

## Login times

## Successful Times

#text
hist(text$Average.Successful.Login.Time,
     main="Histogram of Successful Login Times using Text Scheme", 
     xlab="Average Successful Login Time Per User", 
     ylab="Time",
     border="dark green", 
     col="light green");
histLoginTimeSuccess_text = recordPlot();
plot.new();

#image
hist(image$Average.Successful.Login.Time,
     main="Histogram of Successful Login Times using Image Scheme", 
     xlab="Average Successful Login Time Per User", 
     ylab="Time",
     border="dark green", 
     col="light green");
histLoginTimeSuccess_image = recordPlot();
plot.new();

#both
hist(filteredData$Average.Successful.Login.Time,
     main="Histogram of Successful Login Times Across Both Schemes", 
     xlab="Average Successful Login Time Per User (s)", 
     ylab="Time",
     border="dark green", 
     col="light green");
histLoginTimeSuccess_total = recordPlot();
plot.new();

## Failed Times

#text
hist(text$Average.Failed.Login.Time,
     main="Histogram of Failed Login Times using Text Scheme", 
     xlab="Average Failed Login Time Per User", 
     ylab="Time",
     border="pink", 
     col="red");
histLoginTimeFailed_text = recordPlot();
plot.new();

#image
hist(image$Average.Failed.Login.Time,
     main="Histogram of Failed Login Times using Image Scheme", 
     xlab="Average Failed Login Time Per User", 
     ylab="Time",
     border="pink", 
     col="red");
histLoginTimeFailed_image = recordPlot();
plot.new();

#both
hist(filteredData$Average.Failed.Login.Time,
     main="Histogram of Failed Login Times Across Both Schemes", 
     xlab="Average Failed Login Time Per User", 
     ylab="Time",
     border="pink", 
     col="red");
histLoginTimeFailed_total = recordPlot();
plot.new();

## Boxplots ##

## Login times

#for both
boxplot(filteredData$Average.Successful.Login.Time, 
        filteredData$Average.Failed.Login.Time,
        main="Box Plot of Successful and Failed Login Times Across Both Schemes",
        names=c("Successful Logins", "Failed Logins"),
        col=c("light green", "mistyrose"),
        medcol=c("dark green", "red"), 
        whiskcol=c("dark green", "red"),
        staplecol=c("dark green", "red"), 
        ylab="Time")
boxPlotLoginTime_both  = recordPlot()
plot.new()

#image
boxplot(image$Average.Successful.Login.Time, 
        image$Average.Failed.Login.Time,
        main="Box Plot of Successful and Failed Login Times using Image Password Scheme",
        names=c("Successful Logins", "Failed Logins"),
        col=c("light green", "mistyrose"),
        medcol=c("dark green", "red"), 
        whiskcol=c("dark green", "red"),
        staplecol=c("dark green", "red"), 
        ylab="Time")
boxPlotLoginTime_image  = recordPlot()
plot.new()

#text
boxplot(text$Average.Successful.Login.Time, 
        text$Average.Failed.Login.Time,
        main="Box Plot of Successful and Failed Login Times using Text Password Scheme",
        names=c("Successful Logins", "Failed Logins"),
        col=c("light green", "mistyrose"),
        medcol=c("dark green", "red"), 
        whiskcol=c("dark green", "red"),
        staplecol=c("dark green", "red"), 
        ylab="Time")
boxPlotLoginTime_text  = recordPlot()
plot.new()

##Both Number of Logins
boxplot(image$Number.of.Logins, 
        text$Number.of.Logins,
        main="Box Plot of Number of Logins for Image and Text Password Scheme",
        names=c("Image Logins", "Text Logins"),
        medcol=c("dark blue", "red"), 
        whiskcol=c("dark blue", "red"), 
        staplecol=c("dark blue", "red"), 
        boxcol=c("dark blue", "red"), 
        outcol=c("dark blue", "red"),
        col=c("light blue", "yellow"), 
        ylab="Frequency")
boxPlotNumberOfLogins_both  = recordPlot()
plot.new()

#write all stat tables to CSV
write.csv(meanTable, "means.csv");
write.csv(medianTable, "medians.csv");
write.csv(sdTable, "sds.csv");
