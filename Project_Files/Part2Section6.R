#our passord information
userTestData = data.frame(read.csv("userdata-Part2.csv", header = TRUE, sep = ","))
#data filtered from Part1Section2.R
filteredData = read.csv("Part1DescriptiveStats.csv");
#text password information
textData <- subset(filteredData, Password.Scheme == "testtextrandom")

attempts <- subset(userTestData, attempts > 0);
success <- subset(userTestData, result == "success");
failure <- subset(userTestData, result == "failed");

attemptsTable = table(attempts$userid); # Table containing the total login frequency for each user
successesTable = table(success$userid); # Table containing the successful login frequency for each user
failuresTable = table(failure$userid); # Table containing the failed login frequency for each user

# Creating the final table containing all the summarized data for the text password scheme 
fullData = data.frame(attemptsTable, successesTable, failuresTable);

fullData$Var1 <- NULL;
fullData$Var1.1 <- NULL;
fullData$Var1.2 <- NULL;

colnames(fullData) <- c("attempts", "success", "failure");

write.csv(fullData, "Part2FilteredData.csv");

meanAttempts = mean(fullData$attempts);
meanSuccess = mean(fullData$success);
meanFailure = mean(fullData$failure);

sdAttempts = sd(fullData$attempts);
sdSuccess = sd(fullData$success);
sdFailure = sd(fullData$failure);

medianAttempts = median(fullData$attempts);
medianSuccess = median(fullData$success);
medianFailure = median(fullData$failure);

meanTable <- data.frame(meanAttempts, meanSuccess, meanFailure);
sdTable <- data.frame(sdAttempts, sdSuccess, sdFailure);
medianTable <- data.frame(medianAttempts, medianSuccess, medianFailure);

write.csv(meanTable, "P2means.csv");
write.csv(sdTable, "P2sd.csv");
write.csv(medianTable, "P2medians.csv");

## INFER. STATS. ##

#success
tTestSuccessLoginCount = t.test(textData$Successful.Logins, fullData$success);

t = tTestSuccessLoginCount$statistic
df = tTestSuccessLoginCount$parameter
p_value = tTestSuccessLoginCount$p.value

success <- data.frame(t, df, p_value)

#failure
tTestFailedLoginCount = t.test(textData$Failed.Logins, fullData$failure);

t = tTestFailedLoginCount$statistic
df = tTestFailedLoginCount$parameter
p_value = tTestFailedLoginCount$p.value

failure <- data.frame(t, df, p_value)

t_test <- rbind(success,failure);
write.csv(t_test, "tTest.csv");

hist(fullData$attempts,
     main="Histogram of Login Attempts", 
     xlab="Number of Attemps", 
     ylab="Frequency",
     border="light blue", 
     col="dark blue");
histAttempts = recordPlot();
plot.new();

hist(fullData$success,
     main="Histogram of Successful Login", 
     xlab="Successful Logins Per User", 
     ylab="Frequency",
     border="dark green", 
     col="light green");
histSuccess = recordPlot();
plot.new();

hist(fullData$failure,
     main="Histogram of Successful Login", 
     xlab="Failed Logins Per User", 
     ylab="Frequency",
     border="pink", 
     col="red");
histfailure = recordPlot();
plot.new();

boxplot(fullData$success, 
        fullData$failure,
        main="Box Plot of Successful and Failed Login Attemps",
        names=c("Successful Logins", "Failed Logins"),
        col=c("light green", "mistyrose"),
        medcol=c("dark green", "red"), 
        whiskcol=c("dark green", "red"),
        staplecol=c("dark green", "red"), 
        ylab="Time")
boxPlotResult  = recordPlot()
plot.new()

## Data On Survey Results ##

surveyData = data.frame(read.csv("SurveyDataFiltered.csv", header = TRUE, sep = ","));
surveyDataTransposed = data.frame(read.csv("SurveyTransposed.csv", header = TRUE, sep = ","));

pie(surveyDataTransposed$Q1, main="Is the password easy to remember?");
pie(surveyDataTransposed$Q2, main ="Is the password versatile?");
pie(surveyDataTransposed$Q3, main="Is the password scheme easy to use?");
pie(surveyDataTransposed$Q4, main="Are the vibrations easy to tell apart?");
pie(surveyDataTransposed$Q5, main="Are the vibrations easy to remember?");

pie(surveyDataTransposed$Q6, main="Does having sound on help to remember the sequence?");
pie(surveyDataTransposed$Q7, main="Would you trust your information to be secure with this password scheme?");
pie(surveyDataTransposed$Q8, main="Is it easy to remember passwords for different accounts? ");
pie(surveyDataTransposed$Q9, main="Is it easy to differentiate between passwords?");
pie(surveyDataTransposed$Q10, main="Does having a repeat option help to remember the password?");

pie(surveyDataTransposed$Q11, main="Does the repeat button help you to succeed within 3 attempts?");
pie(surveyDataTransposed$Q12, main="Is the password entry time frame too long?");
pie(surveyDataTransposed$Q13, main="Is the password scheme complex enough?");
pie(surveyDataTransposed$Q14, main="Is the password scheme user friendly?");
pie(surveyDataTransposed$Q15, main="Are you satisfied with this password scheme?");
