textData = read.csv("Text21.csv"); #CSV data set containing the log data for the text password scheme
imageData = read.csv("Imagept21.csv"); #CSV data set containing the log data for the image password scheme


textDataLogins <- subset(textData, register == "login"); # Subset of the text logs containing only login attempts
textDataSuccessful <- subset(textDataLogins, success == "success"); # Successful login logs
textDataFailed <- subset(textDataLogins, success == "failure"); # Failed login logs

textDataDiffTimeSuccess <- data.frame(ID=factor(), timeDiff=factor()); # Empty data frame that will store the successful login times
textDataDiffTimeFail <- data.frame(ID=factor(), timeDiff=factor()); #Empty data frame that will store the failed login times

#Repeating the process above but for the image password scheme
imageDataLogins <- subset(imageData, register == "login");
imageDataSuccessful <- subset(imageDataLogins, success == "success");
imageDataFailed <- subset(imageDataLogins, success == "failure");

imageDataDiffTimeSuccess <- data.frame(ID=factor(), timeDiff=factor());
imageDataDiffTimeFail <- data.frame(ID=factor(), timeDiff=factor());


prevTxtRow <- textData[1, 'X2017.07.05.14.49.36']; # Row that will be used to store what time the user entered the login page 
# This loops through each row in the logs, finds each login attempt, and calculates the amount of time spent on each attempt
for (row in 1:nrow(textData)) {
  id <- textData[row, 'ast103']; # The user ID of the current row
  register <- textData[row, 'register']; # Usage mode of the current row
  login <- textData[row, 'success']; # The event status of the current row
  time  <- textData[row, 'X2017.07.05.14.49.36']; # The timestamp on the current row
  
  if (register == "enter" & login == "start") { # This tracks what time the user first entered the login page
    prevTxtRow <- textData[row, 'X2017.07.05.14.49.36'];
  }
  else if (register == "login" & login == "success") { # If the login was successful, add the number of seconds passed to our table
    newRow <- data.frame(ID=id,timeDiff=difftime(time, prevTxtRow, units='secs'));
    
    textDataDiffTimeSuccess <- rbind(textDataDiffTimeSuccess, newRow);
  }
  else if (register == "login" & login == "failure") { 
    newRow <- data.frame(ID=id,timeDiff=difftime(time, prevTxtRow, units='secs'));
    
    textDataDiffTimeFail <- rbind(textDataDiffTimeFail, newRow);
  }
}

# Repeating the proccess above for the image password scheme
prevImgRow <- imageData[1, 'X2017.07.05.14.32.37'];
for (row in 1:nrow(imageData)) {
  id <- imageData[row, 'ipt101'];
  register <- imageData[row, 'register'];
  login <- imageData[row, 'success'];
  time  <- imageData[row, 'X2017.07.05.14.32.37'];

  if (register == "enter" & login == "start") {
    prevImgRow <- imageData[row, 'X2017.07.05.14.32.37'];
  }
  else if (register == "login" & login == "success") {
    newRow <- data.frame(ID=id,timeDiff=difftime(time, prevImgRow, units='secs'));
    
    imageDataDiffTimeSuccess <- rbind(imageDataDiffTimeSuccess, newRow);
  }
  else if (register == "login" & login == "failure") {
    newRow <- data.frame(ID=id,timeDiff=difftime(time, prevImgRow, units='secs'));
    
    imageDataDiffTimeFail <- rbind(imageDataDiffTimeFail, newRow);
  }
}

textDataScheme = "testtextrandom"; # The text password scheme 
textDataLoginsTable = table(textDataLogins$ast103); # Table containing the total login frequency for each user
textDataSuccessesTable = table(textDataSuccessful$ast103); # Table containing the successful login frequency for each user
textDataFailuresTable = table(textDataFailed$ast103); # Table containing the failed login frequency for each user
textDataDiffTimeSuccessTable = aggregate(textDataDiffTimeSuccess[2], list(textDataDiffTimeSuccess$ID), mean); # Dataframe containing the average time spent logging in on a successful attempt

# Users that did not fail any login attempts were missing from the dataframe contatining the average time spent on a failed login. 
# By joining the dataframe with one containing the frequency of failed logins we were able to keep a row for every user
textDataDiffTimeFailTableAvg = aggregate(textDataDiffTimeFail[2], list(textDataDiffTimeFail$ID), mean);
textDataDiffTimeFailFrequency = data.frame(table(textDataDiffTimeFail$ID));
colnames(textDataDiffTimeFailTableAvg) <- c("ID", "Mean");
colnames(textDataDiffTimeFailFrequency) <- c("ID", "Frequency");
textDataDiffTimeFailTable = merge (textDataDiffTimeFailTableAvg, textDataDiffTimeFailFrequency, by="ID", all = TRUE);
textDataDiffTimeFailTable$Frequency <- NULL;

# Creating the final table containing all the summarized data for the text password scheme 
textDataFull = data.frame(textDataLoginsTable, textDataSuccessesTable, textDataFailuresTable, textDataDiffTimeSuccessTable, textDataDiffTimeFailTable, textDataScheme);
textDataFull$Var1.1 <- NULL;
textDataFull$Var1.2 <- NULL;
textDataFull$Group.1 <- NULL;
textDataFull$ID <- NULL;
colnames(textDataFull) <- c("User ID", "Number of Logins", "Successful Logins", "Failed Logins", "Average Successful Login Time",  "Average Failed Login Time", "Password Scheme");



#Repeating the proccess above for the image password scheme
imageDataScheme = "testpasstiles";
imageDataLoginsTable = table(imageDataLogins$ipt101);
imageDataSuccessesTable = table(imageDataSuccessful$ipt101);
imageDataFailuresTable = table(imageDataFailed$ipt101);
imageDataDiffTimeSuccessTable = aggregate(imageDataDiffTimeSuccess[2], list(imageDataDiffTimeSuccess$ID), mean);

imageDataDiffTimeFailTableAvg = aggregate(imageDataDiffTimeFail[2], list(imageDataDiffTimeFail$ID), mean);
imageDataDiffTimeFailFrequency = data.frame(table(imageDataDiffTimeFail$ID));
colnames(imageDataDiffTimeFailTableAvg) <- c("ID", "Mean");
colnames(imageDataDiffTimeFailFrequency) <- c("ID", "Frequency");
imageDataDiffTimeFailTable = merge (imageDataDiffTimeFailTableAvg, imageDataDiffTimeFailFrequency, by="ID", all = TRUE);
imageDataDiffTimeFailTable$Frequency <- NULL;

imageDataFull = data.frame(imageDataLoginsTable, imageDataSuccessesTable, imageDataFailuresTable, imageDataDiffTimeSuccessTable, imageDataDiffTimeFailTable, imageDataScheme);
imageDataFull$Var1.1 <- NULL;
imageDataFull$Var1.2 <- NULL;
imageDataFull$Group.1 <- NULL;
imageDataFull$ID <- NULL;
colnames(imageDataFull) <- c("User ID", "Number of Logins", "Successful Logins", "Failed Logins", "Average Successful Login Time",  "Average Failed Login Time", "Password Scheme");


#Creating a new dataframe to hold the data from the all the users accross both password schemes
fullData <- data.frame(ID=factor(), total=factor(), succ=factor(), fail=factor(), avgS=factor(), avgF=factor(), scheme=factor());
fullData <- rbind(fullData, textDataFull);
fullData <- rbind(fullData, imageDataFull);
colnames(fullData) <- c("User ID", "Number of Logins", "Successful Logins", "Failed Logins", "Average Successful Login Time",  "Average Failed Login Time", "Password Scheme")

## REMOVE INVALID DATA ##
fullData <- fullData[-c(28), ]

#create csv to use for Part1Section3.R
write.csv(fullData, "Part1DescriptiveStats.csv", row.names = FALSE);

