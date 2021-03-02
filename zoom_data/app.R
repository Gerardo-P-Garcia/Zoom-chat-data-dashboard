# Zoom chat data dashboard
# Author: Gerardo Pelayo Garcia
# Date 12/15/2020
#
#
#

# Functions to create data frames-----------------------------------------------

render_attendance <- function(text){
    require(dplyr)

    # Prepare data
    colnames(text) <- c('names')
    # Cut rows without relevant information
    #text <- text[grep('from',text$names),]
    # Cut names at 'to'
    text$names <- tolower(text$names)
    text$names[!(grepl('from', text$names) & grepl('to', text$names))] <- ""
    text$names <- gsub(" to .*", "", text$names)
    text$names <- gsub(" *. from ", "", text$names) # might be able to remove
    text$names <- gsub(" : .*", "", text$names)
    text$names <- gsub("[0-9]", "", text$names) # might be able to remove
    text$names <- gsub(".*:", "", text$names)

    # Remove leading and trailing white-space
    text$names <- stringr::str_trim(text$names, side='left')
    text$names <- stringr::str_trim(text$names, side='right')

    # Capitalize first letter and first letter after a space
    text$names <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                       text$names, perl=TRUE)

    # Alphabetize
    text <- text%>%
        arrange(names)

    #Get count
    text <- data.frame(table(text))
    colnames(text) <- c('names', 'total_sent')
    # Remove duplicates
    text <- text[text$names!="",]
    rownames(text) <- NULL

    return(text)
}

# Chat data
render_transcript <- function(df){

    # Get date from folder's label
    df <- data.frame(df)


    # Hard fix: only 1 column exists for some reason when run through R Shiny
    df$message <- df[,1]
    colnames(df) <- c('time', 'message')
    df$time <- as.character(df$time)
    df$message <- as.character(df$message)

    df$time <- sub("? From .*","", df$time)

    df$sender <- sub(".*?From","", df$message)
    df$sender <- sub("? to .*","", df$sender)
    df$sender <- sub("?:.*","", df$sender)
    df$sender <- stringr::str_trim(df$sender)

    df$recipient <- sub(".*? to ","", df$message)
    df$recipient <- sub("\\(Privately).*","", df$recipient)
    df$recipient <- stringr::str_trim(df$recipient)

    # edits on 2/22/2021
    df$recipient <- sub("?:.*","", df$recipient)
    df$recipient <- sub("?\\(D.*","", df$recipient)
    #df$recipient <- sub(".*? in waiting room\\)","", df$recipient)


    # Fix recipient when string does not have 'to' before message
    for(i in 1:nrow(df)){
      temp <- sub(".*?From","", df$message[i])
      temp <- sub(":.*", "", temp)

      if(grepl(' to ', temp)==FALSE){
          df$recipient[i] <- "Public"
      }
    }

    # Fix logs
     for(i in 1:nrow(df)){
         if(i>1){
             if(!grepl("[A-Za-z]", df$time[i]) & df$time[i]!="" ){
                 last_entry <- i
             }else{
                 df$message[last_entry]<- paste(df$message[last_entry], df$time[i], sep = ' ')
             }
         }
     }

    # Remove non-valid entries
    df <- df[!grepl("[A-Za-z]", df$time) & df$time!="",]

    # Remove excessive white space and new lines
    df$message <- gsub("\\s+", " ",df$message)

    # Keep only after colon
    df$message <- sub(".*?From","", df$message)
    df$message <- sub(".*?:", "", df$message)

    # Reorganize columns
    df <- df[, c(1, 3, 4, 2)]


    return(df)
}

render_data <- function(x){
    x <- render_transcript(x)
    x$nrow <- seq.int(nrow(x))
    temp_first_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_first_msg) <- c(
        'first_message', 'sender', 'recipient', 'message', 'nrow')
    temp_first_msg <- subset(temp_first_msg, select=c('first_message', 'sender'))
    x <- merge(x, temp_first_msg)
    x <- data.frame(x)

    # Reset order
    x <- x[order(x$nrow, decreasing = FALSE),]
    x<- x[seq(dim(x)[1],1),]
    temp_last_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_last_msg) <- c(
      'sender', 'last_message', 'recipient', 'message', 'nrow', 'first_message')
    temp_last_msg <- subset(temp_last_msg, select=c('last_message', 'sender'))
    x <- merge(x, temp_last_msg)
    x <- x[order(x$nrow, decreasing = FALSE),]
    x <- subset(x, select=-c(nrow))

    # Reset order
    x <- x[,c(2, 1, 3, 4, 5, 6)]

    # Get total count
    count <- data.frame(table(x$sender))
    colnames(count) <- c('sender', 'total_sent')
    x <- merge(x, count)

    # Simplify data frame
    #x <- subset(x, select=c(sender, first_message, last_message, total_sent))

    return(x)
}



# added on 2021/22/02
render_creditR <- function(raw_roster){

  csv_text = 'ROSTERS,,,,,,,,,,,,,
  SESSION 1: 2/22 - 3/5,,,,SCHEDULE BANK,,,,,,,,,
  HUM,STEM/ Math,PE,,,,,,,,,,,Directions
  "Barboza, Sarah E","Beliveau, Keytommy M","Gardner, Jonathan D",,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  "Beltran, Alex M","Berberte, Yan F","Kennison, Trinity S",,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,History/ ELA,,,,Step 1: Open the HS Q2 Credit Recovery sheet.
  "Bouchard, Lukas J","Carpenter, Larell T","Murphy, Bryan R",,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,PE/ Electives,,,,"Step 2: Click on the correct tab for the grade level of your advisee (9th Grade Session Groupings, 10th Grade Session Groupings, 11th/ 12th Grade Session Groupings)."
  "Cruz Rivera, Abdiel N","De La Cruz, Alberto A","Vega, Jaiden A",,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,,Step 3: Use the control + f combination on your keyboard to find your advisee’s name in each session. Note the order in which they will attend their Credit Recovery Zooms.
  "Garcia Colon, Jair I","Francisco, Joseph A",,,,,,,,,,,,Step 4: Locate their schedule in the Schedule Bank. Highlight it and copy it (control + c).
  "Januario, Evan L","Henderson, Andre N",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,Step 5: Paste the schedule into the supporting document you are sending to your scholar (use this Credit Recovery Schedule Notice if you are not currently using another document with your advisory).
  "Leroux, Robert J","Kinsley, Hannah M",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,STEM/ Math,,,,Step 6: Customize the document to include your scholar’s name.
  "Lilley, Aiden J","Larrivee, Arianna M",,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,,"Step 7: Email the schedule document to your advisee! Then, review the new Credit Recovery format with your advisory on Monday 2/22 during homeroom."
  "Maranhao, Quincy X","Lopez-Hernandez, Domingo S",,,Session 3: 3/22 - 4/2,STEM/ Math,,,Session 3: 3/22 - 4/2,,,,,
  "Montero Salgado, Jexiel O","Muhammad, Dwaine P",,,,,,,,,,,,
  "Moton, Janaya L","Pacheco, Phillip J",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  "Nivar Lopez, Jose A","Ramos, Belladora L",,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,STEM/ Math,,,,
  "Rabanales-Lorenzana, Ashley J","Rogers Watler, Tayia Money K",,,Session 2: 3/8 - 3/19,History/ ELA,,,Session 2: 3/8 - 3/19,PE/ Electives,,,,
  "Rivera, Leovette M","Silva, Jeffry M",,,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,,
  "Smith-Woods, Syriah A","Thomas, Kaleb C",,,,,,,,,,,,
  ,"Wallace, Rome A",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  ,,,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,PE/ Electives,,,,
  SESSION 2: 3/8 - 3/19,,,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,,
  HUM,STEM/ Math,PE,,Session 3: 3/22 - 4/2,History/ ELA,,,Session 3: 3/22 - 4/2,,,,,
  "Carpenter, Larell T","Beltran, Alex M","Beliveau, Keytommy M",,,,,,,,,,,
  "Francisco, Joseph A","Bouchard, Lukas J","Berberte, Yan F",,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  "Gardner, Jonathan D","Cruz Rivera, Abdiel N",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,PE/ Electives,,,,
  "Henderson, Andre N","Garcia Colon, Jair I",,,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,STEM/ Math,,,,
  "Kinsley, Hannah M","Januario, Evan L",,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,,
  "Larrivee, Arianna M","Leroux, Robert J",,,,,,,,,,,,
  "Lopez-Hernandez, Domingo S","Lilley, Aiden J",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  "Muhammad, Dwaine P","Maranhao, Quincy X",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,STEM/ Math,,,,
  "Murphy, Bryan R","Montero Salgado, Jexiel O",,,Session 2: 3/8 - 3/19,,,,Session 2: 3/8 - 3/19,,,,,
  "Pacheco, Phillip J","Moton, Janaya L",,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,,
  "Ramos, Belladora L","Nivar Lopez, Jose A",,,,,,,,,,,,
  "Silva, Jeffry M","Rabanales-Lorenzana, Ashley J",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,,
  "Thomas, Kaleb C","Rivera, Leovette M",,,Session 1: 2/22 - 3/5,PE/ Electives,,,Session 1: 2/22 - 3/5,,,,,
  "Wallace, Rome A","Smith-Woods, Syriah A",,,Session 2: 3/8 - 3/19,,,,Session 2: 3/8 - 3/19,,,,,
  ,,,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,,
  SESSION 3: 3/22 - 4/2,,,,,,,,,,,,,
  HUM,STEM/ Math,PE,,,,,,,,,,,
  ,,"Beltran, Alex M",,,,,,,,,,,
  ,,"Bouchard, Lukas J",,,,,,,,,,,
  ,,"Carpenter, Larell T",,,,,,,,,,,
  ,,"Cruz Rivera, Abdiel N",,,,,,,,,,,
  ,,"Francisco, Joseph A",,,,,,,,,,,
  ,,"Garcia Colon, Jair I",,,,,,,,,,,
  ,,"Henderson, Andre N",,,,,,,,,,,
  ,,"Januario, Evan L",,,,,,,,,,,
  ,,"Kinsley, Hannah M",,,,,,,,,,,
  ,,"Larrivee, Arianna M",,,,,,,,,,,
  ,,"Leroux, Robert J",,,,,,,,,,,
  ,,"Lilley, Aiden J",,,,,,,,,,,
  ,,"Lopez-Hernandez, Domingo S",,,,,,,,,,,
  ,,"Maranhao, Quincy X",,,,,,,,,,,
  ,,"Montero Salgado, Jexiel O",,,,,,,,,,,
  ,,"Moton, Janaya L",,,,,,,,,,,
  ,,"Muhammad, Dwaine P",,,,,,,,,,,
  ,,"Nivar Lopez, Jose A",,,,,,,,,,,
  ,,"Pacheco, Phillip J",,,,,,,,,,,
  ,,"Rabanales-Lorenzana, Ashley J",,,,,,,,,,,
  ,,"Ramos, Belladora L",,,,,,,,,,,
  ,,"Rivera, Leovette M",,,,,,,,,,,
  ,,"Silva, Jeffry M",,,,,,,,,,,
  ,,"Smith-Woods, Syriah A",,,,,,,,,,,
  ,,"Thomas, Kaleb C",,,,,,,,,,,
  ,,"Wallace, Rome A",,,,,,,,,,,'

  df <- read.csv(text = csv_text, header = FALSE)
  df <- data.frame(df)

  # Create data frame for required attendance roster

  # Grade 9

  # Humanities
  x <- data.frame(name=df[,1], subject='Humanities')
  x = x[-c(1:3),]
  x = x[c(1:15),]

  # STEM
  y <- data.frame(name=df[,2], subject='STEM')
  y = y[-c(1:3),]
  y = y[c(1:15),]

  # PE
  z <- data.frame(name=df[,3], subject='PE')
  z = z[-c(1:3),]
  z = z[c(1:15),]

  x <- rbind(x, y)
  x <- rbind(x,z)

  x = x[x$name!='',]

  # Get first name

  # Keep text after comma
  x$first <- sub(".*?,","", x$name)
  x$first <- stringr::str_trim(x$first, side='left')
  x$first <- sub(" .*","", x$first)
  x$first <- stringr::str_trim(x$first, side='right')
  x$first <- tolower(x$first)

  # Get last name
  #df$sender <- sub("? to .*","", df$sender)
  x$last <- sub(",.*","", x$name)
  x$last <- stringr::str_trim(x$last, side='left')
  x$last <- sub(" .*","", x$last)
  x$last <- stringr::str_trim(x$last, side='right')
  x$last <- tolower(x$last)

  # Assign grade
  x$grade <- '9'

  temp <- x



  csv_text <- 'ROSTERS,,,,,,,,,,,,
SESSION 1: 2/22 - 3/5,,,,SCHEDULE BANK,,,,,,,,Directions
HUM,STEM/ Math,PE,,,,,,,,,,Step 1: Open the HS Q2 Credit Recovery sheet.
"Abarrota, Brady J","Abreu, Mia L","Fernandez, Anthony L",,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,"Step 2: Click on the correct tab for the grade level of your advisee (9th Grade Session Groupings, 10th Grade Session Groupings, 11th/ 12th Grade Session Groupings)."
"Andrade, Nathan R","Andrade, Kayden J","Gardinere, Nephtalie N",,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,History/ ELA,,,Step 3: Use the control + f combination on your keyboard to find your advisee’s name in each session. Note the order in which they will attend their Credit Recovery Zooms.
"Carter, Xavier H","Carter, Jayden T","Nogueras, La Shawn R",,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,PE/ Electives,,,Step 4: Locate their schedule in the Schedule Bank. Highlight it and copy it (control + c).
"Casey, Austin N","Cruz Rivera, Janiel N","Ourique, Christian I",,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,Step 5: Paste the schedule into the supporting document you are sending to your scholar (use this Credit Recovery Schedule Notice if you are not currently using another document with your advisory).
"Dellatore, Laila R","DaSilva, Kendra L","Panama, Melanie E",,,,,,,,,,Step 6: Customize the document to include your scholar’s name.
"Dias, Kyle P","DeJesus, Jomar X",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,"Step 7: Email the schedule document to your advisee! Then, review the new Credit Recovery format with your advisory on Monday 2/22 during homeroom."
"DosSantos, Kescylla F","DeOliveira, Kianna L",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,STEM/ Math,,,
"Dupree, Tai Vohn L","DosSantos, Bryan",,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,
"Fagundes, Donovan P","Faria, Collin",,,Session 3: 3/22 - 4/2,STEM/ Math,,,Session 3: 3/22 - 4/2,,,,
"Fitzhugh, Stanley P","Francoeur, Preston L",,,,,,,,,,,
"Galego, Jacob M","Goncalves, Alex J",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
"Guffey, Jasmine K","Lopez Cortes, Edniel E",,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,STEM/ Math,,,
"Innocent, Poliker","Lopez-Hernandez, Erick E",,,Session 2: 3/8 - 3/19,History/ ELA,,,Session 2: 3/8 - 3/19,PE/ Electives,,,
"MacMurray, Luke J","Machado, Avaree E",,,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,
"Marshall, Richard M","Maldonado, Isaiah J",,,,,,,,,,,
"Mendell, Emma-Lee M","Medeiros, Dakota R",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
"Moniz, Jordan A","Mello, Jacob A",,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,PE/ Electives,,,
"Penton, Bryce W","Montero Salgado, Jendiel L",,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,
"Randolph Goyco, Giovanni J","Nogueras, Yarian L",,,Session 3: 3/22 - 4/2,History/ ELA,,,Session 3: 3/22 - 4/2,,,,
"Rebello, Alexa R","Reyes Rosario, Omairys E",,,,,,,,,,,
"Rivera, Roberto","Robles Pinto, Ryan E",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
"Rodriguez, Victoria A","Rodriques, Devon J",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,PE/ Electives,,,
"Salibi, Dimitri R","Santiago Rodriguez, Aleshly A",,,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,STEM/ Math,,,
"Souffront, Keiline N","Santos, Giovanny L",,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,
"Thomas, Kristopher D","Swanson, Richie A",,,,,,,,,,,
"Wallace, Breanna M","Thompson, Judayah G",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
,"Utizhungo, Keyla M",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,STEM/ Math,,,
,,,,Session 2: 3/8 - 3/19,,,,Session 2: 3/8 - 3/19,,,,
SESSION 2: 3/8 - 3/19,,,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,
HUM,STEM/ Math,PE,,,,,,,,,,
"Abreu, Mia L","Andrade, Nathan R","Abarrota, Brady J",,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
"Andrade, Kayden J","Carter, Xavier H",,,Session 1: 2/22 - 3/5,PE/ Electives,,,Session 1: 2/22 - 3/5,,,,
"Carter, Jayden T","Casey, Austin N",,,Session 2: 3/8 - 3/19,,,,Session 2: 3/8 - 3/19,,,,
"Cruz Rivera, Janiel N","Dellatore, Laila R",,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,
"DaSilva, Kendra L","Dias, Kyle P",,,,,,,,,,,
"DeJesus, Jomar X","DosSantos, Bryan",,,,,,,,,,,
"DeOliveira, Kianna L","DosSantos, Kescylla F",,,,,,,,,,,
"Faria, Collin","Dupree, Tai Vohn L",,,,,,,,,,,
"Francoeur, Preston L","Fitzhugh, Stanley P",,,,,,,,,,,
"Goncalves, Alex J","Galego, Jacob M",,,,,,,,,,,
"Lopez Cortes, Edniel E","Guffey, Jasmine K",,,,,,,,,,,
"Lopez-Hernandez, Erick E","Innocent, Poliker",,,,,,,,,,,
"Machado, Avaree E","MacMurray, Luke J",,,,,,,,,,,
"Maldonado, Isaiah J","Mendell, Emma-Lee M",,,,,,,,,,,
"Medeiros, Dakota R","Moniz, Jordan A",,,,,,,,,,,
"Montero Salgado, Jendiel L","Ourique, Christian I",,,,,,,,,,,
"Nogueras, Yarian L","Penton, Bryce W",,,,,,,,,,,
"Reyes Rosario, Omairys E","Randolph Goyco, Giovanni J",,,,,,,,,,,
"Robles Pinto, Ryan E","Rebello, Alexa R",,,,,,,,,,,
"Santiago Rodriguez, Aleshly A","Rodriguez, Victoria A",,,,,,,,,,,
"Santos, Giovanny L","Salibi, Dimitri R",,,,,,,,,,,
"Swanson, Richie A","Souffront, Keiline N",,,,,,,,,,,
"Thompson, Judayah G","Thomas, Kristopher D",,,,,,,,,,,
,"Wallace, Breanna M",,,,,,,,,,,
,,,,,,,,,,,,
SESSION 3: 3/22 - 4/2,,,,,,,,,,,,
HUM,STEM/ Math,PE,,,,,,,,,,
"Casey, Austin N","Maldonado, Isaiah J","Abreu, Mia L",,,,,,,,,,
,,"Andrade, Kayden J",,,,,,,,,,
,,"Andrade, Nathan R",,,,,,,,,,
,,"Carter, Jayden T",,,,,,,,,,
,,"Carter, Xavier H",,,,,,,,,,
,,"Cruz Rivera, Janiel N",,,,,,,,,,
,,"DaSilva, Kendra L",,,,,,,,,,
,,"DeJesus, Jomar X",,,,,,,,,,
,,"Dellatore, Laila R",,,,,,,,,,
,,"DeOliveira, Kianna L",,,,,,,,,,
,,"Dias, Kyle P",,,,,,,,,,
,,"DosSantos, Bryan",,,,,,,,,,
,,"DosSantos, Kescylla F",,,,,,,,,,
,,"Faria, Collin",,,,,,,,,,
,,"Fitzhugh, Stanley P",,,,,,,,,,
,,"Francoeur, Preston L",,,,,,,,,,
,,"Galego, Jacob M",,,,,,,,,,
,,"Goncalves, Alex J",,,,,,,,,,
,,"Guffey, Jasmine K",,,,,,,,,,
,,"Lopez-Hernandez, Erick E",,,,,,,,,,
,,"MacMurray, Luke J",,,,,,,,,,
,,"Medeiros, Dakota R",,,,,,,,,,
,,"Mendell, Emma-Lee M",,,,,,,,,,
,,"Moniz, Jordan A",,,,,,,,,,
,,"Montero Salgado, Jendiel L",,,,,,,,,,
,,"Nogueras, Yarian L",,,,,,,,,,
,,"Penton, Bryce W",,,,,,,,,,
,,"Randolph Goyco, Giovanni J",,,,,,,,,,
,,"Rebello, Alexa R",,,,,,,,,,
,,"Reyes Rosario, Omairys E",,,,,,,,,,
,,"Robles Pinto, Ryan E",,,,,,,,,,
,,"Rodriguez, Victoria A",,,,,,,,,,
,,"Salibi, Dimitri R",,,,,,,,,,
,,"Santiago Rodriguez, Aleshly A",,,,,,,,,,
,,"Santos, Giovanny L",,,,,,,,,,
,,"Souffront, Keiline N",,,,,,,,,,
,,"Swanson, Richie A",,,,,,,,,,
,,"Thomas, Kristopher D",,,,,,,,,,
,,"Thompson, Judayah G",,,,,,,,,,
,,"Wallace, Breanna M",,,,,,,,,,'

  df <- read.csv(text = csv_text, header = FALSE)
  df <- data.frame(df)

  # Grade 10

  # Humanities
  x <- data.frame(name=df[,1], subject='Humanities')
  x = x[-c(1:3),]
  x = x[c(1:27),]

  # STEM
  y <- data.frame(name=df[,2], subject='STEM')
  y = y[-c(1:3),]
  y = y[c(1:27),]

  # PE
  z <- data.frame(name=df[,3], subject='PE')
  z = z[-c(1:3),]
  z = z[c(1:27),]

  x <- rbind(x, y)
  x <- rbind(x,z)

  x = x[x$name!='',]

  # Get first name

  # Keep text after comma
  x$first <- sub(".*?,","", x$name)
  x$first <- stringr::str_trim(x$first, side='left')
  x$first <- sub(" .*","", x$first)
  x$first <- stringr::str_trim(x$first, side='right')
  x$first <- tolower(x$first)

  # Get last name
  #df$sender <- sub("? to .*","", df$sender)
  x$last <- sub(",.*","", x$name)
  x$last <- stringr::str_trim(x$last, side='left')
  x$last <- sub(" .*","", x$last)
  x$last <- stringr::str_trim(x$last, side='right')
  x$last <- tolower(x$last)

  # Assign grade
  x$grade <- '10'

  # Merge grades

  temp <- rbind(temp, x)

  csv_text <- 'ROSTER,,,,,,,,,,,,
  SESSION 1: 2/22 - 3/5,,,,SCHEDULE BANK,,,,,,,,directions
  HUM,STEM/ Math,PE,,,,,,,,,,Step 1: Open the HS Q2 Credit Recovery sheet.
  "Arevalo, Steven A","Audet, Jason R","Dinis, Sabrina R",,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,"Step 2: Click on the correct tab for the grade level of your advisee (9th Grade Session Groupings, 10th Grade Session Groupings, 11th/ 12th Grade Session Groupings)."
  "Beliveau, Katherine K","Barreto, Anthony J","Lamar, Spencer K",,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,History/ ELA,,,Step 3: Use the control + f combination on your keyboard to find your advisee’s name in each session. Note the order in which they will attend their Credit Recovery Zooms.
  "Bowdre, Mia D","Bastos, Meadow M","Utizhungo, Yamilex Y",,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,PE/ Electives,,,Step 4: Locate their schedule in the Schedule Bank. Highlight it and copy it (control + c).
  "Cabral, Savannah M","Botelho, Ethan J",,,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,Step 5: Paste the schedule into the supporting document you are sending to your scholar (use this Credit Recovery Schedule Notice if you are not currently using another document with your advisory).
  "Chouinard, Devin M","Camara, Vanessa R",,,,,,,,,,,Step 6: Customize the document to include your scholar’s name.
  "Cruz, Maxemiliano","Cardona, Christion N",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,"Step 7: Email the schedule document to your advisee! Then, review the new Credit Recovery format with your advisory on Monday 2/22 during homeroom."
  "DeSousa, Arissa L","Charette, Ryon H",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,STEM/ Math,,,
  "Grace, Connor J","Cruz, Kenneth M",,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,
  "Henry -Thompson, Zoriah F","DeCosta, Kamryn J",,,Session 3: 3/22 - 4/2,STEM/ Math,,,Session 3: 3/22 - 4/2,,,,
  "Larrivee, Dylan J","Edouarzin, Joshua J",,,,,,,,,,,
  "Longchamp, Jalim Y","Franklin-Matthews, Jaleysia N",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
  "Massaquoi, Denzel A","Gomes, Erica L",,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,STEM/ Math,,,
  "Patron Guzman, Yashira M","King, Ryan T",,,Session 2: 3/8 - 3/19,History/ ELA,,,Session 2: 3/8 - 3/19,PE/ Electives,,,
  "Santos, Nandy O","Murphy, Dylan J",,,Session 3: 3/22 - 4/2,PE/ Electives,,,Session 3: 3/22 - 4/2,,,,
  "Smith, Savannah N","Perry, Kiya J",,,,,,,,,,,
  "Sousa, Zachary I","Randolph Bolding, Alayana D",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
  "Thales, Danny","Rezendes, Elizabeth A",,,Session 1: 2/22 - 3/5,STEM/ Math,,,Session 1: 2/22 - 3/5,PE/ Electives,,,
  "Vargas Diaz, John M","Santana, Xavier R",,,Session 2: 3/8 - 3/19,PE/ Electives,,,Session 2: 3/8 - 3/19,History/ ELA,,,
  ,"Soto, Deja L",,,Session 3: 3/22 - 4/2,History/ ELA,,,Session 3: 3/22 - 4/2,,,,
  ,"Spencer, TreVon K",,,,,,,,,,,
  ,"Thompson, Darienne R",,,Credit Recovery Schedule,,,,Credit Recovery Schedule,,,,
  ,"Todd, Rowan C",,,Session 1: 2/22 - 3/5,History/ ELA,,,Session 1: 2/22 - 3/5,PE/ Electives,,,
  ,"Velasquez, Isaac R",,,Session 2: 3/8 - 3/19,STEM/ Math,,,Session 2: 3/8 - 3/19,STEM/ Math,,,
  ,,,,Session 3: 3/22 - 4/2,,,,Session 3: 3/22 - 4/2,,,,'

  df <- read.csv(text = csv_text, header = FALSE)
  df <- data.frame(df)

  # Grade 11 and 12

  # Humanities
  x <- data.frame(name=df[,1], subject='Humanities')
  x = x[-c(1:3),]
  x = x[c(1:18),]

  # STEM
  y <- data.frame(name=df[,2], subject='STEM')
  y = y[-c(1:3),]
  y = y[c(1:18),]

  # PE
  z <- data.frame(name=df[,3], subject='PE')
  z = z[-c(1:3),]
  z = z[c(1:18),]

  x <- rbind(x, y)
  x <- rbind(x,z)

  x = x[x$name!='',]

  # Get first name

  # Keep text after comma
  x$first <- sub(".*?,","", x$name)
  x$first <- stringr::str_trim(x$first, side='left')
  x$first <- sub(" .*","", x$first)
  x$first <- stringr::str_trim(x$first, side='right')
  x$first <- tolower(x$first)

  # Get last name
  #df$sender <- sub("? to .*","", df$sender)
  x$last <- sub(",.*","", x$name)
  x$last <- stringr::str_trim(x$last, side='left')
  x$last <- sub(" .*","", x$last)
  x$last <- stringr::str_trim(x$last, side='right')
  x$last <- tolower(x$last)

  # Assign grade
  x$grade <- '11/12'

  # Merge grades

  temp <- rbind(temp, x)

  # Check attendance

  raw_roster <- read.csv(text=raw_roster, header = TRUE)
  raw_roster <- data.frame(raw_roster)

  x <- render_data(raw_roster)
  x <- data.frame(sender=x$sender, time=x$first_message)
  x <- unique(x)


  # Remove emoticons from names
  x$sender <- gsub("[^\x01-\x7F]", "", x$sender)
  x$sender <- tolower(x$sender)

  # Hard fixes
  temp$first[temp$first=='la'] <- 'la shawn'
  temp$first[temp$first=='tai'] <- 'tai vohn'

  temp$attendance <- 'Absent'

  for(i in 1:nrow(temp)){
      for(j in 1:nrow(x)){
          if(grepl(temp$first[i], x$sender[j])){
              temp$attendance[i] <- 'Present'
          }
      }
  }


  for(i in 1:nrow(temp)){
    for(j in 1:nrow(x)){
      if(grepl(x$sender[j], temp$first[i])){
        temp$attendance[i] <- 'Present'
      }
    }
  }

  x <- temp

  # Remove extra columns
  x <- subset(x, select=-c(first, last))

  # Reorder columns
  x <- x[c(1, 3, 2, 4)]

  # Rename columns
  colnames(x) <- c('Name', 'Grade', 'Course/Subject', 'Attendance')



  # fix la shawn (la)

  # i <- sapply(seq_along(temp$first), function(i) grepl(temp$first[i], x$sender[i]))
  # x$attendance <- c("No", "Yes")[i + 1L]








  # x <- render_transcript(x)
  # x$nrow <- seq.int(nrow(x))
  # temp_first_msg <- x[match(unique(x$sender), x$sender), ]
  # colnames(temp_first_msg) <- c(
  #   'first_message', 'sender', 'recipient', 'message', 'nrow')
  # temp_first_msg <- subset(temp_first_msg, select=c('first_message', 'sender'))
  # x <- merge(x, temp_first_msg)
  # x <- data.frame(x)
  #
  # # Reset order
  # x <- x[order(x$nrow, decreasing = FALSE),]
  # x<- x[seq(dim(x)[1],1),]
  # temp_last_msg <- x[match(unique(x$sender), x$sender), ]
  # colnames(temp_last_msg) <- c(
  #   'sender', 'last_message', 'recipient', 'message', 'nrow', 'first_message')
  # temp_last_msg <- subset(temp_last_msg, select=c('last_message', 'sender'))
  # x <- merge(x, temp_last_msg)
  # x <- x[order(x$nrow, decreasing = FALSE),]
  # x <- subset(x, select=-c(nrow))
  #
  # # Reset order
  # x <- x[,c(2, 1, 3, 4, 5, 6)]
  #
  # # Get total count
  # count <- data.frame(table(x$sender))
  # colnames(count) <- c('sender', 'total_sent')
  # x <- merge(x, count)

  # Simplify data frame
  #x <- subset(x, select=c(sender, first_message, last_message, total_sent))

  return(x)
}



# Return only information on each sender, with unique rows
# Other chunks relied on render_data() to return redundant information
# with messages attached.
render_data_unique <- function(x){
    x <- render_transcript(x)
    x$nrow <- seq.int(nrow(x))
    temp_first_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_first_msg) <- c(
        'first_message', 'sender', 'recipient', 'message', 'nrow')
    temp_first_msg <- subset(temp_first_msg, select=c('first_message', 'sender'))
    x <- merge(x, temp_first_msg)
    x <- data.frame(x)

    # Reset order
    x <- x[order(x$nrow, decreasing = FALSE),]
    x <- x[seq(dim(x)[1],1),]
    temp_last_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_last_msg) <- c(
        'sender', 'last_message', 'recipient', 'message', 'nrow', 'first_message')
    temp_last_msg <- subset(temp_last_msg, select=c('last_message', 'sender'))
    x <- merge(x, temp_last_msg)
    x <- x[order(x$nrow, decreasing = FALSE),]
    x <- subset(x, select=-c(nrow))

    # Reset order
    x <- x[,c(2, 1, 3, 4, 5, 6)]

    # Get total count
    count <- data.frame(table(x$sender))
    colnames(count) <- c('sender', 'total_sent')
    x <- merge(x, count)

    # Simplify data frame
    x <- subset(x, select=c(sender, first_message, last_message, total_sent))
    x <- unique(x)

    return(x)
}

# Attendance
# simplify render_data() further while tweaking for attendance
render_data_attendance <- function(x){
    x <- render_transcript(x)
    x$nrow <- seq.int(nrow(x))
    temp_first_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_first_msg) <- c(
        'first_message', 'sender', 'recipient', 'message', 'nrow')
    temp_first_msg <- subset(temp_first_msg, select=c('first_message', 'sender'))
    x <- merge(x, temp_first_msg)
    x <- data.frame(x)

    # Reset order
    x <- x[order(x$nrow, decreasing = FALSE),]
    x <- x[seq(dim(x)[1],1),]
    temp_last_msg <- x[match(unique(x$sender), x$sender), ]
    colnames(temp_last_msg) <- c(
        'sender', 'last_message', 'recipient', 'message', 'nrow', 'first_message')
    temp_last_msg <- subset(temp_last_msg, select=c('last_message', 'sender'))
    x <- merge(x, temp_last_msg)
    x <- x[order(x$nrow, decreasing = FALSE),]
    x <- subset(x, select=-c(nrow))

    # Reset order
    x <- x[,c(2, 1, 3, 4, 5, 6)]

    # Get total count
    count <- data.frame(table(x$sender))
    colnames(count) <- c('sender', 'total_sent')
    x <- merge(x, count)

    # Simplify data frame
    temp1 <- data.frame("name"=x$sender, "label"=c('sender'))
    temp2 <- data.frame("name"=x$recipient, "label"=c('recipient'))
    unique_list <- rbind(temp1, temp2)
    unique_list <- unique_list[unique_list$name!='Public',]
    unique_list <- subset(unique_list, select=-c(label))
    unique_list <- unique(unique_list)

    return(unique_list)
}

# Functions: draw figures-------------------------------------------------------

# Render tables
create_dt <- function(x){
    require(DT)
    # Set table page length
    page_length <- nrow(x)

    # Title should use get_date to return date, appended to a title here
    #title <- paste("Zoom chat ")

    # Render table
    DT::datatable(x,
       # caption = htmltools::tags$caption(
       #     style = 'caption-side: top; text-align: center; color:black; font-size:200% ;',
        #    title),
        extensions = 'Buttons',
        options = list(dom = 'Bfrti',
        scrollY=300,
        buttons = c(
            'csv', 'pdf'),
        pageLength=page_length,
        scrollX=TRUE,
        lengthMenu = list(c(10,25,50,-1),
            c(10,25,50,"All"))),
        rownames = FALSE)
}

render_chat_activity <- function(x){
    require(ggplot2)
    require(plotly)

    # Extract date from file name
    # date <- as.Date(str_extract(path, "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
  date <- as.character(Sys.Date())

    # Remove \t at the end of each string (not visible)
    x$time <- gsub("[\t]", "", x$time)

    # Convert strings into date time
    x$date <- paste(date, x$time, sep=" ")
    x$date <- as.POSIXct(x$date)

    # Round to nearest 5 minutes while dropping seconds from time
    x$date <-as.factor(format(lubridate::round_date(x$date, unit = "5 minutes"), format="%H:%M"))
    messages <- x

    # Get date/time count
    messages <- dplyr::count(messages, date)

    title <- paste("Chat activity")

    plot_ly(messages,
        x=~as.factor(date),
        y=~n,
        type='bar',
        text=~n,
        textposition='auto', width = 0.1,
        hoverinfo='text',
        hovertext = paste(
            "<b>Time</b> :", messages$date,
            "<br><b>Messages Sent</b> :", messages$n))%>%
        layout(xaxis=list(fixedrange=TRUE, title='<b>Time</b>'))%>%
        layout(yaxis=list(fixedrange=TRUE, title='<b>Messages Sent</b>'))%>%
        layout(title=list(text=paste0(
            '<b>', title,'</b>')))%>%
        layout(annotations=list(x = 1, y = -0.1, text = "",
            showarrow = FALSE, xref='paper', yref='paper',
            xanchor='right', yanchor='auto', xshift=0, yshift=-10,
            font=list(size=12, color="black")))%>%
        layout(legend=list(orientation="h", x=0.25, y=-0.2))%>%
        config(displaylogo = FALSE)%>%
        config(modeBarButtonsToRemove = c(
            "zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "zoom2d",
            "pan2d", "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
            "drawrect", "drawcircle", "eraseshape", "autoScale2d", "resetScale2d",
            "hoverCloestCartesian", "hoverCompareCartesian", "handleCartesian",
            "toggleSpikelines"))%>%
        layout(margin=0,0,0,0)%>%
        style(textposition="right")%>%
        layout(axis)
}

# Donut charts
render_sent <- function(df, title="", pct=FALSE){
    require(dplyr)
    require(ggplot2)
    require(plotly)
    require(scales)
    require(stats)

    df <- data.frame(table(df$sender))
    df <- df[order(df$Freq, decreasing=T),]
    df <- rename(df, group=Var1, value=Freq)
    row.names(df) <- NULL

    title <- paste("Sent messages", title, sep = " ")

    fig <- df %>% plot_ly(labels = ~group, values = ~value, textinfo= ~value)
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = paste("<b>", title, "</b>", sep=""),  showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
    config(displaylogo = FALSE)%>%
    config(modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "zoom2d",
        "pan2d", "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
        "drawrect", "drawcircle", "eraseshape", "autoScale2d", "resetScale2d",
        "hoverCloestCartesian", "hoverCompareCartesian", "handleCartesian",
        "toggleSpikelines"))%>%
    layout(autosize=F, margin=list(t=75, pad=0), #width=400, height=400,
        legend=list(orientation='v', x=1.2, y=0.5, face='bold', font=list(size=15)))

    return(fig)
}

render_received <- function(df, title="", pct=FALSE){
    require(dplyr)
    require(ggplot2)
    require(plotly)
    require(scales)
    library(stats)

    df <- data.frame(table(df$recipient))
    df <- df[order(df$Freq, decreasing=T),]
    df <- rename(df, group=Var1, value=Freq)
    row.names(df) <- NULL

    title <- paste("Received messages", title, sep = " ")

    fig <- df %>% plot_ly(labels = ~group, values = ~value, textinfo= ~value)
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = paste("<b>", title, "</b>", sep=""),  showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
    config(displaylogo = FALSE)%>%
    config(modeBarButtonsToRemove = c(
        "zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "zoom2d",
        "pan2d", "select2d", "lasso2d", "drawclosedpath", "drawopenpath", "drawline",
        "drawrect", "drawcircle", "eraseshape", "autoScale2d", "resetScale2d",
        "hoverCloestCartesian", "hoverCompareCartesian", "handleCartesian",
        "toggleSpikelines"))%>%
    layout(autosize=F, margin=list(t=75, pad=0), #width=400, height=400,
        legend=list(orientation='v', x=1.2, y=0.5, face='bold', font=list(size=15)))

    return(fig)
}

# Shiny app---------------------------------------------------------------------

library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinydashboardPlus)
library(fs)
library(dygraphs)
library(DT)
library(stringr)
library(plotly)

shinyApp(
ui=dashboardPage(
    dashboardHeader(
        title= paste("Credit Recovery Attendance ", Sys.Date(), sep='')
        # For reference: adding an icon to navheader with pop-out modal
        # ,
        # tags$li(actionLink("openModal", label = "", icon = icon("info")),
        # class = "dropdown")
    ),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Load Data", tabName = "load_data", icon = icon("th"),
                startExpanded = TRUE,
                fluidRow(
                    column(1, offset = 0,
                        #shinyDirButton("directory", "Folder Select",
                        #    "Select a folder with meeting_saved_chat.txt (on the right)",
                        #    icon=icon('folder')))
                        fileInput("upload", "Files",
                            multiple = FALSE,
                            accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")))
                ),
                tags$p(),
                tags$p(HTML("Click the button above and navigate <br/>
                    to your <code>Zoom</code> folder, which is under<br/>
                    the <code>Documents</code> directory by default.<br/>
                    The correct folder will contain a file<br/>
                    named <code>meeting_saved_chat</code>.<br/>
                    Select that file.")),
                tags$hr()
            ),
            menuItem("About this app", tabName = "about", icon=icon("th"),
                startExpanded = FALSE,
                tags$p(HTML("
                    <u>Download a table</u><br/><br/>

                    To download a table, click the button<br/>
                    <code>Show x rows</code> at the top and select<br/>
                    <b>All</b>. Otherwise, only visible rows are<br/>
                    printed, regardless of format. After,<br/>
                    select the desired format and click.<br/><br/>

                    <u>Download an image</u><br/><br/>

                    To download an image, hover and<br/>
                    select the camera icon on the top<br/>
                    right of each figure. It is saved<br/>
                    in its current state, including any<br/>
                    changes from clicking the legend<br/>
                    when it is available.<br/><br/>

                    <u>Addition information</u><br/><br/>

                    The source code is available on<br/>
                    Github. Please ask for permission<br/>
                    before reusing any of this code,<br/>
                    and cite me when appropriate.<br/>
                    For any inquiries, please email at:<br/>
                    gerardo_garcia@alumni.brown.edu.</br></br>"),


                    a("Link to Github",
                        href="https://github.com/Gerardo-P-Garcia",
                        target="_blank"))

            )
        )
    ),
    dashboardBody(
      tags$head(
        tags$style(
          "body {overflow-y: hidden;}"
        )
      ),
        fluidRow(
          valueBoxOutput('unique_participants'),
          valueBoxOutput("who_most_sent"),
          valueBoxOutput("who_most_received"),

          valueBoxOutput("total_sent"),
          valueBoxOutput("most_sent"),
          valueBoxOutput("most_received") ),

        fluidRow(column(width=8, height="30vh",
          navbarPage("",
       # ),
     #   fluidRow(
          #  tabBox(title=HTML("<b>Figures</b>"), id="tabset1", height="35vh",
                # tabPanel("Load data",
                #     tags$h4("Which file to select: meeting_saved_chat"),
                #     tags$p(HTML("The file selected should always be called <code>meeting_saved_chat</code>,\n
                #         located within your <code>Zoom</code>directory under <code>Documents</code>.<br/><br/>
                #
                #         Typical path: <b>C:/Users/user/Documents/Zoom/[<u>Really messy file</u>]/meeting_saved_chat</b>\n\n
                #
                #         You can also press the search icon on your Start panel, \n
                #         search \" This PC\", and select <code>Documents</code>\n
                #         on the left. Your <code>Zoom</code> folder should be there.")),
                #     verbatimTextOutput("directorypath"),
                #     tags$hr()
                # ),
                # tabPanel("Meeting transcript",
                #     # For reference: enabling overflow-y within tabPanels
                #     # div(style = 'overflow-y:scroll;height:500px;',
                #     DT::dataTableOutput('transcript'),
                #     tags$hr()
                #     # )
                # ),
                # tabPanel("Summary statistics",
                #     DT::dataTableOutput('table'),
                #     tags$hr()
                # ),
                # tabPanel( "Attendance",
                #     DT::dataTableOutput('attendance'),
                #     tags$hr()
                # ),
         #   ),
           # tabBox(title=HTML("<b>Figures</b>"), id="tabset2", height="35vh",
                tabPanel("Chat activity", plotlyOutput(outputId = "plotly")),
                tabPanel("Sent messages", plotlyOutput(outputId = "sent_messages")),
                tabPanel("Received messages", plotlyOutput(outputId = "received_messages"))
            )), column(width = 4,

            navbarPage("",

                       tabPanel("Transcript", DT::dataTableOutput('transcript')),

                       tabPanel("Table", DT::dataTableOutput('table')),
                      tabPanel("Attendance", DT::dataTableOutput('attendance'))

        )) )
     #   ) # End of fluid row
    ) # End of body
), # End of UI

server=function(input, output, session) {

    observe({


    # input$upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$upload)
    updateTabItems(session, "tabset1", selected="Meeting transcript")

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        # x <- read.csv(path_to_data, header = FALSE, sep = "\n",
        #                        stringsAsFactors = TRUE, row.names = NULL)
        x <- readr::read_lines(input$upload$datapath)

      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    })

    observe({
        cat("\ninput$upload value:\n\n")
        print(input$upload)
    })

    ## print to browser
    output$uploadpath <- renderPrint({
      req(input$upload)
        if (is.integer(input$upload)) {
            cat("No upload has been selected (shinyDirChoose)")
        } else {
             x <- readr::read_lines(input$upload$datapath)
            x <- render_attendance(x)
            vals$x <- x
            updateTabItems(session, "tabset1", selected="Meeting transcript")
            updateTabItems(session, 'tabs', selected = "about")
            menu
            #cat(path_to_data)
        }
    })

    output$table <- DT::renderDataTable({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
            x <- readr::read_lines(input$upload$datapath)
            x <- render_data_unique(x)
            create_dt(x)
        }
    })

    output$attendance <- DT::renderDataTable({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data_attendance(x)
            create_dt(x)
            #create_dt(x, get_date(path_to_data))
        }
    })

    output$transcript <- DT::renderDataTable({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
            x <- readr::read_lines(input$upload$datapath)
            x <- render_transcript(x)
            create_dt(x)
        }
    })



    # added on 2021/22/02
    output$transcript <- DT::renderDataTable({
      req(input$upload)
      if(is.integer(input$upload)) {
      } else{
        x <- readr::read_lines(input$upload$datapath)
        x <- render_creditR(x)
        create_dt(x)
      }
    })



    output$plotly <-  renderPlotly({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)
            render_chat_activity(x)
        }
    })

    output$sent_messages <-  renderPlotly({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
            x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)
            render_sent(x)
        }
    })

    output$received_messages <-  renderPlotly({
      req(input$upload)
        if(is.integer(input$upload)) {
        } else{
            x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)
            render_received(x)

        }
    })

    # Value boxes
    output$unique_participants <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)

            temp1 <- data.frame("person"=x$sender, "label"=c('sender'))
            temp2 <- data.frame("person"=x$recipient, "label"=c('recipient'))
            unique_list <- rbind(temp1, temp2)
            unique_list <- unique_list[unique_list$person!='Public',]
            unique_list <- unique(unique_list$person)

            valueBox(
                length(unique_list), "Unique participants", icon = icon("user-alt"),
                color = "blue"
        )
        }
    })

    output$who_most_sent <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
            x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)

            x <- data.frame(table(x$sender))
            x <- x[order(x$Freq, decreasing=T),]

            valueBox(
                x[,1][1], "Most Sent", icon = icon("envelope"),
                color = "aqua"
            )
        }
    })

    output$who_most_received <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)

            x <- data.frame(table(x$recipient))
            x <- x[order(x$Freq, decreasing=T),]

            valueBox(
                x[,1][1], "Most Received", icon = icon("envelope-open"),
                color = "purple"
            )
        }
    })

    output$total_sent <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)

            valueBox(
                nrow(x), "Total Messages", icon = icon("mail-bulk"),
                color = "red"
            )
        }
    })

    output$most_sent <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)
            x <- x[order(x$total_sent, decreasing=T),]

            valueBox(
                x$total_sent[1], "Most Messages Sent", icon = icon("paper-plane"),
                color = "green"
            )
        }
    })

    output$most_received <- renderValueBox({
      req(input$upload)
        if(is.integer(input$upload)) {
            valueBox("","")
        } else{
          x <- readr::read_lines(input$upload$datapath)
            x <- render_data(x)
            x <- data.frame(table(x$recipient))
            x <- x[order(x[,2], decreasing=T),]

            valueBox(
                x[,2][1], "Most Messages Received", icon = icon("envelope-open-text"),
                color = "orange"
            )
        }
    })

} # End of server function

) # End of shiny app

# Do not run - Deployment commands
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::deployApp()
# https://gerardo-p-garcia.shinyapps.io/zoom_data/

