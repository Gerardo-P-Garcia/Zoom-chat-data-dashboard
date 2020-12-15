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
    if(nrow(x)>100){
        page_length <- round(nrow(x)/3, digits = 0)
    } else{
        page_length <- nrow(x)
    }

    # Title should use get_date to return date, appended to a title here
    title <- paste("Zoom chat ")

    # Render table
    DT::datatable(x,
        caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center; color:black; font-size:200% ;',
            title),
        extensions = 'Buttons',
        options = list(dom = 'Bfrtip',
        scrollY=300,
        buttons = c(
            'pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),
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
    messages <- count(messages, date)

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
library(fs)
library(dygraphs)
library(DT)
library(stringr)
library(plotly)

shinyApp(
ui=dashboardPage(
    dashboardHeader(
        title="Zoom Chat Data"
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
        fluidRow(
          valueBoxOutput('unique_participants'),
          valueBoxOutput("who_most_sent"),
          valueBoxOutput("who_most_received"),

          valueBoxOutput("total_sent"),
          valueBoxOutput("most_sent"),
          valueBoxOutput("most_received")
        ),
        fluidRow(
            tabBox(title=HTML("<b>Data and tables</b>"), id="tabset1", height="45vh",
                tabPanel("Load data",
                    tags$h4("Which file to select: meeting_saved_chat"),
                    tags$p(HTML("The file selected should always be called <code>meeting_saved_chat</code>,\n
                        located within your <code>Zoom</code>directory under <code>Documents</code>.<br/><br/>

                        Typical path: <b>C:/Users/user/Documents/Zoom/[<u>Really messy file</u>]/meeting_saved_chat</b>\n\n

                        You can also press the search icon on your Start panel, \n
                        search \" This PC\", and select <code>Documents</code>\n
                        on the left. Your <code>Zoom</code> folder should be there.")),
                    verbatimTextOutput("directorypath"),
                    tags$hr()
                ),
                tabPanel("Meeting transcript",
                    # For reference: enabling overflow-y within tabPanels
                    # div(style = 'overflow-y:scroll;height:500px;',
                    DT::dataTableOutput('transcript'),
                    tags$hr()
                    # )
                ),
                tabPanel("Summary statistics",
                    DT::dataTableOutput('table'),
                    tags$hr()
                ),
                tabPanel( "Attendance",
                    DT::dataTableOutput('attendance'),
                    tags$hr()
                )
            ),
            tabBox(title=HTML("<b>Figures</b>"), id="tabset2", height="45vh",
                tabPanel("Chat activity", plotlyOutput(outputId = "plotly")
                ),
                tabPanel("Sent messages", plotlyOutput(outputId = "sent_messages")),
                tabPanel("Received messages", plotlyOutput(outputId = "received_messages"))
            )
        ) # End of fluid row
    ) # End of body
), # End of UI

server=function(input, output, session) {

    observe({


    # input$upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$upload)

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

