library("RColorBrewer")  #color palette
library("shiny")         #for gui
library("shinythemes")   #for the themes of the ui 
library("tidyverse")     # a set of packages
library("arules")        # for the association rules
library("DT")            #for making data tables


options(max.print = 99999)

#>>>>>>>> UI <<<<<<<<<<
ui<- fluidPage(theme = shinytheme("cosmo"),
               titlePanel("Team 22"),
               sidebarLayout(
                 sidebarPanel(
                   textInput("excel","Dataset Path"),
                   actionButton("submit_button","Submit"),
                   selectInput("Number_Of_Clusters","Enter the number of clusters",choices=c(2,3,4)),
                   sliderInput("Min_Support","Enter the minimum apriori Support",min = 0.001, max = 1, value = 0.05, step = 0.001),
                   sliderInput("Min_Confidence","Enter the minimum apriori Confidence",min = 0.001,max = 1,value = 0.06,step = 0.001),
                   actionButton("analyze_button","Analyze"),
                   verbatimTextOutput("summary_text")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Display",DTOutput("display_table")),
                     tabPanel("Cash VS Credit",plotOutput("pie"),verbatimTextOutput("Pie_text")),
                     tabPanel("Age vs Sum TotalSpending",plotOutput("Scatter"),verbatimTextOutput("Scatter_text")),
                     tabPanel("City Total Spending",plotOutput("Bar")),
                     tabPanel("Total Spending",plotOutput("Distribution")),
                     tabPanel("Dashboard",plotOutput("dashboard_plot")),
                     tabPanel("Cluster1",textOutput("Number_Of_Clusters"),plotOutput("clustering1")),
                     tabPanel("Cluster2",textOutput("Number_Of_Clusters"),plotOutput("clustering2")),
                     tabPanel("ClusterTable",DTOutput("clustering_table")),
                     tabPanel("AssocationRules",DTOutput("rules")),
                     tabPanel("AssociationPlot",textOutput("Min_Support"),textOutput("Min_Confidence"),plotOutput("associationPlot"))
                   )
                 )
               ))


#>>>>>>>> SERVER <<<<<<<<<<
server<-function(input ,output, session){
  
  # Displaying inputs and Summary of the data
  output$Number_Of_Clusters<-renderText(paste("Number of clusters: ",input$Number_Of_Clusters))
  output$Min_Support<-renderText(paste("Minimum Support: ",input$Min_Support))
  output$Min_Confidence<-renderText(paste("Minimum Confidence: ",input$Min_Confidence))
  
  data <- reactive({
    req(input$excel)  # Ensure the path is entered
    if (input$submit_button > 0) { # when pressing the submit button
      tryCatch(   # to catch any errors while reading the file and print a custom message.
        { read.csv(input$excel)},
        error = function(e) {  # Error handling
          cat("Error reading the dataset file:", conditionMessage(e), "\n")
          return(NULL)
        }
      )
    }
  })
  
  # cleaning data for summary text
  clean_data_summary<-function(data){
    excel <- data()
    cat("Sum of duplicated values in data: ", sum(duplicated(excel)), "\n")   # see how many duplicates in our data
    dataa= unique(excel)    # remove duplicates
    # check missing values
    cat("Sum of null values in dataframe: ", sum(is.na(dataa)), "\n")
    # to check data structure
    print("Checking data structure:")
    print(paste("Items:", is.character(dataa$items)))
    print(paste("Count:", is.integer(dataa$count)))
    print(paste("Total:", is.integer(dataa$total)))
    print(paste("rnd:", is.integer(dataa$rnd)))
    print(paste("Customer:", is.character(dataa$customer)))
    print(paste("Age:", is.integer(dataa$age)))
    print(paste("City:", is.character(dataa$city)))
    print(paste("PaymentType:", is.character(dataa$paymentType)))
    print("Checking outliers :")
    outliers <- boxplot(dataa$count)$out
    print(paste("Outliers:", length(outliers)))
    cat("Summary of data:\n")
    print(summary(dataa))
    cat("Rows:", nrow(dataa),"\n")
    cat("Columns: ",ncol(dataa),"\n")
  }
  # clean data for data table to be used in display
  clean_data<-function(data){
    excel <- data()
    excel_without= unique(excel)
    dataa <- excel_without
    as.data.frame(dataa) # make it as data frame
  }
  # here we used our clean_data_summary function to print out the summary of our data before and after cleaning .
  dataa <- reactive(clean_data_summary(data()))
  output$summary_text <- renderPrint({
    req(dataa)
    dataa()
  })
  # here we used our clean_data function to print out our data table .
  output$display_table <-renderDT({
    req(data)
    datatable(clean_data(data())) # "datatable()" is required when using "DT"
  })
  # this is our cleaned data frame that we will use on the rest of our code :) .
  data_cleaned <- reactive(clean_data(data())) 
  
  # Visualization
  # we will make each plot as a function to avoid repeating our codes
  # 1-Pie plot 
  plot1<-function(data_cleaned){
    x<-table(data_cleaned()$paymentType)
    percentage <- sprintf("%.2f%%", (100 * x / sum(x)))
    plot1<- pie(x, labels = percentage, main = "Compare Payment Types", col = c("#003366", "#3399cc"))
    legend("left", legend = c("Cash", "Credit"), fill = c("#003366", "#3399cc"))
  }
  # 2-Scatter plot
  plot2<-function(data_cleaned){
    dt2 <- data_cleaned() %>% group_by(age) %>% summarise(sum_total = sum(total))
    plot2 <- plot(x = dt2$age, y = dt2$sum_total, main = "Sum of total spending according to age",
                  xlab = "Age", ylab = "Sum of total spend", col = "#0000ff", type = "b")
  }
  # 3-Bar plot
  plot3<-function(data_cleaned){
    dt3 <- data_cleaned() %>% group_by(city) %>% summarize(spend = sum(total)) %>%
      arrange(desc(spend))
    color_count <- length(dt3$spend)
    my_colors <- colorRampPalette(brewer.pal(9, "Blues"))(color_count)
    plot3 <- barplot(height = dt3$spend, names.arg = dt3$city , col= my_colors,
                     main = "Sum of total spending for each city by Descending order",
                     xlab = "City", ylab = "Sum of Total Spending")
  }
  # 4-Box plot
  plot4<-function(data_cleaned){
    plot4<-boxplot(x = data_cleaned()$total, main = "Distribution of total spending", xlab = "Total spending")
  }
  # Now we will use the <plots functions> in the tabs we made in gui easily .
  output$pie<-renderPlot(plot1(data_cleaned))           #First tab plot
  output$Scatter<-renderPlot(plot2(data_cleaned))       #Second tab plot
  output$Bar<-renderPlot(plot3(data_cleaned))           #Third tab plot
  output$Distribution<-renderPlot(plot4(data_cleaned))  #Fourth tab plot
  # agian we will use our <plots functions> in our Dashboard  
  output$dashboard_plot<-renderPlot({
    par( mfrow = c(2 , 2) )
    plot1(data_cleaned)    #First plot
    plot2(data_cleaned)    #Second plot
    plot3(data_cleaned)    #Third plot
    plot4(data_cleaned)    #Fourth plot
  })
  
  #Clustering 1 between customers and sum of total spending
  output$clustering1<-renderPlot({
    req(data_cleaned()) 
    clean_data <- data_cleaned()
    req(nrow(clean_data) > 0)
    # Ensure that age and total columns are numeric
    clean_data$age <- as.numeric(clean_data$age)
    clean_data$total <- as.numeric(clean_data$total)
    dt2 <- clean_data %>%group_by(customer ,age) %>%summarise(sum_total = sum(total)) %>%as.data.frame()
    # Applying kmeans clustering1
    clusters <- kmeans(dt2[, c("age", "sum_total")], centers = input$Number_Of_Clusters)
    # Add cluster assignments to the original data
    dt2$cluster <- as.factor(clusters$cluster)
    # Check and convert 'customer' to a factor if it isn't already:
    dt2$customer <- as.factor(dt2$customer)
    # Order the data frame by sum_total in descending order
    dt2 <- dt2[order(dt2$sum_total, decreasing = TRUE), ]
    # making colors for the plot
    colors <- c("#003366", "#6699cc" , "#99cccc", "#9999cc" )  
    color_map <- colors[as.numeric(dt2$cluster)]
    
    # Create the bar plot
    options(scipen = 100) 
    # Create positions for tick marks 
    at_tick <- seq_along(dt2$sum_total)# create a sequence of integers from 1 to the length of the dt2$sum_total
    # Plot without axes >> axes = FALSE
    barplot(dt2$sum_total, space = 0  ,main = "Customer sum total",axes = FALSE, xlab = "Customer",ylab = "Sum of Total Spend",col=color_map) 
    # the first arg specifies the hight of the bar according to sum_total
    # (space=0) the space between the bars , (axes=FALSE) to remove x,y axis
    
     # Add y-axis
    axis(side = 2, pos = -0.4) # (side=2) to add the y-axis , (pos=-0.4) to adjust the position of the y-axis
    
    bar_heights <- dt2$sum_total # copy the (sum_total) vector to a new variable
    # now we place text labels above the bars (but the text will be 3% higher than the original length of the bar)
    adjusted_y <- bar_heights + max(bar_heights) * 0.03 
    text(at_tick,adjusted_y, labels = dt2$customer, adj = 1, xpd = TRUE,cex = 0.8)
    # (at_tick) the position of the text labels on the x-axis
    # (adjusted_y) the position of the text labels on the y-axis (above the bar)
    # (adj = 1) adjust the position of the text label above the bars (left and right)
    # (xpd = TRUE) place labels outside the plot without being cut off
    # (cex = 0.8) the font size
    })
  
  #Clustering 2 between age and sum of total spending
    output$clustering2<-renderPlot({
    req(data_cleaned()) 
    clean_data <- data_cleaned()
    req(nrow(clean_data) > 0)
    # Ensure that age and total columns are numeric
    clean_data$age <- as.numeric(clean_data$age)
    clean_data$total <- as.numeric(clean_data$total)
     dt2 <- clean_data %>%group_by(customer,age) %>%summarise(sum_total = sum(total)) %>%as.data.frame()
    # Applying kmeans clustering2
    clusters <- kmeans(dt2[, c("age", "sum_total")], centers = input$Number_Of_Clusters)
    # Add cluster assignments to the original data
    dt2$cluster <- as.factor(clusters$cluster)
    # Check and convert 'customer' to a factor if it isn't already:
    dt2$customer <- as.factor(dt2$customer)
    # Order the data frame by sum_total in descending order
    dt2 <- dt2[order(dt2$sum_total, decreasing = TRUE), ]
    
    # Generate colors for each cluster
    colors <- rainbow(length(unique(dt2$cluster)))
    # Plotting
    plot(x = dt2$age, y = dt2$sum_total, 
         col = colors[as.numeric(dt2$cluster)], pch = 19, 
         xlim = c(22,60),
         main = "Sum of total spending according to age and cluster",
         xlab = "Age", ylab = "Sum of total spend",  
    )
    axis(1, at = seq(22, 60, by = 1), las=1)
    # Add a legend to the plot
    legend("topright", legend = paste("Cluster", levels(dt2$cluster)), 
           col = colors, pch = 19, title = "Clusters")
  
  
  })
    

     # Making the cluster table
  output$clustering_table <- renderDT({
    clean_data <- data_cleaned()
    req(nrow(clean_data) > 0)
    # Ensure that age and total columns are numeric
    clean_data$age <- as.numeric(clean_data$age)
    clean_data$total <- as.numeric(clean_data$total)
    dt2 <- clean_data %>%group_by(customer ,age) %>%summarise(sum_total = sum(total)) %>%as.data.frame()
    # Applying kmeans clustering
    clusters <- kmeans(dt2[, c("age", "sum_total")], centers = input$Number_Of_Clusters)
    # Add cluster assignments to the original data
    dt2$cluster <- as.factor(clusters$cluster)
    datatable(dt2) # making a table (customer, age, total, cluster)
  })
  # Association
  # the Analyze button
  observeEvent(input$analyze_button, {
    req(data_cleaned()) # we make sure we have the required data
    clean_data <- data_cleaned() # assign the required data to a new variable
    # Create a dataframe of items
    tdata <- strsplit(clean_data$items , split = ",") # access the items column and separate the items with ","
    tdata <- as(tdata , "transactions") # transform tdata into transactions  
    # Generate association rules
    apriori_rules <- arules::apriori(tdata, parameter = list(supp = as.numeric(input$Min_Support), 
                                                             conf = as.numeric(input$Min_Confidence), 
                                                             minlen = 2)) #making the apriori rules according to (supp,conf,minlen)
    # the apriori rule shows how frequently an itemset will appear in a transaction (support), and the rules which shows that if A,then B (confidence)
    
    # Output the rules in the UI using DataTables
    output$rules <- renderDT({
      # Convert rules to a data frame
      if (length(apriori_rules) > 0) { # if there are rules in the apriori_rules
        rules_df <- as(apriori_rules, "data.frame") # then transform apriori_rules into dataframe to make a table
        # transform rules into DataTable with length=5 per page and autoWidth to adjust the tables width based on the content
        datatable(rules_df, options = list(pageLength = 5, autoWidth = TRUE)) 
      } else {
        # Return an empty data frame or a message if no rules are found
        # make a dataTable with one column named "Message" that displays "No rules found"
        datatable(data.frame(Message = "No rules found"), options = list(pageLength = 5, autoWidth = TRUE))
      }
    })
    # Plot the item frequency
    output$associationPlot <- renderPlot({
      # we will use our transaction data (tdata) , set (topN) to (5) to display 5 items in the plot ,
      #  set (type) to ("absolute") to show the count of occurrences for each item as a whole number
      arules::itemFrequencyPlot(tdata, topN = 5, type = "absolute") 
      # Add a title
      title(main = "Item Frequency Distribution", sub = "Top 5 Items") 
      
  })
  })
  }
shinyApp(ui = ui, server = server)