library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(scales)
library(shinydashboard)
library(stringr)
library(rsconnect)

df <- read.csv("trend_vids.csv")
df$trendTime <- as.POSIXct(df$trendTime)
df$trending_date <- as.Date(str_extract(df$trending_date, "\\d{4}-\\d{2}-\\d{2}"))

css <- "
body.light {
  background-color: white;
  color: black;
}

body.dark {
  background-color: #333;
  color: white;
}
"


ui <- dashboardPage(
  dashboardHeader(title = "Youtube Video Analysis"),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar-menu li a {
        color: #A9A9A9 !important;
      }
      body.dark .sidebar-menu li a {
        color: #FFFFFF !important;
      }
    ")),
    
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("hand", lib = "font-awesome")),
      menuItem("View Analysis", tabName = "view", icon = icon("television", lib = "font-awesome")),
      menuItem("Categories Analysis", tabName = "channel", icon = icon("play", lib = "font-awesome")),
      menuItem("Summary", tabName = "summary", icon = icon("tags", lib = "font-awesome")),
      br(),
      actionButton("theme_button", label = "Switch Theme")
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
      .sidebar-menu li a {
        color: #A9A9A9 !important;
      }
      body.dark .sidebar-menu li a {
        color: #FFFFFF !important;
      }
    ")),
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              fluidRow(
                box(width = 12,
                    h1("Trending Youtube Videos Analysis"),
                    br(),
                    p("YouTube is one of the largest and longest-standing video social media 
                      platforms in the world. As of 2023, YouTube has around 2.6 billion 
                      active users, 3.7 million video uploads per day by creators worldwide, 
                      and over 122 million people accessing the site each day. YouTube is a 
                      one-stop place for people, where they can find news updates, educational
                      content, tutorials, movies, vlogs, and many more. Throughout the millions
                      of videos uploaded each day, it is fascinating to think about how some videos 
                      go viral and generate high engagement while others struggle to get views. 
                      This made us wonder: what factors contribute to the success or popularity 
                      of a YouTube video? In this project, we will analyze YouTube data related to 
                      likes, shares, saves, comments, video category, length, and posting time in 
                      relation to how well they perform or trend. We will also analyze the most 
                      subscribed channels to observe how well their videos perform and their engagement 
                      rate across multiple platforms."),
                    br(),
                    p("Our project aims to visualize the factors that contribute to the popularity of videos 
                    on YouTube and observe the consumption patterns of users. We will achieve this by comparing 
                    and contrasting data, identifying patterns, and observing how these factors evolve and relate 
                    to each other. This is an interesting story to tell because it will allow content creators to 
                    understand their audience and tailor their content to increase engagement. By analyzing the 
                    unique characteristics of the videos that have generated high engagement, we can understand 
                    the formula for creating viral videos and replicating the success. In addition to that, knowing 
                    what makes a YouTube video popular will improve trend forecasting, as content creators will 
                    be able to stay ahead of the curve and produce relevant content that caters to their audiences."),
                )
              ),
              fluidRow(
                box( 
                  h2("Datasets"),
                  fluidRow(
                    box(
                      h4("Dataset 1: Trending Videos"),
                      br(),
                      p("Source:", a(href="https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset", "Kaggle")),
                      br(),
                      p("Number of Observations: 197590"), 
                      br(),
                      p("Number of Features: 16")
                    ),
                    box(
                      h4("Dataset 2: Most Subscribed"),
                      br(),
                      p("Source:", a(href="https://www.kaggle.com/datasets/themrityunjaypathak/most-subscribed-1000-youtube-channels", "Kaggle")),
                      br(),
                      p("Number of Observations: 1000"), 
                      br(),
                      p("Number of Features: 7")
                    )
                  )
                ),
                box(
                  h2("About us!"),
                  h3("Why are we doing this?"),
                  p("With the increasing demand in content-creating, we think it is helpful to analyze the biggest entertainment platform, Youtube.
                    We three share the same interest in video-making and creating, and we would like to know the anatomy behind trending Youtube videos,
                    the algorithms, as well as the user consumption behavior!"),
                  h3("Members:"),
                  p("Charity Joy Njotorahardjo, Eugene Alexander Wongso, Steven Wilbert Heng")
                )
              )
      ),
      
      tabItem(tabName = "view",
              box(width = 12,
                  h1("Video Views Analysis")
              ),
              tabsetPanel(
                tabPanel("Total Views Throughout the Year",
                         sliderInput(
                           inputId = "total_views",
                           label = "Choose time range:",
                           min = min(df$trendTime),
                           max = max(df$trendTime),
                           value = c(min(df$trendTime), max(df$trendTime)),
                           timeFormat = "%Y-%m-%d",
                           ticks = FALSE
                           
                         ),
                         
                         plotlyOutput("total_views_plot")
                ),
                tabPanel("Top 10 Trending Videos Each Day",
                         sliderInput(
                           inputId = "top_date",
                           label = "Choose time range:",
                           min = min(df$trendTime),
                           max = max(df$trendTime),
                           value = min(df$trendTime),
                           timeFormat = "%Y-%m-%d",
                           ticks = FALSE
                           
                         ),
                         tableOutput("top_10_vids")
                )
                
                
                
              )
      ),
      
      tabItem(tabName = "channel",
              box(width = 12, 
                  h1("Channel Analysis")
              ),
              tabsetPanel(
                tabPanel("Total Views Per Category",
                         sliderInput(
                           inputId = "category_views",
                           label = "Choose time range:",
                           min = min(df$trendTime),
                           max = max(df$trendTime),
                           value = c(min(df$trendTime), max(df$trendTime)),
                           timeFormat = "%Y-%m-%d",
                           ticks = FALSE
                         ),
                         selectInput(
                           inputId = "category_select",
                           label = "Select Category",
                           choices = unique(df$Category),
                           selected = unique(df$Category)[1],
                           multiple = FALSE
                         ),
                         plotlyOutput("category_views_plot")
                ),
                tabPanel("Time it Takes to Go Trending",
                         checkboxGroupInput(
                           inputId = "trending_select",
                           label = "Select Category",
                           choices = unique(df$Category),
                           selected = unique(df$Category)[1],
                           inline = TRUE
                         ),
                         plotlyOutput("days_to_trend_plot")
                )
              
              )
      ),
      
      tabItem(tabName = "summary",
              box(width = 12,
                  h1("Conclusion"),
                  br(),
                  p("From here, we can highlights three of the important notes that are derived from the analysis that are conducted in this study:"),
                  br(),
                  p(HTML("<b>VIEWS</b>")),
                  br(),
                  p("Looking at the average number of total views in YouTube trending, we can see that it is about 50 million. 
                  However, the trend of the number of views seems to be constant with some spikes in the number of views. 
                  The highest number of views achieved so far is 199,236,709, which occurred on June 25th, 2021."),
                  p(HTML("<b>CATEGORIES</b>")),
                  br(),
                  p("Despite Autos and Vehicles (Jeep) having the lowest view count among categories, with only 25,586,095 viewers,
                  it has surpassed Nonprofits & Activism (United Nations), which has only 10,429,565 viewers. 
                  This goes to show that the popularity of a category does not necessarily determine the number of viewers a channel receives, 
                  as there are other factors at play."),
                  br(),
                  p(HTML("<b>TAGS</b>")),
                  br(),
                  p("By analyzing the graph, we can see that the average number of tags used differs between categories. 
                  Interestingly, channels under the Music, Sports, Gaming, and Auto & Vehicles categories use the most tags, averaging over 70 tags. 
                  Conversely, videos under the Nonprofits & Activism category use the lowest number of tags, averaging only 30 tags. 
                  This suggests that the number of tags used may have an impact on the success of a video or channel, depending on the category it falls under."),
                  p(HTML("<b>NOTES!</b>")),
                  br(),
                  p("Through the data story-telling shown in this analysis, we can determine that while category popularity may have some impact, 
                  content creators should also consider the delivery of their content in terms of quality, creating various engagement strategies, 
                  and effective use of tags to attract viewers. By understanding these dynamics behind video views and tag usage patterns across different categories, 
                  as displayed in this analysis, content creators can maximize and optimize their content to better engage with their target audience and potentially increase their viewership. "),
                  br(),
                  p("In conclusion, this analysis of this study provides valuable insights into the factors that influence video popularity and viewership. 
                  It emphasizes the need for content creators to focus on more than just the category of their videos, and highlights the importance of factors such as content quality and effective use of tags. 
                  By leveraging these insights, content creators can improve their chances of creating engaging and popular videos on the platform, reach a wider audience, and achieve greater success."),
                  br(),
                  p("In order to deliver this, it is important to note that content creators have to be authentic to their own self, that is when their best features will come to the surface!"),
                  
                  
                  
                  
              ),
              sidebarLayout(
                position = "left",
                sidebarPanel(
                  dateInput("dateInput", "Select Date:",
                            value = min(df$trending_date),
                            min = min(df$trending_date),
                            max = max(df$trending_date)),
                  br(),
                  checkboxGroupInput("displayInput", "Display:",
                                     choices = c( "Tags", "View Count", "Likes", "Dislikes","Comments"),
                                     selected = c("Tags", "View Count", "Likes", "Dislikes","Comments")),
                  br(),
                  actionButton("resetBtn", "Reset")
                ),
                mainPanel(
                  fluidRow(
                    column(width = 12, plotOutput("tagsPlot")),
                    column(width = 12, plotOutput("viewsPlot")),
                    column(width = 12, plotOutput("likesPlot")),
                    column(width = 12, plotOutput("dislikesPlot")),
                    column(width = 12, plotOutput("commentsPlot"))
                  )
                )

      
      
      )
    )
  )
)
)


server <- function(input, output, session) {
 
   observeEvent(input$theme_button, {
    session$sendCustomMessage(type = 'switch_theme', message = 'Switch')
  })
  
  output$views_vs_likes <- renderPlotly({
    view_likes_plot <- plot_ly(df, x = df$dislikes, y = df$likes, mode = "markers")
    view_likes_plot <- layout(view_likes_plot, xaxis = list(title = "dislikes"), yaxis = list(title = "Likes"))
    return(view_likes_plot)
  })
  
  output$top_10_vids <- renderTable({
    filtered_date <- filter(df, trendTime == input$top_date)
    sorted_videos <- filtered_date[order(-filtered_date$view_count), ]
    sorted_videos$trendTime <- as.factor(sorted_videos$trendTime)
    
    top_10_videos <- by(sorted_videos, sorted_videos$trendTime, function(x) head(x, 10))
    combined_top_videos <- do.call(rbind, top_10_videos)
    combined_top_videos <- select(combined_top_videos, title, channelTitle, Category, view_count, likes)
    
    colnames(combined_top_videos) <- c("Video Title", "Channel", "Category", "Views", "Likes")
    
    combined_top_videos <- mutate(combined_top_videos, Rank = row_number())
    combined_top_videos <- select(combined_top_videos, Rank, everything())
    
    return(combined_top_videos)
  })
  
  output$total_views_plot <- renderPlotly({
    filtered <- filter(df, df$trendTime >= input$total_views[1] & df$trendTime <= input$total_views[2] & df$Category == input$category_select)
    
    if (input$total_views[2] - input$total_views[1] <= 30) {
      date_breaks <- "1 day"
      date_labels <- "%b %d"
    } else if (input$total_views[2] - input$total_views[1] <= 90) {
      date_breaks <- "1 week"
      date_labels <- "%b %d"
    } else if (input$total_views[2] - input$total_views[1] <= 365) {
      date_breaks <- "1 month"
      date_labels <- "%b %Y"
    } else {
      date_breaks <- "3 months"
      date_labels <- "%b %Y"
    }
    
    grouped_date <- group_by(filtered, trendTime)
    view_change_df <- summarize(grouped_date,
                                sum_views = sum(view_count, na.rm = TRUE))
    
    # find max values
    max_values <- view_change_df[view_change_df$sum_views == max(view_change_df$sum_views), ]
    
    # average views
    avg_views <- mean(view_change_df$sum_views, na.rm = TRUE)
    
    spike_threshold <- 2 * avg_views
    spike_points <- view_change_df$sum_views >= spike_threshold
    
    views_per_time <- ggplot(data = view_change_df) +
      geom_line(mapping = aes(x = trendTime, y = sum_views)) +
      geom_hline(yintercept = avg_views, color = "blue", linetype = "dashed") +
      labs(caption = "Horizontal blue line: average total number of views",
           x = "Date", y = "Total Number of Views") +
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels) +
      scale_y_continuous(labels = scales::comma) 
    
    return(views_per_time)
  })
  
  
  output$category_views_plot <- renderPlotly({
    filtered <- filter(df, df$trendTime >= input$category_views[1] & df$trendTime <= input$category_views[2] & df$Category == input$category_select)
    
    if (input$category_views[2] - input$category_views[1] <= 30) {
      date_breaks <- "1 day"
      date_labels <- "%b %d"
    } else if (input$category_views[2] - input$category_views[1] <= 90) {
      date_breaks <- "1 week"
      date_labels <- "%b %d"
    } else if (input$category_views[2] - input$category_views[1] <= 365) {
      date_breaks <- "1 month"
      date_labels <- "%b %Y"
    } else {
      date_breaks <- "3 months"
      date_labels <- "%b %Y"
    }
    
    grouped_date <- group_by(filtered, trendTime, Category)
    view_change_df <- summarize(grouped_date,
                                sum_views = sum(view_count, na.rm = TRUE))
    
    
    # find max values
    max_values <- view_change_df[view_change_df$sum_views == max(view_change_df$sum_views), ]
    
    # average views
    avg_views <- mean(view_change_df$sum_views, na.rm = TRUE)
    
    spike_threshold <- 2 * avg_views
    spike_points <- view_change_df$sum_views >= spike_threshold
    
    views_per_time <- ggplot(data = view_change_df) +
      geom_line(mapping = aes(x = trendTime, y = sum_views)) +
      geom_hline(yintercept = avg_views, color = "blue", linetype = "dashed") +
      labs(caption = "Horizontal blue line: average total number of views",
           x = "Date", y = "Total Number of Views") +
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels) +
      scale_y_continuous(labels = scales::comma) 
    return(views_per_time)
  })
  
  output$days_to_trend_plot <- renderPlotly({
    if (is.null(input$trending_select)) {
      return(plotly::plot_ly())
    }
    
    filtered <- filter(df, df$Category %in% input$trending_select)
    grouped_days <- group_by(filtered, daysToBecomeTrending, Category)
    
    # Filter out bars with 0 value
    non_zero_bars <- filter(grouped_days, daysToBecomeTrending > 0)
    
    days_to_trend <- ggplot(data = non_zero_bars) +
      geom_col(mapping = aes(x = daysToBecomeTrending, y = daysToBecomeTrending, fill = Category), position = "stack", width = 1) +
      scale_x_discrete(labels = non_zero_bars$daysToBecomeTrending) +
      labs(x = "Number of Days", y = "Count")
    
    return(days_to_trend)
  })
  
  output$top_10_vids <- renderTable({
    filtered_date <- filter(df, trendTime == input$top_date)
    sorted_videos <- filtered_date[order(-filtered_date$view_count), ]
    sorted_videos$trendTime <- as.factor(sorted_videos$trendTime)
    
    top_10_videos <- by(sorted_videos, sorted_videos$trendTime, function(x) head(x, 10))
    combined_top_videos <- do.call(rbind, top_10_videos)
    combined_top_videos <- select(combined_top_videos, title, channelTitle, Category, view_count, likes)
    
    colnames(combined_top_videos) <- c("Video Title", "Channel", "Category", "Views", "Likes")
    
    combined_top_videos <- mutate(combined_top_videos, Rank = row_number())
    combined_top_videos <- select(combined_top_videos, Rank, everything())
    
    return(combined_top_videos)
  })
  
  observeEvent(input$resetBtn, {
    updateDateInput(session, "dateInput", value = min(df$trending_date))
  })
  selected_date <- reactive({
    df[df$trending_date == input$dateInput, ]
  })
  
  output$tagsPlot <- renderPlot({
    category_data <- selected_date()
    
    if ("Tags" %in% input$displayInput) {
      tags_by_category <- aggregate(number_of_tags ~ Category, data = category_data, FUN = sum)
      num_categories <- length(unique(tags_by_category$Category))
      tags_colors <- rep("purple", num_categories)
      
      ggplot(tags_by_category, aes(x = Category, y = number_of_tags, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Tags", title = "YouTube Tags by Category") +
        scale_fill_manual(values = tags_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = comma)  
    } else {
      ggplot() +
        theme_void()
    }
  })
  
  output$viewsPlot <- renderPlot({
    category_data <- selected_date()
    
    if ("View Count" %in% input$displayInput) {
      views_by_category <- aggregate(view_count ~ Category, data = category_data, FUN = sum)
      num_categories <- length(unique(views_by_category$Category))
      views_colors <- rep("blue", num_categories)
      
      ggplot(views_by_category, aes(x = Category, y = view_count, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "View Count", title = "YouTube View Count by Category") +
        scale_fill_manual(values = views_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = comma)  
    } else {
      
      ggplot() +
        theme_void()
    }
  })
  
  output$likesPlot <- renderPlot({
    category_data <- selected_date()
    
    if ("Likes" %in% input$displayInput) {
      likes_by_category <- aggregate(likes ~ Category, data = category_data, FUN = sum)
      num_categories <- length(unique(likes_by_category$Category))
      likes_colors <- rep("green", num_categories)
      
      ggplot(likes_by_category, aes(x = Category, y = likes, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Likes", title = "YouTube Likes by Category") +
        scale_fill_manual(values = likes_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = comma)  
    } else {
      
      ggplot() +
        theme_void()
    }
  })
  
  
  output$dislikesPlot <- renderPlot({
    category_data <- selected_date()
    
    if ("Dislikes" %in% input$displayInput) {
      dislikes_by_category <- aggregate(dislikes ~ Category, data = category_data, FUN = sum)
      num_categories <- length(unique(dislikes_by_category$Category))
      dislikes_colors <- rep("red", num_categories)
      
      ggplot(dislikes_by_category, aes(x = Category, y = dislikes, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Dislikes", title = "YouTube Dislikes by Category") +
        scale_fill_manual(values = dislikes_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = comma)  
    } else {
      
      ggplot() +
        theme_void()
    }
  })
  
  
  output$commentsPlot <- renderPlot({
    category_data <- selected_date()
    
    if ("Comments" %in% input$displayInput) {
      comments_by_category <- aggregate(comment_count ~ Category, data = category_data, FUN = sum)
      num_categories <- length(unique(comments_by_category$Category))
      comments_colors <- rep("orange", num_categories)
      
      ggplot(comments_by_category, aes(x = Category, y = comment_count, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Comments", title = "YouTube Comments by Category") +
        scale_fill_manual(values = comments_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = comma)  
    } else {
      ggplot() +
        theme_void()
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
