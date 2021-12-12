#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Users/Josep/OneDrive/School/Fall 2021/D590 - Applied Data Science/Project")
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

#import data
skills999 <- read.csv("Skills999.csv", header=T)
topskill <- read.csv("SkillsData9.csv", header=T)


# Compute distances
distances <- dist(topskill[2:36], method = "euclidean")

# Hierarchical clustering
clustertopskill <- hclust(distances, method = "ward.D")

#ClusterGroups
clusterGroups = cutree(clustertopskill, k = 25)

clusterGroups

cluster1 = subset(topskill, clusterGroups == 1)
# Look at the first 10 titles in this cluster:
cluster1$jobID[1:10]

cluster2 = subset(topskill, clusterGroups == 2)
# Look at the first 10 titles in this cluster:
cluster2$jobID[1:10]

cluster3 = subset(topskill, clusterGroups == 3)
# Look at the first 10 titles in this cluster:
cluster3$jobID[1:10]


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("Job Skills", theme = shinytheme("lumen"),
               tabPanel("Current Job Skills", fluid = TRUE,
                        titlePanel("Skills for Current Job"), 
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("jobname",
                                            "Select Your Current Occuaption:",
                                            choices=unique(skills999$jobID))
                            ),
                            mainPanel(plotOutput("plot"),
                            )
                        )
               ),
               tabPanel("Related Careers", fluid = TRUE,
                        titlePanel("Careers Related to Current Job"),       
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("jobname",
                                            "Select Your Current Occuaption:",
                                            choices=unique(skills999$jobID))
                            ),
                            mainPanel(textOutput("text"),
                            )
                        )
               ),
               tabPanel("About", fluid = TRUE,
                        titlePanel("About the Project"),       
                        fluidRow(
                            column(6,
                                   #br(),
                                   h4(p("Job Skill Project")),
                                   h5(p("This project is intended to allow users to explore the skill ratings of their current job and identity other careers with related skills. The project sought to answer the research questions: What are the known skills for each occupation? What are related careers that a person might consider pursuing?")),
                                   br(),
                                   h5(p("The project included three data science tasks. The first task was data wrangling and exploratory analysis. The second task was clustering. The third task was data visualization. This shiny app is a result of those three tasks.")),
                                   br(),
                                   h5(p("The data used in this project is from O*Net and is available", a("here", href = "https://www.onetcenter.org/database.html"),".")),
                                   br(),
                                   h5(p("The source code for this app is available ", a("on github", href = "https://github.com/IndianaJoseph/ADS_NextCareer"),".")),
                                   
                                   
                                   #hr(),
                                   
                                   
                            ),
                        ),
               ),
    ),
)

#Define server logic
server <- function(input, output, session) {
    
    output$plot <- renderPlot({
        skills999 %>%
            filter(jobID == input$jobname) %>%
            ggplot(aes(x = skill, y = rating)) + geom_bar(stat = "identity", fill = "darkblue") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$text <- renderText({
        cluster2$jobID[1:10]
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
