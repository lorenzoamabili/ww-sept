data = read.csv("/Users/lorenzoamabili/Library/Mobile Documents/com~apple~CloudDocs/Job/job applications/Veramed/dataSept.csv", sep = ",")

library(ggplot2)
library(survminer)
library(cmprsk)

library(shiny)
library(rsconnect)

ui <- fluidPage(
  fluidRow(column(width = 12,
                  plotOutput("plot",         
                             hover = "plot1_hover",
                             brush = brushOpts(
                               id = "plot1_brush",
                               resetOnNew = TRUE
                             ))),
           column(width = 4, plotOutput("plot1")),
           column(width = 4, plotOutput("plot2")),
           column(width = 4, plotOutput("plot3")),
           column(width = 4, plotOutput("plot4")),
           column(width = 4, plotOutput("plot5")),
           column(width = 4, plotOutput("plot6"))),
           
           mainPanel(
             div(style="display: inline-block; width: 150px" ,
                 sliderInput("range", "Range:",
                             min = 0, max = max(data$ftime),
                             value = c(0, max(data$ftime)))),
             div(style="display: inline-block; width: 150px",
                 sliderInput("width", "width", min = 0, max = 800, value = 300)),
             div(style="display: inline-block; width: 150px",
                 sliderInput("height", "height", min = 0, max = 800, value = 250)),
                 radioButtons("sex", "Sex:",
                          c("All" = "all",
                            "Male" = "male",
                            "Female" = "female"),   inline = T),
                 radioButtons("age", "Age:",
                          c("All" = "all",
                            "Under70" = "under",
                            "Over70" = "over"), inline = T),
             ))

server <- function(input, output, session) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)

  output$plot <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      cencode = 0
    )
    
    p = ggcompetingrisks(
      fit = ci_fit, 
      multiple_panels = FALSE,
      ylim = c(0, 1)
    ) + theme_bw()
    
    p
    
  }
  )
  
  output$plot1 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit1 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$sympt_fever,
      cencode = 0
    )
    
    p1 = ggcompetingrisks(
      fit = ci_fit1, 
      multiple_panels = FALSE,
      title = "Fever",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw() 
  
    p1
    
  },
  width = function() input$width,
  height = function() input$height
  )
  
  output$plot2 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit2 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$sympt_dyspnea,
      cencode = 0
    )
    
    p2 = ggcompetingrisks(
      fit = ci_fit2, 
      multiple_panels = FALSE,
      title = "Dyspnea",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw()
    p2
  }, 
  width = function() input$width,
  height = function() input$height
  )
  
  output$plot3 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit3 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$comorb_dm,
      cencode = 0
    )
    
    p3 = ggcompetingrisks(
      fit = ci_fit3, 
      multiple_panels = FALSE,
      title = "Diabetes mellitus",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw() 
    
    p3
    
  },
  width = function() input$width,
  height = function() input$height
  )
  
  output$plot4 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit4 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$comorb_dm,
      cencode = 0
    )
    
    p4 = ggcompetingrisks(
      fit = ci_fit4, 
      multiple_panels = FALSE,
      title = "Diabetes mellitus",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw() 
    
    p4
    
  },
  width = function() input$width,
  height = function() input$height
  )
  
  output$plot5 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit5 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$comorb_dm,
      cencode = 0
    )
    
    p5 = ggcompetingrisks(
      fit = ci_fit5, 
      multiple_panels = FALSE,
      title = "Diabetes mellitus",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw() 
    
    p5
    
  },
  width = function() input$width,
  height = function() input$height
  )
  
  output$plot6 <- renderPlot({
    data <- data
    if(input$sex != "all"){
      data <- data[data$sex == input$sex,]
    } else {
      data = data
    }
    if(input$age != "all"){
      if(input$age == "under"){
        data <- data[data$age < 70,]
      } else {
        data <- data[data$age > 70,]
      }
    } else {
      data = data
    }
    
    ftimeMin <- min(input$range)
    ftimeMax <- max(input$range)
    
    ci_fit6 <- cuminc(
      ftime = data$ftime, 
      fstatus = data$status, 
      group = data$comorb_dm,
      cencode = 0
    )
    
    p6 = ggcompetingrisks(
      fit = ci_fit6, 
      multiple_panels = FALSE,
      title = "Diabetes mellitus",
      xlim = ranges$x, ylim = ranges$y
    ) + theme_bw() 
    
    p6
    
  },
  width = function() input$width,
  height = function() input$height
  )
  
  observeEvent(input$plot1_hover, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

shinyApp(ui, server)



