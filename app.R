# "Trendy cars from 70's" project

# loading and setting global data
library(shiny)
library(ggplot2)
library(data.table)
appDF <- data.table(mtcars)
appDF$carName <- rownames(mtcars)

appDF <- within(appDF, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

# some renaming
names(appDF)[names(appDF)=="am"] <- "Transmission"
names(appDF)[names(appDF)=="disp"] <- "Displacement"

# static stuff for visualisations
gg_range <- function(x) {
  data.frame(ymin = min(x), # Min
             ymax = max(x))} # Max
posn.d <- position_dodge(0.9)
med_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])}  # 3rd quartile

customTitleTheme <- theme(plot.title = element_text(colour = "darkslateblue", face = "italic"))

ui <- fluidPage(
   
   # Application title
   titlePanel("Trendy cars from 70's"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("hp",
                     "Specify horse power range up to your interest:",
                     min = min(appDF$hp),
                     max = max(appDF$hp),
                     value = c(min(appDF$hp),max(appDF$hp))),
         sliderInput("wt",
                     "Weight range you are interested in:",
                     min = min(appDF$wt),
                     max = max(appDF$wt),
                     value = c(min(appDF$wt),max(appDF$wt)))
        ,checkboxGroupInput("Transmission",
                     "Select transmission type",
                     choices = c("manual", "automatic"),
                     selected = c("manual","automatic")),
        checkboxGroupInput("cyl",
                           "Select number of cylinders",
                           choices = levels(appDF$cyl),
                           selected = c(4,6,8))
      ),
      
      mainPanel(
      tabsetPanel(type = "tabs",
                   tabPanel("Power vs. transmission", plotOutput("plot1")), 
                    tabPanel("Weight vs. mileage", plotOutput("plot2")),
                    tabPanel("Time for 1/4 mile", plotOutput("plot3")),
                    tabPanel("V-shaped engines", plotOutput("plot4"))
        )
      )
   )
)

server <- function(input, output) {
  # creating reactive DF
  # @param cyl - category of cars according to user specified number of cylinders
  # @param Transmission - category of cars according to user specified type of transmission
  # @param wt - weight of car
  # @param hp - horse power of car
  # @return reactiveAppDF - reactive (responsible to user input) dataframe
   reactiveAppDF <- reactive({
      appDF[cyl %in% input$cyl & 
              Transmission %in% input$Transmission &
              wt >= input$wt[1] & wt <= input$wt[2] &
              hp >= input$hp[1] & hp <= input$hp[2],]
   })
   output$plot1 <- renderPlot({
     wt.cyl.am <- ggplot(reactiveAppDF(), aes(x = cyl,y = hp, col = Transmission, fill = hp, group = Transmission))
     wt.cyl.am +
       stat_summary(geom = "linerange", fun.data = med_IQR,
                    position = posn.d, size = 3) +
       stat_summary(geom = "linerange", fun.data = gg_range,
                    position = posn.d, size = 3,
                    alpha = 0.4) +
       stat_summary(geom = "point", fun.y = median,
                    position = posn.d, size = 3,
                    col = "black", shape = "X")+
       ggtitle("Most powerful cars seem to have manual transmission")+
       customTitleTheme
   })
   
   output$plot2 <- renderPlot({
     ggplot(reactiveAppDF(), aes(x = wt, y = mpg, col = hp)) +
       geom_point()+
      geom_smooth(method = "loess")+
       geom_text(aes(label=carName),hjust=0, vjust=0, check_overlap = TRUE)+
       scale_colour_gradient(low = "orange", high = "red")+
       ggtitle("The heavier a car the less miles you will drive")+
       customTitleTheme
   })
   
   output$plot3 <- renderPlot({
     ggplot(reactiveAppDF(), aes(x = qsec)) +
       geom_histogram(binwidth = 2, fill = "#ff6200")+
       ggtitle(paste("On average",round(mean(reactiveAppDF()$qsec),2),"seconds for 1/4 mile"))+
       customTitleTheme+
       geom_vline(xintercept = round(mean(appDF$qsec),2))
   })
   
   output$plot4 <- renderPlot({
     ggplot(reactiveAppDF(), aes(x = Displacement, y = hp, col = Transmission, group = vs)) +
       geom_point()+geom_smooth()+
       facet_grid(.~vs)+
       geom_text(aes(label=carName),hjust=0, vjust=0, check_overlap = TRUE)+
       ggtitle("Comparative power of V-shaped engines")+
       customTitleTheme
   })
}

shinyApp(ui = ui, server = server)
