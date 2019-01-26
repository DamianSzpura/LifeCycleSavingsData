library(ggplot2)
library(shiny)
library(reshape2)

first_plot <- ggplot(LifeCycleSavings, aes(x = reorder(row.names(LifeCycleSavings), LifeCycleSavings$sr), y = LifeCycleSavings$sr, fill = LifeCycleSavings$sr)) + 
              geom_col(colour="black") +
              scale_fill_gradient(
                low = "lightskyblue", 
                high = "lightskyblue4", 
                guide = FALSE
              ) + 
              labs(x="Countries", y="Aggregate personal savings") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.1)),
                    axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                    plot.margin = unit(c(2,4.2,2,1), "cm"))
                    

LifeCycleSavings.temp <- LifeCycleSavings[order(-LifeCycleSavings$sr),]
LifeCycleSavings.temp[4] <- row.names(LifeCycleSavings.temp)
LifeCycleSavings.temp[1] <- LifeCycleSavings.temp[2]
LifeCycleSavings.temp[2] <- 100-LifeCycleSavings.temp[1]-LifeCycleSavings.temp[3]
colnames(LifeCycleSavings.temp)[1] <- "Under 15"
colnames(LifeCycleSavings.temp)[2] <- "Between 15-75"
colnames(LifeCycleSavings.temp)[3] <- "Above 75"
LifeCycleSavings.long <- melt(LifeCycleSavings.temp[1:4], id.vars="dpi")

LifeCycleSavings.long$dpi <- factor(LifeCycleSavings.long$dpi, levels = rev(unique(LifeCycleSavings.long$dpi )), ordered=TRUE)

second_plot <-  ggplot(LifeCycleSavings.long, aes(dpi, value,fill=variable))+
                geom_bar(stat="identity", colour="black") +
                scale_colour_manual(
                  name = "Age",
                  values = c("lightskyblue", "lightskyblue2", "lightskyblue4"),
                  aesthetics = c("colour", "fill")
                ) + 
                labs(x="Countries", y="% of people") +
                theme(
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.1)),
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,0.55,2,1), "cm"))

LifeCycleSavings.temp <- LifeCycleSavings[order(-LifeCycleSavings$dpi),]
LifeCycleSavings.temp[3] <- row.names(LifeCycleSavings.temp)
LifeCycleSavings.temp[4] <- LifeCycleSavings.temp[4]/100
LifeCycleSavings.long <- melt(LifeCycleSavings.temp[3:5], id.vars="pop75")

LifeCycleSavings.long$pop75 <- factor(LifeCycleSavings.long$pop75, levels = rev(unique(LifeCycleSavings.long$pop75 )), ordered=TRUE)

third_plot <-  ggplot(LifeCycleSavings.long, aes(pop75, value,fill=variable))+
                geom_bar(stat="identity", position="dodge", colour="black") +
                scale_colour_manual(
                  name = "Values",
                  values = c("lightskyblue", "lightskyblue4"),
                  aesthetics = c("colour", "fill")
                ) + 
                labs(x="Countries", y="Dpi/100") +
                theme(
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.1)),
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,2,2,1), "cm"))

LifeCycleSavings.temp <- LifeCycleSavings[order(-LifeCycleSavings$dpi),]
LifeCycleSavings.temp[4] <- row.names(LifeCycleSavings.temp)
LifeCycleSavings.temp[1] <- LifeCycleSavings.temp[2]
LifeCycleSavings.temp[2] <- 100-LifeCycleSavings.temp[1]-LifeCycleSavings.temp[3]
colnames(LifeCycleSavings.temp)[1] <- "Under 15"
colnames(LifeCycleSavings.temp)[2] <- "Between 15-75"
colnames(LifeCycleSavings.temp)[3] <- "Above 75"
LifeCycleSavings.long <- melt(LifeCycleSavings.temp[1:4], id.vars="dpi")

LifeCycleSavings.long$dpi <- factor(LifeCycleSavings.long$dpi, levels = rev(unique(LifeCycleSavings.long$dpi )), ordered=TRUE)

fourth_plot <- ggplot(LifeCycleSavings.long, aes(dpi, value,fill=variable))+
              geom_bar(stat="identity", colour="black") +
              scale_colour_manual(
                name = "Age",
                values = c("lightskyblue", "lightskyblue2", "lightskyblue4"),
                aesthetics = c("colour", "fill")
              ) + 
              labs(x="Countries", y="% of people") +
              theme(
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.1)),
                axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                plot.margin = unit(c(2,0.55,2,1), "cm"))

fifth_plot <- ggplot(LifeCycleSavings, aes(x = row.names(LifeCycleSavings), y = LifeCycleSavings$dpi, size = LifeCycleSavings$dpi, fill = LifeCycleSavings$dpi)) +
              geom_point(shape=21, color="lightskyblue4")+
              coord_flip()+
              scale_radius(
                range = c(3,10),
                guide = FALSE
              )+
              scale_fill_gradient(
                name = "dpi",
                low = "lightskyblue4", high = "lightskyblue"
              ) + 
                labs(x="Countries", y="Real per-capita disposable income") +
                theme(
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,2,2,1), "cm"))

sixth_plot <- ggplot(LifeCycleSavings, aes(x = LifeCycleSavings$ddpi, y = LifeCycleSavings$dpi, size = LifeCycleSavings$dpi, fill = LifeCycleSavings$dpi)) +
              geom_point(shape=21, color="lightskyblue4")+
              geom_smooth(colour = "black")+
              scale_radius(
                range = c(3,10),
                guide = FALSE
              )+
              scale_fill_gradient(
                name = "dpi",
                low = "lightskyblue4", high = "lightskyblue"
              )+
              labs(x="% growth rate of dpi", y="Real per-capita disposable income") +
              theme(
                axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                plot.margin = unit(c(2,2,2,1), "cm"))


ui <- bootstrapPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Allerta+Stencil');

      h1, h3 {
        font-family: 'Allerta Stencil', sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #000;
        text-align: center;
      }

      div {
        position:relative;
      }
    "))
  ),
  
  h1('Diagrams for Life Cycle Savings'),
  hr(),
  div(
    h3('Aggregate personal savings for each country'),
    plotOutput('oneplot'),
    h3('% of people separated into three groups, sorted for upper diagram'),
    plotOutput('twoplot', height = "500px") 
  ),
  hr(),
  div(
    h3('Comparison of real per-capita disposable income and % growth rate of it for each country'),
    plotOutput('threeplot'),
    h3('% of people separated into three groups, sorted for upper diagram'),
    plotOutput('fourplot', height = "500px")
  ),
  hr(),
  div(
    h3('Real per-capita disposable income for each country'),
    plotOutput('fiveplot', height = "900px") 
  ),
  hr(),
  div(
    h3('Relationship between real per-capita disposable income and % growth rate of it'),
    plotOutput('sixplot', height = "900px") 
  )
)

server <- function(input, output) {
  output$oneplot <- renderPlot({
    first_plot
  })
  output$twoplot <- renderPlot({
    second_plot
  })
  output$threeplot <- renderPlot({
      third_plot
  })
  output$fourplot <- renderPlot({
    fourth_plot
  })
  output$fiveplot <- renderPlot({
    fifth_plot
  })
  output$sixplot <- renderPlot({
    sixth_plot
  })
}

shinyApp(ui = ui, server = server)


