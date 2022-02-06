#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(dplyr)
library(heatmaply)

source('global.R', local=TRUE)



# Define UI for application that draws a histogram
ui <- navbarPage(title="NBA_統計諮詢與實務",
                 navbarMenu("統計分析",
                            tabPanel("30球隊基礎數據",fluidRow(column(3,sliderInput("Year","Year:",min = 2012,max = 2018,value = 2012)),
                                                         column(2, selectInput(inputId = "Ind_1",label ="X1:",choices = c(index),selected ="ORtg")),
                                                         column(2,selectInput(inputId = "Ind_2",label ="X2:",choices = c(index),selected ="DRtg")),
                                                         column(2, radioButtons(inputId = "Conference",label = "Conference:",choices =c("All","East", "West"))),
                                                         column(2, radioButtons(inputId = "Color",label = "Colour:",choices =c("Salary", "Age"),selected ="Age"))
                            ),fluidRow(column(8,plotlyOutput("scatter_fig")),
                                       column(4,plotlyOutput("fundamental_bar",height = 200),plotlyOutput("Shooting_bar",height = 200)))),
                            tabPanel("30球隊薪資與投球比例",sliderInput("Year_2","Year:",min = 2012,max = 2018,value = 2012),
                                     plotlyOutput("Salary"),plotlyOutput("Pitch"))
                 ),
                 
                 navbarMenu(
                     "機器學習",
                     tabPanel("Correlation",plotlyOutput("correlation"),height=400,width=400),
                     tabPanel("Feature Importance",fluidRow(
                         column(6,selectInput(inputId = "model",label ="Model:",choices = c(model),selected ="Lasso")),
                         column(6,radioButtons(inputId = "response",label = "Response:",choices =c("W", "Salary"),selected ="W"))),
                         fluidRow(
                             column(6,plotlyOutput("importance")),
                             column(6,plotlyOutput("MAE")
                             )
                             
                         )
                     )
                 ),
                 navbarMenu(
                     "薪資組成",tabPanel("薪資組合",
                                       fluidRow(height=200,
                         column(6,sliderInput("Year_Salary","Year:",min = 2012,max = 2018,value = 2012)),
                         column(6,radioButtons(inputId = "Conference_Salary",label = "Conference:",choices =c("All","East", "West"))),
                         fluidRow(
                             column(8,plotlyOutput("scatter_Salary")),
                             column(4,plotlyOutput("coef"),height=600)
                         ),
                         fluidRow(
                             column(6,plotlyOutput("Team_Salary")),
                             column(6,plotlyOutput("Position_Salary"))
                         )
                     )
                     
                 )
                 )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    reactive_Data  <-
        reactive({
            if (input$Conference=="All") {
                All_data %>%filter(Year %in% input$Year )
            }else{
                All_data %>%filter(Year %in% input$Year &Conference %in% input$Conference)
            }
            
        })
        output$scatter_fig <- renderPlotly({
        p <- plot_ly(reactive_Data(), x = ~get(input$Ind_1), y = ~get(input$Ind_2),color = ~get(input$Color), type = 'scatter', source = 'scatter_fig', customdata= ~Team,mode = 'markers',
                    marker = list(size = ~W, opacity = 0.5,sizemode = "diameter"), text=~Team,hovertemplate = paste(
                        "<b>%{text}</b><br>",
                        "%{yaxis.title.text}: %{y}<br>",
                        "%{xaxis.title.text}: %{x}<br>",
                        "W: %{marker.size:,}",
                        "<extra></extra>"
                    )) 
        t <- list(
            family = ~Team,
            size = 5,
            color = toRGB("black"))
        p <- p %>% layout(title = paste(input$Ind_1,"v.s",input$Ind_2,"in 30 NBA teams (大小為勝率)"),xaxis = list(title =input$Ind_1),
                          yaxis = list(title =input$Ind_2)) %>% add_text(textfont = t, textposition = "middle", showlegend = FALSE)%>% colorbar(title = input$Color)
        })
        
        
        fundamental_League <-reactive({fundamental_dat%>%filter(Year %in% input$Year&Team %in% "League")})
        Shooting_League <-reactive({Shooting_dat%>%filter(Year %in% input$Year&Team %in% "League")})
        Team_selected <- reactive({event_data("plotly_hover",source = "scatter_fig")})
        if (is.null(Team_selected)) {
            Team_null <- c("Atlanta Hawks")
            fundamental_Team <-reactive({fundamental_dat%>%filter(Year %in% input$Year&Team %in% Team_null)})
        }else{
            fundamental_Team <-reactive({
                Team_selected <- event_data("plotly_hover",source = "scatter_fig")$customdata
                fundamental_dat%>%filter(Year %in% input$Year & Team %in% Team_selected)})
        }
        Shooting_selected <- reactive({event_data("plotly_hover",source = "scatter_fig")})
        if (is.null(Shooting_selected)) {
            Shooting_null <- c("Atlanta Hawks")
            Shooting_Team <-reactive({Shooting_dat%>%filter(Year %in% input$Year&Team %in% Shooting_null)})
        }else{
            Shooting_Team <-reactive({
                Team_selected <- event_data("plotly_hover",source = "scatter_fig")$customdata
                Shooting_dat%>%filter(Year %in% input$Year & Team %in% Team_selected)})
        }    
        
        output$fundamental_bar <- renderPlotly({
            p<-plot_ly()
            p<-add_trace(p,
                        x = fundamental[2:9], 
                        y = as.numeric(fundamental_League()[2:9]), 
                        type = 'bar',name="League")
            p<-add_trace(p,
                     x = fundamental[2:9], 
                     y = as.numeric(fundamental_Team()[2:9]),
                     type = 'bar',name=fundamental_Team()[1])
            p <- p %>% layout(title = 'Basic Stats')
            p
            })
        output$Shooting_bar <- renderPlotly({
            p<-plot_ly()
            p<-add_trace(p,
                         x = Shooting[2:6], 
                         y = as.numeric(Shooting_League()[2:6]), 
                         type = 'bar',name="League")
            p<-add_trace(p,
                         x = Shooting[2:6], 
                         y = as.numeric(Shooting_Team()[2:6]),
                         type = 'bar',name=Shooting_Team()[1])
            p <- p %>% layout(title = 'Shooting Stats')
            p
        })
        Salary_Data  <- reactive({Salary %>%filter(Year %in% input$Year_2)})
        output$Salary <- renderPlotly({
            
            p <-plot_ly(Salary_Data(),x = ~Team, y = ~Salary, type = 'bar'
                        , color = ~Position,marker=list(line=list(color="gray50",width=.5)),text=~Name,hovertemplate = paste(
                            "<b>%{x}</b><br>",
                            "%{text}</b><br>",
                            "%{yaxis.title.text}: %{y}</b><br>",
                            "<extra></extra>"
                        )) %>%layout(barmode = 'stack')
        })
        Pitch_Data  <- reactive({Pitch %>%filter(Year %in% input$Year_2)})
        output$Pitch <- renderPlotly({
            
            p <-plot_ly(Pitch_Data(),x = ~Team, y = ~Ratio, type = 'bar'
                        , color = ~Type,marker=list(line=list(color="gray50",width=.5)),hovertemplate = paste(
                            "<b>%{x}</b><br>",
                            "%{yaxis.title.text}: %{y}</b><br>",
                            "<extra></extra>"
                        )) %>%layout(barmode = 'stack')
        })
        output$correlation <- renderPlotly({
            
           p <-  heatmaply_cor(
                as.matrix(correlation),
                xlab = "Features", 
                ylab = "Features",
                k_col = 2, 
                k_row = 2
            )%>% layout(height = 600, width = 700)
           p 
        })
        importance_dat <- reactive({Feature_importance %>%filter(response %in% input$response&model%in% input$model)})
        output$importance <- renderPlotly({
            p<-plot_ly()
            p<-add_trace(p,
                         data = importance_dat(),
                         x = ~feature, 
                         y = ~regression_importance/10, 
                         type = 'bar',name="regression")
            p<-add_trace(p,
                         data = importance_dat(),
                         x = ~feature, 
                         y = ~permutation_importance,
                         type = 'bar',name="permutation")
            p <- p %>% layout(title = 'Feature importance',yaxis=list(title="value"))
            p
        })
        MAE_dat <- reactive({MAE %>%filter(response %in% input$response)})
        output$MAE <- renderPlotly({
            p<-plot_ly()
            p<-add_trace(p,
                         data = MAE_dat(),
                         x = ~model, 
                         y = ~MAE, 
                         type = 'bar',name="regression")
            p <- p %>% layout(title = 'MAE',yaxis=list(title="value"))
            p
        })
        output$Team_Salary <-renderPlotly({
            ggplotly(
                ggplot(Team_dat, aes(x = Year, y = Salary, color = Category)) +
                    geom_line()+
                    ggtitle("NBA球員薪資趨勢")+
                    scale_x_continuous(breaks =seq(2012,2018))
                )
        })
        output$Position_Salary <-renderPlotly({
            p <- ggplot(Position_dat, aes(x = Year, y = Salary, 
                                          colour = Type,
                                          linetype =Type))+
                
                geom_line()+
                ggtitle("各位置球員薪資趨勢")+
                scale_color_manual(name = "", values = rep(c("blue", "red", "black"), each = 3)) +
                scale_linetype_manual(name = "", values = rep(c(1,2,3), times = 3))+
                scale_x_continuous(breaks =seq(2012,2018) )
            ggplotly(p,tooltip = c("x","y","linetype"))
                
            
            
        })
        output$coef <-renderPlotly({
            p <-ggplot(dat, aes(x = Variable, y = Coefficient,text=paste("Variable:",Variable,"<br>Coefficient:",Coefficient,"<br>p-value:",p.value))) +
                geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
                geom_point(aes(x =factor(Variable, levels=c('CxFxG',"FxG",'CxG','CxF','G_star','F_star','C_star')), 
                               y = Coefficient,colour=p.value)) + 
                scale_colour_gradient(low = "red", high = "black")+
                geom_linerange(aes(x = Variable, 
                                   ymin = conf.low_95,
                                   ymax = conf.high_95),
                               lwd = 1) + 
                ggtitle("Coefficient plot-線性模型") +
                coord_flip()
            ggplotly(p,tooltip = c("text"))
            
        })
        reactive_Salary  <-
            reactive({
                if (input$Conference_Salary=="All") {
                    scatter_Salary %>%filter(Year %in% input$Year_Salary )
                
                }else{
                    scatter_Salary %>%filter(Year %in% input$Year_Salary &Conference %in% input$Conference_Salary)
                   }
                
            })
        output$scatter_Salary <- renderPlotly({
            fig <- plot_ly(reactive_Salary(),x = ~Guard, y = ~Forward,
                color = ~Center, marker = list(size = ~W, opacity = 0.5,sizemode = "diameter"), text=~Team,hovertemplate = paste(
                    "<b>%{text}</b><br>",
                    "%{yaxis.title.text}: %{y}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "W: %{marker.size:,}",
                    "<extra></extra>"
                )
            )
            t <- list(
                family = ~Team,
                size = 5,
                color = toRGB("black"))
            
            fig%>% add_text(textfont = t, textposition = "middle", showlegend = FALSE)%>%layout(title="各位置薪資與勝率關係 (大小為勝率)")
           
        })
    
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
