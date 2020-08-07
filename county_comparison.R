#' @import shiny dplyr plotly
#' @export


county_compare <- function(df){
  shinyApp(
    ui = fluidPage(includeCSS("styles.css"),
                    # Application title
                    #titlePanel("COVID-19 County Comparison"),
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('county1',
                                    h4('Select County 1'),
                                    choices = levels(df$Combined_Key),
                                    selected = 'Ramsey, Minnesota, US'),
                        selectInput('county2',
                                    h4('Select County 2'),
                                    choices = levels(df$Combined_Key),
                                    selected = 'Hennepin, Minnesota, US'),
                        sliderInput('contagious',
                                    h4('Days Contagious'),
                                    min = 1,
                                    max = 20,
                                    step = 1,
                                    value = 10),
                        fluidRow(column(width=6,
                                        sliderInput('presymptoms',
                                          h4('Days to Symptoms'),
                                          min = 0,
                                          max = 14,
                                          step = 1,
                                          value = 5)),
                                 column(width=6,
                                        sliderInput('test_results',
                                          h4('Days to Test Results'),
                                          min = 0,
                                          max = 6,
                                          step = 1,
                                          value = 3,
                                          round = TRUE))),
                        sliderInput('asymptomatic_adj',
                                    h4('Percent Unreported Asymptomatic'),
                                    min = 0,
                                    max = 80,
                                    step = 10,
                                    value = 40,
                                    post = '%')
                       # sliderInput('testing_adj',
                        #            h4('Adjust for Undertesting'),
                        #            min = 0,
                        #            max = 1,
                        #            step = .1,
                        #            value = 1)
                      ),
                      
                      mainPanel(
                        plotlyOutput("linePlot")
                      )
                    )
    ),
    
    # Define server logic required to draw a histogram
    server = function(input, output) {
      
      lst_rpt_dt <- df %>%
        filter(!forecast) %>%
        select(rpt_dt) %>%
        summarize(max(rpt_dt))
      
      lst_rpt_dt <- lst_rpt_dt[1,]
      
      df1 <- reactive(df %>%
                filter(Combined_Key == input$county1) %>%
                select(rpt_dt, cum_confirmed, POPESTIMATE2019))
      
      df2 <- reactive(df %>%
                filter(Combined_Key == input$county2) %>%
                select(rpt_dt, cum_confirmed, POPESTIMATE2019))
      
      df0 <- reactive(df1() %>%
                full_join(df2(), by = 'rpt_dt'))
      
      df_active <- reactive(df0() %>%
                      arrange(rpt_dt) %>%
                      mutate(est_active_per_100k.x = (lead(cum_confirmed.x, 
                                                           n=(input$presymptoms + input$test_results)) -
                                                        coalesce(lag(cum_confirmed.x,
                                                                     n=input$contagious,
                                                                     default=0),0))/POPESTIMATE2019.x*100000,
                             est_active_per_100k.y = (lead(cum_confirmed.y, 
                                                           n=(input$presymptoms + input$test_results)) -
                                                        coalesce(lag(cum_confirmed.y,
                                                                     n=input$contagious,
                                                                     default=0),0))/POPESTIMATE2019.y*100000))
      
      df_active_adj1 <- reactive(df_active() %>%
                           filter(rpt_dt <= lst_rpt_dt) %>%
                           arrange(rpt_dt) %>%
                           mutate(est_active_per_100k.x = est_active_per_100k.x/(1-input$asymptomatic_adj/100),
                                  est_active_per_100k.y = est_active_per_100k.y/(1-input$asymptomatic_adj/100)))
      
      
      output$linePlot <- renderPlotly({
        fig <- df_active_adj1() %>%
          plot_ly(x = ~rpt_dt, y = ~est_active_per_100k.x,
                  type = 'scatter',
                  mode = 'lines',
                  name = input$county1,
                  hoverinfo = "text",
                  text = ~paste(format(as.Date(df_active_adj1()$rpt_dt),"%b, %d %y"),"<br />",
                                format(df_active_adj1()$est_active_per_100k.x,
                                       , big.mark=',',digits=0, trim=TRUE))) %>%
          add_trace(y = ~est_active_per_100k.y, name = input$county2,
                    hoverinfo = "text",
                    text = ~paste(format(as.Date(df_active_adj1()$rpt_dt),"%b, %d %y"),"<br />",
                                  format(df_active_adj1()$est_active_per_100k.y,
                                         , big.mark=',',digits=0, trim=TRUE))) %>%
          layout(font = list(family = 'arial'),
                 legend = list(y = 1.1,
                               x = 0),
                 xaxis = list(title='Report Date',
                              type='date',
                              tickformat="%b, %d %y"),
                 yaxis = list(title='Estimated Active Cases per 100k'),
                 colorway = c('#000000', '#00f3b3', '#000499', '#000499', '#9482ff', '#c7ab75', '#bfd3e6'))
        
        fig
      })
    },
    options = list(height = 660)
  )
}