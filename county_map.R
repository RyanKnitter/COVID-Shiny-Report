county_map <- function(df){
  lst_rpt_dt <- df %>%
    filter(!forecast) %>%
    select(rpt_dt) %>%
    summarize(max(rpt_dt))
  
  lst_rpt_dt <- lst_rpt_dt[1,]
    
  
  shinyApp(
    ui = fluidPage(includeCSS("styles.css"),
                   fluidRow(column(10,
                       plotlyOutput("mapPlot"))),
                   
                   fluidRow(column(10,
                     sliderInput('rpt_dt',
                                 h4('Report Date'),
                                 min = as.Date('2020-02-01'),
                                 max = lst_rpt_dt,
                                 step = 7,
                                 value = lst_rpt_dt,
                                 width = "100%",
                                 animate=TRUE,
                                 timeFormat='%b, %d %y',
                                 animationOptions(interval = 1, 
                                                  loop = TRUE))))
    ),
    
    server = function(input, output) {
      
      df_map <- reactive(df %>%
                   filter(rpt_dt == input$rpt_dt) %>%
                   select(rpt_dt, Combined_Key, Province_State, Lat, Long_,
                          POPESTIMATE2019, est_active_per_100k) %>%
                   mutate(mod_est_active = ifelse(est_active_per_100k < 0,
                                                  0,
                                                  ifelse(est_active_per_100k > 3000,
                                                  3000,
                                                  est_active_per_100k))))
     
      output$mapPlot <- renderPlotly({
        ## Build plotly Map Plot
        g <- list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showland = TRUE,
          landcolor = toRGB("gray85"),
          subunitwidth = 1,
          countrywidth = 1,
          subunitcolor = toRGB("white"),
          countrycolor = toRGB("white")
        )
        
        fig <- plot_geo(df_map(), locationmode = 'USA-states', sizes = c(20, 800))
        fig <- fig %>% 
          add_markers(x = ~Long_, y = ~Lat, size = ~POPESTIMATE2019, 
                      color = ~mod_est_active, colors='Reds',
                      hoverinfo = "text", alpha = 0.5, stroke=I('lightgrey'),span=I(1),
                      text = ~paste(df_map()$Combined_Key,"<br />",'Pop:',
                                    format(df_map()$POPESTIMATE2019,
                                           big.mark=',',digits=0, trim=TRUE),"<br />",
                                    "Est. Active:",
                                    format(df_map()$est_active_per_100k,
                                           big.mark=',',digits=0, trim=TRUE))) %>%
          colorbar(title='Est. Active')
        fig <- fig %>% layout(title = paste('Estimated Active Cases per 100k <br />', 
                                            format(as.Date(input$rpt_dt),'%b, %d %Y')), 
                              geo = g)
        
        fig
        
      })
    },
    options = list(height = 550)
  )
}
