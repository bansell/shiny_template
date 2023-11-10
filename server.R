
server <- function(input, output, session) {
  
  source('global.R')
  

# user pwd authentication code --------------------------------------------

  
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base_hash,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
# UI sidePanel code --------------------------------------------
  
  #requires authentication to view
  
  
  output$sidebarlayout <- renderUI({
    
   req(credentials()$user_auth)
    
    

    sidebarLayout(
      
      
      position='left',
      
      
      
      sidebarPanel(
        
        id='sidebar',
        
        width=3,
        

# sidebar 1 ---------------------------------------------------------------

        
        conditionalPanel(condition='input.tabset==1',
                         
        selectInput("manufacturer", "Filter Manufacturer:", multiple=T, selected = NULL,
                    choices = unique(mpg$manufacturer)),
        
        radioButtons(inputId = 'selection', label = 'Colour by:',
                     choiceNames = c('Class','Manufacturer'),
                     choiceValues = c('class','manufacturer'))
        )

      , conditionalPanel(condition='input.tabset==2'
                         ,h3('Some text here for you')
                         ,br()
                         ,h5('Scroll over chart to reveal reciprocal data subsets'))

    , conditionalPanel(condition='input.tabset==3',
                       h5('ggplot reactive on plotly hover'))
),
        
      
  mainPanel(
    tabsetPanel(#type='tabs',
    
    id='tabset',
    
      
      # tab1 --------------------------------------------------------------------
      tabPanel('MPG scatter', value=1,
               width=9,
               br(),br(),
               plotOutput('plot1',width="800",height="500")),

      # tab2 --------------------------------------------------------------------

      tabPanel('Hover subplot',value=2,
             width=9,
             br(),br(),
             plotlyOutput("subplot", width="1100",height="600")),

# tab 3 -------------------------------------------------------------------

      tabPanel('plotly ~ ggplot react', value=3,
               width=9,
               br(),br(),
               plotlyOutput('plotlybar',width='500',height="300"),
               plotOutput('jitterbox',width="200",height="300"),
               br(),
               textOutput('plotlybar_txt')
      )
)))
    })
    
      

# code for plots and tables -----------------------------------------------

  

# plot 1 -------------------------------------------------------------------


## subset reactive ---------------------------------------------------------

  mpg_sub1 <- reactive({
    if(!is.null(input$manufacturer)){
      mpg %>% filter(manufacturer %in% c(input$manufacturer))
      }else{mpg}
  })
  
  

## plot call ---------------------------------------------------------------

  
  
      output$plot1 <- renderPlot({
        
        
        mpg_sub1() %>% 
          ggplot(aes(x=cty,y=hwy)) + 
          geom_point(aes_string(colour=input$selection),size=2) +
          theme(text =  element_text(size=24))
       
    })
  

# plot group 2 subplot -----------------------------------------------------------------

  output$subplot <- renderPlotly({
  
  #https://plotly-r.com/client-side-linking.html 
  
  #?highlight_key
  m <- highlight_key(mpg, ~class) #creates subgroups by mpg$class (cf group_by ?)
  
  p1 <- ggplot(m, aes(displ, fill = class))    + geom_density()
  p2 <- ggplot(m, aes(displ, hwy, fill = class)) + geom_point()


## plotly call -------------------------------------------------------------

    
  subplot(p1, p2) %>% hide_legend() %>% 
    highlight("plotly_hover") #this links the plots
  
 } )
    

# plot group 3 ------------------------------------------------------------


## plotly  -----------------------------------------------------------------

  
output$plotlybar <- renderPlotly({
  
  pltly_plt <- mpg %>% 
    ggplot(aes(x=class)) + 
    geom_bar(aes(fill=class, text=class)) + tidyExt::no_legend()
  
  ggplotly(pltly_plt, tooltip=c('text'), source='mysource')
  
})    
  

## plotly text -------------------------------------------------------------

  
  output$plotlybar_txt <- renderPrint({
    
    event_dat <- event_data('plotly_hover',    # 'plotly_hover' is a plotly event class
                            source="mysource") # monitors activity in plotly plot
    
    # NB because plotly_hover is not returning the tooltip data ('class'), \
    # use x (numeric) to retrieve class by factor level. messy...
    
    class_fct <- levels(factor(mpg$class))
    myclass <- class_fct[event_dat$x]
    
    if(!is.null(event_dat)){    cat( paste0('This is the ', myclass,' class') )  }
    
  })
  

## linked ggplot -----------------------------------------------------------


    output$jitterbox <- renderPlot({
    
    event_dat <- event_data('plotly_hover',    # 'plotly_hover' is a plotly event class
                            source="mysource") # monitors activity in plotly plot
    
    if (is.null(event_dat)) return(NULL)
    
    # NB because plotly_hover is not returning the tooltip data ('class'), \
    # use x (numeric) to retrieve class by factor level. messy...
    
    class_fct <- levels(factor(mpg$class))
    myclass <- class_fct[event_dat$x]
    
    #if(!is.null(event_dat))
    
    p2 <- reactive(
      
      mpg %>%  filter(class==myclass) %>% 
        ggplot(aes(x=class,y=cty), colour=default_grey) + 
        geom_boxplot() +
        geom_jitter(width=0.2) + 
        ggtitle(myclass) + theme_minimal() + no_legend()  +
        #expand_limits(y = 0) 
        ylim(0, max(mpg$cty))
      
      
    )
    
    print(p2())
  
    })
  
  
}
 