
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
  
# session duration log ----------------------------------------------------
  
  #NB this chunk cannot be sourced from a separate R script it seems.
  
  # record start time
  start_time <- as.character(Sys.time())
  
  #write tsv when session ends (shiny tab closed by user)
  session$onSessionEnded(function() {
    
    end_time = as.character(Sys.time())
    
    session_tbl <- tibble(start_time, end_time) %>%
      mutate(event='tab_close', session=session$token )
    
    if( length(list.files('session_logs/', pattern='sessions.tsv')) == 0){
      write_tsv(session_tbl,
                file = "session_logs/sessions.tsv")
    }else{
      write_tsv(session_tbl,
                file = "session_logs/sessions.tsv",
                append = TRUE)
    }})
  
  #write tsv when session ends (logout button clicked by user)
  observeEvent(logout_init(), {
    
    end_time = as.character(Sys.time())
    
    session_tbl <- tibble(start_time, end_time) %>%
      mutate(event='log_out', session=session$token )
    
    if( length(list.files('session_logs/', pattern='sessions.tsv')) == 0){
      write_tsv(session_tbl,
                file = "session_logs/sessions.tsv")
    }else{
      write_tsv(session_tbl,
                file = "session_logs/sessions.tsv",
                append = TRUE)
    }})
  
  
  
  
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


# sidebar2 ----------------------------------------------------------------


      , conditionalPanel(condition='input.tabset==2'
                         ,h3('Some text here for you')
                         ,br()
                         ,h5('Scroll over chart to reveal reciprocal data subsets'))


# sidebar3 ----------------------------------------------------------------


    , conditionalPanel(condition='input.tabset==3',
                       h5('ggplot reactive on plotly hover'))
),
        
      

# mainPanel code ----------------------------------------------------------

  
  mainPanel(
    tabsetPanel(#type='tabs',
    
    id='tabset',
    

      # tab about ---------------------------------------------------------------
      tabPanel('About',
               br(),
               br(),
              h5(style = "font-size:16px;", 'Welcome!'), br(),
              h5(style = "font-size:14px;", 
                 'This app is a template containing working examples of features & functionality required in many data/dashboard applications, including:')
               ,br()
               ,tags$div(tags$ul(tags$li('hashed user password authentication (shinyauthr)')))
              ,tags$div(tags$ul(tags$li('session time logs')))
               ,tags$div(tags$ul(tags$li('css themes (bslib)')))
               ,tags$div(tags$ul(tags$li('title bar logo/image')))
               ,tags$div(tags$ul(tags$li('interactive tables (gt)')))
               ,tags$div(tags$ul(tags$li('user input radio buttons/text select')))
               ,tags$div(tags$ul(tags$li('loading wheels (shinycssloaders)')))
               ,tags$div(tags$ul(tags$li('conditional sidebars')))
               ,tags$div(tags$ul(tags$li('linked reactive multi-plot panels (subplot)')))
               ,tags$div(tags$ul(tags$li('linked reactive multi-plot panels (plotly ~ ggplot2)')))
              ,br()
              ,h5(style = "font-size:14px;", 
                  'Code available at ', a('github', href="https://www.github.com/bansell/shiny_template"))
      ),
               
      
      # tab1 --------------------------------------------------------------------
      tabPanel('MPG scatter', value=1,
               width=9,
               br(),br(),
               withSpinner( #loading wheel
                 plotOutput('plot1',width="700",height="400"),
                 type = getOption("spinner.type", default = 6),
                 #color.background = getOption("white"),
                 #color = getOption("spinner.color", default = "#0275D8"),
                 color = getOption("spinner.color", default = 'dodgerblue'),
                 size = getOption("spinner.size", default = 1.75),
                 ),
               br(),
               h5('Table - reactively updates based on sidebar selection:'),
               gt_output('gt_tbl')),

      # tab2 --------------------------------------------------------------------

      tabPanel('Hover subplot',value=2,
             width=9,
             br(),br(),
             plotlyOutput("subplot", width="1100",height="600")),

    # tab 3 -------------------------------------------------------------------

      tabPanel('plotly ~ ggplot react', value=3,
               width=9,
               br(),br(),
               plotlyOutput('plotlybar',width='500',height="250"),
               plotOutput('jitterbox',width="200",height="250"),
               br(),
               textOutput('plotlybar_txt')
      )
)))
    })
    
      

# PLOT & TABLE code -----------------------------------------------

  

# plot 1 -------------------------------------------------------------------


## subset reactive ---------------------------------------------------------

  mpg_sub1 <- reactive({
    if(!is.null(input$manufacturer)){
      mpg %>% filter(manufacturer %in% c(input$manufacturer))
      }else{mpg}
  })
  
  

## plot call ---------------------------------------------------------------

  
  
      output$plot1 <- renderPlot({
        
        Sys.sleep(1.5) # to trigger the loading wheel as demo
        
        mpg_sub1() %>% 
          ggplot(aes(x=cty,y=hwy)) + 
          geom_point(aes_string(colour=input$selection),size=2) +
          theme(text =  element_text(size=24))
       
    })
  


# gt table ----------------------------------------------------------------


  tbl_dat <- reactive({
    
    if(!is.null(input$manufacturer)){
    mpg %>% filter(manufacturer==input$manufacturer)
      }else{mpg}
    
    })
  
  
  output$gt_tbl <- render_gt({
    
    tbl_dat() %>% 
      gt() %>% 
      opt_interactive(use_search = TRUE,
                      use_filters = TRUE,
                      use_resizers = TRUE,
                      use_highlight = TRUE,
                      use_compact_mode = FALSE,
                      use_text_wrapping = FALSE,
                      use_page_size_select = TRUE)  %>%
      tab_style(
        style = cell_text(size = "11px"),
        locations = list(cells_body())) %>%
      tab_options( #ihtml.page_size_default = ifelse(nrow(pdat)>50, 50, nrow(pdat))
        ihtml.page_size_default = 50)
    
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

  
#recode mpg  
mpg <- mpg %>% mutate(class=factor(class), year=factor(year))

#create colours from factors (for linked ggplot)
values <- c(hue_pal()(length(levels(mpg$class))))
names(values) <- levels(mpg$class)
    
output$plotlybar <- renderPlotly({
  
  pltly_plt <- mpg %>% 
    ggplot(aes(x=class)) + 
    geom_bar(aes(fill=class, text=class)) + 
    theme(legend.position = "none")
  
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
        ggplot(aes(x=year, y=cty, col=class)) + # colour=default_grey) + 
        geom_boxplot() +
        geom_jitter(width=0.2) + 
        #ggtitle(myclass) + 
        theme_minimal() + 
        theme(text=element_text(size=16)) +
        theme(legend.position = "none") +
        #preserve colour scheme in subset
        scale_colour_manual(values=values) +
        #expand_limits(y = 0) 
        ylim(0, max(mpg$cty)) 
      
      
    )
    
    print(p2())
  
    })
  
  
}
 