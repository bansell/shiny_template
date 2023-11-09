
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
),
        
      
  mainPanel(
    tabsetPanel(#type='tabs',
    
    id='tabset',
    
      
      # tab1 --------------------------------------------------------------------
      tabPanel('MPG scatter', value=1,
               width=9,
               br(),br(),
               plotOutput('plot1',width="600",height="400"))
      )
))})
    
      

# code for plots and tables -----------------------------------------------

  

# subset reactive ---------------------------------------------------------

  mpg_sub1 <- reactive({
    if(!is.null(input$manufacturer)){
      mpg %>% filter(manufacturer %in% c(input$manufacturer))
      }else{mpg}
  })
  
  

  
  
# plot call ---------------------------------------------------------------

  
  
      output$plot1 <- renderPlot({
        
        
        mpg_sub1() %>% 
          ggplot(aes(x=cty,y=hwy)) + 
          geom_point(aes_string(colour=input$selection)) 
       
    })
}
 