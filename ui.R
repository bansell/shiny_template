

# Define the UI

#For password-protected apps (via authr), the ui.R is very sparse; 
# the entire app incl. all the ui layout, is triggered upon authentication, and sits within server.R

#NB here the user is 'ansell.b' and password 'pwd'

ui <- fluidPage(
  

# bslib theme -------------------------------------------------------------

#BSLIB THEME incorporated HERE
theme = custom_theme3,  
  
# title & logo ------------------------------------------------------------
  


  titlePanel(windowTitle = "",
             title =
               div(
                 img(
                   src = "mr_poopy.png",
                   height = 100,
                   width = 40,
                   style = "margin:5px 10px"
                 ),
                 "A shiny application"
               )),
  

# login section -----------------------------------------------------------


  shinyauthr::loginUI(id = "login",
                      title="Please log in please",
                      user_title = "Your user name (try 'u')",
                      pass_title = "Pay the troll toll! (try 'p')"),
  
  
  div(shinyauthr::logoutUI(id = "logout",
                         #logout button features:
                         class = 'btn-dark',
                         style = "position: absolute; top: 10px; right: 10px; ")),

  #remaining bulk of ui code now sits within server.R ; and requires authentication
  uiOutput("sidebarlayout")
  
)






