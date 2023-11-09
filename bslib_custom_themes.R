library(bslib)

# https://rstudio.github.io/bslib/articles/theming/index.html



# Define the custom theme using bslib
custom_theme1 <- bs_theme(
  primary = "#007BFF",  # Change the primary color
  background = "#f8f9fa",  # Change the background color
  text = "#333333",  # Change text color
  font_family = "Cambria",  # Specify the font family
  base_font_size = 14,  # Set the base font size
  #container = container(
  #  padding = "1rem"  # Adjust container padding
  #)
)

custom_theme2 <- bs_theme(
  bg = "#101010",
  fg = "#FFF",
  primary = "#E69F00",
  secondary = "#0072B2",
  success = "#009E73",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

custom_theme3 <- bs_theme(#bootswatch='lumen',
                          bootswatch='flatly',
                          #bootswatch='simplex',
                          base_font_size=12)
