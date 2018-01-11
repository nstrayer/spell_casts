library(shiny)
# devtools::install_github('nstrayer/shinysense')
library(shinysense)
library(tidyverse)
library(keras)
library(showtext)

use_session_with_seed(42, disable_gpu = TRUE, disable_parallel_cpu = FALSE)

font_add_google(name = "Amatic SC", family = "Amatic SC")
showtext.auto()

model <- load_model_hdf5('spell_model.h5')
spells <- readRDS('spell_list.rds')

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Amatic+SC"),
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Amatic+SC');
      
      body {
        font-family: 'Amatic SC', cursive;
      }
      h2 {
        font-size: 50px;
      }
      p {
        font-size: 18px;
        font-weight: bold;
      }
      button {
        font-size: 28px;
      }

    "))
  ),
  titlePanel("Cast your spell!"),
  p("Press the cast button and cast your spell! You have 3 seconds to perform the cast and then the model will predict which spell you performed!"),
  splitLayout(align = 'center',
    img(width = '80%', src = 'alohomora.svg'),
    img(width = '80%', src = 'arresto_momento.svg')
  ),
  splitLayout(align = 'center', style = "padding-bottom: 10px;",
    img(width = '80%', src = 'incendio.svg'),
    img(width = '80%', src = 'winggardium_leviosa.svg')
  ),
  div(
    shinymovrUI(
      "movr_button", 
      resting_msg = 'Press to Cast!', 
      button_width = '100%',
      button_height = '75px'),
    style = "text-align: center;"
  ),
  plotOutput("predictionPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # object to hold all your recordings in to plot
  rvs <- reactiveValues(
    movement = NA
  )
  
  movement <- callModule(shinymovr, "movr_button", 
                         time_limit = 3, 
                         movements = c('x','y','z'),
                         orientations = FALSE,
                         normalize = FALSE)
  
  observeEvent(movement(), {
    rvs$movement <- movement()
    
    gesture_data <- rvs$movement %>% 
      select(m_x,m_y,m_z) %>% 
      as.matrix() %>% 
      list() %>% 
      pad_sequences(maxlen = 60) 
    
    num_spells <- length(spells)
    
    if(dim(rvs$movement)[1] > 0){
      model_preds <- model %>% predict(gesture_data)
    } else {
      model_preds <- rep(1/num_spells, num_spells)
    }
    
    print('model predictions')
    print(model_preds)
    
    output$predictionPlot <- renderPlot(height = 250, {
      data_frame(
        spell = spells,
        probs = as.numeric(model_preds)
      ) %>% 
        ggplot(aes(x = spell, y = probs)) +
        geom_bar(stat = 'identity', fill = 'steelblue') +
        theme_minimal(base_size = 28) +
        theme(
          text = element_text(family = 'Amatic SC'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 28)
        )+
        labs(y = 'probability', x = '')
    })
  })
}

shinyApp(ui = ui, server = server)