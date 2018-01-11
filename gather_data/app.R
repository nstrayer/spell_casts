library(shiny)
# devtools::install_github("nstrayer/shinysense")
library(shinysense)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Let's Gather some Spells"),
  fluidRow(
    div(
      style = "height:100%;",
      column(
        4, offset = 1,
        shinymovrUI("movr_button", button_width = '100%')
      ),
      column(
        6,
        textInput("label", "gesture label"),
        offset = 1
      )
    )
  ),
  plotOutput("movementPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  session_id <- sample(letters, 5) %>% paste(collapse = "")

  session_data <- sprintf("movement_data_%s.csv", session_id)

  # object to hold all your recordings in to plot
  rvs <- reactiveValues(
    movements = NA,
    counter = 0
  )

  movement <- callModule(shinymovr, "movr_button", 
                         time_limit = 3, 
                         movements = c('x','y','z'),
                         orientations = FALSE,
                         normalize = FALSE)

  observeEvent(movement(), {
    new_movement <- movement() %>%
      gather(direction, accel, -time) %>%
      mutate(label = input$label, recording_num = paste("gesture", rvs$counter))

    # rvs$movements <- new_movement
    if (rvs$counter == 0) {
      rvs$movements <- new_movement
    } else {
      rvs$movements <- rvs$movements %>%
        bind_rows(new_movement) %>%
        filter(recording_num != "gesture 0")
    }

    rvs$counter <- rvs$counter + 1

    # Generate a plot of the recording we just made
    output$movementPlot <- renderPlot({
      write_csv(rvs$movements, session_data)

      ggplot(rvs$movements, aes(x = time, y = accel)) +
        geom_line(aes(color = direction)) +
        facet_wrap(~paste0(recording_num, "|", label), scales = "free_x") +
        theme(
          axis.text.x = element_blank()
        )
    })

    output$downloadData <- downloadHandler(
      filename = "my_gestures.csv",
      content = function(file) {
        write.csv(rvs$movements, file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)