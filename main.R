library(shiny)
library(ggplot2)

mandelbrot_set <- function(xlim, ylim, res = 100, max_iter = 100) {
  x <- seq(xlim[1], xlim[2], length.out = res)
  y <- seq(ylim[1], ylim[2], length.out = res)
  C <- outer(x, complex(real = y), FUN = "+")

  Z <- matrix(0, res, res)
  K <- matrix(0, res, res)

  for (n in 1:max_iter) {
    mask <- abs(Z) <= 2
    Z[mask] <- Z[mask]^2 + C[mask]
    K[mask] <- K[mask] + 1
  }

  return(K)
}

ui <- fluidPage(
  titlePanel("Interaktive Mandelbrot-Menge"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("max_iter",
                  "Maximale Iterationen:",
                  min = 10, max = 500, value = 100, step = 10),
      sliderInput("res",
                  "Auflösung:",
                  min = 50, max = 500, value = 200, step = 50),
      actionButton("reset_zoom", "Zoom zurücksetzen")
    ),

    mainPanel(
      plotOutput("mandelbrotPlot",
                 click = "plot_click",
                 height = "600px")
    )
  )
)

server <- function(input, output, session) {
  xlim <- reactiveVal(c(-2, 1))
  ylim <- reactiveVal(c(-1.5, 1.5))

  observeEvent(input$reset_zoom, {
    xlim(c(-2, 1))
    ylim(c(-1.5, 1.5))
  })

  observeEvent(input$plot_click, {
    zoom_factor <- 0.5
    x_range <- diff(xlim()) * zoom_factor
    y_range <- diff(ylim()) * zoom_factor

    xlim(c(input$plot_click$x - x_range / 2, input$plot_click$x + x_range / 2))
    ylim(c(input$plot_click$y - y_range / 2, input$plot_click$y + y_range / 2))
  })

  output$mandelbrotPlot <- renderPlot({
    K <- mandelbrot_set(xlim = xlim(), ylim = ylim(), res = input$res, max_iter = input$max_iter)

    df <- expand.grid(x = seq(xlim()[1], xlim()[2], length.out = input$res),
                      y = seq(ylim()[1], ylim()[2], length.out = input$res))
    df$K <- as.vector(K)

    ggplot(df, aes(x, y, fill = K)) +
      geom_raster() +
      scale_fill_viridis_c(option = "plasma", direction = -1) +
      theme_void() +
      coord_fixed(ratio = 1) +
      labs(title = "Mandelbrot-Menge")
  })
}

shinyApp(ui = ui, server = server)
