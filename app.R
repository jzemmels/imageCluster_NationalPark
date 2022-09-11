library(shiny)
library(tidyverse)
library(imager)
library(colourpicker)

ui <- fluidPage(
  shinybusy::add_busy_spinner(),
  # Application title
  titlePanel("National Park Poster - Color Clustering"),

  sidebarLayout(
    sidebarPanel(
      shiny::fileInput(inputId = "imgPath",
                       label = "Upload an Image",accept = "image/*"),
      shiny::numericInput(inputId = "clustTotal",label = "Total # of Colors",
                          value = 5,min = 1,step = 1),
      selectInput(inputId = "clusterSelect",
                  label = "Select a color to change",
                  choices = "")
      ,colourpicker::colourInput(inputId = "clusterColorSelect",
                                label = "Select a new color")
    ),

    mainPanel(
      plotOutput("originalImgPlot"),
      plotOutput("colorClusterPlot"),
      plotOutput("clusterSelectPlot")
    )
  )
)

server <- function(input, output,session) {

  img <- reactiveVal()

  observeEvent(input$imgPath,{

    img(imager::load.image(input$imgPath$datapath))

  })

  output$originalImgPlot <- renderPlot({

    req(input$imgPath)

    # browser()

    img() %>%
      as.data.frame() %>%
      filter(cc != 4) %>%
      pivot_wider(id_cols = c(x,y),names_from = cc,values_from = value) %>%
      mutate(red = `1`*255,
             green = `2`*255,
             blue = `3`*255) %>%
      mutate(hex = rgb(red = red,green = green,blue = blue,maxColorValue = 255)) %>%
      ggplot(aes(x=x,y=y,fill=hex)) +
      geom_raster() +
      coord_fixed(expand = FALSE) +
      scale_fill_identity() +
      scale_y_reverse() +
      theme_void()

  })

  imColorBlock <- reactiveVal()

  output$colorClusterPlot <- renderPlot({

    req(input$imgPath)

    imgIsol <- img()

    imDim <- expand_grid(row = 1:dim(imgIsol)[1],
                         col = 1:dim(imgIsol)[2])%>%
      mutate(red = c(imgIsol[,,,1])*255,
             green = c(imgIsol[,,,2])*255,
             blue = c(imgIsol[,,,3])*255)

    imClust <- imDim %>%
      select(-c(row,col)) %>%
      kmeans(centers = input$clustTotal)

    ret <- imClust$cluster %>%
      matrix(nrow = dim(imgIsol)[1],ncol = dim(imgIsol)[2]) %>%
      imager::as.cimg() %>%
      as.data.frame()  %>%
      rename(clust = value) %>%
      left_join(imClust$centers %>%
                  as.data.frame() %>%
                  rownames_to_column(var = "clust") %>%
                  mutate(clust = as.integer(clust)) %>%
                  rename(centerRed = red,
                         centerGreen = green,
                         centerBlue = blue),
                by = "clust") %>%
      mutate(centerHex = rgb(red = round(centerRed),
                             green = round(centerGreen),
                             blue = round(centerBlue),
                             maxColorValue = 255))

    imColorBlock(ret)

    updateSelectInput(session = session,
                      inputId = "clusterSelect",
                      choices = c("",unique(ret$centerHex)))

    ret %>%
      ggplot(aes(x=x,y=y,fill=centerHex)) +
      geom_raster() +
      coord_fixed(expand = FALSE) +
      scale_fill_identity() +
      scale_y_reverse() +
      theme_minimal() +
      theme(legend.position = "bottom")

  })

  # output$clusterSelectPlot <- renderPlot({
  #
  #   # browser()
  #
  #   req(input$imgPath)
  #   req(input$clusterSelect != "")
  #
  #   imgIsol <- img()
  #
  #   imDim <- expand_grid(row = 1:dim(imgIsol)[1],
  #                        col = 1:dim(imgIsol)[2])%>%
  #     mutate(red = c(imgIsol[,,,1])*255,
  #            green = c(imgIsol[,,,2])*255,
  #            blue = c(imgIsol[,,,3])*255)
  #
  #   imClust <- imDim %>%
  #     select(-c(row,col)) %>%
  #     kmeans(centers = input$clustTotal)
  #
  #   imColorBlock <- imClust$cluster %>%
  #     matrix(nrow = dim(imgIsol)[1],ncol = dim(imgIsol)[2]) %>%
  #     imager::as.cimg() %>%
  #     as.data.frame()  %>%
  #     rename(clust = value) %>%
  #     left_join(imClust$centers %>%
  #                 as.data.frame() %>%
  #                 rownames_to_column(var = "clust") %>%
  #                 mutate(clust = as.integer(clust)) %>%
  #                 rename(centerRed = red,
  #                        centerGreen = green,
  #                        centerBlue = blue),
  #               by = "clust") %>%
  #     mutate(centerHex = rgb(red = round(centerRed),
  #                            green = round(centerGreen),
  #                            blue = round(centerBlue),
  #                            maxColorValue = 255),
  #            centerHex = ifelse(clust != input$clusterSelect,
  #                               "#000000",centerHex))
  #
  #   imColorBlock %>%
  #     ggplot(aes(x=x,y=y,fill=centerHex)) +
  #     geom_raster() +
  #     coord_fixed(expand = FALSE) +
  #     scale_fill_identity() +
  #     scale_y_reverse() +
  #     theme_void()
  #
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
