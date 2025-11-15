library(tidyverse)
library(dplyr)
library(shiny)
library(popgraph)
library(igraph)
library(visNetwork)
library(bslib)
library(rsconnect)


# Reading in the data
data <- read_csv("data.csv")

# Creating labels for dropdown menu

dropdown_labels <- c(
  "Christopher Newport University,<br>Environmental Studies, BA",
  "George Mason University,<br>Business and Sustainability, BA",
  "George Mason University,<br>Climate Change and Society, BA",
  "George Mason University,<br>Conservation, BS",
  "George Mason University,<br>Conservation and Sustainability, BA",
  "George Mason University,<br>Ecological Science, BS",
  "George Mason University,<br>Environmental Health, BS",
  "George Mason University,<br>Equity and Environmental Justice, BA",
  "George Mason University,<br>Environmental Policy, BA",
  "George Mason University,<br>Human and Ecosystem Response<br>to Climate Change, BS",
  "George Mason University,<br>Marine, Estuarine, and Freshwater Ecology, BS",
  "George Mason University,<br>Sustainable Food and Agriculture, BA",
  "George Mason University,<br>Wildlife Conservation and Management, BS",
  "James Madison University,<br>Environment and Sustainability, BS",
  "Longwood University,<br>Earth Sciences, BS",
  "Longwood University,<br>Life Sciences, BS",
  "Longwood University,<br>Social Sciences, BS",
  "Longwood University,<br>Water Resources, BS",
  "University of Mary Washington,<br>Applied Environmental Science, BS",
  "University of Mary Washington,<br>Environmental Sustainability and Policy, BS",
  "University of Virginia,<br>Environmental Sciences, BA",
  "University of Virginia,<br>Environmental Sciences, BS",
  "University of Virginia College at Wise,<br>Environmental Science, BA",
  "University of Virginia College at Wise,<br>Environmental Science, Biology Track, BS",
  "University of Virginia College at Wise,<br>Environmental Science, Chemistry Track, BS",
  "University of Virginia College at Wise,<br>Environmental Science, Earth Sciences Track, BS",
  "Virginia Commonwealth University,<br>Environmental Studies, BS",
  "Virginia Tech,<br>Environmental Science, BS",
  "William & Mary,<br>Environmental Humanities, BA",
  "William & Mary,<br>Environmental Policy, BA",
  "William & Mary,<br>Environmental Science, BS"
)


# Define UI
ui <- page_sidebar(
  title = "Virginia ESS Degree Programs Network",
  sidebar = sidebar("This graph displays the
results of mapping the linguistic topology of environmental
studies/sciences undergraduate degree programs at public universities in
Virginia. Select a degree program to view its nearest neighbors."),
  sliderInput("alpha", "Adjust alpha in popgraph:",
              min = 0.01, max = 0.1,
              value = 0.05, step = 0.01),
  card(
      visNetworkOutput("mynetworkid")
    ),
  theme = bs_theme(
    preset = "minty"
  )
)


# Define server
# To enable selection by dropdown (not yet working how I want), use selectedBy = "dropdown_labels" in visOptions

server <- function(input, output) {
      output$mynetworkid <- renderVisNetwork({
        groups <- data$degree_program
        data %>% 
          select( -degree_program, -Course) %>% 
          as.matrix() -> M
        popgraph(M, groups = groups, alpha = input$alpha) -> pg
        
        coords <- igraph::layout_with_fr( pg )
        
        nodes <- data.frame(id = V(pg)$name, label = V(pg)$name, title = dropdown_labels, shape = "box", color = list(background = "#CCCCFF", hover = "#CCFF66")) # This hover color doesn't seem to be working
        edges <- as_data_frame(pg, what = "edges")
        edges <- edges %>% mutate(value = weight)
        
        nodes$x <- coords[,1] * 100  # 100 is to scale the data
        nodes$y <- coords[,2] * 100
        visNetwork(nodes, edges) %>%
          visNodes(shape = "box", 
                   color = list(background = "lightblue", 
                                border = "darkblue",
                                highlight = "lightyellow"),
                   shadow = list(enabled = TRUE, size = 10),
                   font = "25px") %>% 
          visOptions(highlightNearest = TRUE) %>%
          visPhysics(enabled = TRUE)
        
      })
    }


# Run the app ----
shinyApp(ui = ui, server = server)
