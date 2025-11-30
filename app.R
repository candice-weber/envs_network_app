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

# Creating labels for nodes

dropdown_labels <- c(
  "Christopher Newport University,<br>Environmental Studies, BA",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Business and Sustainability, BA",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Climate Change and Society, BA",
  "George Mason University,<br>Environmental Science,<br>Conservation, BS",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Conservation and Sustainability, BA",
  "George Mason University,<br>Environmental Science,<br>Ecological Science, BS",
  "George Mason University,<br>Environmental Science,<br>Environmental Health, BS",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Equity and Environmental Justice, BA",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Environmental Policy, BA",
  "George Mason University,<br>Environmental Science,<br>Human and Ecosystem Response to Climate Change, BS",
  "George Mason University,<br>Environmental Science,<br>Marine, Estuarine, and Freshwater Ecology, BS",
  "George Mason University,<br>Environmental and Sustainability Studies,<br>Sustainable Food and Agriculture, BA",
  "George Mason University,<br>Environmental Science,<br>Wildlife Conservation and Management, BS",
  "James Madison University,<br>Integrated Science and Technology,<br>Environment and Sustainability, BS",
  "Longwood University,<br>Integrated Environmental Sciences,<br>Earth Sciences, BS",
  "Longwood University,<br>Integrated Environmental Sciences,<br>Life Sciences, BS",
  "Longwood University,<br>Integrated Environmental Sciences,<br>Social Sciences, BS",
  "Longwood University,<br>Integrated Environmental Sciences,<br>Water Resources, BS",
  "University of Mary Washington,<br>Environmental Science and Geology,<br>Applied Environmental Science, BS",
  "University of Mary Washington,<br>Environmental Science and Geology,<br>Environmental Sustainability and Policy, BS",
  "University of Virginia,<br>Environmental Sciences, BA",
  "University of Virginia,<br>Environmental Sciences, BS",
  "University of Virginia College at Wise,<br>Natural Sciences,<br>Environmental Science, BA",
  "University of Virginia College at Wise,<br>Environmental Science and Geology,<br>Environmental Science, Biology Track, BS",
  "University of Virginia College at Wise,<br>Environmental Science and Geology,<br>Environmental Science, Chemistry Track, BS",
  "University of Virginia College at Wise,<br>Environmental Science and Geology,<br>Environmental Science, Earth Sciences Track, BS",
  "Virginia Commonwealth University,<br>Environmental Studies, BS",
  "Virginia Tech,<br>Environmental Science, BS",
  "William & Mary,<br>Interdisciplinary Studies,<br>Environmental Humanities, BA",
  "William & Mary,<br>Interdisciplinary Studies,<br>Environmental Policy, BA",
  "William & Mary,<br>Interdisciplinary Studies,<br>Environmental Science, BS"
)


# Define UI

ui <- page_sidebar(
  title = "Curricular Linguistic Networks of Virginia Environmental Undergraduate Programs",
  class = "bslib-page-dashboard",
  sidebar = sidebar("Hover over any node to view which degree program it represents.",
                    br(),
                    br(),
                    "Click on any node to view its nearest connections.",
                    sliderInput("alpha", "Adjust global connectivity level:",
                                min = 0.01, max = 0.1,
                                value = 0.05, step = 0.01),
                    "This graph maps the linguistic topology of environmental undergraduate programs at public universities in Virginia.  The data underlying the graph are a result of text-mining the written descriptions of each programs' required courses.",
                    actionButton("show", "Click to learn more")
  ),
  card(
    modalDialog(
      title = "Curricular Linguistic Networks of Virginia Environmental Undergraduate Programs",
      tags$div(
        "This app provides an interactive display of the linguistic topology of environmental undergraduate degree programs at public universities in Virginia.  To create this app, undergraduate programs in environmental studies, environmental sciences, or interdisciplinary studies with a focus on environment and sustainability were selected from universities across Virginia.  Details about each of the degree programs ",
        tags$a(
          "are available here.",
          target = "_blank",
          href = "https://www.candiceweber.com/degree_programs.html"
        ),
        " A text corpus was created from the course description text of all programs' required courses.  Using this corpus, the frequency of all terms (after removing capitalization, punctuation, spaces, and stop words) was measured across all courses and weighted by each course's credit load.  These resulting frequency vectors were then used to visually map the relationships between degree programs based on shared linguistic structures of their required courses.",
        tags$br(),
        tags$br(),
        "This app allows the user to visualize changes in the linguistic map as more or less global connectivity is allowed between degree programs.  Moving the input slider in the sidebar will change the significance level for edge retention in the network topology.  Lower values result in less global connectivity: more unconnected clusters, fewer edges between degree programs.  Higher values result in more global connectivity, eventually collapsing the network into a single cluster with edges connecting all degree programs.",
        tags$br(),
        tags$br(),
        "At lower significance levels, the degree programs of William & Mary, UVA Wise, Longwood, and George Mason form isolated subgraphs.  It is only as the significance level for edge retention (retaining connections between degree programs) increases that these universities’ programs connect themselves to the main cluster.  William & Mary becomes the first to connect with the main cluster at a significance level of 0.03, then next UVA Wise at 0.04, etc.",
        tags$br(),
        tags$br(),
        "Using our graphing function's default significance level of 0.05, the degree programs at George Mason and Longwood maintain their independence in the network topology.  As the significance level increases beyond 0.05, they eventually join the main cluster until all programs are connected in some way.  Even when all programs are linked in a single cluster, the connections between those programs at the center such as James Madison, VCU, and Christopher Newport will be stronger than others.",
        tags$br(),
        tags$br(),
        "The data and code used for this analysis are publicly available for review on GitHub at ",
        tags$a(
          "/va_ess_programs.",
          target = "_blank",
          href = "https://github.com/candice-weber/va_ess_programs"
        ),
        "The underlying code for this Shiny app is also available at ",
        tags$a(
          "/envs_network_app.",
          target = "_break",
          href = "https://github.com/candice-weber/envs_network_app"
        ),
        tags$br(),
        "Please reach out to the author Candice Weber at ",
        tags$a(
          "cweber@vcu.edu",
          target = "_blank",
          href = "mailto:cweber@vcu.edu"
        ),
        "with any comments or questions."
      ),
      footer = modalButton("Try the app"),
      size = "xl",
      easyClose = TRUE,
      fade = FALSE,
      scrollableContentClassName = "my-scroll-area"
    ),
    visNetworkOutput("mynetworkid")
  )
)



# Define server

server <- function(input, output) {
  observe({ 
    showModal( 
      modalDialog(
        title = "Curricular Linguistic Networks of Virginia Environmental Undergraduate Programs",
        tags$div(
          "This app provides an interactive display of the linguistic topology of environmental undergraduate degree programs at public universities in Virginia.  To create this app, undergraduate programs in environmental studies, environmental sciences, or interdisciplinary studies with a focus on environment and sustainability were selected from universities across Virginia.  Details about each of the degree programs ",
          tags$a(
            "are available here.",
            target = "_blank",
            href = "https://www.candiceweber.com/degree_programs.html"
          ),
          " A text corpus was created from the course description text of all programs' required courses.  Using this corpus, the frequency of all terms (after removing capitalization, punctuation, spaces, and stop words) was measured across all courses and weighted by each course's credit load.  These resulting frequency vectors were then used to visually map the relationships between degree programs based on shared linguistic structures of their required courses.",
          tags$br(),
          tags$br(),
          "This app allows the user to visualize changes in the linguistic map as more or less global connectivity is allowed between degree programs.  Moving the input slider in the sidebar will change the significance level for edge retention in the network topology.  Lower values result in less global connectivity: more unconnected clusters, fewer edges between degree programs.  Higher values result in more global connectivity, eventually collapsing the network into a single cluster with edges connecting all degree programs.",
          tags$br(),
          tags$br(),
          "At lower significance levels, the degree programs of William & Mary, UVA Wise, Longwood, and George Mason form isolated subgraphs.  It is only as the significance level for edge retention (retaining connections between degree programs) increases that these universities’ programs connect themselves to the main cluster.  William & Mary becomes the first to connect with the main cluster at a significance level of 0.03, then next UVA Wise at 0.04, etc.",
          tags$br(),
          tags$br(),
          "Using our graphing function's default significance level of 0.05, the degree programs at George Mason and Longwood maintain their independence in the network topology.  As the significance level increases beyond 0.05, they eventually join the main cluster until all programs are connected in some way.  Even when all programs are linked in a single cluster, the connections between those programs at the center such as James Madison, VCU, and Christopher Newport will be stronger than others.",
          tags$br(),
          tags$br(),
          "The data and code used for this analysis are publicly available for review on GitHub at ",
          tags$a(
            "/va_ess_programs.",
            target = "_blank",
            href = "https://github.com/candice-weber/va_ess_programs"
          ),
          "The underlying code for this Shiny app is also available at ",
          tags$a(
            "/envs_network_app.",
            target = "_break",
            href = "https://github.com/candice-weber/envs_network_app"
          ),
          tags$br(),
          "Please reach out to the author Candice Weber at ",
          tags$a(
            "cweber@vcu.edu",
            target = "_blank",
            href = "mailto:cweber@vcu.edu"
          ),
          "with any comments or questions."
        ),
        footer = modalButton("Try the app"),
        size = "xl",
        easyClose = TRUE,
        fade = FALSE,
        scrollableContentClassName = "my-scroll-area"
      ) 
    ) 
  }) |> 
    bindEvent(input$show) 
  
  output$mynetworkid <- renderVisNetwork({
    groups <- data$degree_program
    data %>% 
      select( -degree_program, -Course) %>% 
      as.matrix() -> M
    popgraph(M, groups = groups, alpha = input$alpha) -> pg
    
    coords <- igraph::layout_with_fr( pg )
    
    nodes <- data.frame(id = V(pg)$name, label = V(pg)$name, title = dropdown_labels, shape = "box", color = list(background = "lightgreen")) 
    edges <- as_data_frame(pg, what = "edges")
    edges <- edges %>% mutate(value = weight)
    
    nodes$x <- coords[,1] * 100  # 100 is to scale the data
    nodes$y <- coords[,2] * 100
    visNetwork(nodes, edges) %>%
      visNodes(shape = "box", 
               color = list(background = "lightgreen", 
                            border = "darkgreen",
                            highlight = "lightyellow"),
               shadow = list(enabled = TRUE, size = 10),
               font = "25px") %>% 
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(enabled = TRUE)
    
  })
  
    }


# Run the app ----
shinyApp(ui = ui, server = server)
