
library(shiny)
library(ggmap)
library(dplyr)
library(tidyr)
library(leaflet)
library(DT)
library(rvest)

## Load and geocode FEC data

# candidates <- data.table::fread('CandidateSummaryAction.csv', data.table = FALSE)
# candidates <- candidates %>% rowwise() %>%
#     mutate(address = paste(can_str1, can_str2, can_cit, can_sta, can_zip, collapse = ' ') %>%
#                geocode() %>%
#                list()) %>%
#     unnest(address) %>%
#     arrange(desc(extract_numeric(cas_on_han_clo_of_per)))
# 
# write.csv(candidates, 'candidates.csv', row.names = FALSE)
candidates <- read.csv('candidates.csv')

candidates_for_map <- candidates %>% filter(!is.na(lat))

## Grab column names from FEC
url <- 'http://fec.gov/finance/disclosure/metadata/metadataforcandidatesummary.shtml'
metadata <- url %>% read_html() %>% 
    html_nodes('table') %>% 
    html_table(header = TRUE) %>% 
    data.frame()


ui <- shinyUI(
    navbarPage(
        '2016 US Presidential Candidates',
        theme = 'http://bootswatch.com/flatly/bootstrap.min.css',
        
        tabPanel(
            'Home', 
            div(style = 'margin-left:10%; margin-right:10%',
                column(width = 8, includeMarkdown('president.md'))
            )
        ),
        
        tabPanel('A Map', leafletOutput('map', height = '650px')),
        
        tabPanel(
            'The Data',
            verticalLayout(
                
                wellPanel(
                    selectizeInput(
                        inputId = 'cols', label = 'Columns to show:',
                        choices = setNames(seq_along(candidates), 
                                           c(metadata$Field.Name, 'Longitude', 'Latitude')),
                        selected = c(3,7,9:13,20,24,29,42,44:46),
                        multiple = TRUE,
                        options = list(plugins = list('remove_button', 'drag_drop')),
                        width = '100%'
                    )
                ),
                
                dataTableOutput('dt')
                
            )
        )
        
    ))


server <- shinyServer(function(input, output) {
   
   output$map <- renderLeaflet({
       leaflet(candidates_for_map) %>% 
           setView(-95.71289, 37.09024, zoom = 4) %>%
           addProviderTiles('CartoDB.PositronNoLabels') %>% 
           addMarkers(
               popup = paste(
                   '<table><tr><th>Candidate:</th><th>', candidates_for_map$can_nam, '</th></tr>',
                   '<tr><td>Address:</td><td>', 
                   candidates_for_map$can_str1, candidates_for_map$can_str2, '<br />',
                   candidates_for_map$can_cit, candidates_for_map$can_sta, candidates_for_map$can_zip, '</td></tr>',
                   '<tr><td>Net Contributions:</td><td>', candidates_for_map$net_con,
                   '</td></tr></table>'
                   )
               )
   })
   
   output$dt <- renderDataTable({
       datatable(
           candidates %>% 
               setNames(c(metadata$Field.Name, 'Longitude', 'Latitude')) %>% 
               select(as.integer(input$cols)),
           rownames = FALSE, style = 'bootstrap', 
           extensions = c('Responsive', 'Scroller'), 
           options = list(
               deferRender = TRUE,
               scrollY = 500,
               scroller = TRUE
           )
       )
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

