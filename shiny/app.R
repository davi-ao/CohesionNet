# ------------------------------------------------------------------------------
# Analyze indices of textual cohesion of an English text using semantic networks
# of cliques with a Shiny GUI
#
# Authors: Davi Alves Oliveira and Hernane Borges de Barros Pereira
#
# Last update: November 19, 2024
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install the necessary packages
# install.packages('shiny')
# install.packages('tidyverse')
# install.packages('udpipe')
# install.packages('SnowballC')
# install.packages('openxlsx')
# install.packages('shinyWidgets')
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")
# install.packages('bslib')
# install.packages('DT')
# install.packages('igraph')
# install.packages('shinyjs')

# Load the necessary packages --------------------------------------------------
library(shiny)
library(tidyverse)
library(udpipe)
library(SnowballC)
library(openxlsx)
library(shinyWidgets)
library(shinycssloaders)
library(bslib)
library(DT)
library(igraph)
library(shinyjs)

# Load local dependencies ------------------------------------------------------
# Cohesion funcions
source('cohesion_indices_functions.R')
# Udpipe parsing function
source('udpipe_parsing_function.R')
# Create network function
source('create_network_function.R')

################################################################################
# SHINY CODE                                                                   #
################################################################################
# Define UI --------------------------------------------------------------------
# Fluid container
ui = page_sidebar(
  title = 'CohesionNet',
  sidebar = sidebar(
    width = '400px',
    useShinyjs(),
    div(id = 'fileInput', 
        fileInput('file', label = 'Select text files', multiple = T)),
    div(id = 'reset',
        actionButton('reset', 
                     label = 'Analyze other texts', 
                     icon = icon('rotate')),
        style = 'display: none'),
    selectInput('language', 
                label = 'Language',
                choices = list('English' = 1, 
                               'Portuguese' = 2, 
                               'Spanish' = 3),
                selected = 1),
    selectInput('vertex_type',
                label = 'Vertex definition',
                choices = list('token' = 1, 
                               'word' = 2,
                               'lemma' = 3,
                               'stem' = 4),
                selected = 4),
    selectInput('edge_type',
                label = 'Segment representation',
                choices = list('line' = 1, 
                               'clique' = 2,
                               'dependency-based' = 3),
                selected = 3),
    checkboxInput('lexical', 'Keep only lexical tokens', T),
    actionButton('analyze', 
                 label = 'Run analysis', 
                 icon = icon('play'), 
                 disabled = T),
    helpText('Please, cite the following article when using this app: Oliveira, D. 
              A., Senna, V., & Pereira, H. B. B. (2024). Indices of Textual 
              Cohesion by Lexical Repetition Based on Semantic Networks of 
              Cliques. Expert Systems with Applications, 237(2024), 121580. 
             https://doi.org/10.1016/j.eswa.2023.121580')
  ),
  navset_card_tab(
    id = 'results',
    nav_panel('Instructions', 
              div(
                p('Click on the \'Browse...\' button to select at least one
                  .txt file for analysis. Ensure the file contains a single 
                  text. Precision can be improved if [.:?!…] are used 
                  exclusively as sentence delimiters. You may any other uses of 
                  these characters with different ones; for instance replace 
                  decimal separators like in "1.5" with underscores, making it 
                  "1_5".'),
                p('Select the language of the texts. Currently, Spanish and 
                  Portuguese are limited to vertex cohesion indices.'),
                p('Select the vertex definition. The definition "token" includes 
                  all sets of characters delimited by a whitespace. The 
                  definition "word" includes all tokens except those identified 
                  as punctuation marks. The definition "lemma" includes only 
                  unique lemmatized words. The definition "stem" includes only 
                  unique stemmed lemmas.'),
                p('Select the segment representation. The "line" representation
                  connects each consecutive token in a sentence. The "clique" 
                  representation connects every token in a sentence with each 
                  other. The "dependecy-based" representation creates a tree of
                  dependency relations for each sentence.'),
                p('The option "Keep only lexical tokens" removes all tokens that
                  are not identified as nouns, adjectives, verbs or adverbs.'),
                p('After setting the appropriate values for these settings, 
                  click the "Run analysis" button. Long texts may take several 
                  minutes to process.')
              ), 
              style = 'padding:1em'),
    nav_panel('Error',
              value = 'error1',
              div(p('You must select at least one text file.'),
                  style = 'padding:1em;color:red')),
    nav_panel('Error',
              value = 'error2',
              div(p('Invalid file: files must be plain texts.'),
                  style = 'padding:1em;color:red'))
  ),
  div(
    downloadButton('download_processed_texts',
                   'Processed texts (.xlsx)',
                   icon = icon('download')),
    downloadButton('download_results',
                   'Results (.xlsx)',
                   icon = icon('download')),
    downloadButton('download_networks',
                   'Networks (.xlsx)',
                   icon = icon('download'))
  )
)

# Define server logic ----------------------------------------------------------
server = function(input, output, session) {
  nav_hide('results', 'error1')
  nav_hide('results', 'error2')
  
  observeEvent(input$file, {
    if (is.null(input$file)) {
      nav_show('results', 'error1', T)
      hidePageSpinner()
      validate(F)
    } else if (!(input$file$type == 'text/plain') %>% all()) {
      nav_show('results', 'error2', T)
      hidePageSpinner()
      validate(F)
    } else {
      nav_hide('results', 'error1')
      nav_hide('results', 'error2')
      updateActionButton(inputId = 'analyze', disabled = F)
    }
  })
  
  observeEvent(input$analyze, {
    showPageSpinner()
    nav_hide('results', 'Instructions')
    
    req(results())
    
    lapply(1:length(results()), function(i) {
      nav_remove('results', paste('Text', i))

      nav_insert('results',
                 nav_panel(paste('Text', i),
                           h4(renderText(paste('Results for',
                                                input$file$name[i]))),
                           renderDataTable(results()[[i]]),
                           hr(),
                           h4(renderText('Mean values')),
                           renderTable(results()[[i]] %>%
                                         group_by(Index) %>%
                                         summarize(Vertex = mean(Vertex),
                                                   Edge = mean(Edge))),
                           style = 'padding:1em'),
                 select = i == 1)
    })
    
    hide('fileInput')
    show('reset')
    updateActionButton(inputId = 'analyze', disabled = T)
    hidePageSpinner()
  })
  
  observeEvent(input$reset, {
    session$reload()
  })
  
  results = reactive({
    req(networks())
    
    # English -nyms networks
    nyms = switch(input$vertex_type,
                  '1' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                  '2' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                  '3' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                  '4' = read_graph('./english_nyms_stems.net', 'pajek'))
     
    lapply(networks(), function(G) {
      global_indices = calculate_global_cohesion(G$segments, G$G_next, nyms)
      local_indices = calculate_local_cohesion(G$segments, nyms)
      pairwise_indices = calculate_pairwise_cohesion(G$segments, nyms)
      
      global_indices %>%
        bind_rows(local_indices) %>%
        bind_rows(pairwise_indices) %>%
        rename(Segment = segment,
               Index = index,
               Vertex = v,
               Edge = e)
    })
  })
  
  networks = reactive({
    req(data())

    lapply(data(), function(d) {
      create_network(d, input$lexical, input$vertex_type, input$edge_type)
    })
  })
  
  data = reactive({
    lapply(input$file$datapath, function(path) {
      read_file(path) %>%
        # Parse with udpipe
        udpipe_parsing(input$language, input$lexical)
    })
  })
  
  network_files = reactive({
    showPageSpinner()
    req(networks())
    
    # Create the network files for download
    files = lapply(networks(), function(G) {
      vertices = G$network %>%
        igraph::as_data_frame('vertices') %>%
        rename(Id = id,
               Label = name,
               Timestamp = segments) %>%
        select(Id, Label, Timestamp)
      
      edges = G$network %>%
        igraph::as_data_frame('edges') %>%
        rename(Source = from,
               Target = to,
               CohesionEdge = cohesion_edge) %>%
        mutate(Type = "Undirected",
               CohesionEdge = CohesionEdge %>% as.numeric()) %>%
        select(Source, Target, Type, CohesionEdge)
      
      list(Vertices = vertices, Edges = edges)
    })
    
    hidePageSpinner()
    files
  })
  
  output$download_processed_texts = downloadHandler(
    filename = function(){
      paste("processed_texts_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){
      temp_directory = file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      data() %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name = paste0(str_sub(input$file$name[y], 0, -5), '_data.xlsx')
            write.xlsx(x, file.path(temp_directory, file_name))
          }
        })
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$download_results = downloadHandler(
    filename = function(){
      paste("results_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){
      temp_directory = file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      results() %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name = paste0(str_sub(input$file$name[y], 0, -5),
                               '_cohesion_indices.xlsx')
            write.xlsx(x, file.path(temp_directory, file_name))
          }
        })
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
  
  output$download_networks = downloadHandler(
    filename = function(){
      paste("networks_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file){
      temp_directory = file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      network_files() %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name = paste0(str_sub(input$file$name[y], 0, -5), 
                               '_network.xlsx')
            write.xlsx(x, file.path(temp_directory, file_name))
          }
        })
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)