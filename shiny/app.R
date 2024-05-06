# ------------------------------------------------------------------------------
# Analyze indices of textual cohesion of an English text using semantic networks
# of cliques with a Shiny GUI
#
# Authors: Davi Alves Oliveira, Valter de Senna, and Hernane Borges de Barros 
# Pereira
#
# Last update: November 30, 2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install the necessary packages
# install.packages('shiny')
# install.packages('tidyverse')
# install.packages('udpipe')
# install.packages('SnowballC')
# install.packages('openxlsx')
# install.packages('shinyWidgets')

# Load the necessary packages --------------------------------------------------
library(shiny)
library(tidyverse)
library(udpipe)
library(SnowballC)
library(openxlsx)
library(shinyWidgets)
library(bslib)
library(DT)

# Load local dependencies ------------------------------------------------------
# Cohesion funcions
source('cohesion_functions.R')
# Udpipe parsing function
source('udpipe_parsing_function.R')

################################################################################
# SHINY CODE                                                                   #
################################################################################
# Define UI --------------------------------------------------------------------
# Fluid container
ui = page_sidebar(
  title = 'CohesionNet',
  sidebar = sidebar(
    width = '400px',
    fileInput('file', label = h4('Select text files'), multiple = T),
    selectInput('language', 
                label = h4('Select the language'),
                choices = list('English' = 1, 
                               'Portuguese' = 2, 
                               'Spanish' = 3)),
    actionButton('analyze', label = 'Run analysis', icon = icon('play')),
    helpText(h5('Instructions')),
    helpText('Click on the \'Browse...\' button to select a .txt file for
               analysis. Ensure the file contains a single text and that [.:?!…]
               are used exclusively as sentence delimiters. Replace any other
               # uses of these characters with different ones; for instance
               replace decimal separators like in "1.5" with underscores, making
               it "1_5".'),
    helpText(h5('References')),
    helpText('Please, cite the following work when using this app: Oliveira, D. 
              A., Senna, V., & Pereira, H. B. B. (2024). Indices of Textual 
              Cohesion by Lexical Repetition Based on Semantic Networks of 
              Cliques. Expert Systems with Applications, 237(2024), 121580. 
             https://doi.org/10.1016/j.eswa.2023.121580')
  ),
  navset_card_tab(
    id = 'results',
    title = 'Results'
  ),
  div(
    downloadButton('download_processed_texts',
                   'Processed texts (.xlsx)',
                   icon = icon('download')),
    downloadButton('download_results_cliques',
                   'Results (.xlsx)',
                   icon = icon('download')),
    downloadButton('download_networks',
                   'Networks (.net)',
                   icon = icon('download'))
  )
)

# Define server logic ----------------------------------------------------------
server = function(input, output, session) {
  data = reactive({
    validate(
      need(input$file, 'You must select a text file.'),
      need(input$file$type[1] == 'text/plain',
           'Invalid file: file must be plain text.')
    )
    
    lapply(input$file$datapath, function(path) {
      read_file(path) %>%
        # Parse with udpipe
        udpipe_parsing(input$language)
    })
  })
  
  results = reactive({
    req(data())
    
    lapply(data(), function(d) {
      indices = global_backward_cohesion(d) %>%
        select(clique_id, v, e) %>%
        # Identify the index
        mutate(index = 'Global Backward Cohesion') %>%
        bind_rows(
          local_backward_cohesion(d) %>%
            select(clique_id, v, e) %>%
            # Identify the index
            mutate(index = 'Local Backward Cohesion')
        ) %>%
        bind_rows(
          mean_pairwise_cohesion(d) %>%
            select(clique_id, v, e) %>%
            # Identify the index
            mutate(index = 'Mean Pairwise Cohesion')
        ) %>%
        rename(Index = index,
               `Clique ID` = clique_id,
               Vertex = v,
               Edge = e) %>%
        select(Index, `Clique ID`, Vertex, Edge)
    })
  })
  
  observeEvent(input$analyze, {
    req(data())
    req(results())
    
    nav_remove('results', 'Error')
    
    if(is.null(input$file)) {
      nav_insert('results', 
                 nav_panel('Error',
                           div(p('You must select at least one text file.')), 
                           style = 'padding:1em;color:red'),
                 select = T)
    } else if (!(input$file$type == 'text/plain') %>% all()) {
      nav_insert('results', 
                 nav_panel('Error',
                           div(p('Files must be in .txt format')), 
                           style = 'padding:1em;color:red'),
                 select = T)
    } else {
      nav_insert('results', 
                 nav_panel('Processing...',
                           div(p('Parsing texts...')), 
                           style = 'padding:1em'),
                 select = T)
      
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
      
      nav_remove('results', 'Processing...')
    }
  })
  
  networks = reactive({
    req(data())
    
    # Create the network files for download
    lapply(data(), function(d) {
      # Use words stems as vertices
      stems = d$stem %>% unique()
      edgelist = d %>%
        select(clique_id, stem) %>%
        mutate(Source = match(stem, stems),
               Target = Source) %>%
        group_by(clique_id) %>%
        # Create edge list for a network of cliques
        expand(Source, Target) %>%
        # Remove self loops
        filter(Source != Target) %>%
        mutate(edge_id = mapply(function(s, t) {
          sort(c(s, t)) %>%
            paste(collapse = '_')
        },
        Source, Target)) %>%
        ungroup() %>%
        # Remove mutual edges
        distinct(edge_id, .keep_all = T) %>%
        select(Source, Target)
      
      # Create string in Pajek NET format
      "*Vertices " %>%
        str_c(stems %>% length(), '\n') %>%
        str_c(paste0(1:length(stems), ' "', stems, '"', collapse = '\n'),
              '\n') %>%
        str_c('*Edges\n') %>%
        str_c(paste(edgelist$Source, edgelist$Target, collapse = '\n'))
    })
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
  
  output$download_results_cliques = downloadHandler(
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
      
      networks() %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name = paste0(str_sub(input$file$name[y], 0, -5), '.net')
            write_file(x, file.path(temp_directory, file_name))
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