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
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")
# install.packages('bslib')
# install.packages('DT')

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
    fileInput('file', label = 'Select text files', multiple = T),
    selectInput('language', 
                label = 'Language',
                choices = list('English' = 1, 
                               'Portuguese' = 2, 
                               'Spanish' = 3)),
    selectInput('vertex_type',
                label = 'Vertex type',
                choices = list('token', 'word', 'lemma', 'stem'),
                selected = 'stem'),
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
                  .txt file for analysis. Ensure the file contains a single text 
                  and that [.:?!…] are used exclusively as sentence delimiters. 
                  Replace any other uses of these characters with different 
                  ones; for instance replace decimal separators like in "1.5" 
                  with underscores, making it "1_5".'),
                p('Select the language of the texts.'),
                p('Select the vertex type. The type "token" includes all set of
                  characters delimited by a whitespace. The type "word" includes
                  all tokens except those identified as punctuation marks. The
                  type "lemma" includes only unique lemmatized words (i.e., 
                  words with inflections removed). The type "stem" includes only
                  unique stemmed lemmas.'),
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
  nav_hide('results', 'error1')
  nav_hide('results', 'error2')
  
  data = reactive({
    lapply(input$file$datapath, function(path) {
      read_file(path) %>%
        # Parse with udpipe
        udpipe_parsing(input$language, input$lexical, input$vertex_type)
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
    
    hidePageSpinner()
  })
  
  networks = reactive({
    showPageSpinner()
    req(data())
    
    # Create the network files for download
    networks = lapply(data(), function(d) {
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
    
    hidePageSpinner()
    networks
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