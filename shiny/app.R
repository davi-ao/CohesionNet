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
library(shinycssloaders)

# Load local dependencies ------------------------------------------------------
# Cohesion funcions
source('cohesion_functions.R')
# Udpipe parsing function
source('udpipe_parsing_function.R')

# English model for udpipe
model = udpipe_load_model('./english-lines-ud-2.5-191206.udpipe')
synonyms_hypernyms = read_csv('./english_synonyms_hypernyms.csv',
                              show_col_types = F)

################################################################################
# SHINY CODE                                                                   #
################################################################################
# Define UI --------------------------------------------------------------------
# Fluid container
ui = fluidPage(
  tags$head(
    tags$style(
      HTML("#dashboard{margin-bottom:50px;}")
    )
  ),
  titlePanel('CohesionNet'),

  sidebarLayout(
    sidebarPanel(
      helpText('Calculate the cohesion indices of Oliveira, Senna e Pereira
               (2024) of a text.'),
      fileInput('file', label = h3('Select text file')),
      actionButton('analyze', label = 'Run analysis', icon = icon('play')),
      hr(),
      helpText(h4('Instructions')),
      helpText('Click on the \'Browse...\' button to select a .txt file for
               analysis. Ensure the file contains a single text and that [.:?!…]
               are used exclusively as sentence delimiters. Replace any other
               uses of these characters with different ones; for instance
               replace decimal separators like in "1.5" with underscores, making
               it "1_5".'),
      hr(),
      helpText(h4('References')),
      helpText('Please, cite the following work when using this app:'),
      helpText('Oliveira, D. A., Senna, V., & Pereira, H. B. B. (2024).
               Indices of Textual Cohesion by Lexical Repetition Based on
               Semantic Networks of Cliques. Expert Systems with Applications,
               237(2024), 121580. https://doi.org/10.1016/j.eswa.2023.121580')
    ),
    mainPanel(
      div(
        style="margin-bottom: 15px;",
        h3(withSpinner(textOutput('results_title'))),
        h4(textOutput('results_cliques_title')),
        tableOutput('results'),
        h4(textOutput('results_text_title')),
        tableOutput('results_mean'),
        hr(),
        h4(textOutput('download_processed_text_title')),
        textOutput('download_processed_text_info'),
        uiOutput('download_processed_text'),
        hr(),
        h4(textOutput('download_results_cliques_title')),
        uiOutput('download_results_cliques'),
        hr(),
        h4(textOutput('download_network_title')),
        textOutput('download_network_info'),
        uiOutput('download_network'),
      )
    )
  )
)

# Define server logic ----------------------------------------------------------
server = function(input, output) {
  # Preprocess
  data = eventReactive(input$analyze, {
    validate(
      need(input$file, 'You must select a text file.'),
      need(input$file$type[1] == 'text/plain',
           'Invalid file: file must be plain text.')
    )

    data = read_file(input$file$datapath[1]) %>%
      # Parse with udpipe
      udpipe_parsing(model, synonyms_hypernyms)
  })

  results = eventReactive(input$analyze, {
    req(data())
    # Combine the results into a single tibble
    indices = global_backward_cohesion(data()) %>%
      select(clique_id, v, e) %>%
      # Identify the index
      mutate(index = 'Global Backward Cohesion') %>%
      bind_rows(
        local_backward_cohesion(data()) %>%
          select(clique_id, v, e) %>%
          # Identify the index
          mutate(index = 'Local Backward Cohesion')
      ) %>%
      bind_rows(
        mean_pairwise_cohesion(data()) %>%
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

  # Create the network file for download
  network = eventReactive(input$analyze, {
    req(data())
    # Use words stems as vertices
    stems = data()$stem %>% unique()
    edgelist = data() %>%
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

  # Get filename
  selectedTextFileName = eventReactive(input$analyze, {
    req(input$file$name)
    req(input$file$type[1] == 'text/plain')
    paste('Results for file', input$file$name)
  })

  # Render results title
  output$results_title = renderText(
    selectedTextFileName()
  )

  # Render results subheading
  output$results_cliques_title = renderText({
    req(results())
    'Indices values for the first 5 sentences/cliques'
  })

  # Render results - preview table
  output$results = renderTable({
    results() %>%
      group_by(Index) %>%
      top_n(5, wt = -`Clique ID`)
  })

  # Render results subheading
  output$results_text_title = renderText({
    req(results())
    'Mean values'
  })

  # Render results - mean values table
  output$results_mean = renderTable({
    results() %>%
      group_by(Index) %>%
      summarize(Vertex = mean(Vertex),
                Edge = mean(Edge))
  })

  # Render instructions for downloading the processed text
  output$download_processed_text_title = renderText({
    req(data())
    'Use the buttons below to download the processed text.'
  })

  # Render information about the download of the processed text
  output$download_processed_text_info = renderText({
    req(data())
    'This allows
    you inspect the results of tokenization, lemmatization, POS tagging,
    stemming and the identification of synonyms and hypernyms.'
  })

  # Render buttons to download the processed text in CSV and XLSX
  output$download_processed_text = renderUI({
    req(data())
    list(
      downloadButton('download_data_csv', 'Processed text (CSV)'),
      downloadButton('download_data_xlsx', 'Processed text (XLSX)')
    )
  })

  # Render instructions for downloading the complete results
  output$download_results_cliques_title = renderText({
    req(data())
    'Use the buttons below to download the complete results.'
  })

  # Render the buttons to download the complete results in CSV and XLSX
  output$download_results_cliques = renderUI({
    req(results())
    list(
      downloadButton('download_results_csv', 'Complete results (CSV)'),
      downloadButton('download_results_xlsx', 'Complete results (XLSX)')
    )
  })

  # Render instructions for downloading the network
  output$download_network_title = renderText({
    req(data())
    'Use the button below to download the text network.'
  })

  # Render information about the network
  output$download_network_info = renderText({
    req(data())
    'Cohesion edges not included.'
  })

  # Render the button to download the network in Pajek NET format
  output$download_network = renderUI({
    req(results())
    downloadButton('download_network_net', 'Network (Pajek NET)')
  })

  # Prepare the CSV file with the processed text data
  output$download_data_csv = downloadHandler(
    filename = function() {
      'Processed text ' %>%
        str_c(
          selectedTextFileName() %>%
            str_sub(18, -5) %>%
            str_c('.csv')
        )
    },
    content = function(file) {
      write.csv(data(), file, row.names = F)
    }
  )

  # Prepare the XLSX file with the processed text data
  output$download_data_xlsx = downloadHandler(
    filename = function() {
      'Processed text ' %>%
        str_c(
          selectedTextFileName() %>%
            str_sub(18, -5) %>%
            str_c('.xlsx')
        )
    },
    content = function(file) {
      write.xlsx(data(), file, rowNames = F)
    }
  )

  # Prepare the CSV file with the complete results data
  output$download_results_csv = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('.csv')
    },
    content = function(file) {
      write.csv(results(), file, row.names = F)
    }
  )

  # Prepare the XLS file with the complete results data
  output$download_results_xlsx = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('.xlsx')
    },
    content = function(file) {
      write.xlsx(results(), file, rowNames = F)
    }
  )

  # Prepare the Pajek NET file with the network data
  output$download_network_net = downloadHandler(
    filename = function() {
      'Network for ' %>%
        str_c(
          selectedTextFileName() %>%
            str_sub(18, -5) %>%
            str_c('.net')
        )
    },
    content = function(file) {
      write_file(network(), file)
    }
  )
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)

################################################################################
# CODEOCEAN REPRODUCIBILITY CODE                                               #
################################################################################
# NOTICE: For reproducibility, the Shiny code is commented and the steps for
# computing the indices are available below. A sample text is used to 
# demonstrate how the indices are calculated.
# reproducibility_text = read_file(
#   'reproducibility_oliveira_senna_pereira_text1.txt') %>%
#   # Parse with udpipe
#   udpipe_parsing(model, synonyms_hypernyms)
# 
# # Combine the results into a single tibble
# reproducibility_indices = global_backward_cohesion(reproducibility_text) %>%
#   select(clique_id, v, e) %>%
#   # Identify the index
#   mutate(index = 'Global Backward Cohesion') %>%
#   bind_rows(
#     local_backward_cohesion(reproducibility_text) %>%
#       select(clique_id, v, e) %>%
#       # Identify the index
#       mutate(index = 'Local Backward Cohesion')
#   ) %>%
#   bind_rows(
#     mean_pairwise_cohesion(reproducibility_text) %>%
#       select(clique_id, v, e) %>%
#       # Identify the index
#       mutate(index = 'Mean Pairwise Cohesion')
#   ) %>%
#   rename(Index = index,
#          `Clique ID` = clique_id,
#          Vertex = v,
#          Edge = e) %>%
#   select(Index, `Clique ID`, Vertex, Edge)
# 
# # Use words stems as vertices
# reproducibility_stems = reproducibility_text$stem %>% unique()
# reproducibility_edgelist = reproducibility_text %>%
#   select(clique_id, stem) %>%
#   mutate(Source = match(stem, reproducibility_stems),
#          Target = Source) %>%
#   group_by(clique_id) %>%
#   # Create edge list for a network of cliques
#   expand(Source, Target) %>%
#   # Remove self loops
#   filter(Source != Target) %>%
#   mutate(edge_id = mapply(function(s, t) {
#     sort(c(s, t)) %>%
#       paste(collapse = '_')
#   },
#   Source, Target)) %>%
#   ungroup() %>%
#   # Remove mutual edges
#   distinct(edge_id, .keep_all = T) %>%
#   select(Source, Target)
# 
# # Create string in Pajek NET format
# reproducibility_network = "*Vertices " %>%
#   str_c(reproducibility_stems %>% length(), '\n') %>%
#   str_c(paste0(1:length(reproducibility_stems), 
#                ' "', 
#                reproducibility_stems, 
#                '"', 
#                collapse = '\n'), 
#         '\n') %>%
#   str_c('*Edges\n') %>%
#   str_c(paste(reproducibility_edgelist$Source, 
#               reproducibility_edgelist$Target, 
#               collapse = '\n'))
# 
# # Write reproducibility results
# write.csv(reproducibility_text, 
#           '/results/Processed text.csv', 
#           row.names = F)
# write.xlsx(reproducibility_text, 
#            '/results/Processed text.xlsx', 
#            rowNames = F)
# write.csv(reproducibility_indices, 
#           '/results/Complete results.csv', 
#           row.names = F)
# write.xlsx(reproducibility_indices, 
#            '/results/Complete results.xlsx', 
#            rowNames = F)
# write_file(reproducibility_network, '/results/Network.net')