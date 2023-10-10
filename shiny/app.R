library(shiny)
library(tidyverse)
library(udpipe)
library(SnowballC)
library(openxlsx)
library(shinyWidgets)

# Load dependencies ------------------------------------------------------------
source('cohesion_functions.R')

model = udpipe_load_model('./english-ewt-ud-2.5-191206.udpipe')
synonyms_hypernyms = read_csv('./english_synonyms_hypernyms.csv',
                              show_col_types = F)

# Define UI --------------------------------------------------------------------
ui = fluidPage(
  titlePanel('CohesionNet'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Calculate the cohesion indices of Oliveira, Senna e Pereira 
               (2024) of a text.'),
      fileInput('file', label = h3('Select text file')),
      actionButton('analyze', label = 'Run analysis', icon = icon('play'))
    ),
    mainPanel(
      h3(textOutput('results_title')),
      uiOutput('download_processed_text'),
      h4(textOutput('results_cliques_title')),
      uiOutput('download_results_cliques'),
      tableOutput('results'),
      h4(textOutput('results_text_title')),
      tableOutput('results_mean')
    )
  )
)

# Define server logic ----------------------------------------------------------
server = function(input, output) {
  data = eventReactive(input$analyze, {
    validate(
      need(input$file, 'You must select a text file.'),
      need(input$file$type[1] == 'text/plain', 
           'Invalid file: file must be plain text.')
    )
    
    data = read_file(input$file$datapath[1]) %>%
      udpipe(object = model) %>%
      filter(upos %in% c('ADJ', 'ADV', 'NOUN', 'NUM', 'PROPN', 'VERB')) %>%
      select(sentence_id, token, lemma, upos) %>%
      rename(clique_id = sentence_id,
             pos = upos) %>%
      mutate(lemma = str_to_lower(lemma),
             stem = wordStem(str_to_lower(token)),
             pos = case_match(pos, 
                              'ADJ' ~ 'JJ',
                              'ADV' ~ 'RB',
                              'NOUN' ~ 'NN',
                              'NUM' ~ 'NN',
                              'PROPN' ~ 'NN',
                              'VERB' ~ 'VB',
                              .default = pos)) %>%
      group_by(clique_id) %>%
      distinct(stem, pos, .keep_all = T) %>%
      left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
      ungroup()
  })
  
  results = eventReactive(input$analyze, {
    req(data())
    # Combine the results into a single tibble
    # Combinar os resultados em uma única tabela
    indices = global_backward_cohesion(data()) %>%
      select(clique_id, v, e) %>%
      # Identify the index
      # Identificar o índice
      mutate(index = 'Global Backward Cohesion') %>%
      bind_rows(
        local_backward_cohesion(data()) %>%
          select(clique_id, v, e) %>%
          # Identify the index
          # Identificar o índice
          mutate(index = 'Local Backward Cohesion')
      ) %>%
      bind_rows(
        mean_pairwise_cohesion(data()) %>%
          select(clique_id, v, e) %>%
          # Identify the index
          # Identificar o índice
          mutate(index = 'Mean Pairwise Cohesion')
      ) %>%
      rename(Index = index,
             `Clique ID` = clique_id,
             Vertex = v,
             Edge = e) %>%
      select(Index, `Clique ID`, Vertex, Edge)
  })
  
  selectedTextFileName = eventReactive(input$analyze, {
    req(input$file$name)
    req(input$file$type[1] == 'text/plain')
    input$file$name
  })
  
  output$results_title = renderText(
    selectedTextFileName()
  )
  
  output$download_processed_text = renderUI({
    req(data())
    list(
      downloadButton('download_data_csv', 'Download processed text (CSV)'),
      downloadButton('download_data_xlsx', '(XLSX)')
    )
  })
  
  output$results_cliques_title = renderText({
    req(results())
    'Values for the first 5 sentences/cliques'
  })
  
  output$download_results_cliques = renderUI({
    req(results())
    list(
      downloadButton('download_results_csv', 'Download complete results (CSV)'),
      downloadButton('download_results_xlsx', '(XLSX)')
    )
  })
  
  output$results = renderTable({
    results() %>%
      group_by(Index) %>%
      top_n(5, wt = -`Clique ID`)
  })
  
  output$results_text_title = renderText({
    req(results())
    'Mean values'
  })
  
  output$results_mean = renderTable({
    results() %>%
      group_by(Index) %>%
      summarize(Vertex = mean(Vertex),
                Edge = mean(Edge))
  })
  
  output$download_data_csv = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('_processed.csv')
    },
    content = function(file) {
      write.csv(data(), file, row.names = F)
    }
  )
  
  output$download_data_xlsx = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('_processed.xlsx')
    },
    content = function(file) {
      write.xlsx(data(), file, rowNames = F)
    }
  )
  
  output$download_results_csv = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('_results.csv')
    },
    content = function(file) {
      write.csv(results(), file, row.names = F)
    }
  )
  
  output$download_results_xlsx = downloadHandler(
    filename = function() {
      selectedTextFileName() %>%
        str_sub(1, -5) %>%
        str_c('_results.xlsx')
    },
    content = function(file) {
      write.xlsx(results(), file, rowNames = F)
    }
  )
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)