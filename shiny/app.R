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
        h3(textOutput('results_title')),
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
  
  network = eventReactive(input$analyze, {
    req(data())
    stems = data()$stem %>% unique()
    edgelist = data() %>%
      select(clique_id, stem) %>%
      mutate(Source = match(stem, stems),
             Target = Source) %>%
      group_by(clique_id) %>%
      expand(Source, Target) %>%
      filter(Source != Target) %>%
      mutate(edge_id = mapply(function(s, t) {
        sort(c(s, t)) %>%
          paste(collapse = '_')
      },
      Source, Target)) %>%
      ungroup() %>%
      distinct(edge_id, .keep_all = T) %>%
      select(Source, Target)
    
    "*Vertices " %>%
      str_c(stems %>% length(), '\n') %>%
      str_c(paste0(1:length(stems), ' "', stems, '"', collapse = '\n'), 
            '\n') %>%
      str_c('*Edges\n') %>%
      str_c(paste(edgelist$Source, edgelist$Target, collapse = '\n'))
  })
  
  selectedTextFileName = eventReactive(input$analyze, {
    req(input$file$name)
    req(input$file$type[1] == 'text/plain')
    paste('Results for file', input$file$name)
  })
  
  output$results_title = renderText(
    selectedTextFileName()
  )
  
  output$results_cliques_title = renderText({
    req(results())
    'Indices values for the first 5 sentences/cliques'
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
  
  output$download_processed_text_title = renderText({
    req(data())
    'Use the buttons below to download the processed text.'
  })
  
  output$download_processed_text_info = renderText({
    req(data())
    'This allows 
    you inspect the results of tokenization, lemmatization, POS tagging, 
    stemming and the identification of synonyms and hypernyms.'
  })
  
  output$download_processed_text = renderUI({
    req(data())
    list(
      downloadButton('download_data_csv', 'Processed text (CSV)'),
      downloadButton('download_data_xlsx', 'Processed text (XLSX)')
    )
  })
  
  output$download_results_cliques_title = renderText({
    req(data())
    'Use the buttons below to download the complete results.'
  })
  
  output$download_results_cliques = renderUI({
    req(results())
    list(
      downloadButton('download_results_csv', 'Complete results (CSV)'),
      downloadButton('download_results_xlsx', 'Complete results (XLSX)')
    )
  })
  
  output$download_network_title = renderText({
    req(data())
    'Use the button below to download the text network.'
  })
  
  output$download_network_info = renderText({
    req(data())
    'Cohesion edges not included.'
  })
  
  output$download_network = renderUI({
    req(results())
    downloadButton('download_network_net', 'Network (Pajek NET)')
  })
  
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