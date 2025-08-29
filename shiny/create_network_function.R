create_network = function(
    parsed_text,
    lexical,
    vertex_type,
    edge_type,
    add_weights,
    incidence_fidelity) {
  # English -nyms networks
  nyms = switch(vertex_type,
                '1' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                '2' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                '3' = read_graph('./english_nyms_lemmas.net', 'pajek'),
                '4' = read_graph('./english_nyms_stems.net', 'pajek'))

  if (lexical) {
    parsed_text = parsed_text %>%
      # Keep only the word classes that are useful for lexical cohesion analysis
      filter(upos %in% c('ADJ', 'ADV', 'NOUN', 'NUM', 'PROPN', 'VERB'))
  }

  if (vertex_type == 1) { # token
    parsed_text = parsed_text %>%
      distinct(segment_id, token, .keep_all = T) %>%
      ungroup() %>%
      select(segment_id, token, token_id, head_token_id) %>%
      rename(vertex = token)
  } else if (vertex_type == 2) { # word
    parsed_text = parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(segment_id, word, .keep_all = T) %>%
      ungroup() %>%
      select(segment_id, word, token_id, head_token_id) %>%
      rename(vertex = word)
  } else if (vertex_type == 3) { # lemma
    parsed_text = parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(segment_id, lemma, .keep_all = T) %>%
      ungroup() %>%
      select(segment_id, lemma, token_id, head_token_id) %>%
      rename(vertex = lemma)
  } else if (vertex_type == 4) { # stem
    parsed_text = parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(segment_id, stem, .keep_all = T) %>%
      ungroup() %>%
      select(segment_id, stem, token_id, head_token_id) %>%
      rename(vertex = stem)
  }
  
  if (incidence_fidelity) {
    cooccurrences = parsed_text %>%
      group_by(segment_id) %>%
      select(vertex) %>%
      rename(Source = vertex) %>%
      mutate(Target = Source) %>%
      expand(Source, Target) %>%
      filter(Source != Target) %>%
      rowwise() %>%
      mutate(edge_id = sort(c(Source, Target)) %>% 
               paste(collapse = '¬')) %>%
      group_by(segment_id) %>%
      distinct(edge_id, .keep_all = T) %>%
      separate(edge_id, into = c('Source', 'Target'), sep = '¬')
  }
  
  G = switch(edge_type,
             '1' = { # line
               edge_list = parsed_text %>%
                 select(segment_id, vertex) %>%
                 rename(Source = vertex) %>%
                 group_by(segment_id) %>%
                 mutate(Target = Source %>% lead()) %>%
                 ungroup() %>%
                 filter(!is.na(Target) & Source != '' & Target != '')
               
               G =  edge_list %>%
                 select(Source, Target) %>%
                 as.matrix() %>%
                 graph_from_edgelist(directed = F)
               
               if (add_weights) {
                 E(G)$Weight = 1
                 
                 G = G %>%
                   simplify(edge.attr.comb = list(Weight = 'sum'))
               } else {
                 G = G %>%
                   simplify()
               }
               
               G = G %>%
                 set_edge_attr('cohesion_edge', value = F)
               
               nyms_sub_edges = nyms %>%
                 induced_subgraph(V(G)$name[V(G)$name %in% 
                                              V(nyms)$name]) %>%
                 ends(., E(.))
               
               if (nrow(nyms_sub_edges) > 0) {
                 for (row in 1:nrow(nyms_sub_edges)) {
                   if (!G %>% are_adjacent(nyms_sub_edges[row,1], 
                                           nyms_sub_edges[row,2])) {
                     G = G %>%
                       add_edges(c(nyms_sub_edges[row,1], nyms_sub_edges[row,2]),
                                 cohesion_edge = T)
                   }
                 }
               }
               
               if (incidence_fidelity) {
                 IF_indices = as_data_frame(G, 'edges') %>%
                   mutate(IF = mapply(function(a, b) {
                     a_and_b = cooccurrences %>%
                       filter((Source == a & Target == b)|
                                (Source == b & Target == a)) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     a_or_b = cooccurrences %>%
                       filter(Source == a | 
                                Target == a | 
                                Source == b | 
                                Target == b) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     (a_and_b^2)/
                       (length(cooccurrences$segment_id %>% unique()) * a_or_b)
                   }, from, to))
                 
                 E(G)$IF = IF_indices$IF
               }
               
               v = tibble(name = V(G)$name) %>%
                 left_join(
                   edge_list %>%
                     arrange(segment_id) %>%
                     pivot_longer(c(Source, Target),
                                  names_to = 'type',
                                  values_to = 'name') %>%
                     unique() %>%
                     group_by(name) %>%
                     summarise(segments = paste0(
                       '<[',
                       paste(unique(segment_id[!is.na(segment_id)]), 
                             collapse = ','), 
                       ']>'))
                 )

               V(G)$id = V(G)$name
               V(G)$segments = v$segments
               
               segments = V(G)$segments %>%
                 str_remove_all('[<\\[\\]>]') %>%
                 str_split(',') %>%
                 unlist() %>%
                 unique() %>%
                 as.numeric() %>%
                 sort() %>%
                 lapply(function(segment) {
                   induced_subgraph(G, V(G)[which(
                     V(G)$segments %>%
                       str_detect(paste0('[\\[,]', segment, '[,\\]]')))]) %>%
                     delete_edge_attr('cohesion_edge')
                 })
               
               G_next = 1:length(segments) %>%
                 lapply(function(s) {
                   V_G_next = segments[1:s] %>%
                     reduce(union) %>%
                     V() %>%
                     .$name
                   
                   G %>%
                     induced_subgraph(V_G_next)
                 })
               
               list(network = G, segments = segments, G_next = G_next)
             },
             '2' = { # clique
               edge_list = parsed_text %>%
                 select(segment_id, vertex) %>%
                 rename(Source = vertex) %>%
                 group_by(segment_id) %>%
                 mutate(Target = Source) %>%
                 expand(Source, Target) %>%
                 rowwise() %>%
                 mutate(edge_id = paste(sort(c(Source, Target)), 
                                        collapse = '-')) %>%
                 ungroup() %>%
                 filter(!is.na(Target) & 
                          Source != '' & 
                          Target != '' & 
                          Source != Target) %>%
                 distinct(segment_id, edge_id, .keep_all = T)
               
               G =  edge_list %>%
                 select(Source, Target) %>%
                 as.matrix() %>%
                 graph_from_edgelist(directed = F)
                 
               if (add_weights) {
                 E(G)$Weight = 1
                 
                 G = G %>%
                   simplify(edge.attr.comb = list(Weight = 'sum'))
               } else {
                 G = G %>%
                   simplify()
               }
             
               G = G %>%
                 set_edge_attr('cohesion_edge', value = F)
               
               nyms_sub_edges = nyms %>%
                 induced_subgraph(V(G)$name[V(G)$name %in% 
                                              V(nyms)$name]) %>%
                 ends(., E(.))
               
               if (nrow(nyms_sub_edges) > 0) {
                 for (row in 1:nrow(nyms_sub_edges)) {
                   if (!G %>% are_adjacent(nyms_sub_edges[row,1], 
                                           nyms_sub_edges[row,2])) {
                     G = G %>%
                       add_edges(c(nyms_sub_edges[row,1], nyms_sub_edges[row,2]),
                                 cohesion_edge = T)
                   }
                 }
               }
               
               if (incidence_fidelity) {
                 IF_indices = as_data_frame(G, 'edges') %>%
                   mutate(IF = mapply(function(a, b) {
                     a_and_b = cooccurrences %>%
                       filter((Source == a & Target == b)|
                                (Source == b & Target == a)) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     a_or_b = cooccurrences %>%
                       filter(Source == a | 
                                Target == a | 
                                Source == b | 
                                Target == b) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     (a_and_b^2)/
                       (length(cooccurrences$segment_id %>% unique()) * a_or_b)
                   }, from, to))
                 
                 E(G)$IF = IF_indices$IF
               }
               
               v = tibble(name = V(G)$name) %>%
                 left_join(
                   edge_list %>%
                     arrange(segment_id) %>%
                     pivot_longer(c(Source, Target),
                                  names_to = 'type',
                                  values_to = 'name') %>%
                     unique() %>%
                     group_by(name) %>%
                     summarise(segments = paste0(
                       '<[',
                       paste(unique(segment_id[!is.na(segment_id)]), 
                             collapse = ','), 
                       ']>'))
                 )
               
               V(G)$id = V(G)$name
               V(G)$segments = v$segments
               
               segments = V(G)$segments %>%
                 str_remove_all('[<\\[\\]>]') %>%
                 str_split(',') %>%
                 unlist() %>%
                 unique() %>%
                 as.numeric() %>%
                 sort() %>%
                 lapply(function(segment) {
                   induced_subgraph(G, V(G)[which(
                     V(G)$segments %>%
                       str_detect(paste0('[\\[,]', segment, '[,\\]]')))]) %>%
                     delete_edge_attr('cohesion_edge')
                 })
               
               G_next = 1:length(segments) %>%
                 lapply(function(s) {
                   V_G_next = segments[1:s] %>%
                     reduce(union) %>%
                     V() %>%
                     .$name
                   
                   G %>%
                     induced_subgraph(V_G_next)
                 })
               
               list(network = G, segments = segments, G_next = G_next)
             },
             '3' = { # dependency-based
               vertices = parsed_text %>%
                 select(segment_id, vertex, token_id, head_token_id) %>%
                 rename(Source = vertex)
               
               dependencies = parsed_text %>%
                 select(segment_id, vertex, token_id) %>%
                 rename(head_token_id = token_id,
                        Target = vertex)
               
               edge_list = vertices %>%
                 left_join(dependencies,
                           by = c('segment_id', 'head_token_id')) %>%
                 filter(!is.na(Target) & Source != '' & Target != '')
               
               G =  edge_list %>%
                 select(Source, Target) %>%
                 as.matrix() %>%
                 graph_from_edgelist(directed = F)
               
               if (add_weights) {
                 E(G)$Weight = 1
                 
                 G = G %>%
                   simplify(edge.attr.comb = list(Weight = 'sum'))
               } else {
                 G = G %>%
                   simplify()
               }
               
               G = G %>%
                 set_edge_attr('cohesion_edge', value = F)
               
               nyms_sub_edges = nyms %>%
                 induced_subgraph(V(G)$name[V(G)$name %in% 
                                              V(nyms)$name]) %>%
                 ends(., E(.))
               
               if (nrow(nyms_sub_edges) > 0) {
                 for (row in 1:nrow(nyms_sub_edges)) {
                   if (!G %>% are_adjacent(nyms_sub_edges[row,1], 
                                           nyms_sub_edges[row,2])) {
                     G = G %>%
                       add_edges(c(nyms_sub_edges[row,1], nyms_sub_edges[row,2]),
                                 cohesion_edge = T)
                   }
                 }
               }
               
               if (incidence_fidelity) {
                 IF_indices = as_data_frame(G, 'edges') %>%
                   mutate(IF = mapply(function(a, b) {
                     a_and_b = cooccurrences %>%
                       filter((Source == a & Target == b)|
                                (Source == b & Target == a)) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     a_or_b = cooccurrences %>%
                       filter(Source == a | 
                                Target == a | 
                                Source == b | 
                                Target == b) %>%
                       distinct(segment_id) %>%
                       nrow()
                     
                     (a_and_b^2)/
                       (length(cooccurrences$segment_id %>% unique()) * a_or_b)
                   }, from, to))
                 
                 E(G)$IF = IF_indices$IF
               }
               
               v = tibble(name = V(G)$name) %>%
                 left_join(
                   edge_list %>%
                     arrange(segment_id) %>%
                     pivot_longer(c(Source, Target),
                                  names_to = 'type',
                                  values_to = 'name') %>%
                     unique() %>%
                     group_by(name) %>%
                     summarise(segments = paste0(
                       '<[',
                       paste(unique(segment_id[!is.na(segment_id)]), 
                             collapse = ','), 
                       ']>'))
                 )
               
               V(G)$id = V(G)$name
               V(G)$segments = v$segments
               
               segments = V(G)$segments %>%
                 str_remove_all('[<\\[\\]>]') %>%
                 str_split(',') %>%
                 unlist() %>%
                 unique() %>%
                 as.numeric() %>%
                 sort() %>%
                 lapply(function(segment) {
                   induced_subgraph(G, V(G)[which(
                     V(G)$segments %>%
                       str_detect(paste0('[\\[,]', segment, '[,\\]]')))]) %>%
                     delete_edge_attr('cohesion_edge')
                 })
               
               G_next = 1:length(segments) %>%
                 lapply(function(s) {
                   V_G_next = segments[1:s] %>%
                     reduce(union) %>%
                     V() %>%
                     .$name
                     
                     G %>%
                       induced_subgraph(V_G_next)
                 })
               
               list(network = G, segments = segments, G_next = G_next)
             })
}