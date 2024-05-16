udpipe_parsing = function(text, language, lexical, vertex_type) {
  model = switch(language,
         '1' = udpipe_load_model('./english-lines-ud-2.5-191206.udpipe'),
         '2' = udpipe_load_model('./portuguese-gsd-ud-2.5-191206.udpipe'),
         '3' = udpipe_load_model('./spanish-ancora-ud-2.5-191206.udpipe'))
  
  # English synonyms and hypernyms
  synonyms_hypernyms = read_csv('./english_synonyms_hypernyms.csv',
                                show_col_types = F)
  
  # Parsing with udpipe
  parsed_text = text %>%
    udpipe(object = model) %>%
    select(sentence_id, token, lemma, upos) %>%
    rename(clique_id = sentence_id) %>%
    mutate(pos = upos,
           word = str_to_lower(token),
           lemma = str_to_lower(lemma),
           stem = wordStem(str_to_lower(lemma),
                           language = switch(language,
                                             '1' = 'english',
                                             '2' = 'portuguese',
                                             '3' = 'spanish'))) %>%
    mutate(pos = case_match(pos, 
                            'ADJ' ~ 'JJ',
                            'ADV' ~ 'RB',
                            'NOUN' ~ 'NN',
                            'NUM' ~ 'NN',
                            'PROPN' ~ 'NN',
                            'VERB' ~ 'VB',
                            .default = pos)) %>%
    # If lemmatization or stemming returned NA, use the lowercase token instead
    mutate(lemma = ifelse(is.na(lemma), str_to_lower(token), lemma),
           stem = ifelse(is.na(stem), lemma, stem)) %>%
    group_by(clique_id) %>%
    left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
    relocate(clique_id, token, word, lemma, stem)
  
  if (lexical) {
    parsed_text = parsed_text %>%
      # Keep only the word classes that are useful for lexical cohesion analysis
      filter(upos %in% c('ADJ', 'ADV', 'NOUN', 'NUM', 'PROPN', 'VERB'))
  }
  
  if (vertex_type == 'token') {
    parsed_text %>%
      distinct(word, pos, .keep_all = T) %>%
      ungroup()
  } else if (vertex_type == 'word') {
    parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(word, pos, .keep_all = T) %>%
      ungroup()
  } else if (vertex_type == 'lemma') {
    parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(lemma, pos, .keep_all = T) %>%
      ungroup()
  } else if (vertex_type == 'stem') {
    parsed_text %>%
      filter(upos != 'PUNCT') %>%
      distinct(stem, pos, .keep_all = T) %>%
      ungroup()
  }
}
