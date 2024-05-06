udpipe_parsing = function(text, language) {
  model = switch(language,
         '1' = udpipe_load_model('./english-lines-ud-2.5-191206.udpipe'),
         '2' = udpipe_load_model('./portuguese-gsd-ud-2.5-191206.udpipe'),
         '3' = udpipe_load_model('./spanish-ancora-ud-2.5-191206.udpipe'))
  
  # English synonyms and hypernyms
  synonyms_hypernyms = read_csv('./english_synonyms_hypernyms.csv',
                                show_col_types = F)
  
  # Parsing with udpipe
  text %>%
    udpipe(object = model) %>%
    # Keep only the word classes that are useful for lexical cohesion analysis
    filter(upos %in% c('ADJ', 'ADV', 'NOUN', 'NUM', 'PROPN', 'VERB')) %>%
    select(sentence_id, token, lemma, upos) %>%
    rename(clique_id = sentence_id,
           pos = upos) %>%
    mutate(lemma = str_to_lower(lemma),
           stem = wordStem(str_to_lower(lemma), 
                           language = switch(language,
                                             '1' = 'english',
                                             '2' = 'portuguese',
                                             '3' = 'spanish')),
           pos = case_match(pos, 
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
    distinct(stem, pos, .keep_all = T) %>%
    left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
    ungroup()
}
