udpipe_parsing = function(text, language, complete = T) {
  model = switch(
    language,
    '1' = udpipe_load_model('./english-lines-ud-2.5-191206.udpipe'),
    '2' = udpipe_load_model('./portuguese-gsd-ud-2.5-191206.udpipe'),
    '3' = udpipe_load_model('./spanish-ancora-ud-2.5-191206.udpipe'))

  if (complete) {
    # Parsing with udpipe
    parsed_text = text %>%
      udpipe(object = model) %>%
      select(sentence_id, token, lemma, upos, token_id, head_token_id) %>%
      rename(segment_id = sentence_id) %>%
      mutate(pos = upos,
             word = token %>% str_to_lower(),
             lemma = lemma %>% str_to_lower(),
             stem = lemma %>%
               str_to_lower() %>%
               wordStem(language = switch(language,
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
      group_by(segment_id) %>%
      relocate(segment_id, 
               token, 
               word, 
               lemma, 
               stem, 
               upos, 
               pos, 
               token_id, 
               head_token_id)
  } else {
    parsed_text = text %>%
      udpipe_annotate(object = model, tagger = 'none', parser = 'none') %>%
      as_tibble() %>%
      select(sentence_id)
  }

  parsed_text
}
