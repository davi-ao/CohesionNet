udpipe_parsing = function(text, model, synonyms_hypernyms_list) {
  # Parsing with udpipe
  text %>%
    udpipe(object = model) %>%
    # Keep only the word classes that are useful for lexical cohesion analysis
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
    left_join(synonyms_hypernyms_list, by = c('lemma', 'pos')) %>%
    ungroup()
}
