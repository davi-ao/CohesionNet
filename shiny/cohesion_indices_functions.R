calculate_global_cohesion = function(segments, G_next, nyms_network) {
  1:length(segments) %>%
      lapply(function(s) {
        if (s == 1) {
          v_cohesion = 0
          e_cohesion = 0
        } else {
          s_i = V(segments[[s]])$name
          G_prev = V(G_next[[s-1]])$name
          
          r = s_i %in% G_prev %>% sum()
          
          n_s = length(s_i)
          
          non_r_v = union(setdiff(s_i, G_prev), setdiff(G_prev, s_i))
          
          m = nyms_network %>%
            induced_subgraph(non_r_v[non_r_v %in% V(nyms_network)$name]) %>%
            ends(., E(.)) %>%
            apply(1, function(row) {
              ((row[1] %in% G_prev) && (row[2] %in% s_i)) ||
                ((row[2] %in% G_prev) && (row[1] %in% s_i))
            }) %>%
            sum()
          
          n_prev = length(G_prev)
          
          if (r == n_s || r == n_prev) {
            v_cohesion = 1
            e_cohesion = 0
          } else {
            v_cohesion = r/n_s
            e_cohesion = m/((n_s - r) * (n_prev - r))
          }
        }
        
        tibble(
          segment = s,
          index = 'global',
          v = v_cohesion,
          e = e_cohesion
        )
      }) %>%
      bind_rows()
}

calculate_local_cohesion = function(segments, nyms_network) {
  1:length(segments) %>%
    lapply(function(t) {
      1:length(segments[[t]]) %>%
        lapply(function(s) {
          if (s == 1) {
            v_cohesion = 0
            e_cohesion = 0
          } else {
            s_i = V(segments[[t]][[s]])$name
            s_prev = V(segments[[t]][[s-1]])$name
            
            r = s_i %in% s_prev %>% sum()
            
            n_s = length(s_i)
            
            non_r_v = union(setdiff(s_i, s_prev), setdiff(s_prev, s_i))
            
            m = nyms_network %>%
              induced_subgraph(non_r_v[non_r_v %in% V(nyms_network)$name]) %>%
              ends(., E(.)) %>%
              apply(1, function(row) {
                ((row[1] %in% s_prev) && (row[2] %in% s_i)) ||
                  ((row[2] %in% s_prev) && (row[1] %in% s_i))
              }) %>%
              sum()
            
            n_prev = length(s_prev)
            
            if (r == n_s || r == n_prev) {
              v_cohesion = 1
              e_cohesion = 0
            } else {
              v_cohesion = r/n_s
              e_cohesion = m/((n_s - r) * (n_prev - r))
            }
          }
          
          tibble(
            text = texts[[t]],
            segment = s,
            index = 'local',
            v = v_cohesion,
            e = e_cohesion
          )
        }) %>%
        bind_rows()
    }) %>%
    bind_rows()
}

calculate_pairwise_cohesion = function(segments, nyms_network) {
  1:length(segments) %>%
    lapply(function(t) {
      v_cohesion_matrix = matrix(nrow = length(segments[[t]]), 
                                 ncol = length(segments[[t]]))
      
      e_cohesion_matrix = matrix(nrow = length(segments[[t]]), 
                                 ncol = length(segments[[t]]))
      
      cohesion_t = 1:length(segments[[t]]) %>%
        lapply(function(i) {
          cohesion_s = 1:length(segments[[t]]) %>%
            lapply(function(j) {
              if (j <= i) {
                v_cohesion = 0
                e_cohesion = 0
              } else {
                s_i = V(segments[[t]][[i]])$name
                s_j = V(segments[[t]][[j]])$name
                
                r = s_i %in% s_j %>% sum()
                
                n_s = length(s_i)
                
                non_r_v = union(setdiff(s_i, s_j), setdiff(s_j, s_i))
                
                m = nyms_network %>%
                  induced_subgraph(non_r_v[non_r_v %in% V(nyms_network)$name]) %>%
                  ends(., E(.)) %>%
                  apply(1, function(row) {
                    ((row[1] %in% s_j) && (row[2] %in% s_i)) ||
                      ((row[2] %in% s_j) && (row[1] %in% s_i))
                  }) %>%
                  sum()
                
                n_j = length(s_j)
                
                if (r == n_s || r == n_j) {
                  v_cohesion = 1
                  e_cohesion = 0
                } else {
                  v_cohesion = r/n_s
                  e_cohesion = m/((n_s - r) * (n_j - r))
                }
              }
              
              tibble(v_cohesion, e_cohesion)
            }) %>%
            bind_rows()
          
          list(v_cohesion = cohesion_s$v_cohesion,
               e_cohesion = cohesion_s$e_cohesion)
        })
      
      v_cohesion_t = cohesion_t %>%
        sapply(function(c_s) {
          c_s$v_cohesion
        })
      
      e_cohesion_t = cohesion_t %>%
        sapply(function(c_s) {
          c_s$e_cohesion
        })
      
      v_cohesion_t = v_cohesion_t + t(v_cohesion_t)
      e_cohesion_t = e_cohesion_t + t(e_cohesion_t)
      
      tibble(
        text = texts[[t]],
        segment = 1:length(segments[[t]]),
        index = 'pairwise',
        v = v_cohesion_t %>% rowMeans(),
        e = e_cohesion_t %>% rowMeans()
      )
    }) %>%
    bind_rows()
}
