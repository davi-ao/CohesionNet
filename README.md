[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11206286.svg)](https://doi.org/10.5281/zenodo.11206286)

# CohesionNet
Software for Network-Based Textual Cohesion Analysis

Analyze indices of textual cohesion of an English text using semantic networks of cliques with a Shiny GUI. The indices used are those proposed in Oliveira et al. (2024).

The app was based on the script available at https://doi.org/10.24433/CO.0568085.v1, which was developed and used for the study reported in the article “Indices of Textual Cohesion by Lexical Repetition Based on Semantic Networks of Cliques” (Oliveira et al. 2024).

This app is an improvement over the script on which it was based because it can be used with any English, Spanish or Portuguese text, provided it was manually cleaned, and uses a Shiny graphical user interface to facilitate the process of calculating the cohesion indices. Manual cleaning is necessary to ensure that the file to be processed contains a single text and that the caracters [.:?!…] are used exclusively as sentence delimiters. The process of manual cleaning must replace any other uses of these characters with different ones; for instance replacing decimal separators like in "1.5" with underscores, making it "1_5". Additionally, this script uses a different udpipe model (English-LinES, Straka et al. 2017, Straka et al. 2016) to parse the text, with improved performance in universal part of speech tagging and lemmatization.

## How to cite
Oliveira, D. A., & Pereira, H. B. B. (2024a). CohesionNet (v3.1.1). Zenodo. https://doi.org/10.5281/zenodo.11206286

Oliviera, D. A., & Pereira, H. B. B. (2024b). Modeling texts with networks: comparing five approaches to sentence representation. <em>The European Physical Journal B</em>, 97(6), 77. https://doi.org/10.1140/epjb/s10051-024-00717-0

Oliveira, D. A., Senna, V., & Pereira, H. B. B. (2024). Indices of textual cohesion by lexical repetition based on semantic networks of cliques. <em>Expert Systems with Applications</em>, 237, 121580. https://doi.org/10.1016/j.eswa.2023.121580

## References

Straka, M., & Straková, J. (2017) Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe. In Proceedings of the CoNLL 2017 Shared Task: Multilingual Parsing from Raw Text to Universal Dependencies, Vancouver, Canada. https://ufal.mff.cuni.cz/~straka/papers/2017-conll_udpipe.pdf

Straka, M., Hajič, J., & Straková, J. (2016). UDPipe: Trainable Pipeline for Processing CoNLL-U Files Performing Tokenization, Morphological Analysis, POS Tagging and Parsing. In Proceedings of the Tenth International Conference on Language Resources and Evaluation (LREC 2016), Portorož, Slovenia. http://ufal.mff.cuni.cz/~straka/papers/2016-lrec_udpipe.pdf

## License
Copyright 2024 Davi Alves Oliveira

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
