Explanation of the columns/variables in "Appendix_D_stimuli_verbs.csv"
======================================================================

Variables
---------

- word: Word (verb) shown to participants
- category: Whether the word is a arm-related or leg-related verb
- arousal: Mean judgements of word arousal on a scale from 1 (very weak) to 7 (very strong). The task was carried out in Swedish.
- imageability: Mean judgements of word imageability on a scale from 1 (very weak) to 7 (very strong). The task was carried out in Swedish.
- valence: Mean judgements of word valence on a scale from 1 (very negative) to 7 (very positive). The task was carried out in Swedish.
- arm: Mean arm-relatedness judgement on a scale from 1 (not at all arm-related) to 7 (strongly arm-related). The task was carried out in Swedish.
- leg: Mean leg-relatedness judgement on a scale from 1 (not at all leg-related) to 7 (strongly leg-related). The task was carried out in Swedish.
- bias: arm score minus leg score
- bias_absol: bias in the congruent direction (i.e., a negative score would mean that an arm word obtained a higher leg-relatedness than arm-relatedness score, and vice versa)
- word_logfreq: log10 of the frequency of a word (word form) in the corpus
- lemma_logfreq: log10 of the frequency of a lemma (occurrences of the word tagged as verbs) in the corpus
- pos_ambig: Part-of-speech (POS) ambiguity, computed as 1 minus the proportion of verb occurrences of a lemma divided by all occurrences of the same lemma (tagged as any other POS). Thus we obtain a score between 0 and 1, where a score close to 0 tells us that there is little ambiguity (the word almost always occurs as a verb) and a score close to 1 tells us that there is maximal ambiguity (the word hardly ever occurs as a verb).
- bigram_logfreq: mean log10 frequency of all the successive bigrams that make up a word (e.g., for "climb", the mean log10 frequencies of "cl", "li", "im" and "mb")
- trigram_logfreq: similar to bigram_logfreq but computed for the trigrams in each word. 
- nletters: number of letters
- nphon: number of phonemes, taking only into account phoneme quality and not length. Based on correspondence with Swedish phonologist Tomas Riad.


Frequency measures
------------------

All frequency counts were obtained from the Swedish version of OpenSubtitles (retrieved on 2019-04-15):

P. Lison, J. Tiedemann, “OpenSubtitles2016: Extracting large parallel corpora from movie and TV subtitles” in Proceedings of the 10th Annual Conference on
Language Resources and Evaluation, N. Calzolari et al., Eds. (European Language Resources Association, Paris, 2016), pp. 923–929.


Ratings
-------

All ratings were provided by native speakers of Swedish. These were collected on two different occasions: 1) arm- and leg-relatedness ratings (12 participants) and 2) arousal, imageability, and valence ratings (25 participants).
