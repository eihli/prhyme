======
 TODO
======

- Allow a depth with thesaurus lookups.
- Allow restriction to rhymes with certain number of syllables.
- Word graph with weights to form most likely sentences.
- Use CMU LexTool to find pronunciations for words not in dictionary.
  http://www.speech.cs.cmu.edu/tools/lextool.html

=============
 Terminology
=============

Use case:
---------

I want to find phrase B that rhymes with phrase A where phrase B has a
specifiable sentiment.

Something like:
"please turn on your magic beam"
"queeze churn horrific bloodstream"

I want these settings to be optional:
- phrase B to conform to certain grammatical structure.
- config of which words I prefer rhymes in phrase be
- config for rhyming rimes, onsets, and/or nuclei
- preferred number of syllables

Some of those settings make more sense with individual words than with phrases.

Here's a tricky consideration. Let's break those phrases down into syllables.

"please turn on your magic beam"
"queeze churn horrific bloodstream"

("P" "L" "IY" "Z") ("T" "ER" "N") ("AO" "N") ("Y" "AO" "R") ("M" "AE" "JH" "IH" "K") ("B" "IY" "M")
("K" "W" "IY" "Z") ("CH" "ER" "N") ("H" "AO" "R" "IH" "F" "EU" "K") ("B" "L" "AH" "D" "S" "T" "R" "IY" "M")

We are imagining turning
("AO" "N") ("Y" "AO" "R") ("M" "AE" "JH")
into
("H" "AO" "R" "IH" "F" "IH" "K")

There's this difficulty in deciding how to group the syllables into words.

("P" "L" "IY" "Z" "T" "ER" "N" "AO" "N" "Y" "AO" "R" "M" "AE" "JH" "IH" "K" "B" "IY" "M")
("K" "W" "IY" "Z" "CH" "ER" "N" "H" "AO" "R" "IH" "F" "IH" "K" "B" "L" "AH" "D" "S" "T" "R" "IY" "M")

If you take just the raw syllables and ignore the words, you get
("P" "L" "IY" "Z") ("T" "ER" "N")  ("AO" "N")     ("Y" "AO" "R")   ("M" "AE") ("JH" "IH" "K")     ("B" "IY" "M")
and
("K" "W" "IY" "Z") ("CH" "ER" "N") ("H" "AO" "R") ("IH" "F")       ("IH" "K") ("B" "L" "AH" "D") ("S" "T" "R" "IY" "M")

If you leave the syllables grouped into words, then MAGIC rhymes with
TRAGIC, PELAGIC, etc... If you ignore the groupings of syllables into
words, then you're stuck trying to rhyme with the single syllable
"word" ("M" "AE"), which doesn't have any rhymes.

Implementation
--------------

What would it look like to solve the problem for a single grouping of syllables into words?

In the case of
("P" "L" "IY" "Z") ("T" "ER" "N") ("AO" "N") ("Y" "AO" "R") ("M" "AE" "JH" "IH" "K") ("B" "IY" "M")

We wouldn't get
("K" "W" "IY" "Z") ("CH" "ER" "N") ("H" "AO" "R" "IH" "F" "EU" "K") ("B" "L" "AH" "D" "S" "T" "R" "IY" "M")

We might get the following, since the syllable groupings align.
"queeze churn don more tragic fiends"

We don't want to always restrict it to matching syllable groups,
especially not for single words. If we give it the word "nation" we
almost surely want words like "approbation" and "creation"; speaking
from the use case of trying to find a rhyme to the last word of a
phrase.

Back to the entire phrase - the idea is we solve the problem for a
single grouping of syllables and then we use the "partitions" function
to get every possible combination of grouping of syllables and apply
the solution to each of those.

Performance
+++++++++++

Let's say we want to see all possible rhyming phrases of
("P" "L" "IY" "Z") ("T" "ER" "N") ("AO" "N") ("Y" "AO" "R") ("M" "AE" "JH" "IH" "K") ("B" "IY" "M")

Let's assume each syllable grouping has an average of 10 rhyming words.

That's 10^6 possible phrases. We need a way to limit our search for
both computational reasons and for UX reasons.

There's going to be a lot of redundancy there.

We don't need each of:
"please churn don poor tragic fiend"
"squeeze churn don poor tragic fiend"
"freeze churn don poor tragic fiend"
"sneeze churn don poor tragic fiend"
\...

So maybe we cycle through each word list.

please  churn don  poor  tragic  fiend
squeeze burn  con  door  plagic mean
freeze  turn  fawn store tragic  stream
sneeze  yearn don  more  plagic steam
peas    churn con  four  tragic  deem
queeze  burn  fawn yore  plagic reem

A seperate process can search through these phrases and rank them by grammatical structure, sentiment, etc...

We could also pre-filter the possible words by sentiment.

Or, we could assign grammatical restrictions to each word and
pre-filter the words by grammar. Then we'd get something like
(adjective noun verb adjective noun) and it would really reduce the
search space.

But would we do that by hand? That might work for an individual
grouping of syllables, but how would we restrict to that for each
possible combination of grouping of syllables?

One possible solution would be to have a list of all valid grammar structures for a certain number of words.

"please churn don poor tragic fiends"
(adj noun adv verb adj noun)
(adj adj noun verb adj noun)
(noun verb noun conj noun verb)
\...

Output
++++++

::

    (["TEASE" "STERN" "CON" "SCORE" "MANIC" "STEAM"]
    ["SQUEEZE" "BURN" "WAN" "OR" "BEATNIK" "TEAM"]
    ["WHEEZE" "CHURN" "ON" "WHORE" "FABRIC" "SCREAM"]
    ["SNEEZE" "TURN" "CON" "GORE" "FRANTIC" "SCHEME"]
    ["FREEZE" "EARN" "WAN" "CORE" "EPIC" "STREAM"]
    ["EASE" "STERN" "ON" "FLOOR" "CRYPTIC" "SEAM"]
    ["SEIZE" "BURN" "CON" "BORE" "TOPIC" "THEME"]
    ["TEASE" "CHURN" "WAN" "SNORE" "TOXIC" "DREAM"]
    ["SQUEEZE" "TURN" "ON" "STORE" "TONIC" "STEAM"]
    ["WHEEZE" "EARN" "CON" "SORE" "MYSTIC" "TEAM"]
    ["SNEEZE" "STERN" "WAN" "ROAR" "STATIC" "SCREAM"]
    ["FREEZE" "BURN" "ON" "FOR" "CLASSIC" "SCHEME"]
    ["EASE" "CHURN" "CON" "CORPS" "SEPTIC" "STREAM"]
    ["SEIZE" "TURN" "WAN" "BOAR" "CRITIC" "SEAM"]
    ["TEASE" "EARN" "ON" "POUR" "CHRONIC" "THEME"]
    ["SQUEEZE" "STERN" "CON" "SCORE" "LIPSTICK" "DREAM"]
    ["WHEEZE" "BURN" "WAN" "OR" "PANIC" "STEAM"]
    ["SNEEZE" "CHURN" "ON" "WHORE" "SEISMIC" "TEAM"]
    ["FREEZE" "TURN" "CON" "GORE" "FROLIC" "SCREAM"]
    ["EASE" "EARN" "WAN" "CORE" "GOTHIC" "SCHEME"]
    ["SEIZE" "STERN" "ON" "FLOOR" "TRAGIC" "STREAM"]
    ["TEASE" "BURN" "CON" "BORE" "CATHOLIC" "SEAM"]
    ["SQUEEZE" "CHURN" "WAN" "SNORE" "CYNIC" "THEME"]
    ["WHEEZE" "TURN" "ON" "STORE" "COMIC" "DREAM"]
    ["SNEEZE" "EARN" "CON" "SORE" "PSYCHIC" "STEAM"]
    ["FREEZE" "STERN" "WAN" "ROAR" "RELIC" "TEAM"]
    ["EASE" "BURN" "ON" "FOR" "COSMIC" "SCREAM"]
    ["SEIZE" "CHURN" "CON" "CORPS" "DRASTIC" "SCHEME"])


::

    TEASE   STERN CON SCORE  MANIC      STEAM
    BREEZE  BURN  WAN OR     FABRIC     GLEAM
    SQUEEZE CHURN ON  WHORE  FRANTIC    TEAM
    WHEEZE  TURN      GORE   EPIC       SCREAM
    SNEEZE            CORE   CRYPTIC    SCHEME
    FREEZE            FLOOR  PUBLIC     STREAM
    EASE              BORE   TOPIC      BEAM
    SEIZE             SNORE  TOXIC      SEAM
                      STORE  TONIC      THEME
                      SORE   MYSTIC     DREAM
                      ROAR   STATIC
                      WAR    SIDEKICK
                      FOR    SEPTIC
                      CORPS  BROOMSTICK
                      DRAWER CHRONIC
                      POUR   LIPSTICK
                             PANIC
                             SEISMIC
                             LOGIC
                             FROLIC
                             TRAGIC
                             ATTIC
                             CYNIC
                             RELIC
                             COSMIC
                             DRASTIC

Features
--------

Given an output like the above, a user might see a word or phrase they really
like.

"FRANTIC SCREAM" for example.

The rest of the sentence doesn't need to rhyme or necesarily contain words that
are synonyms.

Can we provide suggestions for the rest of the sentence? We know the number of
syllables we want and the sentiment we want.

Could we use something like a Markov chain to work backwards? Given some corpus,
what words most likely preceed "frantic scream" that also align with our
syllabic requirements?

==============
 Articulation
==============

Terminology and types of rhymes
-------------------------------

1. HAT - CAT
2. HAT - HALF
3. HAT - PACK

The first of those examples clearly rhymes by anyone's definition of "rhyme". The first sound of the syllable known as the "onset", differs. The vowel sound, known as the "nuclei", and the final consonant sound, known as the "coda", are the same.

The second example might not technically rhyme, but it can still be useful. The "onset", the "H" sound, and the "nuclei", the "AE" sound, are the same in both HAT and HALF. But they differ in their "coda".

The third example is even less of a proper rhyme, but again it can be useful. The only matching sound is the "nuclei", the "AE" sound.

Words with multiple syllables give us even more options.

What is more important: to find the fewest words that rhyme any number of syllables (STUPIFIED - DIGNIFIED), or to find the fewest words that rhyme the greatest number of onsets/nuclei/codas (STUPIFIED - SCOOBY DIED)?

1. STUPIFIED - SCOOBY DIED
1. STUPIFIED - GROOVY FINE
1. STUPIFIED - DIGNIFIED
1. STUPIFIED - PRIDE

Program Output
--------------

Perfect rhymes
DOG     -> [ [ [FOG COG HOG ...] ] ]

Onset rhymes
DOG     -> [ [ [DOLL DAWN ...] ] ]

Nuclei rhymes
DOG     -> [ [ [BALL CAUGHT FOUGHT ...] ] ]

For multiple syllables, show rhymes for each possible partitioning of syllables.
Order by rhymes that use the fewest number of words.
BEEHIVE -> [ [ [REVIVE DEPRIVE] ]
             [ [SEE WE BE ...] [THRIVE DIVE ...] ] ]

For multi-syllable words, remove restriction to rhyme on every syllable.
Order by words matching greatest number of syllables.
BEEHIVE -> [ [ [REVIVE DEPRIVE ALIVE] ]
             [ [SEE WE BE ... ] [THRIVE DIVE ...] ] ]

Syllables
---------
Typical model

In the typical theory[citation needed] of syllable structure, the general structure of a syllable (σ) consists of three segments. These segments are grouped into two components:

Onset (ω)
    a consonant or consonant cluster, obligatory in some languages, optional or even restricted in others
Rime (ρ)
    right branch, contrasts with onset, splits into nucleus and coda

    Nucleus (ν)
        a vowel or syllabic consonant, obligatory in most languages
    Coda (κ)
        consonant, optional in some languages, highly restricted or prohibited in others

Rules
~~~~~

Also, for "ellipsis", /ps/ is not a legal internal coda in English. The /s/ can only occur as an appendix, e.g. the plural -s at the end of a word. So it should be e.lip.sis

http://www.glottopedia.org/index.php/Sonority_hierarchy

http://www.glottopedia.org/index.php/Maximal_Onset_Principle

Nasal
-----

Air flow goes through nose.

Examples: "n" in "nose", "m" in "may", "ŋ" in "funk".

"ŋ" is known as the letter "eng" and the technical name of the consonant is the "voiced velar nasal"

"voiced" in the above sentence refers to whether or not your vocal chords are active. Your voice chord doesn't vibrate with voiceless consonants, like "sh" "th" "p" "f". In contrast, notice the vibration in phonemes like "m" "r" "z".


=========
 Example
=========

Mister Sandman, bring me a dream
Make him the cutest that I've ever seen
Give him two lips like roses in clover
Then tell him that his lonesome nights are over

Mister Sandman, bring me a dream
Blood guts and gore, a nightmare machine


\...

Please turn on your magic beam
Mister Sandman bring me a dream

Fire burn attrocious bloodstream
Mister Sandman, bring me a dream
