library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)

# read and preprocess data (columns hard coded as CSVs have no header row)
eqinfo <- read_csv("SAFEEqualiserUserData.csv", col_names=F) %>%
  mutate(word=str_to_lower(X2), genre=str_to_lower(X19), instrument=str_to_lower(X20)) %>% 
  select(word, genre, instrument) %>%
  mutate(source="EQ")

compinfo <- read_csv("SAFECompressorUserData.csv", col_names=F) %>%
  mutate(word=str_to_lower(X2), genre=str_to_lower(X12), instrument=str_to_lower(X13)) %>% 
  select(word, genre, instrument) %>%
  mutate(source="Compressor")

distinfo <- read_csv("SAFEDistortionUserData.csv", col_names=F) %>%
  mutate(word=str_to_lower(X2), genre=str_to_lower(X11), instrument=str_to_lower(X12)) %>% 
  select(word, genre, instrument) %>%
  mutate(source="Distortion")

verbinfo <- read_csv("SAFEReverbUserData.csv", col_names=F) %>%
  mutate(word=str_to_lower(X2), genre=str_to_lower(X15), instrument=str_to_lower(X16)) %>% 
  select(word, genre, instrument) %>%
  mutate(source="Reverb")

# stick them all together
allinfo <- rbind(eqinfo, compinfo, verbinfo, distinfo)

# helper function to do naive word substitiuion (to get a usable number of instrument groups)
squashInstruments <- function(words)
{
  f <- factor(words)
  lev <- levels(f)
  
  lev[grepl('drum', lev)] <- "Drums"
  lev[grepl('snar', lev)] <- "Drums"
  lev[grepl('hat', lev)] <- "Drums"
  lev[grepl('clap', lev)] <- "Drums"
  lev[grepl('kick', lev)] <- "Drums"
  lev[grepl('kcik', lev)] <- "Drums"
  lev[grepl('kik', lev)] <- "Drums"
  lev[grepl('kit', lev)] <- "Drums"
  lev[grepl('overhead', lev)] <- "Drums"
  lev[grepl('oh mono', lev)] <- "Drums"
  lev[grepl('base', lev)] <- "Drums"
  
  lev[grepl('keyboard', lev)] <- "Keyboard"
  lev[grepl('piano', lev)] <- "Keyboard"
  lev[grepl('organ', lev)] <- "Keyboard"
  lev[grepl('key', lev)] <- "Keyboard"
  lev[grepl('synth', lev)] <- "Keyboard"
  lev[grepl('cimbalom', lev)] <- "Keyboard"
  lev[grepl('glocken', lev)] <- "Keyboard"
  
  lev[grepl('bass', lev)] <- "Bass"
  
  lev[grepl('guitar', lev)] <- "Guitar"
  lev[grepl('string', lev)] <- "Guitar"
  lev[grepl('acoustic', lev)] <- "Guitar"
  lev[grepl('gtr', lev)] <- "Guitar"
  lev[grepl('telecaster', lev)] <- "Guitar"
  lev[grepl('electric', lev)] <- "Guitar"
  lev[grepl('tanglewood', lev)] <- "Guitar"
  lev[grepl('charango', lev)] <- "Guitar"
  
  lev[grepl('vocal', lev)] <- "Vocal"
  lev[grepl('voice', lev)] <- "Vocal"
  lev[grepl('vox', lev)] <- "Vocal"
  lev[grepl('singing', lev)] <- "Vocal"
  
  lev[grepl('trumpet', lev)] <- "Brass"
  lev[grepl('sax', lev)] <- "Brass"
  lev[grepl('sav', lev)] <- "Brass"
  
  lev[grepl('master', lev)] <- "Master / Aux"
  lev[grepl('main', lev)] <- "Master / Aux"
  lev[grepl('mix', lev)] <- "Master / Aux"
  lev[grepl('aux', lev)] <- "Master / Aux"
  lev[grepl('bus', lev)] <- "Master / Aux"
  
  # these appear a lot, but I don't know what to do with them. Setting them to NA
  # drops them from our analysis... not sure whether that's a good thing or not!
  lev[grepl('metal', lev)] <- NA # eg. metal3
  lev[grepl('jazz', lev)] <- NA  # eg. jazz5
  
  # finally, splat everything we haven't touched (ie. everything without a capital
  # letter) into a new group called "Other".
  lev[!str_detect(lev, "[A-Z]")] <- "Other"
 
  levels(f) <- lev
  return (f)
}

squashDescriptions <- function(words)
{
  f <- factor(words)
  lev <- levels(f)
  
  lev[grepl('bass', lev)] <- "Bass"
  lev[grepl('warm', lev)] <- "Warm"
  lev[grepl('bright', lev)] <- "Bright"
  lev[grepl('dark', lev)] <- "Dark"
  lev[grepl('punch', lev)] <- "Punch"
  lev[grepl('crunch', lev)] <- "Crunch"
  lev[grepl('crush', lev)] <- "Crush"
  lev[grepl('thick', lev)] <- "Thick"
  lev[grepl('fuzz', lev)] <- "Fuzz"
  lev[grepl('thin', lev)] <- "Thin"
  lev[grepl('smooth', lev)] <- "Smooth"
  lev[grepl('clear', lev)] <- "Clear"
  lev[grepl('air', lev)] <- "Air"
  lev[grepl('boom', lev)] <- "Boom"
  lev[grepl('squash', lev)] <- "Squash"
  lev[grepl('tight', lev)] <- "Tight"
  lev[grepl('echo', lev)] <- "Echo"
  lev[grepl('hall', lev)] <- "Hall"
  lev[grepl('distan', lev)] <- "Distant"
  lev[grepl('room', lev)] <- "Room"
  lev[grepl('massiv', lev)] <- "Massive"
  lev[grepl('huge', lev)] <- "Huge"
  lev[grepl('big', lev)] <- "Big"
  lev[grepl('deep', lev)] <- "Deep"
  lev[grepl('destroy', lev)] <- "Destroy"
  lev[grepl('crisp', lev)] <- "Crisp"
  lev[grepl('fizz', lev)] <- "Fizz"
  lev[grepl('fat', lev)] <- "Fat"
  lev[grepl('sharp', lev)] <- "Sharp"
  lev[grepl('wide', lev)] <- "Wide"
  lev[grepl('pop', lev)] <- "Pop"
  lev[grepl('vocal', lev)] <- "Vocal"
  lev[grepl('voice', lev)] <- "Vocal"
  lev[grepl('vox', lev)] <- "Vocal"
  lev[grepl('soft', lev)] <- "Soft"
  lev[grepl('harsh', lev)] <- "Harsh"
  lev[grepl('subtle', lev)] <- "Subtle"
  lev[grepl('effective', lev)] <- "Effective"
  
  # as for the jazzX and metalX instruments, I don't kwow what to do with these,
  # so I just drop them and shrug.
  lev[grepl('sofa1', lev)] <- NA
  lev[grepl('re27', lev)] <- NA
  
  levels(f) <- lev
  return (f)
}

# now build tokens from all of our descriptions
tokenised <- allinfo %>% 
  # to tokenise on something else, just pick a different column (eg. genre)
  rename(comments=word) %>%
  unnest_tokens(word, comments) %>%
  # get rid of common, unhelpful words?
  anti_join(stop_words) %>%
  # collapse related instruments into groups
  mutate(instrument=squashInstruments(instrument)) %>%
  # collapse related words (*after* tokenising)
  mutate(word=squashDescriptions(word)) %>%
  # treat them as factors
  mutate(instrument=fct_explicit_na(instrument), word=fct_explicit_na(word)) %>%
  # ignore test data (is this test data?)
  filter(word != "test")


# look at instrument breakdown by effect type
tokenised %>%
  filter(instrument != "(Missing)") %>%
  count(source, instrument) %>%
  # finally, plot it
  ggplot(aes(x=instrument, y=n, fill=source)) + 
    geom_col() + 
    coord_flip() +
    xlab('') + 
    ylab('number of uses') + 
    ggtitle('Effect processing by instrument')

# now let's do some plots!
# First, look at overall word popularity
tokenised %>%
  filter(word != "(Missing)") %>%
  count(word) %>%
  # change this to look at less popular words
  filter(n > 7) %>%
  # sorting factors in plots is a pain, we need these two lines:
  arrange(n) %>%
  mutate(word=factor(word, levels=unique(word))) %>% 
  # finally, plot it
  ggplot(aes(x=word, y=n, fill=word)) + 
  geom_col(show.legend=FALSE) + 
  coord_flip() +
  xlab('') + 
  ylab('number of uses') + 
  ggtitle('Most popular words used to decribe effect processing')

# how about all the words used to describe a particular effect?
tokenised %>%
  filter(source == "Compressor") %>%
  count(word) %>%
  filter(n > 3, word != "(Missing)") %>%
  arrange(n) %>%
  mutate(word=factor(word, levels=unique(word))) %>% 
  # plot it
  ggplot(aes(x=word, y=n, fill=word)) + 
  geom_col(show.legend=FALSE) + 
  coord_flip() + 
  xlab('') + 
  ylab('number of uses') + 
  ggtitle('Most popular words used to describe compressors')

# now let's see per-instrument words, eg. to describe drum processing, by effect type
tokenised %>%
  filter(instrument == "Drums") %>%
  count(source, word) %>%
  filter(n > 1) %>%
  # finally, plot it
  ggplot(aes(x=word, y=n, fill=source)) + 
    geom_col() + 
    coord_flip() +
    xlab('') + 
    ylab('number of uses') + 
    ggtitle('Words used to describe drum processing')

# this is the big ol' plot of everything
tokenised %>%
  filter(instrument != "(Missing)") %>%
  group_by(instrument) %>%
  count(word, source) %>%
  filter(n > 1) %>%
  top_n(10) %>% 
  # plot it
  ggplot(aes(x=word, y=n, fill=source)) + 
    geom_col() + 
    coord_flip() +
    xlab('') +
    ylab('') +
    ggtitle('Number of word uses, broken down by instrument and effect type') +
    facet_wrap(~instrument, scale="free")

# RStudio and comparison.cloud are frenemies
dev.off()

# But and this makes for a nice cloud (I had to drop Warm and Bright
# because they are so massively overused!)
tokenised %>% 
  filter(word != "Warm", word != "Bright", word != "test") %>%
  filter(word != "(Missing)") %>%
  # count words by type
  count(word, source) %>%
  # create a table suitable for passing to the comparison plot
  acast(word ~ source, value.var='n', fill=0) %>%
  # plot it!
  comparison.cloud(max.words=600, title.size=1)

