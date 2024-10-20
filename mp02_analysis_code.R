library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(ggplot2)
library(plotly)
# Import data
get_imdb_file <- function(fname) {
  BASE_URL <- "https://datasets.imdbws.com/"
  fname_ext <- paste0(fname, ".tsv.gz")
  if (!file.exists(fname_ext)) {
    FILE_URL <- paste0(BASE_URL, fname_ext)
    download.file(FILE_URL,
      destfile = fname_ext
    )
  }
  as.data.frame(readr::read_tsv(fname_ext, lazy = FALSE))
}

# Sort data into tables
NAME_BASICS <- get_imdb_file("name.basics")

TITLE_BASICS <- get_imdb_file("title.basics")

TITLE_EPISODES <- get_imdb_file("title.episode")

TITLE_RATINGS <- get_imdb_file("title.ratings")

TITLE_CREW <- get_imdb_file("title.crew")

TITLE_PRINCIPALS <- get_imdb_file("title.principals")

# Sub-sampling data
NAME_BASICS <- NAME_BASICS |>
  filter(str_count(knownForTitles, ",") > 1)

# Graph that shows that majority of IMDB titles have less than 100 ratings
TITLE_RATINGS |>
  ggplot(aes(x = numVotes)) +
  geom_histogram(bins = 30) +
  xlab("Number of IMDB Ratings") +
  ylab("Number of Titles") +
  ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") +
  labs(caption = "Figure 1: IMDb Ratings Distribution") +
  theme_bw() +
  scale_x_log10(label = scales::comma) +
  scale_y_continuous(label = scales::comma)

# Shows majority of the data is skewed
TITLE_RATINGS |>
  pull(numVotes) |>
  quantile()

# Filter only for titles that have 100+ ratings
TITLE_RATINGS <- TITLE_RATINGS |>
  filter(numVotes >= 100)

# Filter the same requirement on the other TITLE tables
TITLE_BASICS <- TITLE_BASICS |>
  semi_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  )

TITLE_CREW <- TITLE_CREW |>
  semi_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  )

TITLE_EPISODES_1 <- TITLE_EPISODES |>
  semi_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  )
TITLE_EPISODES_2 <- TITLE_EPISODES |>
  semi_join(
    TITLE_RATINGS,
    join_by(parentTconst == tconst)
  )

TITLE_EPISODES <- bind_rows(
  TITLE_EPISODES_1,
  TITLE_EPISODES_2
) |>
  distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  semi_join(TITLE_RATINGS, join_by(tconst == tconst))


rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)

# Clean the NAMES_BASICS table, replace missing string values to numeric NA values
NAME_BASICS <- NAME_BASICS |>
  mutate(
    birthYear = as.numeric(birthYear),
    deathYear = as.numeric(deathYear)
  )

# Task 1: Correct the column types of the TITLE tables using a combination of mutate and the coercion functions as.numeric and as.logical.

# TITLE_BASICS has 4 column types to correct, isAdult, startYear, endYear and runtimeMinutes
TITLE_BASICS <- TITLE_BASICS |>
  mutate(
    isAdult = as.logical(isAdult),
    startYear = as.numeric(startYear),
    endYear = as.numeric(endYear),
    runtimeMinutes = as.numeric(runtimeMinutes)
  )

# TITLE_EPISODES has 2 column types to correct, seasonNumber and episodeNumber
TITLE_EPISODES <- TITLE_EPISODES |>
  mutate(
    seasonNumber = as.numeric(seasonNumber),
    episodeNumber = as.numeric(episodeNumber)
  )

# TITLE_RATINGS has no columns to correct

# TITLE_CREW has the correct column types but I want to convert the \\N values to NA instead
TITLE_CREW <- TITLE_CREW |>
  mutate(
    directors = na_if(directors, "\\N"),
    writers = na_if(writers, "\\N")
  )

# TITLE_PRINCIPALS has the correct column types but I want to convert the \\N values to NA instead
TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
  mutate(
    job = na_if(job, "\\N"),
    characters = na_if(characters, "\\N")
  )

# Task 2: Instructor provided questions

# 1: How many movies are in our data set? How many TV series? How many TV episodes?
unique(TITLE_BASICS$titleType) # this will show us the types of media

count_types <- TITLE_BASICS |>
  filter(titleType %in% c("movie", "tvSeries", "tvEpisode")) |> # for this question I will only use movie, tvSeries, and tvEpisode
  group_by(titleType) |>
  summarise(count = n())

datatable(setNames(count_types, c("Type", "Total")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Number of movies, TV series, and TV episodes"
)

# 2: Who is the oldest living person in our data set?
oldest_living_person <- NAME_BASICS |>
  filter(
    is.na(deathYear), # NA in deathYear indicates person is living
    birthYear >= 1908
  ) |> # data is incomplete from deathYear, only considering people born after 1908 as there are incomplete entries (the oldest verified living person in the world as of Oct 2024 was born in 1908)
  arrange(birthYear) |> # order from oldest to youngest
  slice(1) # note that only the birth year is available, no months or dates so this may not be as accurate as I'd like
print(oldest_living_person$primaryName)

# 3: There is one TV Episode in this data set with a perfect 10/10 rating and 200,000 IMDb ratings. What is it? What series does it belong to?
# Find the episode with a perfect rating and over 200,000 ratings
perfect_episode <- TITLE_BASICS |>
  left_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  ) |>
  filter(
    titleType == "tvEpisode",
    averageRating == 10,
    numVotes >= 200000
  )

# Join back to TITLE_EPISODES and TITLE_BASICS to get the series name
perfect_episode_series <- perfect_episode |>
  left_join(
    TITLE_EPISODES,
    join_by(tconst == tconst)
  ) |>
  left_join(
    TITLE_BASICS,
    join_by(parentTconst == tconst)
  ) |>
  rename(
    episode_title = primaryTitle.x, # rename column to episode title
    series_title = primaryTitle.y
  ) |> # rename column to series title
  select(
    series_title,
    episode_title
  )
print(perfect_episode_series)

# 4: What four projects is the actor Mark Hamill most known for?
# Find the four projects Mark Hamill is known for first
mark_hamill <- NAME_BASICS |>
  filter(primaryName == "Mark Hamill") |>
  separate_longer_delim(knownForTitles, ",") |>
  select(knownForTitles)
# this gives us the IDs, so we need to make further joins to get the names of the projects

# Join results to TITLE_BASICS to get the name of the projects
mark_hamill_projects <- mark_hamill |>
  left_join(
    TITLE_BASICS,
    join_by(knownForTitles == tconst)
  ) |>
  select(primaryTitle)

datatable(setNames(mark_hamill_projects, c("Project Title")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "The 4 Projects Mark Hamill Is Most Known For"
)

# 5: What TV series, with more than 12 episodes, has the highest average rating?

# Find series with more than 12 episodes first
tv_series_12 <- TITLE_BASICS |>
  filter(titleType == "tvSeries") |> # Only want TV series
  left_join(
    TITLE_EPISODES,
    join_by(tconst == parentTconst)
  ) |> # Join with TITLE_EPISODES to count number of episodes
  group_by(tconst, primaryTitle) |> # Group by series to count episodes
  summarise(total_episodes = n()) |> # Count total number of episodes
  filter(total_episodes > 12)

# Join onto TABLE_RATINGS to sort by ratings, this will get the tvSeries average rating which may be different from the mean ratings of the episodes?
highest_rated_tv_series <- tv_series_12 |>
  left_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  ) |> # Join with TITLE_RATINGS to get average ratings
  ungroup() |>
  arrange(desc(averageRating)) |>
  select(tconst, primaryTitle, averageRating) |>
  slice_head(n=10)

datatable(setNames(highest_rated_tv_series, c("ID", "Title", "Average Rating")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Highest Rated TV Series (Series Ratings)"
)


# alternative, find the average ratings of the episodes for the tv series instead, i dont like this answer though since some episodes are rated higher/missing ratings but the tv series rating overall is lower
series_ratings_byEpisode <- TITLE_EPISODES |>
  semi_join(
    tv_series_12,
    join_by(parentTconst == tconst)
  ) |>
  left_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  ) |>
  group_by(parentTconst) |>
  summarise(avg_ratings = mean(averageRating)) |>
  arrange(desc(avg_ratings))

series_ratings_byEpisode <- series_ratings_byEpisode |>
  left_join(
    TITLE_BASICS,
    join_by(parentTconst == tconst)
  ) |>
  ungroup() |>
  select(parentTconst, primaryTitle, avg_ratings) |>
  slice_head(n=10)

datatable(setNames(series_ratings_byEpisode, c("ID", "Title", "Average Rating")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Highest Rated TV Series (Episode Ratings)"
)
# 6: The TV series Happy Days (1974-1984) gives us the common idiom “jump the shark”. The phrase comes from a controversial fifth season episode (aired in 1977) in which a lead character literally jumped over a shark on water skis. Idiomatically, it is used to refer to the moment when a once-great show becomes ridiculous and rapidly looses quality.
# Is it true that episodes from later seasons of Happy Days have lower average ratings than the early seasons?

# Get tconst for the series first so we can find the episode ratings after
happy_days_id <- TITLE_BASICS |>
  filter(
    primaryTitle == "Happy Days",
    titleType == "tvSeries",
    startYear == 1974,
    endYear == 1984
  ) |>
  select(tconst)
# There are 11 seasons, I will be splitting them between 1-6 and 7-11
early_happy_days_episode_ratings <- TITLE_EPISODES |>
  semi_join(
    happy_days_id,
    join_by(parentTconst == tconst)
  ) |>
  left_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  ) |>
  drop_na() |> # get rid of any ratings that are NA
  filter(seasonNumber < 7) |> # only care about seasons 1-6
  summarize(avg_rating = mean(averageRating))

datatable(setNames(early_happy_days_episode_ratings, c("Average Rating")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Happy Days Seasons 1-6 Average Rating"
) |>
  formatRound(columns = "Average Rating", digits = 2)

later_happy_days_episode_ratings <- TITLE_EPISODES |>
  semi_join(
    happy_days_id,
    join_by(parentTconst == tconst)
  ) |>
  left_join(
    TITLE_RATINGS,
    join_by(tconst == tconst)
  ) |>
  drop_na() |> # get rid of any ratings that are NA
  filter(seasonNumber >= 7) |> # only care about seasons 7-11
  summarize(avg_rating = mean(averageRating))

datatable(setNames(later_happy_days_episode_ratings, c("Average Rating")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Happy Days Seasons 7-11 Average Rating"
) |>
  formatRound(columns = "Average Rating", digits = 2)

# Task 3: Custom Success Metric
# Design a success measure for IMDb entries, reflecting both quality and broad popular awareness.
# Implement your success metric using a mutate operator to add a new column to the TITLE_RATINGS table

# measurement needs to incorporate both average rating value and number of voters

# the ratings will have some uncertainty due to the fact a movie can have high ratings but a low amount of votes
# to combat this, incorporating a Bayseian average can help mitigate the weight of movies that have lower amount of votes
# to make things simple, focus on movies first and lets find the average number of voters for movies and average movie rating in data set
avg_num_voters <- TITLE_RATINGS |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  filter(titleType == "movie") |>
  summarize(avg_votes = mean(numVotes))
print(avg_num_voters) # in this dataset, the average is 8691.414 but for simplicity lets round this to 8700

avg_movie_rating <- TITLE_RATINGS |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  filter(titleType == "movie") |>
  summarize(avg_rating = mean(averageRating))
print(avg_movie_rating) # returns about 5.9

# now that we have a baseline for the average amount of voters in the movies subset of data, I want to create a weighted average for the ratings
# the weighted rating will take into account the number of voters on a movie so that movies with high ratings but little voters will be less significant
# based on bayesian average, weighted_rating = [(averageRating * numVotes) / (numVotes + avg_num_voters)] + [avg_num_voters/(numVotes + avg_num_voters)]*avg_movie_rating
movie_ratings <- TITLE_RATINGS |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  filter(titleType == "movie") |>
  mutate(weighted_rating = ((averageRating * numVotes) / (numVotes + 8700)) + ((8700 * 5.9) / (numVotes + 8700))) |>
  select(tconst, primaryTitle, weighted_rating) |>
  arrange(desc(weighted_rating))

limited_movie_ratings <- movie_ratings[1:1000, ]  # limit to the first 1000 rows intending to save memory usage

datatable(setNames(limited_movie_ratings, c("ID", "Title", "Weighted Rating")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Movie Titles With Weighted Rating"
) |>
  formatRound(columns = "Weighted Rating", digits = 2)

# the influence of the number of voters needs to be taken into account as well, but to get a better idea why we will find the max and min number of votes on movies
max(
  TITLE_RATINGS |>
    left_join(TITLE_BASICS, join_by(tconst == tconst)) |>
    filter(titleType == "movie") |>
    pull(numVotes)
) # returns 2946100

min(
  TITLE_RATINGS |>
    left_join(TITLE_BASICS, join_by(tconst == tconst)) |>
    filter(titleType == "movie") |>
    pull(numVotes)
) # returns 100
# since there is such a large discrepancy, a logarithmic scale should be used to scale the number of votes down

popularity_scaling <- TITLE_RATINGS |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  filter(titleType == "movie") |>
  mutate(scaled_numVotes = log(numVotes + 1)) |>
  select(tconst, primaryTitle, scaled_numVotes) |>
  arrange(desc(scaled_numVotes))

limited_popularity_scaling <- popularity_scaling[1:1000, ]  # limit to the first 1000 rows intending to save memory usage

datatable(setNames(limited_popularity_scaling, c("ID", "Title", "Scaled Number of Votes")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Movie Titles With Scaled Votes"
) |>
  formatRound(columns = "Scaled Number of Votes", digits = 2)

# tying it together, I want to multiply the weighted_rating by the scaled_numVotes to measure success

TITLE_RATINGS_MOVIES <- TITLE_RATINGS |>
  left_join(
    movie_ratings,
    join_by(tconst == tconst)
  ) |>
  left_join(
    popularity_scaling,
    join_by(tconst == tconst)
  ) |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  mutate(success_score = weighted_rating * scaled_numVotes) |>
  select(tconst, primaryTitle.x, success_score, averageRating, numVotes, startYear) |>
  arrange(desc(success_score))

# 1: Choose the top 5-10 movies on your metric and confirm that they were indeed box office successes.
top10_TITLE_RATINGS_MOVIES <- TITLE_RATINGS_MOVIES[1:10, ]

datatable(setNames(top10_TITLE_RATINGS_MOVIES, c("ID", "Title", "Success Score", "Average IMDb Rating", "Number of Voters", "Release  Year")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Top 10 Movies With Success Score"
) |>
  formatRound(columns = "Success Score", digits = 2)

# 2: Choose 3-5 movies with large numbers of IMDb votes that score poorly on your success metric and confirm that they are indeed of low quality.
bad_movies <- TITLE_RATINGS_MOVIES |>
  arrange(success_score, desc(numVotes))

ten_bad_movies <- bad_movies[1:10, ]

datatable(setNames(ten_bad_movies, c("ID", "Title", "Success Score", "Average IMDb Rating", "Number of Voters", "Release  Year")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Bad Movies"
) |>
  formatRound(columns = "Success Score", digits = 2)

# 3: Choose a prestige actor or director and confirm that they have many projects with high scores on your success metric.
# choosing Christopher Nolan

NAME_BASICS |> filter(primaryName == "Christopher Nolan") # nm0634240

nolan_projects <- NAME_BASICS |>
  filter(nconst == "nm0634240") |>
  left_join(
    TITLE_PRINCIPALS,
    join_by(nconst == nconst)
  ) |>
  left_join(
    TITLE_RATINGS_MOVIES,
    join_by(tconst == tconst)
  ) |>
  select(tconst, primaryTitle.x, success_score) |>
  drop_na() |>
  distinct()

datatable(setNames(nolan_projects, c("ID", "Title", "Success Score")),
          options = list(pageLength = 11, autoWidth = TRUE),
          caption = "Christopher Nolan Projects"
) |>
  formatRound(columns = "Success Score", digits = 2)

# 4: Perform at least one other form of ‘spot check’ validation.
# I will check the success_score(s) of the last 5 Oscar winners: Oppenheimer, Everything Everywhere All at Once, CODA, Nomadland & Parasite
oscar_winners <- TITLE_RATINGS_MOVIES |>
  filter(
    (primaryTitle.x == "Oppenheimer" & startYear == 2023) |
      (primaryTitle.x == "Everything Everywhere All at Once" & startYear == 2022) |
      (primaryTitle.x == "CODA" & startYear == 2021) |
      (primaryTitle.x == "Nomadland" & startYear == 2020) |
      (primaryTitle.x == "Parasite" & startYear == 2019)
  )

datatable(setNames(oscar_winners, c("ID", "Title", "Success Score", "Average IMDb Rating", "Number of Voters", "Release Year")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Last 5 Oscar Winners"
) |>
  formatRound(columns = "Success Score", digits = 2)

# 5: Come up with a numerical threshold for a project to be a ‘success’; that is, determine a value V such that movies above V are all “solid” or better.
TITLE_RATINGS_MOVIES |>
  pull(success_score) |>
  quantile(na.rm = TRUE)
# the 75% quantile shows us that 75% of the films fall under 43.6 points, setting this as a benchmark for success

# plot of distribution, the shape makes sense as far as i am concerned, I think i would use this as extra analysis instead
ggplot(TITLE_RATINGS_MOVIES, aes(x = success_score)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white") +
  labs(title = "Distribution of Success Scores")


# Task 4: Trends in Success Over Time

# 1: What was the genre with the most successes in each decade?

# create new table with genres, decades with the success score
movie_genre_success <- TITLE_RATINGS_MOVIES |>
  left_join(
    TITLE_BASICS,
    join_by(tconst == tconst)
  ) |>
  filter(!is.na(genres) & !is.na(startYear.x)) |>
  separate_longer_delim(genres, ",") |>
  mutate(decade = floor(startYear.x / 10) * 10) |>
  select(tconst, primaryTitle.x, success_score, startYear.x, decade, genres)


decade_success <- movie_genre_success |>
  filter(success_score >= 43.6) |>
  group_by(decade, genres) |>
  summarize(
    total_movies = n(), # Count the total number of successful movies
    .groups = "drop"
  ) |>
  group_by(decade) |>
  slice_max(total_movies, n = 1) |>
  ungroup()

ggplot(decade_success, aes(x = factor(decade), y = total_movies, fill = genres)) +
  geom_bar(stat = "identity", position = "dodge") + # Grouped bars
  labs(
    title = "Top Genre per Decade",
    x = "Decade",
    y = "Number of Successful Movies",
    fill = "Genres"
  ) +
  theme_minimal() + # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for clarity


# 2: What genre consistently has the most “successes”? Answer: Drama
# What genre used to reliably produced “successes” and has fallen out of favor? Answer: Romance

# look at overall success of genre type
genre_success <- movie_genre_success |>
  filter(success_score >= 43.6) |>
  group_by(genres) |>
  summarize(total_movies = n()) |>
  arrange(desc(total_movies))
print(genre_success)

# look at success of genre type by decade again, but this time including 10 genres to gauge how it changes
decade_success_top10 <- movie_genre_success |>
  filter(success_score >= 43.6) |>
  group_by(decade, genres) |>
  summarize(
    total_movies = n(), # Count the total number of successful movies
    .groups = "drop"
  ) |>
  group_by(decade) |>
  slice_max(total_movies, n = 10, with_ties = FALSE) |>
  mutate(rank = dense_rank(desc(total_movies))) |>
  ungroup() |>
  arrange(decade, rank) # Sort by decade and rank

# visually plot
interactive_decade_success <- ggplot(decade_success_top10, aes(x = decade, y = total_movies, color = genres, group = genres)) +
  geom_line(linewidth = 1) + # Plot the lines
  geom_point(size = 3) + # Add points at each decade
  labs(
    title = "Top 10 Genres per Decade by Number of Successful Movies",
    x = "Decade",
    y = "Number of Successful Movies",
    color = "Genres"
  ) +
  scale_x_continuous(breaks = seq(min(decade_success_top10$decade), max(decade_success_top10$decade), by = 10)) + # Set breaks every 10 years
  theme_minimal() + # Use a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for clarity
    legend.position = "right"
  )
# Convert ggplot to interactive plot using plotly
ggplotly(interactive_decade_success)

# 3: What genre has produced the most “successes” since 2010? Does it have the highest success rate or does it only have a large number of successes because there are many productions in that genre?

# count number of successful projects since 2010 by genre
successes_since_2010 <- movie_genre_success |>
  filter(
    success_score >= 43.6,
    decade >= 2010
  ) |>
  group_by(genres) |>
  summarize(total_movies = n()) |>
  arrange(desc(total_movies))

# rename columns for data table display
colnames(successes_since_2010) <- c("Genre", "Total Successful Movies")

datatable(successes_since_2010,
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Number of Successful Movies by Genre Since 2010"
)

# count how many movies were made per genre since 2010
movie_genres_produced <- movie_genre_success |>
  filter(decade >= 2010) |>
  group_by(genres) |>
  summarize(total_movies_produced = n()) |>
  arrange(desc(total_movies_produced))

# rename columns for data table display
colnames(movie_genres_produced) <- c("Genre", "Total Movies Produced")

datatable(movie_genres_produced,
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Number of Movies Produced by Genre Since 2010"
)


genre_distribution_2010 <- left_join(successes_since_2010, movie_genres_produced, by = "Genre") |>
  mutate(`Unsuccessful Movies` = `Total Movies Produced` - `Total Successful Movies`) |>
  arrange(desc(`Total Movies Produced`)) |>
  slice_head(n = 10)

# Pivot the data for long format
genre_distribution_2010 <- genre_distribution_2010 |>
  pivot_longer(cols = c(`Unsuccessful Movies`, `Total Successful Movies`),
               names_to = "Type",
               values_to = "Count") |>
  mutate(Type = ifelse(Type == "Total Successful Movies", "Successful Movies", Type))

# Plot the stacked bar chart
ggplot(genre_distribution_2010, aes(x = reorder(Genre, -`Total Movies Produced`), y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Movies Produced and Successful Movies by Genre Since 2010",
    x = "Genre",
    y = "Number of Movies",
    fill = "Movie Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 4: What genre has become more popular in recent years? Answer: Action

# choose drama as genre of choice for movie project

top_10 <- TITLE_RATINGS_MOVIES |>
  left_join(TITLE_BASICS,
            join_by(tconst == tconst)) |>
  select(primaryTitle.x, genres) |>
  slice_head(n=10)

datatable(setNames(top_10, c("Title", "Genre(s)")),
          options = list(pageLength = 10, autoWidth = TRUE),
          caption = "Top 10 Successful Movies"
)

# Task 5: Key Personnel
# Identify (at least) two actors and one director who you will target as the key talent for your movie. Write a short “pitch” as to why they are likely to be successful. You should support your pitch with at least one graphic and one table.

# Christopher Nolan, Cillian Murphy, Paul Mescal
NAME_BASICS |> filter(primaryName == "Christopher Nolan") # nm0634240
NAME_BASICS |> filter(primaryName == "Cillian Murphy") # nm0614165 
NAME_BASICS |> filter(primaryName == "Paul Mescal") # nm8958770

# Look at Nolan & Murphy's history together

murphy_nolan_projects <- TITLE_PRINCIPALS |>
  filter(nconst %in% c("nm0634240", "nm0614165")) |>
  distinct(tconst, nconst) |>  # ensure only distinct person-title pairs
  group_by(tconst) |>
  summarize(duo_works = n()) |> # counts distinct personnel
  filter(duo_works == 2) # only keep titles where both are present

murphy_nolan_projects <- murphy_nolan_projects |>
  left_join(TITLE_RATINGS_MOVIES,
            join_by(tconst == tconst)) |>
  select(tconst, primaryTitle.x, success_score, startYear)

ggplot(murphy_nolan_projects, aes(x = reorder(primaryTitle.x, success_score), y = success_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar chart with success scores
  geom_hline(yintercept = 43.6, linetype = "dashed", color = "red", linewidth = 1) +  # horizontal line at y = 43.6 represents solid score
  labs(
    title = "Success Scores of Murphy and Nolan Projects",
    x = "Project Title",
    y = "Success Score"
  ) +
  theme_minimal()

# Look at Mescal's works in dramas

mescal <- NAME_BASICS |>
  filter(nconst == "nm8958770") |>
  separate_longer_delim(knownForTitles, ",") |>
  select(knownForTitles)

mescal <- mescal |>
  left_join(
    TITLE_BASICS,
    join_by(knownForTitles == tconst)
  ) |>
  select(primaryTitle, genres)

datatable(setNames(mescal, c("Project Title", "Genre(s)")),
  options = list(pageLength = 10, autoWidth = TRUE),
  caption = "Paul Mescal Known Works"
)
# Task 6: Finding a Classic Movie to Remake
# Metropolis

#show metropolis has no remakes and its imdb rating and number of votes.
metropolis <- TITLE_BASICS |>
  filter(primaryTitle == "Metropolis",
         titleType == "movie")
print(metropolis)

metropolis <- TITLE_BASICS |>
  filter(primaryTitle == "Metropolis",
         titleType == "movie",
         startYear == 1927) |>
  left_join(TITLE_RATINGS,
            join_by(tconst == tconst)) |>
  select(primaryTitle, startYear, genres, averageRating, numVotes)

datatable(setNames(metropolis, c("Project Title", "Release Year", "Genre(s)", "Average IMDb Rating", "Number of Votes")),
          caption = "Metropolis"
)

# check if any key personnel are still alive: Fritz Lang, Thea von Harbou, Brigitte Helm, Alfred Abel, Gustav Fröhlich

metropolis_personnel <- TITLE_PRINCIPALS |>
  filter(tconst == "tt0017136") |>
  left_join(NAME_BASICS, join_by(nconst == nconst)) |>
  select(primaryName, birthYear, deathYear) |>
  distinct() |>
  filter(primaryName == "Fritz Lang" | primaryName == "Thea von Harbou" | primaryName == "Brigitte Helm" | primaryName == "Alfred Abel" | primaryName == "Gustav Fröhlich")

datatable(setNames(metropolis_personnel, c("Name", "Birth Year", "Death Year")),
  caption = "Metropolis Key Personnel"
)





#############

successes_since_2010 <- movie_genre_success |>
  filter(
    success_score >= 43.6,
    decade >= 2010
  ) |>
  group_by(genres) |>
  summarize(total_movies = n()) |>
  arrange(desc(total_movies))

# rename columns for data table display
colnames(successes_since_2010) <- c("Genre", "Total Successful Movies")

datatable(successes_since_2010,
          options = list(pageLength = 10, autoWidth = TRUE),
          caption = "Number of Successful Movies by Genre Since 2010"
)


# count how many movies were made per genre since 2010
movie_genres_produced <- movie_genre_success |>
  filter(decade >= 2010) |>
  group_by(genres) |>
  summarize(total_movies_produced = n()) |>
  arrange(desc(total_movies_produced))

# rename columns for data table display
colnames(movie_genres_produced) <- c("Genre", "Total Movies Produced")

datatable(movie_genres_produced,
          options = list(pageLength = 10, autoWidth = TRUE),
          caption = "Number of Movies Produced by Genre Since 2010"
)






