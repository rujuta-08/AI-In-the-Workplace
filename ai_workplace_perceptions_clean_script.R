#Required packages
library(rvest)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(topicmodels)
library(tm)
library(reshape2)

#Function to Scrape a Single URL
scrape_website <- function(url) {
  tryCatch({
    # Read the HTML content of the page
    page <- read_html(url)
    
    # Extract the title (try different selectors)
    title <- page %>% html_node("title") %>% html_text() %>% trimws()
    if (is.null(title) || title == "") {
      title <- page %>% html_node("h1") %>% html_text() %>% trimws()
      if (is.null(title)) title <- NA_character_
    }
    
    
    # Extract all text content (clean up whitespace)
    all_text <- page %>%
      html_nodes("p, h1, h2, h3, h4, h5, h6, li, span, div") %>% # Include more tag types
      html_text() %>%
      trimws() %>%
      paste(collapse = " ") %>%
      gsub("\\s+", " ", .) # Replace multiple spaces with single space
    
    # Extract publication date if possible (using a robust approach)
    pub_date <- NA_character_
    date_patterns <- c(
      "%Y-%m-%d",# YYYY-MM-DD
      "%B %d, %Y",# March 20, 2024
      "%d %B %Y",# 20 March 2024
      "%m/%d/%Y",# 03/20/2024
      "%b %d, %Y", # Use to handle "Mar 20, 2025"
      "%d %b %Y"
    )
    
    # Look for meta tags first
    date_meta <- page %>% html_node("meta[property='article:published_time']") %>% html_attr("content")
    if (!is.na(date_meta)) {
      parsed_date <- lubridate::parse_date_time(date_meta, orders = date_patterns, quiet = TRUE)
      if(!is.na(parsed_date)) {
        pub_date <- as.character(parsed_date)
        
      }
    }
    if (is.na(pub_date)){
      date_meta <- page %>% html_node("meta[name='date']") %>% html_attr("content")
      if (!is.na(date_meta)) {
        parsed_date <- lubridate::parse_date_time(date_meta, orders = date_patterns, quiet = TRUE)
        if(!is.na(parsed_date)) {
          pub_date <- as.character(parsed_date)
        }
      }
    }
    if (is.na(pub_date)){
      date_meta <- page %>% html_node("meta[name='last-modified']") %>% html_attr("content")
      if (!is.na(date_meta)) {
        parsed_date <- lubridate::parse_date_time(date_meta, orders = date_patterns, quiet = TRUE)
        if(!is.na(parsed_date)) {
          pub_date <- as.character(parsed_date)
        }
      }
    }
    
    #If not in meta tags, search within the text
    if (is.na(pub_date)) {
      # Regular expression to find potential date strings
      date_matches <- str_extract_all(all_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}\\b|\\b\\d{4}-\\d{2}-\\d{2}\\b|\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")[[1]]
      
      for (match in date_matches) {
        
        parsed_date <- lubridate::parse_date_time(match, orders = date_patterns, quiet=TRUE)
        
        if (!is.na(parsed_date)) {
          pub_date <- as.character(parsed_date)
          break # Stop after finding the first valid date
        }
      }
    }
    
    
    
    # Return a data frame
    return(data.frame(
      url = url,
      title = title,
      publication_date = pub_date,
      content = all_text,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    message(paste("Error scraping", url, ":", e$message))
    return(data.frame(
      url = url,
      title = NA_character_,
      publication_date = NA_character_,
      content = NA_character_,
      stringsAsFactors = FALSE
    ))
  })
}


#List of URLs
urls <- c(
  "https://www.axios.com/2025/03/18/enterprise-ai-tension-workers-execs",
  "https://www.mpg.de/24215804/0220-bild-how-do-people-feel-about-ai-replacing-human-jobs-149835-x",
  "https://www.shrm.org/enterprise-solutions/insights/tailor-ai-adoption-strategies-to-meet-workforce-needs",
  "https://idahocapitalsun.com/2024/09/03/americans-perception-of-ai-is-generally-negative-though-they-see-beneficial-applications/",
  "https://www.business-reporter.co.uk/ai--automation/strengthening-employee-perceptions-of-ai",
  "https://news.sap.com/2024/10/research-shows-mixed-attitudes-ai-at-work/",
  "https://www.fastcompany.com/91213583/ai-workplace-replace-people-slack-researcher",
  "http://www.mi-3.com.au/04-02-2025/qualtrics-study-reveals-ai-adoption-divide-between-workers-and-bosses",
  "https://www.spiceworks.com/tech/artificial-intelligence/articles/ai-adoption-shaping-futur,e-workplaces-aberdeen-data/",
  "https://newsroom.accenture.com/news/2024/accenture-report-finds-perception-gap-between-workers-and-c-suite-around-work-and-generative-ai",
  "https://www.cnbc.com/2024/06/07/some-workers-are-worried-using-ai-will-make-them-look-lazy-survey.html",
  "https://www.peoplemanagement.co.uk/article/1882131/people-think-ai-will-human-jobs-%E2%80%93-not-theirs-research-reveals",
  "https://timesofindia.indiatimes.com/india/iim-a-study-indian-workforce-caught-between-ai-optimism-and-job-security-anxiety/articleshow/112743165.cms",
  "https://www.mckinsey.com/capabilities/mckinsey-digital/our-insights/superagency-in-the-workplace-empowering-people-to-unlock-ais-full-potential-at-work",
  "https://www.apa.org/topics/healthy-workplaces/artificial-intelligence-workplace-worry",
  "https://www.bcg.com/publications/2023/what-people-are-saying-about-ai-at-work",
  "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2023.1128945/full",
  "https://www.nature.com/articles/s41599-024-03265-1",
  "https://www.pewresearch.org/short-reads/2023/11/21/what-the-data-says-about-americans-views-of-artificial-intelligence/",
  "https://www.hungarianconservative.com/articles/current/survey-hungarian-opinion-ai-workforce-positive-outlook/",
  "https://www.hrdive.com/news/workers-overconfident-AI/728554/",
  "https://www.pewresearch.org/internet/2023/04/20/ai-in-hiring-and-evaluating-workers-what-americans-think/",
  "https://www.nature.com/articles/s41599-024-04139-2",
  "https://news.gallup.com/poll/654905/americans-everyday-products-without-realizing.aspx",
  "https://www.aarp.org/pri/topics/work-finances-retirement/employers-workforce/workforce-trends-older-adults-artificial-intelligence/",
  "https://www.technologyreview.com/2023/07/25/1076532/generative-ai-is-empowering-the-digital-workforce/",
  "https://www.sacbee.com/news/business/article289188759.html",
  "https://business.yougov.com/content/50484-has-public-perception-of-generative-ai-shifted",
  "https://thepienews.com/ai-is-here-to-stay-but-do-we-know-it/",
  "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2024.1359164/full",
  "https://news.stonybrook.edu/pending/alda-center-pending/socjs-li-publishes-research-on-perceptions-of-ai-refugees-in-social-media/",
  "https://www.nature.com/articles/s41599-024-02625-1",
  "https://hrexecutive.com/hrs-role-in-empowering-women-to-thrive-in-an-ai-driven-era/",
  "https://journals.aom.org/doi/10.5465/amd.2022.0115",
  "https://campustechnology.com/articles/2023/09/27/university-of-phoenix-survey-explores-adults-perceptions-of-ai-for-use-in-work-and-education.aspx",
  "https://www.ey.com/en_gl/about-us/corporate-responsibility/how-can-we-upskill-gen-z-as-fast-as-we-train-ai",
  "https://www.mckinsey.com/industries/healthcare/our-insights/the-pulse-of-nurses-perspectives-on-ai-in-healthcare-delivery",
  "https://www.statista.com/chart/30443/share-of-employers-that-have-consulted-workers-on-ai/",
  "https://www.nature.com/articles/s41599-023-02079-x",
  "https://fortune.com/europe/2024/07/16/bosses-employees-different-expectations-about-how-much-time-save-ai-productivity/",
  "https://finance.yahoo.com/news/research-finds-65-generation-z-140000374.html",
  "https://theconversation.com/ai-in-hr-are-you-cool-with-being-recruited-by-a-robot-our-studies-reveal-job-candidates-true-feelings-219354",
  "https://www.peoplemattersglobal.com/article/technology/genai-saves-time-but-will-it-save-jobs-41855",
  "https://fortune.com/2023/09/01/ai-artificial-intelligence-employees-professional-identity/",
  "https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2022.999308/full",
  "https://www.technologydecisions.com.au/content/it-management/news/employees-and-leaders-not-seeing-eye-to-eye-on-ai-1006398018",
  "https://www.koreatimes.co.kr/www/nation/2025/03/113_373800.html",
  "https://itbrief.com.au/story/study-reveals-stark-discrepancy-in-perceived-ai-use-at-work",
  "https://www.cipd.org/en/knowledge/bitesize-research/fairness-ai-recruitment/",
  "https://www.deloitte.com/uk/en/about/press-room/more-than-four-million-people-in-the-uk-have-used-generative-ai-for-work-deloitte.html",
  "https://www.straitstimes.com/singapore/focus-on-the-benefits-of-ai-in-workplace-not-job-displacement-say-industry-experts",
  "https://www.hrmagazine.co.uk/content/news/hr-has-a-place-in-the-driver-s-seat-of-workplace-automation/",
  "https://www.axios.com/2025/03/18/enterprise-ai-tension-workers-execs",
  "https://www.business-reporter.co.uk/ai--automation/strengthening-employee-perceptions-of-ai",
  "https://www.shrm.org/enterprise-solutions/insights/tailor-ai-adoption-strategies-to-meet-workforce-needs",
  "https://www.nature.com/articles/s41599-024-03265-1",
  "https://www.hrdive.com/news/workers-overconfident-AI/728554/",
  "http://www.mi-3.com.au/04-02-2025/qualtrics-study-reveals-ai-adoption-divide-between-workers-and-bosses",
  "https://www.thehindu.com/news/cities/bangalore/ai-based-virtual-assistant-positively-impacts-employee-engagement-and-perceptions-of-fairness-finds-study/article68694596.ece",
  "https://www.raconteur.net/technology/qa-overhyped-or-underused-why-ai-is-no-longer-optional",
  "https://sg.news.yahoo.com/workers-singapore-hide-ai-fear-010213894.html",
  "https://www.cnbc.com/2024/06/07/some-workers-are-worried-using-ai-will-make-them-look-lazy-survey.html",
  "https://www.nature.com/articles/s41599-024-04139-2",
  "https://timesofindia.indiatimes.com/india/iim-a-study-indian-workforce-caught-between-ai-optimism-and-job-security-anxiety/articleshow/112743165.cms",
  "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2024.1359164/full",
  "https://www.nature.com/articles/s41599-024-03842-4",
  "https://www.researchgate.net/publication/382000437_Ethical_Considerations_of_Artificial_Intelligence_in_Human_Resources_Impact_on_Job_Performance_and_Organizational_Culture",
  "https://www.business-reporter.co.uk/human-resources/upskilling-the-uk-for-artificial-intelligence",
  "https://phys.org/news/2025-01-employee-ai-linked.html",
  "https://www.prnewswire.com/news-releases/new-ey-research-reveals-the-majority-of-us-employees-feel-ai-anxiety-amid-explosive-adoption-302002508.html",
  "https://www.bwpeople.in/article/indias-evolving-landscape-of-employment-ai-in-the-country-537747",
  "https://www.weforum.org/stories/2025/01/rebuilding-trust-ai-intelligent-age/",
  "https://www.businessrecord.com/survey-ais-increasing-prevalence-employee-well-being-poised-to-drive-business-in-2025/",
  "https://www.nature.com/articles/s41599-023-01843-3",
  "https://www.frontiersin.org/journals/computer-science/articles/10.3389/fcomp.2023.1113903/full",
  "https://www.pewresearch.org/internet/2023/04/20/ai-in-hiring-and-evaluating-workers-what-americans-think/",
  "https://www.pymnts.com/artificial-intelligence-2/2025/82-of-us-workforce-believes-genai-boosts-productivity/",
  "https://www.technologyreview.com/2023/07/25/1076532/generative-ai-is-empowering-the-digital-workforce/",
  "https://hrexecutive.com/is-hr-doing-enough-to-invest-in-tech-centered-learning/",
  "https://betakit.com/workers-are-embracing-ai-faster-than-employers-can-keep-up/",
  "https://www.mckinsey.com/capabilities/people-and-organizational-performance/our-insights/the-human-side-of-generative-ai-creating-a-path-to-productivity",
  "https://www.forbes.com/sites/paolacecchi-dimeglio/2024/05/28/how-leaders-can-enhance-employee-ai-usage/",
  "https://journals.aom.org/doi/10.5465/amd.2022.0115",
  "https://fortune.com/2023/09/01/ai-artificial-intelligence-employees-professional-identity/",
  "https://www.statista.com/chart/30443/share-of-employers-that-have-consulted-workers-on-ai/",
  "https://www.hrmorning.com/articles/train-employees-for-ai-driven-future/",
  "https://www.hcamag.com/ca/specialization/hr-technology/ais-role-in-elevating-employee-experience/492071",
  "https://www.peoplemattersglobal.com/article/technology/genai-saves-time-but-will-it-save-jobs-41855",
  "https://www.business-reporter.co.uk/management/keeping-gen-z-happy-at-work",
  "https://www.forbes.com/councils/forbestechcouncil/2023/11/21/18-technology-trends-that-may-soon-revolutionize-hr-management/",
  "https://www.nationthailand.com/business/corporate/40032674",
  "https://www.unite.ai/could-robot-phobia-worsen-the-hospitality-industrys-labor-shortage/",
  "https://channellife.com.au/story/ai-adoption-trust-gap-between-employees-leaders-grows",
  "https://www.peoplemanagement.co.uk/article/1882131/people-think-ai-will-human-jobs-%E2%80%93-not-theirs-research-reveals",
  "https://www.business-reporter.co.uk/technology/the-eu-ai-act-a-new-global-standard-for-artificial-intelligence",
  "https://www.researchgate.net/publication/384967774_A_STUDY_ON_EMPLOYEE'S_PERCEPTION_OF_HR_PRACTICES_IN_THE_INFORMATION_TECHNOLOGY_IT_INDUSTRY_WITH_SPECIAL_REFERENCE_TO_CHENNAI_CITY",
  "https://www.business-reporter.co.uk/ai--automation/optimistic-about-ai",
  "https://www.koreatimes.co.kr/www/nation/2025/03/113_373800.html",
  "https://www.business-reporter.co.uk/management/do-you-feel-like-you-belong-at-work-heres-why-its-so-important-for-your-health-happiness-and-productivity",
  "https://www.business-reporter.co.uk/ai--automation/leveraging-spreadsheets-with-ai",
  "https://www.business-reporter.co.uk/news/micron-rises-13-as-strong-forecast-impresses-ai-hungry-investors-11409",
  "https://venturebeat.com/ai/ai-weekly-lamdas-sentient-ai-triggers-memories-of-ibm-watson/",
  "https://www.business-reporter.co.uk/responsible-business/making-ai-sustainable-with-digital-twins",
  "https://www.business-reporter.co.uk/ai--automation/leveraging-ai-for-effective-business-planning",
  "https://www.frontiersin.org/journals/artificial-intelligence/articles/10.3389/frai.2020.578983/full",
  "https://www.business-reporter.co.uk/ai--automation/a-failure-to-lead-the-uks-ai-sector",
  "https://www.business-reporter.co.uk/news/safran-buys-ai-firm-preligens-for-220-million-euros-11093",
  "https://timestech.in/how-automation-and-ai-in-itsm-are-changing-customer-and-employee/",
  "https://www.gov.uk/government/publications/public-attitudes-to-data-and-ai-tracker-survey-wave-3/public-attitudes-to-data-and-ai-tracker-survey-wave-3",
  "https://www.business-reporter.co.uk/technology/trial-and-error-the-human-flaws-in-machine-learning",
  "https://www.hrmagazine.co.uk/content/news/workers-fear-losing-their-jobs-to-ai/"
)

#Scrape All URLs and combine into a single dataframe
all_data <- map_df(urls, scrape_website)

#Data Cleaning
all_data <- all_data %>%
  filter(!is.na(content)) %>% # Remove rows with missing content
  distinct(url, .keep_all = TRUE) # Remove duplicate URLs

#Output to CSV
write_csv(all_data, "ai_workplace_perceptions.csv")

print("Scraping and data export complete!")


#---Qualitative analyses---

# Load Data
data <- read.csv("ai_workplace_perceptions.csv", stringsAsFactors = FALSE)

# Data Cleaning
data_clean <- data %>%
  mutate(content = as.character(content),   # Convert to character
         content = str_to_lower(content),   # Convert to lowercase
         content = str_replace_all(content, "[^[:alnum:] ]", " "),  # Remove punctuation
         content = str_squish(content))  # Remove extra whitespace

# Tokenization
tokens <- data_clean %>%
  unnest_tokens(word, content)

# Remove Stop Words
data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Word Frequency Analysis
word_freq <- tokens_clean %>%
  count(word, sort = TRUE)

# View Most Common Words
print(head(word_freq, 20))

# Sentiment Analysis (Using Bing Lexicon)
bing <- get_sentiments("bing")

sentiment_counts <- tokens_clean %>%
  inner_join(bing, by = "word") %>%
  count(sentiment, sort = TRUE)

# Plot Sentiment Counts
ggplot(sentiment_counts, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Sentiment Analysis of AI Perceptions in the Workplace")

# Sentiment per URL
sentiment_by_url <- tokens_clean %>%
  inner_join(bing, by = "word") %>%
  count(url, sentiment)

ggplot(sentiment_by_url, aes(x = url, y = n, fill = sentiment)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Sentiment Distribution per Article")

# Creating a more accurate word cloud.

# Load Bing sentiment lexicon
bing <- get_sentiments("bing")

# Tokenize the text data
tokens <- data_clean %>%
  unnest_tokens(word, content)

# Perform sentiment analysis
sentiment_counts <- tokens %>%
  inner_join(bing, by = "word") %>%
  count(word, sentiment, sort = TRUE)

# Reshape data for word cloud (words vs. sentiment)
sentiment_matrix <- sentiment_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)

# Generate the sentiment-based word cloud
comparison.cloud(sentiment_matrix, 
                 colors = c("#F8766D", "#00BFC4"),  # Red for negative, Blue for positive
                 max.words = 100)

