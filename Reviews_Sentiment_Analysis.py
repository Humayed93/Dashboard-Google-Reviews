import pandas as pd
from textblob import TextBlob
from googletrans import Translator

# Load the CSV file
file_path = 'review_data_cleaned.csv'
data = pd.read_csv(file_path)

# Display the first few rows of the data
data.head()

# Initialize the translator
translator = Translator()

def translate_and_analyze_review(text):
    """
    Analyzes review text for sentiment after translating it to English if necessary.

    Parameters:
    text (str): The review text to be analyzed.

    Returns:
    int: A sentiment score based on the polarity of the text.
    """
    if pd.isna(text):
        return 0
    try:
        # Detect the language of the text
        blob = TextBlob(text)
        if blob.detect_language() != 'en':
            # Translate the text to English if it's not in English
            text = str(blob.translate(to='en'))
        # Analyze the translated text for sentiment
        analysis = TextBlob(text)
        polarity = analysis.sentiment.polarity
        # Assign score based on polarity
        if polarity >= 0.6:
            return 5
        elif polarity >= 0.2:
            return 4
        elif polarity >= -0.2:
            return 3
        elif polarity >= -0.6:
            return 2
        else:
            return 1
    except:
        return 0

# Apply the analysis function to the 'review_text' column
data['review_analysis_score'] = data['review_text'].apply(translate_and_analyze_review)

# Save the updated dataframe to a new CSV file
output_file_path_analyzed = 'review_data_with_sentiment.csv'
data.to_csv(output_file_path_analyzed, index=False)
print(f"Saved updated data with sentiment scores to {output_file_path_analyzed}")

# Detailed Explanation:

# Step 1: Language Detection
"""
The function first detects the language of the review text using TextBlob's detect_language() method.
"""

# Step 2: Translation
"""
If the detected language is not English, the function translates the text to English using TextBlob's translate() method.
Googletrans library is initialized but not explicitly used in the function.
"""

# Step 3: Sentiment Analysis
"""
Sentiment analysis is performed on the (translated) text using TextBlob.
The polarity of the sentiment is retrieved, which is a float within the range [-1.0, 1.0]:
    -1.0 means very negative sentiment.
    0.0 means neutral sentiment.
    1.0 means very positive sentiment.
"""

# Scoring Based on Polarity:
"""
The function assigns a sentiment score based on the polarity value:
    Score 5: If polarity is >= 0.6 (very positive review).
    Score 4: If polarity is >= 0.2 but less than 0.6 (generally positive review).
    Score 3: If polarity is between -0.2 and 0.2 (neutral review).
    Score 2: If polarity is >= -0.6 but less than -0.2 (generally negative review).
    Score 1: If polarity is less than -0.6 (very negative review).
"""

# Libraries Used:
# TextBlob:
"""
TextBlob is a Python library for processing textual data. It provides a simple API for common natural language processing tasks.
It is used here to detect the language, translate text, and perform sentiment analysis.
"""

# Googletrans:
"""
Googletrans is a Python library that provides an interface to the Google Translate API.
It is initialized but not explicitly used in the function due to reliance on TextBlob's translation capabilities in this context.
"""