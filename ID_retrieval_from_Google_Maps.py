import requests
import json
import pandas as pd
import time

def get_places_info(api_key, location, radius, types):
    """
    Fetches place information from the Google Places API.

    Parameters:
    api_key (str): Your API key for accessing the Google Places API.
    location (str): The latitude and longitude of the location to search, in the format "lat,lng".
    radius (str): The radius of the search area, in meters.
    types (str): The types of places to search for, separated by a vertical bar '|'.

    Returns:
    list: A list of dictionaries containing information about the places.
    """
    places_info = []
    url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={location}&radius={radius}&type={types}&key={api_key}"
    
    while True:
        response = requests.get(url)
        res_json = json.loads(response.text)
        places = res_json.get('results', [])
        
        for place in places:
            # Collect basic information for each place
            place_info = {
                'place_id': place.get('place_id'),
                'name': place.get('name'),
                'address': place.get('vicinity'),
                'rating': place.get('rating')
            }
            places_info.append(place_info)
        
        # Check if there is a next_page_token for additional results
        pagetoken = res_json.get('next_page_token')
        if not pagetoken:
            break
        else:
            # Include a short delay before requesting the next page to ensure the token is valid
            time.sleep(2)
            url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?pagetoken={pagetoken}&key={api_key}"
    
    return places_info

# Your Google Places API key
api_key = "YOUR_API_KEY"

# Specify the location (latitude and longitude) and radius (in meters)
location = "41.4061976,2.1572022"  # Example: Gracia, Barcelona
radius = "2500"  # Example: 2500 meters
types = "restaurant|cafe"  # Fetch both restaurants and cafes

# Fetch place information
places_info = get_places_info(api_key, location, radius, types)

# Create a DataFrame and save to CSV
df = pd.DataFrame(places_info)
csv_file_name = "restaurants_cafes_info.csv"
df.to_csv(csv_file_name, index=False)
print(f"Saved {len(df)} records to {csv_file_name}")