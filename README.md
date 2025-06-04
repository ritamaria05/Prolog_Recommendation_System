# Movie Recommendation System

## How to Setup
1. Download the folder  
2. Go to "data" directory in terminal  
3. Run command: `swipl server.pl`  
4. Query: `server(PORT).` (Replace PORT with your desired port, e.g., 8080)  
5. Server will run at: `localhost:PORT`

## System Architecture

### Building our Knowledge Base (KB)
- **Source Data**: IMDB movies.csv  
- **Parser**: gendb.pl converts CSV lines to Prolog facts in movies.pl

### Recommendation Approaches

#### 1. Question-Based Recommender
- Asks questions and filters KB based on answers  
- Example filters:  
  - Films from the US  
  - Films released after 2000  
- Implemented in: recommend.pl

#### 2. KNN-Based Recommender
##### Cosine Similarity (Core Metric)
**How it works**:
- Each movie represented as vector:
  - Dimensions = Users
  - Values = Ratings (0 if unrated)
- Implemented in: knn.pl

**Example**:

Movie A: [User1-4, User2-3, User3-5]

Movie B: [User1-5, User2-2, User4-4]


**Calculation**:

 #### $\text{Similarity} = \frac{\text{Dot Product}(A, B)}{\text{Norm}(A) \times \text{Norm}(B)}$

- **Dot Product**: Sum of products of matching user ratings  
- **Norm**: √(sum of squared ratings)  
- **Result**: 0 (no similarity) to 1 (identical patterns)

**Key Properties**:
- User-based comparison (only shared raters contribute)  
- Focuses on rating patterns, not absolute scores  
- Handles sparse data (low overlap → similarity → 0)

##### Additional Considerations
a) **Co-rated Users**  
   - Movies are similar if same users rated them highly  
   *Example*: Users who loved *The Godfather* also loved *Goodfellas*

b) **Genre**  
   - Fallback when KNN data is insufficient  
   - Not part of cosine calculation  
   - `recommend_by_genre/4`

c) **Popularity**  
   - Fallback when KNN + Genre data is insufficient   (Unlikely to reach this function)
   - Based on average ratings
   - `popular_items/2`

d) **Random**
   - Last fallback when the other 3 methods don't give the number of recommendations  (Unlikely to reach this function)
   - Chooses films randomly with rating >= 4
   - `random_good_movies/3`

### Users
- **Register**: When a user registers for the first time, new user is added to usersdb.pl (users KB)

  - Password is encrypted before being added to KB
- **Login**: Checks if User is in KB and if Passoword is correct.

  - If so, User is logged in which gives him access to several Fixtures.
- **Ratings**: Each User can rate the films they have watched and write a review. (rating.pl, users.pl)
- **Explore**: Each User can discover new films, and see relevant information about each film. 
- **Logout**: Logout from account.

### Server (Site)
Each page on the site has the currently logged in user stated in the top left and a logo in the top right. The logo on the top right links the user back to the home page when clicked.

#### **Home Page**
The home page displays a heading "Welcome to Prolog, the Movie Recommender" and subtitle "All of your movie needs, in one place! :)" and has buttons for the following:
- `Register`
- `Login`
- `Get Recommendations`
- `Show Your Films`
- `Show Your Ratings`
- `Show All Films`
- `Logout`

#### **Register**
Page allows the user to create an account by providing a username and password. The username has to be one that does not already exist in the database.

#### **Login**
Allows the user to login to their account with their username-password credentials.

#### **Get Recommendations**
Opens a menu page with the following recommendation options:
- `Based on Your Films`: User must be logged in to work. Generates a list of recommedations based on the user's ratings made and other similar highly rated films.
- `By Specific Questions`: Opens a page with the following questions and generates recommendations based on the answers (if provided):
  - Do you prefer older movies (pre-2000)?
    - yes/no
  - What types of movies are you in the mood for?
    - Emotional/Historical/Cerebral/Adventurous/Funny
  - Do you have time for longer movies?
    - yes/no
  - Which country's movie do you prefer?
    - US/UK/Canada/Japan/Korea/China
  - Do you prefer high-scoring movies?
    - yes (unanswered is assumed as no preference)
  - Options to show recommendations based on responses or return home 
- `Return Home`

#### **Show Your Films**
Must be logged in to have access. Displays a list of the films the user has added to their account, with options remove or rate next to each 'title (year)' in the list.
The user can click on the film's name to open the respective film page.

#### **Show Your Ratings**
Must be logged in to have access. Displays a list of all ratings made by the currently logged in user and an option to see your films or return home.

#### **Show All Films**
Shows a list of all the films in the database with filter options (year, country, genre), a field to search by the name of the film and buttons to apply the user-provided filters and reset the filters to default. At the bottom of the page there is an option to return home.

#### **Film Pages**
Page that displays a poster and detail for any film, as well as options to add to your films, rate or go to the official IMDB page. User-made ratings for the film are also listed on the film page along with an option to return home.

#### **Logout**
Ends the current user's session and stays on home page.