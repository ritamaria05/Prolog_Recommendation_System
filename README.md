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

### Server

Completar...


