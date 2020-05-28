# Bird ID Helper

This application helps in narrowing down identification of a bird. Often is the
case when you have identified the family of the bird, but is unable to
conclusively identify the exact species. In that case, you frantically search
for different species of that family and looking at their photographs.

This application simply does it for you!

Enter a family name (flycatcher, eagle, ducks) and the region name - and this
application will list all the species of that family available in that region,
with multiple photographs. Helping you to identify your bird.

# How it works
This application uses the taxonomy database from ebird, and its checklist API to
find out all the species (of a family) in a particular region from last 30 days
of observation. Then it takes those common species name, and does a Flickr
search for photos, and displays them to you!

# Development
The backend is in Haskell. The frontend is planned in HyperApp.

## How to get the taxonomy db                                                                                                  │
1. Download the taxonomy db from ebird.                                                                                        │
2. It is a csv file.                                                                                                           │
3. Import the file into a postgres database.                                                                                     │
4. Use that postgres database as your data source.

# License
BSD3 license. See LICENSE file.

# Contributing
Submit an issue or a pull request.
