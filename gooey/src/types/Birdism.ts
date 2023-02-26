interface Family {
  common_name: string,
  scientific_name: string,
}

interface Region {
  region_name: string,
  region_code: string,
}

interface ImageUrls {
  state: 'success',
  urls: string[]
}

interface FlickrError {
  state: 'error',
  code: string,
  error: string,
}

interface SpeciesResult {
  commonName: string,
  speciesCode: string,
  imageResult?: FlickrError | ImageUrls,
}

interface ImageResult {
  result: string[]
}

interface RecentSighting {
  speciesCode: string,
  commonName: string,
  scientificName: string,
  observationDate: string,
  locationId: string,
  location: Location,
  locationName: string,
  subId: string,
  count?: number,
  observationValid: boolean,
  observationReviewed: boolean,
  locationPrivate: boolean,
}

interface Location {
  latitude: number,
  longitude: number
}

export type { Family, Region, ImageUrls, FlickrError, SpeciesResult, ImageResult, RecentSighting, Location }
