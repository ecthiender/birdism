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
  imageResult?: FlickrError | ImageUrls,
}

export type { Family, Region, ImageUrls, FlickrError, SpeciesResult }
