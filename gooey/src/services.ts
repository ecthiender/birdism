import {
  Region,
  Family,
  ImageUrls,
  FlickrError,
  SpeciesResult,
  ImageResult,
  RecentSighting,
} from 'types/Birdism'

const API_HOST = process.env.REACT_APP_API_HOST
const API_BASE_URL = `${API_HOST}/api`
const ALL_REGIONS_URL = `${API_BASE_URL}/v1/regions`
const ALL_FAMILIES_URL = `${API_BASE_URL}/v1/families`
const SPECIES_SEARCH_URL = `${API_BASE_URL}/v1/search/species`
const IMAGES_SEARCH_URL = `${API_BASE_URL}/v1/search/images`
const TRACK_SPECIES_URL = `${API_BASE_URL}/v1/species/track`

interface ApiResponse<T> {
  result: T
}

type SearchSpeciesApiResponseNonStandard =
  ApiResponse<Array<{
    common_name: string,
    species_code: string,
    image_urls?: any
  }>>

type SearchSpeciesResponse = ApiResponse<SpeciesResult[]>

async function getAllRegions(): Promise<Region[]> {
  return fetch(ALL_REGIONS_URL)
    .then(response => response.json())
}

async function getAllFamilies(): Promise<Family[]> {
  return fetch(ALL_FAMILIES_URL)
   .then(response => response.json())
}

async function getSpecies<T>(payload: T): Promise<SearchSpeciesResponse> {
  return fetch(SPECIES_SEARCH_URL, postOptions(payload))
    .then(response => response.json())
    .then(data => responseToResult(data))
}

async function getImages(commonNames: string[]): Promise<SearchSpeciesResponse> {
  return fetch(IMAGES_SEARCH_URL, postOptions(commonNames))
    .then(response => response.json())
    .then(data => responseToResult(data))
}

async function getImagesBySpecies(speciesCode: string): Promise<ImageResult> {
  return fetch(IMAGES_SEARCH_URL, postOptions({species_code: speciesCode}))
    .then(response => response.json())
}

async function getRecentObservationsOfSpeciesInRegion(speciesCode: string, regionCode: string): Promise<ApiResponse<RecentSighting[]>> {
  return fetch(TRACK_SPECIES_URL, postOptions({species_code: speciesCode, region_code: regionCode}))
    .then(response => response.json())
    .then(data => transformApiReponseCasing(data))
}

function postOptions<T> (payload: T): object {
  return {
    method: 'POST',
    mode: 'cors',
    headers: {
      'content-type': 'application/json'
    },
    body: JSON.stringify(payload)
  }
}

export { getAllRegions, getAllFamilies, getSpecies, getImages, getImagesBySpecies, getRecentObservationsOfSpeciesInRegion }

/*
 * helper functions
 */

function responseToResult(resp: SearchSpeciesApiResponseNonStandard): SearchSpeciesResponse {
  let transformed = resp.result.map(r => ({
    commonName: r.common_name,
    speciesCode: r.species_code,
    imageResult: transformImageUrls(r?.image_urls)
  }))
  return {result: transformed}
}

function transformImageUrls(urls: any): FlickrError | ImageUrls {
  if (urls === undefined || urls === null)
    return urls
  if (urls instanceof Array) {
    return {
      state: 'success',
      urls: urls,
    }
  }
  else {
    return {
      state: 'error',
      ...urls
    }
  }
}

// API response keys are in snake-case. All TS types/interface fields are in
// camel-case. This function performs the transformation.
function transformApiReponseCasing<T>(resp: ApiResponse<T>): ApiResponse<T> {
  return keysToCamel(resp);
}

function toCamel(s: string) {
  return s.replace(/([-_][a-z])/ig, ($1) => {
    return $1.toUpperCase()
      .replace('-', '')
      .replace('_', '');
  });
}

function keysToCamel(o: any): any {
  if (o === Object(o) && !Array.isArray(o) && typeof o !== 'function') {
    const n: Record<string, any> = {};
    Object.keys(o).forEach((k) => {
        n[toCamel(k)] = keysToCamel(o[k]);
      });
    return n;
  } else if (Array.isArray(o)) {
    return o.map((i) => {
      return keysToCamel(i);
    });
  }
  return o;
}
