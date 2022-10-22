import {
  Region,
  Family,
  ImageUrls,
  FlickrError,
  SpeciesResult,
  ImageResult,
} from 'types/Birdism'

const API_HOST = process.env.REACT_APP_API_HOST
const API_BASE_URL = `${API_HOST}/api`
const ALL_REGIONS_URL = `${API_BASE_URL}/v1/regions`
const ALL_FAMILIES_URL = `${API_BASE_URL}/v1/families`
const SPECIES_SEARCH_URL = `${API_BASE_URL}/v1/search/species`
const IMAGES_SEARCH_URL = `${API_BASE_URL}/v1/search/images`

interface ApiResponse {
  result: Array<{
    common_name: string,
    species_code: string,
    image_urls?: any
  }>
}

interface ApiResult {
  result: SpeciesResult[]
}

async function getAllRegions(): Promise<Region[]> {
  return fetch(ALL_REGIONS_URL)
    .then(response => response.json())
}

async function getAllFamilies(): Promise<Family[]> {
  return fetch(ALL_FAMILIES_URL)
   .then(response => response.json())
}

async function getSpecies<T>(payload: T): Promise<ApiResult> {
  return fetch(SPECIES_SEARCH_URL, postOptions(payload))
    .then(response => response.json())
    .then(data => responseToResult(data))
}

async function getImages(commonNames: string[]): Promise<ApiResult> {
  return fetch(IMAGES_SEARCH_URL, postOptions(commonNames))
    .then(response => response.json())
    .then(data => responseToResult(data))
}

async function getImagesBySpecies(speciesCode: string): Promise<ImageResult> {
  return fetch(IMAGES_SEARCH_URL, postOptions({species_code: speciesCode}))
    .then(response => response.json())
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

export { getAllRegions, getAllFamilies, getSpecies, getImages, getImagesBySpecies }

/*
 * helper functions
 */

function responseToResult(resp: ApiResponse): ApiResult {
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
