const API_BASE_URL = 'http://localhost:8888/api'
const ALL_REGIONS_URL = `${API_BASE_URL}/v1/regions`
const ALL_FAMILIES_URL = `${API_BASE_URL}/v1/families`
const SPECIES_SEARCH_URL = `${API_BASE_URL}/v1/search/species`
const IMAGES_SEARCH_URL = `${API_BASE_URL}/v1/search/images`

function getAllRegions() {
  return fetch(ALL_REGIONS_URL)
    .then(response => response.json())
}

function getAllFamilies() {
  return fetch(ALL_FAMILIES_URL)
   .then(response => response.json())
}

function getSpecies(payload) {
  return fetch(SPECIES_SEARCH_URL, postOptions(payload))
    .then(response => response.json())
}

function getImages(commonNames) {
  return fetch(IMAGES_SEARCH_URL, postOptions(commonNames))
    .then(response => response.json())
}

const postOptions = (payload) => {
  return {
    method: 'POST',
    mode: 'cors',
    headers: {
      'content-type': 'application/json'
    },
    body: JSON.stringify(payload)
  }
};


export { getAllRegions, getAllFamilies, getSpecies, getImages }
