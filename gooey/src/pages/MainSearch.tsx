import { useEffect, useState, useReducer } from 'react';
import Box from '@mui/material/Box';

import { Region, Family, SpeciesResult } from 'types/Birdism'
import { getAllRegions, getAllFamilies, getSpecies, getImages } from 'services'
import SearchResult from 'components/SearchResult'
import SearchErrors from 'components/SearchErrors'
import SearchForm from 'components/SearchForm'
import { initialMainSearchState, mainSearchReducer } from 'reducers/mainSearchReducer'

export default function MainSearch() {
  const [state, dispatch] = useReducer(mainSearchReducer, initialMainSearchState)
  // const [allRegions, setAllRegions] = useState<Region[]>([])
  // const [allFamilies, setAllFamilies] = useState<Family[]>([])

  // const [region, setRegion] = useState<Region | null>(null)
  // const [family, setFamily] = useState<Family | null>(null)

  // const [results, setResults] = useState<SpeciesResult[]>([])
  // const [noResults, setNoResults] = useState<boolean>(false)
  // const [searching, setSearching] = useState<boolean>(false)
  // const [searchErrs, setSearchErrs] = useState<string | null>(null)

  useEffect(() => {
    // fetch regions
    getAllRegions().then(setAllRegions)
    // fetch families
    getAllFamilies().then(setAllFamilies)
  }, []);

  const runSearch = async () => {
    setSearchErrs(null)
    if (state.family === null || state.region === null) {
      setSearchErrs('Please select a region and a family.')
      return
    }
    setSearching(true)
    const payload = {
      family: state.family.scientific_name,
      region: state.region.region_code,
    }
    try {
      const data = await getSpecies(payload)
      setSearching(false)
      setNoResults(false)
      setResults(data.result)
      if (data.result && !data.result.length) {
        setNoResults(true)
        return
      }
      const commonNames = data.result.map(sp => sp.commonName)
      const images = await getImages(commonNames)
      setResults(images.result)
    }
    catch (err: any) {
      console.error('search images error', err)
      setSearchErrs(err)
    }
    finally {
      setSearching(false)
    }
  }

  return (
    <Box>
      <SearchForm
        allFamilies={state.allFamilies}
        allRegions={state.allRegions}
        selectedFamily={state.family}
        setSelectedFamily={setFamily}
        selectedRegion={state.region}
        setSelectedRegion={setRegion}
        searchAction={runSearch}
        searching={state.searching}
      />
      <SearchErrors errors={state.searchErrs} />
      <SearchResult results={state.results} noResults={state.noResults} />
    </Box>
  )
}
