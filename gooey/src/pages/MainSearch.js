import { useEffect, useState } from 'react';
import Box from '@mui/material/Box';

import { getAllRegions, getAllFamilies, getSpecies, getImages } from '../services'
import SearchResult from '../components/SearchResult'
import SearchErrors from '../components/SearchErrors'
import SearchForm from '../components/SearchForm'

export default function MainSearch() {
  const [allRegions, setAllRegions] = useState([])
  const [allFamilies, setAllFamilies] = useState([])

  const [region, setRegion] = useState(null)
  const [family, setFamily] = useState(null)

  const [results, setResults] = useState([])
  const [noResults, setNoResults] = useState(false)
  const [searching, setSearching] = useState(false)
  const [searchErrs, setSearchErrs] = useState(null)

  useEffect(() => {
    // fetch regions
    getAllRegions().then(setAllRegions)
    // fetch families
    getAllFamilies().then(setAllFamilies)
  }, []);

  const runSearch = () => {
    setSearching(true)
    const payload = {family: family.scientific_name, region: region.region_code}
    getSpecies(payload)
      .then(data => {
        setSearching(false)
        setNoResults(false)
        setResults(data.result)
        if (data.result && !data.result.length) {
          setNoResults(true)
          return
        }
        const commonNames = data.result.map(sp => sp.common_name)
        getImages(commonNames)
          .then((data) => {
            setSearching(false)
            setResults(data.result)
          })
          .catch(err => {
            console.error('search images error', err)
            setSearching(false)
            setSearchErrs(err)
          })
      })
      .catch((err) => {
        console.error('search species error', err)
        setSearching(false)
        setSearchErrs(err)
      })
  }

  return (
    <Box>
      <SearchForm
        allFamilies={allFamilies}
        allRegions={allRegions}
        selectedFamily={family}
        setSelectedFamily={setFamily}
        selectedRegion={region}
        setSelectedRegion={setRegion}
        searchAction={runSearch}
        searching={searching}
      />
      <SearchErrors errors={searchErrs} />
      <SearchResult results={results} noResults={noResults} />
    </Box>
  )
}
