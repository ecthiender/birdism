import { useEffect, useState } from 'react'
import Box from '@mui/material/Box'
import Divider from '@mui/material/Divider'
//import Typography from '@mui/material/Typography'
//import InfoOutlinedIcon from '@mui/icons-material/InfoOutlined'
//import Stack from '@mui/material/Stack'

import { Region, Family, SpeciesResult } from 'types/Birdism'
import { getAllRegions, getAllFamilies, getSpecies, getImages } from 'services'
import SearchResult from 'components/SearchResult'
import SearchErrors from 'components/SearchErrors'
import SearchForm from 'components/SearchForm'

export default function MainSearch() {
  const [allRegions, setAllRegions] = useState<Region[]>([])
  const [allFamilies, setAllFamilies] = useState<Family[]>([])

  const [region, setRegion] = useState<Region | null>(null)
  const [family, setFamily] = useState<Family | null>(null)

  const [results, setResults] = useState<SpeciesResult[]>([])
  const [noResults, setNoResults] = useState<boolean>(false)
  const [searching, setSearching] = useState<boolean>(false)
  const [searchErrs, setSearchErrs] = useState<string | null>(null)

  useEffect(() => {
    // fetch regions
    getAllRegions().then(setAllRegions)
    // fetch families
    getAllFamilies().then(setAllFamilies)
  }, []);

  const runSearch = async () => {
    setSearchErrs(null)
    if (family === null || region === null) {
      setSearchErrs('Please select a region and a family.')
      return
    }
    setSearching(true)
    const payload = {
      family: family.scientific_name,
      region: region.region_code,
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
        allFamilies={allFamilies}
        allRegions={allRegions}
        selectedFamily={family}
        setSelectedFamily={setFamily}
        selectedRegion={region}
        setSelectedRegion={setRegion}
        searchAction={runSearch}
        searching={searching}
      />
      <Divider />
      <SearchErrors errors={searchErrs} />
      <SearchResult results={results} noResults={noResults} />
    </Box>
  )
}

/*
function InfoHeading() {
  return (
    <Stack direction="row"
      marginLeft={2}
      marginBottom={{xs: 3, sm: 4, md: 5}}
      sx={{
        backgroundColor: '#eee',
        padding: 2,
        borderRadius: 2,
        width: {xs: 300, sm: 500},
      }}
    >
      <InfoOutlinedIcon fontSize="medium" sx={{marginRight: 1, color: '#333'}} />
      <Typography color="#333" sx={{fontSize: 14}}>
        Search for a family of birds in a specific region
      </Typography>
    </Stack>
  );
}
*/
