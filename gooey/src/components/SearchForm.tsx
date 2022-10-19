import Autocomplete from '@mui/material/Autocomplete';
import { createFilterOptions } from '@mui/material/Autocomplete';
import Box from '@mui/material/Box';
import Stack from '@mui/material/Stack';
import Button from '@mui/material/Button';
// import LoadingButton from '@mui/lab/LoadingButton';
import CircularProgress from '@mui/material/CircularProgress';
import SearchIcon from '@mui/icons-material/Search';
import TextField from '@mui/material/TextField';

import { Family, Region } from 'types/Birdism'
import VirtualizedAutocomplete from 'components/VirtualizedAutocomplete';

interface SearchFormProps {
  allFamilies: Family[],
  allRegions: Region[],
  selectedFamily: Family | null,
  setSelectedFamily: any,
  selectedRegion: Region | null,
  setSelectedRegion: any,
  searchAction: any,
  searching: boolean,
}

const SearchForm: React.FC<SearchFormProps> = (props) => {

  const {
    allFamilies,
    allRegions,
    selectedFamily,
    setSelectedFamily,
    selectedRegion,
    setSelectedRegion,
    searchAction,
    searching
  } = props

  return (
    <Stack
      spacing={{ xs: 1, sm: 2, md: 4 }}
      direction={{ xs: 'column', sm: 'row' }}
      alignItems={{xs: 'center', sm: 'left'}}
      sx={{m: 2}}
    >
      <Autocomplete
        disablePortal
        id="family-combo-box"
        sx={{ width: 300 }}
        options={allFamilies}
        getOptionLabel={(option) => `${option.common_name} (${option.scientific_name})`}
        isOptionEqualToValue={(option, value) => option.scientific_name === value.scientific_name}
        filterOptions={familyFilter}
        value={selectedFamily}
        onChange={(ev, newVal) => setSelectedFamily(newVal)}
        renderInput={(params) => <TextField {...params} label="Family" />}
      />
      <VirtualizedAutocomplete
        id="region-combo-box"
        sx={{ width: 300 }}
        options={allRegions}
        // groupBy={(option) => option.country_code}
        filterOptions={regionFilter}
        isOptionEqualToValue={(option: Region, value: Region) => option.region_code === value.region_code}
        getOptionLabel={(option: Region) => `${option.region_name} (${option.region_code})`}
        value={selectedRegion}
        onChange={(ev: any, newVal: Region) => setSelectedRegion(newVal)}
        renderInput={(params: any) => <TextField {...params} label="Region" />}
      />
      <Box>
        <Button
          variant="contained"
          onClick={searchAction}
          disabled={searching}
          startIcon={<SearchIcon/>}
          sx={{
            width: 300,
            height: 50,
            marginTop: {xs: 2, sm: 0},
          }}
        >
          {searching ? 'Searching...' : 'Search'}
          {searching && (<CircularProgress size={24} />)}
        </Button>
      </Box>
    </Stack>
  )
}

const familyFilter = createFilterOptions({
  stringify: (option: Family) => option.common_name
});

const regionFilter = createFilterOptions({
  stringify: (option: Region) => option.region_name,
});

export default SearchForm
export type { SearchFormProps, Region, Family }
