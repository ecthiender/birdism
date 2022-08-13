import Autocomplete from '@mui/material/Autocomplete';
import { createFilterOptions } from '@mui/material/Autocomplete';
import Box from '@mui/material/Box';
import Button from '@mui/material/Button';
// import LoadingButton from '@mui/lab/LoadingButton';
import CircularProgress from '@mui/material/CircularProgress';
import SearchIcon from '@mui/icons-material/Search';
import TextField from '@mui/material/TextField';

import VirtualizedAutocomplete from './VirtualizedAutocomplete';

export default function SearchForm(props) {

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
    <Box
      component="form"
      sx={{
        'width': '75%',
        'display': 'flex',
        'flexDirection': 'row',
        '& > :not(style)': { m: 1, width: '40ch' },
      }}
      noValidate
      autoComplete="off"
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
        isOptionEqualToValue={(option, value) => option.region_code === value.region_code}
        getOptionLabel={(option) => `${option.region_name} (${option.region_code})`}
        value={selectedRegion}
        onChange={(ev, newVal) => setSelectedRegion(newVal)}
        renderInput={(params) => <TextField {...params} label="Region" />}
      />
      <Box>
        <Button
          variant="contained"
          onClick={searchAction}
          disabled={searching}
          startIcon={<SearchIcon/>}
          sx={{minHeight: '99%', minWidth: 140}}
        >
          {searching ? 'Searching...' : 'Search'}
          {searching && (<CircularProgress size={24} />)}
        </Button>
      </Box>
    </Box>
  )
}

const familyFilter = createFilterOptions({
  stringify: (option) => option.common_name
});

const regionFilter = createFilterOptions({
  stringify: (option) => option.region_name,
});