import { useEffect, useState } from 'react'
// import Autocomplete from '@mui/material/Autocomplete';
import { createFilterOptions } from '@mui/material/Autocomplete';
import Box from '@mui/material/Box'
import Stack from '@mui/material/Stack';
import Typography from '@mui/material/Typography';
import TextField from '@mui/material/TextField';
import Button from '@mui/material/Button';
import CircularProgress from '@mui/material/CircularProgress';
import SearchIcon from '@mui/icons-material/Search';
import Grid from '@mui/material/Grid';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemText from '@mui/material/ListItemText';
import Link from '@mui/material/Link';

import { MapContainer, TileLayer, useMap, Popup, Marker } from 'react-leaflet'

import VirtualizedAutocomplete from 'components/VirtualizedAutocomplete';
import { Region, RecentSighting, Location } from 'types/Birdism'
import { getRecentObservationsOfSpeciesInRegion, getAllRegions } from 'services'
// import SearchResult from 'components/SearchResult'
// import SearchErrors from 'components/SearchErrors'
// import SearchForm from 'components/SearchForm'

import './styles.css'

export default function TrackSpecies() {
  const [allRegions, setAllRegions] = useState<Region[]>([])
  // const [allFamilies, setAllFamilies] = useState<Family[]>([])

  const [species, setSpecies] = useState<string | null>(null)
  const [region, setRegion] = useState<Region | null>(null)

  const [results, setResults] = useState<RecentSighting[]>([])
  const [noResults, setNoResults] = useState<boolean>(false)
  const [searching, setSearching] = useState<boolean>(false)
  const [searchErrs, setSearchErrs] = useState<string | null>(null)

  const mapRef = useMap()

  useEffect(() => {
    // fetch regions
    getAllRegions().then(setAllRegions)
  }, []);

  const runSearch = async () => {
    setSearching(true);
    if (!species || !region) {
      setSearching(false);
      setSearchErrs("You have to enter both species and region code");
      return;
    }
    console.log('Search Request', species, region)
    getRecentObservationsOfSpeciesInRegion(species, region.region_code)
      .then(data => {
        setSearching(false)
        setResults(data.result)
        console.log('RECVD RESPONSE _---', data)
      });
  }

  return (
    <Box>
      <Stack
        spacing={{ xs: 1, sm: 2, md: 4 }}
        direction={{ xs: 'column', sm: 'row' }}
        alignItems={{xs: 'center', sm: 'left'}}
        sx={{m: 2}}
      >
        <TextField
          id="species-combo-box"
          label="Species Code"
          sx={{ width: 300 }}
          onChange={(event) => setSpecies(event.target.value)}
        />
        <VirtualizedAutocomplete
          id="region-combo-box"
          sx={{ width: 300 }}
          options={allRegions}
          // groupBy={(option) => option.country_code}
          filterOptions={regionFilter}
          isOptionEqualToValue={(option: Region, value: Region) => option.region_code === value.region_code}
          getOptionLabel={(option: Region) => `${option.region_name} (${option.region_code})`}
          value={region}
          onChange={(ev: any, newVal: Region) => setRegion(newVal)}
          renderInput={(params: any) => <TextField {...params} label="Region" />}
        />
        <Box>
          <Button
            variant="contained"
            onClick={runSearch}
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
      <ResultsView sightings={results} />
    </Box>
  )
}

const regionFilter = createFilterOptions({
  stringify: (option: Region) => option.region_name,
});

const speciesFilter = createFilterOptions({
  stringify: (option: string) => option,
});

interface ResultsViewProps {
  sightings: RecentSighting[]
}

const defaultLocation: Location = {
  latitude: 27.03,
  longitude: 88.46,
}

const ResultsView: React.FC<ResultsViewProps> = ({sightings}) => {
  return (
    <Box>
      <Grid container spacing={2} sx={{height: '60vh', margin: 0}}>
        <Grid item xs={9} sx={{paddingRight: 1}}>
          <SightingsMapView
            initialPosition={sightings.length ? sightings[0].location : defaultLocation}
            sightings={sightings}
          />
        </Grid>
        <Grid item xs={3} sx={{borderLeft: '1px solid #ccc'}}>
          <Typography variant="h4" sx={{m: '1px auto'}}>
            Recent Sightings
          </Typography>
          <List>
            { sightings.map((sighting: any, idx: number) => {
                return (
                  <ListItem key={idx}>
                    <ListItemText
                      primary={sighting.locationName}
                      secondary={sighting.observationDate}
                    />
                  </ListItem>
                )
            }) }
          </List>
        </Grid>
      </Grid>
    </Box>
  );
}

interface SightingsMapViewProps {
  initialPosition: Location
  sightings: RecentSighting[]
}

const SightingsMapView: React.FC<SightingsMapViewProps> = ({initialPosition, sightings}) => {
  const initPos = [initialPosition.latitude, initialPosition.longitude];
  return (
    <MapContainer
      center={[initialPosition.latitude, initialPosition.longitude]}
      zoom={10}
      scrollWheelZoom={false}
      className="sightings-map"
    >
      <TileLayer
        attribution='&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      />
      {sightings.map((l, i) => (<SightingLocationMarker sighting={l} key={i} />))}
    </MapContainer>
  );
}

const SightingLocationMarker: React.FC<{sighting: RecentSighting}> = ({sighting}) => {
  return (
    <Marker
      position={[sighting.location.latitude, sighting.location.longitude]}
    >
      <Popup>
        <div> <b> {sighting.locationName} </b> </div>
        <div> <i> {sighting.observationDate} </i> </div>
        <div> Count: {sighting.count} </div>
        <div> <Link href={`https://ebird.org/checklist/${sighting.subId}`}> Checklist </Link> </div>
      </Popup>
    </Marker>
  )
}
