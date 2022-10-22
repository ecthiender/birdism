import Box from '@mui/material/Box';
import Typography from '@mui/material/Typography';

import { SpeciesResult } from 'types/Birdism'
import Bird from 'components/Bird'

interface SearchResultProps {
  results: SpeciesResult[],
  noResults: boolean,
}

const SearchResult: React.FC<SearchResultProps> = ({results, noResults}) => {
  return (
    <>
      <Box sx={{m: 2}}>
        {noResults ? (<Typography> No species found </Typography>) : null}
      </Box>
      <Box sx={{m: 2}}>
        {results?.length ? (<Typography> {results?.length} species found </Typography>) : null}
      </Box>
      <Box sx={{display: 'flex', minHeight: {xs: '50vh', sm: '60vh'}, m: 1}}>
        <Box>
          {results.map((bird, idx) => <Bird {...bird} key={idx} />)}
        </Box>
      </Box>
    </>
  );
}

export default SearchResult
