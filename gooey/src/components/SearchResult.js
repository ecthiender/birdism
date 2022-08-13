import Box from '@mui/material/Box';
import Typography from '@mui/material/Typography';

import Bird from './Bird'

export default function SearchResult({results, noResults}) {
  return (
    <>
      <Box sx={{m: 2}}>
        {noResults ? (<Typography> No species found </Typography>) : null}
      </Box>
      <Box sx={{m: 2}}>
        {results?.length ? (<Typography> {results?.length} species found </Typography>) : null}
      </Box>
      <Box sx={{display: 'flex', minHeight: '50vh', m: 1}}>
        <Box>
          {results.map((bird, idx) => <Bird {...bird} key={idx} />)}
        </Box>
      </Box>
    </>
  );
}
