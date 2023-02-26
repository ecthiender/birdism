import Box from '@mui/material/Box';
import Stack from '@mui/material/Stack';
import Typography from '@mui/material/Typography';
import Link from '@mui/material/Link';
// import { Link } from 'react-router-dom';

import '../App.css';

export default function Header() {
  return (
    <Box className="App-header">
      <Stack
        direction="row"
        justifyContent="space-between"
        alignItems="center"
        spacing={2}
      >
        <Box>
          <Typography variant="h2" component="div"> Birdism </Typography>
          <Typography variant="subtitle1" gutterBottom component="div">
            {/* Bird identification and more */}
            Search for a family of birds in a region
          </Typography>
        </Box>
        <Box>
          { /* TODO: integrate this Link with react-router-dom: https://mui.com/material-ui/guides/routing/#component-prop */ }
          <Link href="/species/track"> Recent Sightings </Link> |
          <Link href="/"> Main Search </Link>
        </Box>
      </Stack>
    </Box>
  )
}
