import Box from '@mui/material/Box';
import Typography from '@mui/material/Typography';
import Link from '@mui/material/Link';
import Divider from '@mui/material/Divider';

export default function Footer() {
  return (
    <Box sx={{m: 1}}>
      <Divider />
      <Typography component="div" sx={{textAlign: 'center', paddingTop: '5px'}} color="#555">
        Powered by <Link href="https://ebird.org/" color="inherit"> eBird </Link>
      </Typography>
    </Box>
  )
}
