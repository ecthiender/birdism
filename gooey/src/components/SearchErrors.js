import Alert from '@mui/material/Alert';

export default function SearchErrors({errors}) {
  if (!errors) return null
  return (
    <Alert severity="error">{errors}</Alert>
  )
}
