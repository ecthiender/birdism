import * as React from 'react'
import Alert from '@mui/material/Alert'

interface SearchErrorsProps {
  errors: string | null,
}

const SearchErrors: React.FC<SearchErrorsProps> = ({errors}) => {
  if (!errors) return null
  return (
    <Alert sx={{m: 2}} severity="error">{errors}</Alert>
  )
}

export default SearchErrors
