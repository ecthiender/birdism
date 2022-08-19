import * as React from "react";
import Box from '@mui/material/Box';
import ImageList from '@mui/material/ImageList';
import ImageListItem from '@mui/material/ImageListItem';
import Card from '@mui/material/Card';
import CardHeader from '@mui/material/CardHeader';
import CardContent from '@mui/material/CardContent';
import Skeleton from '@mui/material/Skeleton';

import { SpeciesResult } from 'types/Birdism'

const Bird: React.FC<SpeciesResult> = ({commonName, imageResult}) => {

  let imagesFC = null

  if (imageResult === undefined) {
    imagesFC = (<SpeciesImagesPlaceholder />)
  }
  else {
    switch (imageResult.state) {
      case 'error':
        imagesFC = (<p>Error with search</p>)
        break
      case 'success':
        imagesFC = (<SpeciesImages imageUrls={imageResult.urls} commonName={commonName} />)
        break
    }
  }

  return (
    <Box sx={{marginTop: 1, minWidth: 300}}>
      <Card>
        <CardHeader title={commonName} />
        <CardContent>
          {imagesFC}
        </CardContent>
      </Card>
    </Box>
  );
}

interface SpeciesImagesProps {
  imageUrls: string[],
  commonName: string,
}

const SpeciesImages: React.FC<SpeciesImagesProps> = ({imageUrls, commonName}) => {
  // https://stackoverflow.com/questions/69597992/how-to-implement-horizontal-scrolling-of-tiles-in-mui-gridlist
  return (
    <ImageList
      sx={{
        gridAutoFlow: "column",
        gridTemplateColumns: "repeat(auto-fit, minmax(160px,1fr)) !important",
        gridAutoColumns: "minmax(160px, 1fr)"
      }}
    >
      {imageUrls.map((photo: string, idx: number) => (
        <ImageListItem key={idx}>
          <img src={photo} alt={commonName} />
        </ImageListItem>
      ))}
    </ImageList>
  )
}

const SpeciesImagesPlaceholder: React.FC<{}> = () => {
  return (
    <ImageList
      sx={{
        gridAutoFlow: "column",
        gridTemplateColumns: "repeat(auto-fit, minmax(160px,1fr)) !important",
        gridAutoColumns: "minmax(160px, 1fr)",
      }}
    >
      <ImageListItem key={1}>
        <Skeleton variant="rectangular" width={300} height={200} />
      </ImageListItem>
      <ImageListItem key={2}>
        <Skeleton variant="rectangular" width={300} height={200} />
      </ImageListItem>
      <ImageListItem key={3}>
        <Skeleton variant="rectangular" width={300} height={200} />
      </ImageListItem>
    </ImageList>
  )
}

export default Bird
