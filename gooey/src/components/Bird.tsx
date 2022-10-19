import * as React from 'react';
import Box from '@mui/material/Box';
import ImageList from '@mui/material/ImageList';
import ImageListItem from '@mui/material/ImageListItem';
import Card from '@mui/material/Card';
import CardHeader from '@mui/material/CardHeader';
import CardContent from '@mui/material/CardContent';
import Skeleton from '@mui/material/Skeleton';
import useMediaQuery from '@mui/material/useMediaQuery';
import { useTheme } from '@mui/material/styles';
import Carousel from "react-material-ui-carousel";
import { CarouselProps } from 'react-material-ui-carousel/dist/components/types';

import { SpeciesResult } from 'types/Birdism'

const Bird: React.FC<SpeciesResult> = ({commonName, imageResult}) => {

  let imagesFC = null;
  const theme = useTheme();
  const notMobileScreen = useMediaQuery(theme.breakpoints.up('sm'));

  if (imageResult === undefined) {
    imagesFC = (<SpeciesImagesPlaceholder />)
  }
  else {
    switch (imageResult.state) {
      case 'error':
        imagesFC = (<p>Error with search</p>)
        break
      case 'success':
        if (notMobileScreen) {
          imagesFC = (<SpeciesImagesRow imageUrls={imageResult.urls} commonName={commonName} />)
        }
        else {
          imagesFC = (<SpeciesImagesCarousel imageUrls={imageResult.urls} commonName={commonName} />)
        }
        break
    }
  }

  return (
    <Box sx={{marginTop: 1, minWidth: 300, width: {xs: 'calc(100vw - 10px)', sm: '95%'}}}>
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

const SpeciesImagesRow: React.FC<SpeciesImagesProps> = ({imageUrls, commonName}) => {
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

const carouselSettings: CarouselProps = {
  autoPlay: false,
  animation: "slide",
  indicators: true,
  navButtonsAlwaysVisible: true,
  cycleNavigation: true,
  fullHeightHover: true,
  swipe: true
};

const SpeciesImagesCarousel: React.FC<SpeciesImagesProps> = ({imageUrls, commonName}) => {
  return (
    <Carousel {...carouselSettings}>
      {
        imageUrls.map((photo, index) => (
          <div key={index}>
            <Box
              component="img"
              src={photo}
              alt={commonName}
              sx={{
                height: 255,
                display: 'block',
                maxWidth: 400,
                overflow: 'hidden',
                width: '100%',
              }}
            />
          </div>
        ))
      }
    </Carousel>
  );
};

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
