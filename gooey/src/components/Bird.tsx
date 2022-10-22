import * as React from 'react';
import Box from '@mui/material/Box';
import ImageList from '@mui/material/ImageList';
import Link from '@mui/material/Link';
import ImageListItem from '@mui/material/ImageListItem';
import Card from '@mui/material/Card';
import Avatar from '@mui/material/Avatar';
import IconButton  from '@mui/material/IconButton';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import CardHeader from '@mui/material/CardHeader';
import CardContent from '@mui/material/CardContent';
import Skeleton from '@mui/material/Skeleton';
import useMediaQuery from '@mui/material/useMediaQuery';
import { useTheme } from '@mui/material/styles';
import Carousel from "react-material-ui-carousel";
import { CarouselProps } from 'react-material-ui-carousel/dist/components/types';

import { SpeciesResult, ImageResult } from 'types/Birdism'
import { getImagesBySpecies } from 'services';

const Bird: React.FC<SpeciesResult> = ({commonName, speciesCode}) => {

  let imagesFC = null;
  const theme = useTheme();
  const notMobileScreen = useMediaQuery(theme.breakpoints.up('sm'));
  const [imageUrls, setImageUrls] = React.useState<ImageResult|null>(null);
  const [searchError, setSearchError] = React.useState<string|null>(null);

  React.useEffect(() => {
    setSearchError(null);
    setImageUrls(null);
    getImagesBySpecies(speciesCode)
      .then(setImageUrls)
      .catch((e) => setSearchError(e.toString()))
  }, [speciesCode]);


  if (imageUrls === null) {
    imagesFC = (<SpeciesImagesPlaceholder />)
  }
  else if (searchError !== null) {
    imagesFC = (<p>Error with search</p>)
  }
  else {
    if (notMobileScreen) {
      imagesFC = (<SpeciesImagesRow imageUrls={imageUrls.result} commonName={commonName} />)
    }
    else {
      imagesFC = (<SpeciesImagesCarousel imageUrls={imageUrls.result} commonName={commonName} />)
    }
  }

  return (
    <Box
      sx={{
        marginTop: 1,
        marginBottom: 2,
        minWidth: 300,
        width: {xs: 'calc(100vw - 10px)', sm: '95%'}
      }}
    >
      <Card>
        <CardHeader
          title={
            <Link
              href={`https://ebird.org/species/${speciesCode}`}
              color="inherit"
              underline="none"
              target="_blank"
              rel="noopener"
            >
              {commonName}
            </Link>
          }
          titleTypographyProps={{fontSize: 24}}
          avatar={<Avatar>{commonName.split(' ').map((part) => part[0])}</Avatar>}
          action={
            <IconButton>
              <ExpandMoreIcon />
            </IconButton>
          }
        />

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
        <Skeleton variant="rectangular" width={300} height={250} />
      </ImageListItem>
      <ImageListItem key={2}>
        <Skeleton variant="rectangular" width={300} height={250} />
      </ImageListItem>
      <ImageListItem key={3}>
        <Skeleton variant="rectangular" width={300} height={250} />
      </ImageListItem>
    </ImageList>
  )
}

export default Bird
