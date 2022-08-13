import Box from '@mui/material/Box';
import ImageList from '@mui/material/ImageList';
import ImageListItem from '@mui/material/ImageListItem';
import Card from '@mui/material/Card';
import CardHeader from '@mui/material/CardHeader';
import CardContent from '@mui/material/CardContent';
import Skeleton from '@mui/material/Skeleton';

export default function Bird({image_urls, common_name}) {
  const photos = image_urls
  let images = null
  if (photos === undefined) {
    images = (
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
  else if ('error' in photos && photos.error) {
    images = (<p>Error with search</p>)
  }
  else {
    // https://stackoverflow.com/questions/69597992/how-to-implement-horizontal-scrolling-of-tiles-in-mui-gridlist
    images = (
      <ImageList
        sx={{
          gridAutoFlow: "column",
          gridTemplateColumns: "repeat(auto-fit, minmax(160px,1fr)) !important",
          gridAutoColumns: "minmax(160px, 1fr)"
        }}
      >
        {photos.map((photo, idx) => (
          <ImageListItem key={idx}>
            <img src={photo} alt={common_name} />
          </ImageListItem>
        ))}
      </ImageList>
    )
  }

  return (
    <Box sx={{marginTop: 1, minWidth: 300}}>
      <Card>
        <CardHeader title={common_name} />
        <CardContent>
          {images}
        </CardContent>
      </Card>
    </Box>
  );
}
