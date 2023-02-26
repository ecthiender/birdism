import { ThemeProvider, createTheme } from '@mui/material/styles';
import Box from '@mui/material/Box';
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";

import Header from './components/Header'
import MainSearch from './pages/MainSearch'
import TrackSpecies from './pages/TrackSpecies'

const appTheme = createTheme({
  palette: {
    mode: 'light',
  },
});

function App() {
  return (
    <ThemeProvider theme={appTheme}>
      <Router>
        <Header />
        <Routes>
          <Route path="/" element={<MainSearch />} />
          <Route path="/species/track" element={<TrackSpecies />} />
        </Routes>
      </Router>
    </ThemeProvider>
  );
}

export default App;
