import { ThemeProvider, createTheme } from '@mui/material/styles';

import Header from './components/Header'
import MainSearch from './pages/MainSearch'

const appTheme = createTheme({
  palette: {
    mode: 'light',
  },
});

function App() {
  return (
    <ThemeProvider theme={appTheme}>
      <Header />
      <MainSearch />
    </ThemeProvider>
  );
}

export default App;
