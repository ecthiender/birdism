import { ThemeProvider, createTheme } from '@mui/material/styles';
import Paper from '@mui/material/Paper';

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
      <Paper>
        <Header />
        <MainSearch />
      </Paper>
    </ThemeProvider>
  );
}

export default App;
