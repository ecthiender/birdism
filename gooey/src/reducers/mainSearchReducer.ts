import { Region, Family, SpeciesResult } from 'types/Birdism'

interface MainSearchState {
  allRegions: Region[],
  allFamilies: Family[],
  region: Region | null,
  family: Family | null,
  results: SpeciesResult[],
  noResults: boolean,
  searching: boolean,
  searchErrs: string | null,
}

const initialMainSearchState: MainSearchState = {
  allRegions: [],
  allFamilies: [],
  region: null,
  family: null,
  results: [],
  noResults: false,
  searching: false,
  searchErrs: null,
}

enum MainSearchActionType {
  SetAllRegions,
  SetAllFamilies,
  SetRegion,
  SetFamily,
  SearchStart,
  SearchDone,
  SetResults,
  NoResults,
  SearchErrors,
}

interface SetAllRegionsAction {
  type: MainSearchActionType.SetAllRegions,
  payload: Region[],
}

interface SetAllFamiliesAction {
  type: MainSearchActionType.SetAllFamilies,
  payload: Family[],
}

interface SetRegionAction {
  type: MainSearchActionType.SetRegion,
  payload: Region,
}

interface SetFamilyAction {
  type: MainSearchActionType.SetFamily,
  payload: Family,
}

interface SetResultsAction {
  type: MainSearchActionType.SetResults,
  payload: SpeciesResult[],
}

interface SetSearchErrorsAction {
  type: MainSearchActionType.SearchErrors,
  payload: string,
}

type MainSearchAction
  = SetAllRegionsAction
  | SetAllFamiliesAction
  | SetRegionAction
  | SetFamilyAction
  | SetResultsAction
  | SetSearchErrorsAction
  | { type: MainSearchActionType.SearchStart }
  | { type: MainSearchActionType.SearchDone }
  | { type: MainSearchActionType.SearchDone }
  | { type: MainSearchActionType.NoResults }


const mainSearchReducer = (state: MainSearchState, action: MainSearchAction) => {
  switch (action.type) {
    case MainSearchActionType.SetAllRegionsAction:
      return {
        ...state,
        allRegions: action.payload
      }
    case MainSearchActionType.SetAllFamiliesAction:
      return {
        ...state,
        allFamilies: action.payload
      }
    case MainSearchActionType.SetRegion:
      return {
        ...state,
        region: action.payload
      }
    case MainSearchActionType.SetFamily:
      return {
        ...state,
        family: action.payload
      }
    case MainSearchActionType.SetResults:
      return {
        ...state,
        results: action.payload
      }
    case MainSearchActionType.SearchErrors:
      return {
        ...state,
        searchErrs: action.payload
      }
    case MainSearchActionType.SearchStart:
      return {
        ...state,
        searching: true,
      }
    case MainSearchActionType.SearchDone:
      return {
        ...state,
        searching: false,
      }
    case MainSearchActionType.NoResults:
      return {
        ...state,
        noResults: true,
      }
    default:
      return state
  }
}

export { initialMainSearchState, mainSearchReducer }
