module Halogen.CalendarDatePicker 
  ( calendar
  , Message (..)
  , Query
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (cons, range, replicate)
import Data.Date (Date, Month(..), Year, canonicalDate, day, exactDate, lastDayOfMonth, month, weekday, year)
import Data.Enum (fromEnum, pred, succ, toEnum, upFromIncluding)
import Data.Foreign (Foreign)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (fromNonEmpty)
import Data.String (length)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = SwitchMonth String a
  | SelectDay Int a
  | PrevYear a
  | NextYear a
  | ChangeStagedMonth String a
  | EnterDay String a
  | EnterYear String a

data Message = DateSelected Date

type State =
  { selectedDate :: Maybe Date
  , currentPage :: { year :: Year, month :: Month }
  , stagedDate :: { month :: String, day :: String, year :: String }
  }

setStagedDate :: Date → State → State
setStagedDate dt st =
  let m = show <<< fromEnum $ month dt
      d = show <<< fromEnum $ day dt
      y = show <<< fromEnum $ year dt
  in st { stagedDate = { month : m, day : d, year : y }}

type Cell = Maybe Int

foreign import setFocus :: ∀ e. Foreign → Eff (dom :: DOM | e) Unit

monthCells :: Year → Month → Array Cell
monthCells y m =
  replicate beginPad Nothing <> map Just (range 1 n) <> replicate endPad Nothing 
  where n        = fromEnum $ lastDayOfMonth y m
        beginPad = mod 7 $ fromEnum $ weekday $ canonicalDate y m bottom 
        endPad   = 35 - n - beginPad

calendar :: ∀ e. H.Component HH.HTML Query Unit Message (Aff (dom :: DOM | e))
calendar =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { selectedDate : Nothing
    , currentPage : { year : fromMaybe bottom $ toEnum 2017, month : November }
    , stagedDate : { month : "", day : "", year : "" }
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [HP.class_ $ ClassName "calendar__container"]
    [ HH.div [HP.class_ $ ClassName "calendar__datePicker"] datePicker 
    , HH.div [HP.class_ $ ClassName "calendar__yearLabel"] [ yearChooser ]
    , HH.div [HP.class_ $ ClassName "calendar__monthLabel"] [ monthChooser ] 
    , HH.div [HP.class_ $ ClassName "calendar__header"] $ map renderHeaderCell $ daysOfTheWeek
    , HH.div [HP.class_ $ ClassName "calendar__body"] $ map renderCell $ monthCells state.currentPage.year state.currentPage.month
    ]
    where
      datePicker =
        [ HH.input 
          [ HE.onValueInput $ HE.input ChangeStagedMonth
          , HP.ref $ H.RefLabel "MonthStage"
          , HP.attr (HH.AttrName "maxlength") "2"
          , HP.placeholder $ show $ fromEnum $ state.currentPage.month
          , HP.value $ state.stagedDate.month
          , HP.class_ $ ClassName "calendar__monthPicker" 
          ]
        , slash
        , HH.input 
          [ HE.onValueInput $ HE.input EnterDay
          , HP.ref $ H.RefLabel "DayStage"
          , HP.attr (HH.AttrName "maxlength") "2"
          , HP.placeholder "dd"
          , HP.value $ state.stagedDate.day
          , HP.class_ $ ClassName "calendar__dayPicker" 
          ]
        , slash
        , HH.input 
          [ HE.onValueInput $ HE.input EnterYear
          , HP.ref $ H.RefLabel "YearStage"
          , HP.attr (HH.AttrName "maxlength") "4"
          , HP.placeholder $ show $ fromEnum $ state.currentPage.year
          , HP.value $ state.stagedDate.year
          , HP.class_ $ ClassName "calendar__yearPicker" 
          ]
        ]
      slash = HH.span_ [ HH.text "/" ]

      monthChooser = HH.select [ HP.class_ $ ClassName "calendar__monthSelect", HE.onValueChange $ HE.input SwitchMonth] $ map monthOption months
      monthOption month = HH.option
        [ HP.selected $ state.currentPage.month == month
        , HP.value $ show $ fromEnum month
        ]
        [ HH.text $ show month ]
        where isSelected = state.currentPage.month == month

      yearChooser = HH.div_
        [ prevYear
        , HH.text $ show $ fromEnum $ state.currentPage.year
        , nextYear
        ]
        where prevYear = HH.button [HP.class_ $ ClassName "calendar__prevButton" , HE.onClick $ HE.input_ PrevYear] [HH.text "<"]
              nextYear = HH.button [HP.class_ $ ClassName "calendar__nextButton" , HE.onClick $ HE.input_ NextYear] [HH.text ">"]

      months :: Array Month
      months = fromNonEmpty cons $ upFromIncluding January

      renderHeaderCell d = HH.div [ HP.class_ $ ClassName "calendar__headerCell" ] [ HH.text $ d ]
      daysOfTheWeek = [ "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" ]

      renderCell c = case c of
        Nothing → HH.div
          [ HP.class_ $ ClassName "calendar__cell__empty"
          ] []
        Just n → HH.a
          [ HP.href "javascript:void(0);"
          , HP.class_ $ ClassName $ if isSelected n then "calendar__cell__selected" else "calendar__cell"
          , HE.onClick $ HE.input_ $ SelectDay n
          , HE.onMouseDown $ HE.input_ $ SelectDay n
          ]
          [ HH.text $ show n ]
          where isSelected n = exactDate state.currentPage.year state.currentPage.month (fromMaybe bottom $ toEnum n) == state.selectedDate

  eval :: ∀ eff. Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | eff))
  eval = case _ of
    SwitchMonth s next → do
       H.modify (\state →
         ( let month = fromString s >>= toEnum # fromMaybe state.currentPage.month
           in state { currentPage = state.currentPage { month = month } }
         )
       )
       pure next

    SelectDay n next -> do
      state <- H.get
      let day = fromMaybe bottom $ toEnum n
          month = state.currentPage.month
          year = state.currentPage.year
          selectedDate = exactDate year month day
          stagedDate =
            { year : show $ fromEnum year
            , month : show $ fromEnum month
            , day : show $ fromEnum day
            }
      H.put state { selectedDate = selectedDate, stagedDate = stagedDate }
      case selectedDate of
        Nothing → pure unit
        Just d → H.raise $ DateSelected d
      pure next

    PrevYear next → do
      H.modify (\state → state {currentPage = state.currentPage { year = fromMaybe state.currentPage.year $ pred state.currentPage.year } } )
      pure next

    NextYear next → do
      H.modify (\state → state {currentPage = state.currentPage { year = fromMaybe state.currentPage.year $ succ state.currentPage.year } } )
      pure next

    ChangeStagedMonth s next → do
      dayStage <- H.getRef (H.RefLabel "DayStage")
      if length s == 2
        then H.liftEff $ fromMaybe (pure unit) $ setFocus <$> dayStage
        else pure unit
      H.modify (\state → state { stagedDate { month = s } } )
      pure next

    EnterDay s next → do
      monthStage <- H.getRef (H.RefLabel "MonthStage")
      yearStage <- H.getRef (H.RefLabel "YearStage")
      if length s == 2
        then H.liftEff $ fromMaybe (pure unit) $ setFocus <$> yearStage
        else pure unit
      if length s == 0
        then H.liftEff $ fromMaybe (pure unit) $ setFocus <$> monthStage
        else pure unit
      H.modify (\state → state { stagedDate { day = s } } )
      handleStageChange
      pure next

    EnterYear s next → do
      dayStage <- H.getRef (H.RefLabel "DayStage")
      if length s == 0
        then H.liftEff $ fromMaybe (pure unit) $ setFocus <$> dayStage
        else pure unit
      H.modify (\state → state { stagedDate { year = s } } )
      handleStageChange
      pure next

    where
      handleStageChange = do
        stagedDate <- H.get <#> (\x -> x.stagedDate) <#> asDate
        case stagedDate of
          Nothing → pure unit
          Just d →  H.modify (\state → state { selectedDate = Just d } )

      asDate stage = do
        m <- fromString stage.month >>= toEnum
        y <- fromString stage.year >>= toEnum
        d <- fromString stage.day >>= toEnum
        exactDate y m d
