{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( counterComponent
    , opComponent
    , resComponent
    , mapComponent
    , coordComponent
    ) where

import           Data.Monoid                    ((<>))

import           Lubeck.App                     (Html)
import           Lubeck.Forms.Select
import           Lubeck.FRP                     
import           Lubeck.Util                    (showJS)
import qualified Components.Map                 as Map
import           UICombinators


data Action = Inc | Dec

counterComponent :: Int -> FRP (Signal Html, Signal Int)
counterComponent z = do
    (u, es) <- newEvent :: FRP (Sink Action, Events Action)
    let es' = fmap handler es

    model <- accumS z es'

    let v = fmap (view u) model

    pure (v, model)

    where
        handler :: Action -> (Int -> Int)
        handler Inc = (+1)
        handler Dec = \x -> x - 1

        view :: Sink Action -> Int -> Html
        view u x = panel 
                        [ label "(Counter comp.)"
                        , button "-" (const $ u Dec)
                        , label $ showJS x 
                        , button "+" (const $ u Inc) 
                        ]

--------------------------------------------------------------------------------

data Op = Op0 | Op1 | Op2 | Op3 deriving (Eq)

opComponent :: Signal Int -> FRP (Signal Html, Signal Int)
opComponent inp = do
    (u, es) <- newEvent :: FRP (Sink Op, Events Op)

    model <- stepperS Op0 es :: FRP (Signal Op)

    let outp = handler <$> inp <*> model

    let v = fmap (view u) model

    pure (v, outp)

    where
        handler :: Int -> Op -> Int
        handler val Op0 = val
        handler val Op1 = val * val
        handler val Op2 = val * val * val
        handler val Op3 = 0 - val

        view :: Sink Op -> Op -> Html
        view u x = panel 
                        [ label "(Op comp.)"
                        , selectWidget [(Op0, "id"), (Op1, "^2"), (Op2, "^3"), (Op3, "0-")] u x ]                        

--------------------------------------------------------------------------------

resComponent :: Signal Int -> FRP (Signal Html)
resComponent inp = do
    let v = fmap view inp
    pure v

    where
        view :: Int -> Html
        view x = panel [ label "(Result comp.)" 
                       , label $ showJS x ] 

--------------------------------------------------------------------------------

coordComponent ::  Signal (Int, Int) -> FRP (Signal Html, Signal (Double, Double))
coordComponent inp = do
    let outp = fmap convertToCoord inp
    let v = fmap view outp
    
    pure (v, outp)

    where
        view :: (Double, Double) -> Html
        view (lat, lon) = panel 
                            [ label "(Coord. component/Input `mod` 360)"
                            , label $ "Lat: " <> showJS lat
                            , label $ "Lon: " <> showJS lon ]

        convertToCoord :: (Int, Int) -> (Double, Double)
        convertToCoord (x, y) = (fromIntegral (x `mod` 90), fromIntegral (y `mod` 180))

--------------------------------------------------------------------------------

mapComponent :: Signal (Double, Double) -> FRP (Signal Html)                        
mapComponent inp = do
    (v, u, _) <- Map.mapComponent (Map.MapCfg mapTileLayerUrl mapAttribution 15) []
    _ <- reactimateIOS $ fmap (updateMarkers u) inp
    u Map.MapInit
    u Map.InvalidateSize

    pure (fmap view v)

    where
        view :: Html -> Html
        view x = panel [ label "(Map comp.)", x ]

        makeMarkers lat lon = [ Map.Marker (Map.Point (lat - 10) (lon - 10)) Nothing
                              , Map.Marker (Map.Point (lat + 10) (lon + 10)) Nothing
                              , Map.Marker (Map.Point lat lon) Nothing ]

        updateMarkers :: Sink Map.MapCommand -> (Double, Double) -> FRP ()
        updateMarkers u (lat, lon) = do
            u Map.ClearMap
            u Map.InvalidateSize
            u $ Map.AddClusterLayer $ makeMarkers lat lon

        mapTileLayerUrl = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        mapAttribution = "&copy; <a href='http://osm.org/copyright'>OpenStreetMap</a> contributors, Points &copy 2012 LINZ, &copy; Map tiles by MapBox"

--------------------------------------------------------------------------------

-- data MetaAction = More | Less

-- metacounterComponent :: JSString -> (FRP (Signal Html, Signal Int)) 
--                      -> FRP (Signal Html)
-- metacounterComponent title action = do
--     (u, es) <- newEvent :: FRP (Sink MetaAction, Events MetaAction)

--     let es' = fmap handler es :: Events ([FRP (Signal Html)] -> [FRP (Signal Html)])

--     ios       <- accumS ([] :: [FRP (Signal Html)]) es' :: FRP (Signal [FRP (Signal Html)])
--     let ios'  = fmap sequence ios                       :: Signal (FRP [Signal Html])
--     let ios'' = fmap (fmap sequence) ios'               :: Signal (FRP (Signal [Html]))
--     ios'''    <- reactimateIOS ios''                    :: FRP (Signal (Signal [Html]))

--     let model = join ios''' :: Signal [Html]

--     let v = fmap (view u) model

--     pure v

--     where
--         handler :: MetaAction -> ([FRP (Signal Html)] -> [FRP (Signal Html)])
--         handler x = case x of
--             More -> \ys -> ys <> [action] 
--             Less -> \xs -> case xs of 
--                              [] -> []
--                              xs' -> init xs'

--         view :: Sink MetaAction -> [Html] -> Html
--         view u xs = 
--             blockPanel 
--                  [ blockPanel [ button "Less" (const $ u Less) 
--                               , label title
--                               , button "More" (const $ u More) ]
--                  , blockPanel xs ]


