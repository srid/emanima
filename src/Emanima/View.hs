module Emanima.View where

import Data.Generics.Sum.Any (AsAny (_As))
import Data.IxSet.Typed qualified as Ix
import Ema qualified
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Emanima.Model (Model (modelNotes, modelStatic))
import Emanima.Route
import Emanote.Model.Type qualified as Em
import Emanote.Route.SiteRoute.Type qualified as Em
import Optics.Core (Prism', (%))
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderDashboard :: Prism' FilePath Route -> Model -> LByteString
renderDashboard rp m = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m
      H.body $ do
        renderBody rp m

renderHead :: Prism' FilePath Route -> Model -> H.Html
renderHead rp model = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ "Emanima"
  H.base ! A.href "/"
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

renderBody :: Prism' FilePath Route -> Model -> H.Html
renderBody rp model = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    H.h1 ! A.class_ "text-3xl font-bold" $ "Emanima"
    H.section ! A.class_ "py-4 px-4 my-2 bg-gray-200" $ do
      let notes = Ix.toList (Em._modelNotes $ modelNotes model)
      H.h2 ! A.class_ "font-bold text-xl" $ "Notes we got"
      forM_ notes $ \note -> do
        H.li $ H.span ! A.class_ "font-mono text-xs" $ show note
    let notesIndexUrl = Ema.routeUrl rp $ Route_Html $ HtmlRoute_Notes $ Em.SiteRoute_VirtualRoute Em.VirtualRoute_Index
    H.a ! A.href (H.toValue notesIndexUrl) $
      H.button ! A.class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" $
        "Go to Emanote index"
    H.img ! A.src (staticRouteUrl rp model "logo.svg") ! A.class_ "py-4 w-32" ! A.alt "Ema Logo"

routeLink :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeLink rp r =
  H.a ! A.href (H.toValue $ Ema.routeUrl rp $ Route_Html r)
    ! A.class_ "text-rose-400"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)
