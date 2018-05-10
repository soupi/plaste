{- | Html templates and pages using Lucid

-}


module Web.Plaste.Html where

import qualified Lucid as H
import Lucid.Html5
import Data.Text (Text)
import Data.Int

type Html = H.Html ()


-- | A page template
showCode :: Int64 -> Text -> Html
showCode id' body =
  doctypehtml_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]

      title_ (H.toHtml $ show id')

      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]

      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/normalize.css" ]
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/skeleton.css"  ]

    body_ $ do
      div_ [class_ "container"] $ do
        div_ [id_ "main"] $ do
          pre_ (H.toHtml body)

