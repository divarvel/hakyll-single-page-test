{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text (pack, unpack, replace, empty)

import Hakyll

data SectionParams = SectionParams
    { elementPattern :: Pattern
    , elementTemplateId :: Identifier
    , sectionTemplateId :: Identifier
    , sectionId :: Identifier
    , elementsFieldName :: String
    }

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Compile page elements
    match "blocks/**.md" $ do
        compile pandocCompiler

    -- Compile templates
    match "templates/**" $ compile templateCompiler

    create ["index.html"] $ do
        route idRoute
        compile $ do
            indexTexts <- load "blocks/index.md"
            t <- sectionCompiler $ sectionDefaultParams "technos"
            r <- sectionCompiler $ sectionDefaultParams "refs"
            p <- sectionCompiler $ sectionDefaultParams "people"
            f <- sectionCompiler $ sectionDefaultParams "formations"
            page <- loadAndApplyTemplate "templates/default.html" (indexContext t r p f) indexTexts
            makeItem $ itemBody page

sectionCompiler sp = let
        sectionContext es =
            constField (elementsFieldName sp) es `mappend`
            defaultContext
    in do
        elements <- loadAll $ elementPattern sp
        elementTemplate <- loadBody $ elementTemplateId sp
        elementsList <- applyTemplateList elementTemplate blockContext elements
        sectionTemplate <- loadBody $ sectionTemplateId sp
        sectionData <- load $ sectionId sp
        section <- applyTemplate sectionTemplate (sectionContext elementsList) sectionData
        return section

sectionDefaultParams sectionName = SectionParams
    { elementPattern = fromGlob ("blocks/" ++ sectionName ++ "/*.md")
    , elementTemplateId = fromFilePath ("templates/blocks/" ++ sectionName ++ ".html")
    , sectionTemplateId = fromFilePath ("templates/" ++ sectionName ++ ".html")
    , sectionId = fromFilePath ("blocks/" ++ sectionName ++ ".md")
    , elementsFieldName = "elements"
    }

indexContext :: Item String -> Item String -> Item String -> Item String -> Context String
indexContext t r p f =
    let bodies = map itemBody [t, r, p, f]
        names = ["technos", "refs", "people", "formations"]
        fields = mconcat $ zipWith constField names bodies
    in fields `mappend` defaultContext

metadataContext :: String -> Context a
metadataContext name = field name $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe name $ M.lookup name metadata

blockContext = (metadataContext "image") `mappend` defaultContext
