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

sectionCompiler :: SectionParams -> Compiler String
sectionCompiler sp = let
        sectionContext es =
            constField (elementsFieldName sp) es `mappend`
            defaultContext
    in do
        elements <- loadAll $ elementPattern sp
        elementTemplate <- loadBody $ elementTemplateId sp
        elementsList <- applyTemplateList elementTemplate defaultContext elements
        sectionTemplate <- loadBody $ sectionTemplateId sp
        sectionData <- load $ sectionId sp
        section <- applyTemplate sectionTemplate (sectionContext elementsList) sectionData
        return $ itemBody section

sectionDefaultParams :: String -> SectionParams
sectionDefaultParams sectionName = SectionParams
    { elementPattern = fromGlob ("blocks/" ++ sectionName ++ "/*.md")
    , elementTemplateId = fromFilePath ("templates/blocks/" ++ sectionName ++ ".html")
    , sectionTemplateId = fromFilePath ("templates/" ++ sectionName ++ ".html")
    , sectionId = fromFilePath ("blocks/" ++ sectionName ++ ".md")
    , elementsFieldName = "elements"
    }

indexContext :: String -> String -> String -> String -> Context String
indexContext t r p f =
    let bodies = [t, r, p, f]
        names = ["technos", "refs", "people", "formations"]
        fields = mconcat $ zipWith constField names bodies
    in fields `mappend` defaultContext

