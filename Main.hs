{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Functor ((<$>))
import           Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack, replace, empty)
import qualified Data.Set as S
import           Text.Pandoc.Options

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Copy Files
    match "files/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= (externalizeUrls $ feedRoot feedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot feedConfiguration)
            >>= loadAndApplyTemplate "templates/base.html" (tagsCtx tags)
            >>= relativizeUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/posts.html" allPostsCtx
                >>= loadAndApplyTemplate "templates/base.html" allPostsCtx
                >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx tags list)
                >>= loadAndApplyTemplate "templates/base.html" (homeCtx tags list)
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title <>
                            constField "body" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/base.html"
                        (constField "title" title <>
                            defaultContext)
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedCtx (take 10 sorted)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderAtom feedConfiguration feedCtx sorted

    -- Read templates
    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" <>
    postCtx

homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list <>
    constField "title" "Index" <>
    field "taglist" (\_ -> renderTagList tags) <>
    defaultContext

feedCtx :: Context String
feedCtx =
    bodyField "description" <>
    postCtx

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags <>
    postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "clrnd's - RSS feed"
    , feedDescription = "Soy un pibe de Lanús al que le gustan los cartogramas y Haskell.\n Estudio Cs. de la Computación en la UBA y el flan me gusta con crema."
    , feedAuthorName  = "Ezequiel A. Alvarez"
    , feedAuthorEmail = "welcometothechango@gmail.com"
    , feedRoot        = "http://clrnd.com.ar"
    }

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -> String -> String
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x

-- TODO: clean me
unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -> String -> String
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x

postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions
