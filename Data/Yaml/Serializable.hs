-- @+leo-ver=4-thin
-- @+node:gcross.20090530015605.17:@thin Serializable.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090530015605.30:<< Language Extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- @-node:gcross.20090530015605.30:<< Language Extensions >>
-- @nl

module Data.Yaml.Serializable where

-- @<< Imports >>
-- @+node:gcross.20090530015605.18:<< Imports >>
import Data.Yaml.Syck
-- @-node:gcross.20090530015605.18:<< Imports >>
-- @nl

-- @<< Class Definitions >>
-- @+node:gcross.20090530015605.20:<< Class Definitions >>
class Serializable a where
    toYamlNode :: a -> YamlNode
    fromYamlNode :: YamlNode -> Either [String] a
    toYamlElem :: a -> YamlElem
    fromYamlElem :: YamlElem -> Either [String] a

    toYamlNode = mkNode . toYamlElem
    fromYamlNode = fromYamlElem . n_elem
-- @-node:gcross.20090530015605.20:<< Class Definitions >>
-- @nl

-- @+others
-- @+node:gcross.20090530015605.21:Instances
-- @+node:gcross.20090530015605.22:Show/Read based
instance (Show a, Read a) => Serializable a where
    toYamlElem = EStr . packBuf . show
    fromYamlElem (EStr buf) =
        let str = unpackBuf $ buf
        in case readsPrec 0 str of
            [] -> Left ["Unable to parse string: " ++ str]
            (x,_):_ -> Right x
    fromYamlElem bad_elem = Left ["Unable to parse:  was expecting a string type, but instead got the following: " ++ show bad_elem]
-- @-node:gcross.20090530015605.22:Show/Read based
-- @-node:gcross.20090530015605.21:Instances
-- @-others
-- @-node:gcross.20090530015605.17:@thin Serializable.hs
-- @-leo
