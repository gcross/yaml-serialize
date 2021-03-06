-- @+leo-ver=4-thin
-- @+node:gcross.20090530015605.23:@thin runtests.hs
-- @@language Haskell

module Main where

-- @<< Imports >>
-- @+node:gcross.20090530015605.25:<< Imports >>
import Control.Monad.Trans
import Data.Either.Unwrap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Data.Yaml.Serializable
import Data.Yaml.Syck
-- @-node:gcross.20090530015605.25:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090530015605.26:Tests
tests =
    [   show_read_group
    ,   list_group
    ]
-- @+node:gcross.20090530015605.27:show read group
show_read_group = testGroup "show/read"
    [   testProperty "Double" prop_Double
    ,   testProperty "Int" prop_Int
    ,   testProperty "(Double,Int)" prop_Double_Int
    ,   testCase "parse error" test_parse_error
    ]

prop_Double x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::Double

prop_Int x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::Int

prop_Double_Int x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::(Double,Int)

test_parse_error = do
    liftIO . parseYaml $ "1.0"
    >>=
    assertBool "Is an error reported?" . isLeft . (fromYamlNode :: YamlNode -> Either [String] Int)
-- @-node:gcross.20090530015605.27:show read group
-- @+node:gcross.20090530015605.33:list group
list_group = testGroup "list"
    [   testProperty "Double" prop_list_Double
    ,   testProperty "Int" prop_list_Int
    ,   testProperty "nested Int" prop_list_nested_Int
    ]

prop_list_Double x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::[Double]

prop_list_Int x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::[Int]

prop_list_nested_Int x = (fromYamlNode . toYamlNode $ x) == Right x
    where types = x::[[Int]]
-- @-node:gcross.20090530015605.33:list group
-- @-node:gcross.20090530015605.26:Tests
-- @-others

main = defaultMain tests
-- @nonl
-- @-node:gcross.20090530015605.23:@thin runtests.hs
-- @-leo
