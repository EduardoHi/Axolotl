module Gen where 

import Test.QuickCheck
import Control.Monad
import Numeric


import AxoParser

-- to manually check the generators, use: `sample` e.g. `sample gentIntLit`

genIntLit :: Gen Literal
genIntLit = (IntLit . show) <$> (arbitrary :: Gen Int)


-- small function to avoid scientific notation while printing float value
showFullPrecision x = showFFloat Nothing x ""

genFloatLit :: Gen Literal
genFloatLit = (FloatLit . showFullPrecision) <$> (arbitrary :: Gen Float)


-- not as strong as all possible strings might be more things than alphaNumeric chars
genStringLit :: Gen Literal
genStringLit = StringLit <$> (sized $ \n -> do
                              k <- choose (0,n)
                              replicateM k $ (do
                                ch <- oneof [
                                  choose ('a','z'),
                                  choose ('A','Z'),
                                  choose ('0','9') ]                                  
                                when (ch == '"') discard
                                return ch))
