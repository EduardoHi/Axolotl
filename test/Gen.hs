module Gen where 

import Test.QuickCheck
import Control.Monad

import AxoParser


genIntLit :: Gen Literal
genIntLit = IntLit <$> (sized $ \n ->
                           do k <- choose (1,n)
                              replicateM k $ choose ('0', '9'))
