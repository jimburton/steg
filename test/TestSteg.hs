module TestSteg
    where
import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_buryThenDig :: [Char] -> Property
prop_buryTheDig msg = not (null msg) ==> monadicIO test
    where test = do run $ buryAct ["img.pmg",msg,"img2.pmg"] 
                    readMsg <- run $ dig "img2.pmg"
                    assert $ msg == fromJust readMsg
