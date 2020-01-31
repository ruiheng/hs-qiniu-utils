module Qiniu.PersistOpsSpec where

-- {{{1 imports
import           ClassyPrelude
import           Test.Hspec

import           Control.Monad.Reader
import           Data.Default

import Qiniu
-- }}}1


spec :: Spec
spec = do
  describe "encodeFopCmdList" $ do
    let fop1 = SomePersistFop $ (runReader def (ImageView2Mode1MinSize, ImageView2DimX 400) :: ImageView2)

    it "returns correct result for input 1" $ do
      encodeFopCmdList [(fop1, Nothing)] `shouldBe` "imageView2/1/w/400"

    it "returns correct result for input 2" $ do
      let save_as = SaveAs saveas_entry Nothing
      encodeFopCmdList [(fop1, Just save_as)] `shouldBe` "imageView2/1/w/400|saveas/c2F2ZV90b19idWNrZXQ6c2F2ZV90b19ya2V5"
  where
    -- dummy_bucket = Bucket "bucket"
    -- dummy_rkey = ResourceKey "rkey"
    -- dummy_entry = (dummy_bucket, dummy_rkey)

    saveas_bucket = Bucket "save_to_bucket"
    saveas_rkey = ResourceKey "save_to_rkey"
    saveas_entry = (saveas_bucket, saveas_rkey)
