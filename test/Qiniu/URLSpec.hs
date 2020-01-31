module Qiniu.URLSpec where

-- {{{1 imports
import           ClassyPrelude
import           Data.List.NonEmpty (NonEmpty(..))
import           Test.Hspec

import           Control.Monad.Reader
import           Data.Default

import Qiniu
-- }}}1


spec :: Spec
spec = do
  describe "resourceDownloadUrlX" $ do
    it "without domain or qs or fop" $ do
      let url = resourceDownloadUrlX True Nothing dummy_entry Nothing Nothing
      asText url `shouldBe` "https://bucket.qiniudn.com/rkey"

    it "without domain, with qs, no fop" $ do
      let qs = "abc=1"
      let url = resourceDownloadUrlX True Nothing dummy_entry (Just qs) Nothing
      asText url `shouldBe` "https://bucket.qiniudn.com/rkey?abc=1"

    it "without domain, with qs, fop" $ do
      let qs = "abc=1"
      let fop1 = SomePersistFop $ (runReader def (ImageView2Mode1MinSize, ImageView2DimX 400) :: ImageView2)
      let save_as = SaveAs saveas_entry Nothing
      let url = resourceDownloadUrlX True Nothing dummy_entry (Just qs) (Just ((dummy_secret, dummy_akey), pure (fop1, Just save_as)))
      asText url `shouldBe` "https://bucket.qiniudn.com/rkey?imageView2/1/w/400%7csaveas/c2F2ZV90b19idWNrZXQ6c2F2ZV90b19ya2V5/sign/accesskey:SLZLeB6DRK6gITZ7PZio4_NhEfE=&abc=1"

    it "without domain, with qs, 2 fop" $ do
      let qs = "abc=1"
      let fop1 = SomePersistFop $ (runReader def (ImageView2Mode1MinSize, ImageView2DimX 400) :: ImageView2)
      let fop2 = SomePersistFop $ (runReader def (asText "blabla")) { wmtTitledTextColor = Just "grey", wmtTitledRotate = Just 30 }
      let fop = SomePersistFop $ PersistFopSeries $ fop1 :| [ fop2 ]
      let save_as = SaveAs saveas_entry Nothing
      let url = resourceDownloadUrlX True Nothing dummy_entry (Just qs) (Just ((dummy_secret, dummy_akey), pure (fop, Just save_as)))
      asText url `shouldBe` "https://bucket.qiniudn.com/rkey?imageView2/1/w/400%7cwatermark/4/text/YmxhYmxh/rotate/30%7csaveas/c2F2ZV90b19idWNrZXQ6c2F2ZV90b19ya2V5/sign/accesskey:T8tCaAw4xJ1RdFr-DUf9GaiWpew=&abc=1"

  where
    dummy_bucket = Bucket "bucket"
    dummy_rkey = ResourceKey "rkey"
    dummy_entry = (dummy_bucket, dummy_rkey)

    dummy_secret = SecretKey "thekey"
    dummy_akey = AccessKey "accesskey"

    saveas_bucket = Bucket "save_to_bucket"
    saveas_rkey = ResourceKey "save_to_rkey"
    saveas_entry = (saveas_bucket, saveas_rkey)
