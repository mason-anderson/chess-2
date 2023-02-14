module ImageLoader where

import qualified Data.ByteString as B
import Data.Text
import Data.Vector.Storable.ByteString

import qualified Codec.Picture as Pic
-- import Monomer.Graphics
import Monomer
import Control.Monad

pieces = ["Pawn", "Rook", "Knight", "Bishop", "Queen", "King"]
colors = ["White", "Black"]

loadPieces :: Renderer -> IO ()
loadPieces renderer = do
    let flags = []
    -- let size = Size 60 60
    -- (name, imgBS, size) <- pawnWhite
    -- addImage renderer name size imgBS flags
    imgs <- sequence [readImage (p <> c <> ".png") | p<-pieces, c<-colors]
    forM_ imgs $ \(name, imgBS, size) ->
        addImage renderer name size imgBS flags

readImage :: Text -> IO (Text, B.ByteString, Size)
readImage name = do
    img <- decodeImage <$> B.readFile ( "res/images/" <> unpack name)
    let imgBS = vectorToByteString $ Pic.imageData img
    let h = Pic.imageHeight img
    let w = Pic.imageWidth img
    pure (name, imgBS, Size (fromIntegral w) (fromIntegral h))
    where
        decodeImage :: B.ByteString -> Pic.Image Pic.PixelRGBA8
        decodeImage file = case Pic.decodeImage file of
            Left _ -> error "couldn't decode image"
            Right dimg -> Pic.convertRGBA8 dimg

-- >>> :t Pic.imageData
-- Pic.imageData :: Image a -> Vector (PixelBaseComponent a)

pawnWhite = readImage "PawnWhite.png"
