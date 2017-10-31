module Data.Geometry.Cube
( volume  
, area  
) where  
  
import qualified Data.Geometry.Cuboid as Cuboid
  
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
  
area :: Float -> Float  
area side = Cuboid.area side side side  